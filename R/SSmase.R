#' SSmase() computes MASE for one-step ahead hindcasting cross-validations of indices
#'
#' MASE for one-step ahead hindcasting cross-validations and computes MASE from prediction redisuals. 
#' MASE is calculated the average ratio of mean absolute error (MAE) of prediction residuals (MAE.PR) and Naive Predictions (MAE.base)
#' MASE.adj sets the MAE.base to a minimum MAE.base.adj (default=0.1)
#' MASE.adj allow passing (MASE<1) if MAE.PE < 0.1 and thus accurate, when obs show extremely little variation   
#'
#' @param retroSummary List created by r4ss::SSsummarize() 
#' @param quants data type c("cpue","len","age)
#' @param models Optional subset of the models described in
#' r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param Season option to specify Season - Default uses first available, i.e. usual Seas = 1
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param indexselect = Vector of fleet numbers for each model for which to compare
#' @param indexfleets CHECK IF NEEDED or how to adjust indexfleets. TODO: Clarifications? -ef
#' @param MAE.base.adj minimum MASE demoninator (naive predictions) for MASE.adj (default = 0.1)   
#' @param verbose Report progress to R GUI?
#' @param residuals TODO! Default is FALSE.
#' @return MASE and hcxval statistic
#' @author Henning Winker (JRC-EC) and Laurence Kell (Sea++)
#' @export
SSmase<- function(retroSummary,quants=c("cpue","len","age"),Season="default",
                        models="all",endyrvec="default",indexselect = NULL,MAE.base.adj=0.1,residuals=FALSE,
                        verbose=FALSE, indexfleets=1
                        ){ 
  
  hcruns =retroSummary #added for now
  xmin = NULL
  subplots = quants[1]
  if(is.null(hcruns$indices) & subplots[1] == "cpue"){
     stop("Require input object from r4ss::SSsummarize()") 
   }  
  
  if(subplots[1] %in% c("len","age")){
    if(is.null(hcruns$age) & is.null(hcruns$len)){
    stop("Require input object from ss3diags::SSdiagsComps") 
  }}  
  
  if(subplots[1]=="len"){
    if(is.null(hcruns$len)) stop("No Length Comps found")
    hcruns$indices = hcruns$len
  }
    
  if(subplots[1]=="age"){
    if(is.null(hcruns$age)) stop("No Age Comps found")
    hcruns$indices = hcruns$age
  }
    
  # subset if indexselect is specified
  if(is.null(indexselect) ==F & is.numeric(indexselect)){
    iname =  unique(hcruns$indices$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    hcruns$indices = hcruns$indices[hcruns$indices$Fleet_name%in%iname,]
  }
  
  mase <- function(indexfleets=1){  
    #-------------------------------------------------------------
    # mase function
    #-------------------------------------------------------------
    # get stuff from summary output (minimized)
    n             <- hcruns$n
    startyrs      <- hcruns$startyrs
    endyrs        <- hcruns$endyrs
    indices       <- hcruns$indices
    
    if(models[1]=="all") models <- 1:n    
    nlines <- length(models) 
    
    if(endyrvec[1]=="default"){
      endyrvec <- endyrs-seq(0,n-1,1) 
    }
    if(length(endyrvec)==1){
      stop("SSplotHCxval requires a minimum of one reference and one retro peel")
    }
    # check length of indexfleets
    if(!is.null(indexfleets) && length(indexfleets) < n){
      if(length(indexfleets)==1){
        indexfleets <- rep(indexfleets, n)
      }else{
        warning("'indexfleets' needs to have length either 1 or n=",n,"\n",
                "with each value a fleet number for the index to compare.\n")
        indexfleets <- NULL
      }
    }
    
    # Exclude all Time steps not use in reference run replist1
    if(subplots[1]%in%c("len","age")){
      indices$Use =  ifelse(is.na(indices$Like),-1,1)
    }
    RefUse = indices[indices$imodel==1 & indices$Use==1,]
    RefUse = paste0(RefUse$Fleet_name,".",RefUse$Time)
    indices = indices[paste0(indices$Fleet_name,".",indices$Time)%in%RefUse,]
    
    indices2 <- NULL
    for(iline in 1:nlines){
      imodel <- models[iline]
      subset1 <- indices$imodel==imodel & !is.na(indices$Like) & indices$Use == 1
      subset2 <- indices$imodel==imodel #& indices$Use == 1 #><>
      if(length(unique(indices$Fleet[subset1])) > 1){
        if(!is.null(indexfleets[imodel])){
          ifleet <- indexfleets[imodel]
          indices2 <- rbind(indices2,indices[subset2 & indices$Fleet==ifleet,])
        }else{
          if(verbose){cat("some models have multiple indices, 'indexfleets' required\n",
              "to compare fits to indices.\n")}
          return()
        }
      }else{
        indices2 <- rbind(indices2,indices[subset2,])
      }
    }
    
    
    # Subset by month
    if(Season=="default"){
                       Season = unique(indices2$Seas)[1]  
                       if(verbose & length(unique(indices2$Seas))>1){cat("Taking Season",Season,"by default for Index",unique(indices2$Fleet_name))}
                       
    } else {
      Season = as.numeric(Season)[1]
      if(is.na(Season)) stop("Season must a default or and or the integer of indices$Seas 1,2,3,4")
    }
    
    indices       <- indices[indices$Seas==Season,]
    indices2       <- indices2[indices2$Seas==Season,]
    # get quantities for plot
    yr <- indices2$Yr
    obs <- indices2$Obs
    exp <- indices2$Exp
    imodel <- indices2$imodel
    Q <- indices2$Calc_Q
    
    meanQ <- rep(NA,nlines)
    imodel <- models[which(endyrvec==max(endyrvec))[1]]
    xmin = min(endyrvec)-5
    subset <- indices2$imodel==imodel & !is.na(indices2$Like) & yr>= xmin
    
    
    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    sub <- !is.na(indices2$Like) & yr>= xmin
    
    
    # hcxval section
    yr.eval <- c(endyrvec)
    yr.eval <- (sort(yr.eval))
    yr.obs <- yr.eval%in%yr
    pe.eval = which(yr.eval%in%yr)
    
    if(length(which(yr.eval%in%yr))-length(pe.eval)<1){
      pe.eval = pe.eval[-1]
    } 
    npe <- length(pe.eval)  # number of prection errors
    obs.eval <- rep(NA,length(yr.eval))
    obs.eval[yr.eval%in%yr] = obs[subset][yr[subset] %in%yr.eval]
    #if(length(obs.eval)>length(pe.eval)) obs.eval=obs.eval[-1] # first non NA = NA
    nhc = length(endyrvec)-1
    
    
    if(length(endyrvec[yr%in%endyrvec])>0 & length(which(yr.eval%in%yr))>1){ # ><>
      if(verbose) cat(paste("\n","Computing MASE with",ifelse(npe<(length(endyrvec)-1),"only","all"),
                            npe,"of",length(endyrvec)-1," prediction residuals for Index",indices2$Fleet_name[1]),"\n")
      if(verbose & npe<(length(endyrvec)-1))cat(paste("\n","Warning:  Unequal spacing of naive predictions residuals may influence the interpretation of MASE","\n"))
      
      index.i = unique(indices2$Fleet_name)
      pred.resid = NULL # Note Prediction Residuals
      for(iline in (2:nlines)){
        imodel <- models[iline]
        subset <- indices2$imodel==imodel & yr <= endyrvec[iline]+1 & yr>=xmin
        subset.ref <- indices2$imodel==imodel
        
        if(endyrvec[iline-1]%in%yr){
          x <- yr[subset]
          y <- exp[subset]
          yobs = obs[subset]
          pred.resid = c(pred.resid,log(y[length(x)])-log(yobs[length(x)])) # add log() for v1.1
          
                  }  
        }
        
      #}
      
      if(length(pred.resid)>length(pe.eval)) pred.resid=pred.resid[-1]
      maepr =  mean(abs(pred.resid))
      #nhc = length(endyrvec)-1
      #naive.eval = log(obs.eval[1:nhc])-log(obs.eval[2:(nhc+1)]) # add log for v1.1   
      #npe <- length(naive.eval[is.na(naive.eval)==F])  # number of prection errors
      naive.eval=log(obs.eval[pe.eval])-log(obs.eval[is.na(obs.eval)==F][-(npe+1)])
      scaler = mean(abs(naive.eval))
      
      mase=maepr/scaler
      mase.adj = maepr/max(scaler,MAE.base.adj) 
      MASE.i = res.i = NULL
      MASE.i = data.frame(Index=unique(indices2$Fleet_name)[1],Season=Season, MASE=mase,MAE.PR=maepr,MAE.base=scaler,MASE.adj=mase.adj,n.eval=npe)
      res.i = data.frame(Index=rep(unique(indices2$Fleet_name)[1],length(pred.resid)),Season=rep(Season,length(pred.resid)),Year=yr.eval[pe.eval],Pred.Res=pred.resid,Naive.Res=naive.eval,n.eval=npe)
      
      
    } else {
      if(verbose) cat(paste0("\n","No observations in evaluation years to compute prediction residuals for Index ",indices2$Fleet_name[1]),"\n")
      MASE.i = res.i = NULL
      MASE.i = data.frame(Index=unique(indices2$Fleet_name)[1],Season=Season, MASE=NA,MAE.PR=NA,MAE.base=NA,MASE.adj=NA,n.eval=0)    }
   
    out = list(MASE=MASE.i,Residuals=res.i)
       
    return(out)
  } # End of mase function  
  #------------------------------------------------------------
  
    # LOOP through fleets
    nfleets=length(unique(hcruns$indices$Fleet))
      
      MASE =  Residuals = NULL
      for(fi in 1:nfleets){
        indexfleets = unique(hcruns$indices$Fleet)[fi] 
        get_mase = mase(indexfleets)  
        MASE = rbind(MASE,get_mase$MASE)
        Residuals = rbind(Residuals,get_mase$Residuals)
      } # End of Fleet Loop
      
    # Add new joint MASE  
    jstats = apply(abs(Residuals[c("Pred.Res","Native.Res")]),2,mean)
    joint = data.frame(Index="joint",Season="",
                       MASE=jstats[1]/jstats[2],MAE.PR=jstats[1],MAE.base=jstats[2],
                       MASE.adj=jstats[1]/max(jstats[2],MAE.base.adj),n.eval=nrow(Residuals))  
    MASE = rbind(MASE,joint)
    rownames(MASE) = 1:nrow(MASE)
    
 if(verbose) cat(paste0("\n","MASE stats by Index:","\n"))
  ret = MASE
  if(residuals) ret = list(MASE=MASE,Residuals=Residuals) 
  return(ret)
} # end of SSmase()
#-----------------------------------------------------------------------------------------
