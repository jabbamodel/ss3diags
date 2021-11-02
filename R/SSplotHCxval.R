#' SSplotHCxval() for one-step ahead hindcasting cross-validations of indices
#'
#' Plots one-step ahead hindcasting cross-validations and computes MASE from prediction residuals. 
#' MASE is calculated as the average ratio of mean absolute error (MAE) of prediction residuals (MAE.PR) and naive predictions (MAE.base)
#' MASE.adj sets the MAE.base to a minimum MAE.base.adj (default=0.1)
#' MASE.adj allow passing (MASE<1) if MAE.PE < 0.1 and thus accurate if obs show very little annual variation   
#'
#' @param retroSummary List created by r4ss::SSsummarize() or ss3diags::SSretroComps() 
#' @param subplots optional use of c("cpue","len","age"), yet to be tested for age.
#' @param models Optional subset of the models described in
#' r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param Season option to specify Season - Default uses first available, i.e. usual Seas = 1
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param indexselect = Vector of fleet numbers for each model for which to compare
#' @param MAE.base.adj minimum MASE denominator (naive predictions) for MASE.adj (default = 0.1)   
#' @param show.mase.adj if TRUE it show mase.adj in () in plot
#' @param indexfleets CHECK IF NEEDED or how to adjust indexfleets
#' @param xmin optional number first year shown in plot (if available)  
#' @param indexUncertainty Show fixed uncertainty intervals on index (not estimated)
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param pdf not tested for TRUE
#' @param col Optional vector of colors to be used for lines. Input NULL
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param tickEndYr TRUE/FALSE switch to turn on/off extra axis mark at final
#' year in timeseries plots.
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' @param ylim will over-write ylimAdj if specified
#' @param xaxs Choice of xaxs parameter (see ?par for more info)
#' @param yaxs Choice of yaxs parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param legend Add a legend?
#' @param legendlabels Optional vector of labels to include in legend.
#' @param legendloc Location of legend. Either a string like "topleft" or a vector
#' of two numeric values representing the fraction of the maximum in the x and y
#' dimensions, respectively. See ?legend for more info on the string options.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param legendcex=1 Allows to adjust legend cex
#' @param legendsp Space between legend labels
#' @param legendindex Allows to add legend for selected indices (plots)
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param filenameprefix Additional text to append to PNG or PDF file names.
#' It will be separated from default name by an underscore.
#' @param par list of graphics parameter values passed to par() function
#' @param verbose Report progress to R GUI?
#' @param shadecol uncertainty shading of hcxval horizon
#' @param shadecol1 uncertainty shading of early years not affected by hindcast
#' @param new Create new empty plot window
#' @param add suppresses par() to create multiplot figs
#' @param mcmcVec NOT TESTED Vector of TRUE/FALSE values (or single value) indicating
#' @param indexQlabel Add catchability to legend in plot of index fits (TRUE/FALSE)?
#' @param indexQdigits Number of significant digits for catchability in legend
#' @author Henning Winker (JRC-EC) and Laurence Kell (Sea++)
#' @export
SSplotHCxval<- function(retroSummary,subplots=c("cpue","len","age"),Season="default",
                        print=FALSE,png=print,pdf=FALSE,
                        models="all",
                        endyrvec="default",
                        xmin = NULL,
                        indexselect = NULL,
                        MAE.base.adj=0.1,
                        show.mase.adj = TRUE,
                        indexUncertainty=TRUE,
                        col=NULL, 
                        pch=NULL, lty=1, lwd=2,
                        tickEndYr=TRUE,
                        xlim="default", 
                        ylimAdj=1.15,ylim=NULL,
                        xaxs="i", yaxs="i",
                        xylabs=TRUE,
                        type="o", uncertainty=TRUE, 
                        legend=TRUE, legendlabels="default", legendloc="topright",
                        legendorder="default",legendncol=1,legendcex=1,legendsp=0.9,legendindex = NULL,
                        pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
                        plotdir=NULL,
                        filenameprefix="",
                        par=list(mar=c(5,4,1,1)+.1),
                        verbose=TRUE,
                        shadecol = grey(0.5,0.4),shadecol2=grey(0.5,0.4),new=TRUE,
                        add=FALSE,mcmcVec=FALSE,indexQlabel=TRUE,
                        indexQdigits=4
                        ){ # plot different fits to a single index of abundance
  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if(!add) graphics.off()
  plot=TRUE
  hcruns =  retroSummary #added for now
  pngfun <- function(file){
    # if extra text requested, add it before extention in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  
  

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
  
  
  log=FALSE #(no option to plot on log scale)
  if(is.null(legendindex))  legendindex= 1:hcruns$n
  if(!legend) legendindex=10000
  
  
  plot_hcxval <- function(indexfleets=1){  
  
   
    
    if(png) print <- TRUE
    if(png & is.null(plotdir))
      stop("to print PNG files, you must supply a directory as 'plotdir'")
    
    # check for internal consistency
    if(pdf & png){
      stop("To use 'pdf', set 'print' or 'png' to FALSE.")
    }
    if(pdf){
      if(is.null(plotdir)){
        stop("to write to a PDF, you must supply a directory as 'plotdir'")
      }
      pdffile <- file.path(plotdir,
                           paste0(filenameprefix, "SSplotComparisons_",
                                  format(Sys.time(), '%d-%b-%Y_%H.%M' ), ".pdf"))
      pdf(file=pdffile, width=pwidth, height=pheight)
      if(verbose) cat("PDF file with plots will be:",pdffile,'\n')
      par(par)
    }
    
    # subfunction to add legend
    legendfun <- function(legendlabels,cumulative=FALSE) {
      if(cumulative){
        legendloc="topleft"
      }
      if(is.numeric(legendloc)) {
        Usr <- par()$usr
        legendloc <- list(x = Usr[1] + legendloc[1] * (Usr[2] - Usr[1]),
                          y = Usr[3] + legendloc[2] * (Usr[4] - Usr[3]))
      }
      
      # if type input is "l" then turn off points on top of lines in legend
      legend.pch <- -1
      if(type=="l"){
        legend.pch <- rep(NA,length(pch))
      }
      legend(legendloc, legend=legendlabels[legendorder],
             col=col[legendorder], lty=lty[legendorder],seg.len = 2,
             lwd=lwd[legendorder], pch=legend.pch[legendorder], bty="n", ncol=legendncol,pt.cex=0.7,cex=legendcex,y.intersp = legendsp)
    }
    
    # r4ss Colors
    rc <- function(n,alpha=1){
      # a subset of rich.colors by Arni Magnusson from the gregmisc package
      # a.k.a. rich.colors.short, but put directly in this function
      # to try to diagnose problem with transparency on one computer
      x <- seq(0, 1, length = n)
      r <- 1/(1 + exp(20 - 35 * x))
      g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
      b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
      rgb.m <- matrix(c(r, g, b), ncol = 3)
      rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
    }
    
    labels=c("Year",             #1
             "Index",            #2
             "Log index")        #3
    
      if(subplots[1]=="len") labels[2] = "Mean length"
      if(subplots[1]=="age") labels[2] = "Mean age"
    
    
    #-------------------------------------------------------------
    # plot_hcxal function
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
    # setup colors, points, and line types
    if(is.null(col) & nlines>3)  col <- rc(nlines+1)[-1]
    if(is.null(col) & nlines<3)  col <- rc(nlines)
    if(is.null(col) & nlines==3) col <- c("blue","red","green3")
    if(is.null(shadecol)){
      # new approach thanks to Trevor Branch
      shadecol <- adjustcolor(col, alpha.f=shadealpha)
    }
    # set pch values if no input
    if(is.null(pch)){
      pch <- rep(1:25,10)[1:nlines]
    } else {
      pch <- rep(pch[1],1000)[1:nlines]
    }
    
    # if line stuff is shorter than number of lines, recycle as needed
    if(length(col) < nlines) col <- rep(col,nlines)[1:nlines]
    if(length(pch) < nlines) pch <- rep(pch,nlines)[1:nlines]
    if(length(lty) < nlines) lty <- rep(lty,nlines)[1:nlines]
    if(length(lwd) < nlines) lwd <- rep(lwd,nlines)[1:nlines]
    
    if(!is.expression(legendlabels[1]) &&
       legendlabels[1]=="default") legendlabels <- c("Ref",paste(endyrvec)[-1])
    if(legendorder[1]=="default") legendorder <- 1:nlines
    
    # open new window if requested
    if(plot & png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
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
    if(log){
      obs <- log(obs)
      exp <- log(exp)
      ylab=labels[3]
    }else{
      ylab=labels[2]
    }
    
    # get uncertainty intervals if requested
    if(indexUncertainty){
      
      subset <- indices2$imodel==models[1]&indices2$Use==1
      indexSEvec <- indices2$SE[subset]
      y <- obs[subset]
      if(log){
        uppers <- qnorm(.975,mean=y,sd=indexSEvec)
        lower <- qnorm(.025,mean=y,sd=indexSEvec)
      }else{
        upper <- qlnorm(.975,meanlog=log(y),sdlog=indexSEvec)
        lower <- qlnorm(.025,meanlog=log(y),sdlog=indexSEvec)
      }
      
    }else{
      upper <- NULL
      lower <- NULL
    }
    
    
    if(is.null(xmin)){
      xmin = min(endyrvec)-5} else {
        xmin = min(xmin,min(endyrvec)-3)  
      }
    
    meanQ <- rep(NA,nlines)
    imodel <- models[which(endyrvec==max(endyrvec))[1]]
    subset <- indices2$imodel==imodel & !is.na(indices2$Like) & yr>= xmin
    
    
    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    sub <- !is.na(indices2$Like) & yr>= xmin
    
    if(is.null(ylim)){
    ylim <- ylimAdj*range(c(exp[sub], obs[sub], lower[sub], upper[sub]), na.rm=TRUE)
    # if no values included in subset, then set ylim based on all values
    if(!any(sub)){
      ylim <- ylimAdj*range(exp, obs, lower, upper, na.rm=TRUE)
    }
    
    if(!log){
      # 0 included if not in log space
      ylim <- range(0,ylim*1.1,na.rm = T)
    }
    }
    
    
    # hcxval section
    yr.eval <- c(endyrvec)
    yr.eval <- (sort(yr.eval))
    yr.obs <- yr.eval%in%yr
    pe.eval = which(yr.eval%in%yr)[-1]
    
    if(length(which(yr.eval%in%yr))-length(pe.eval)<1){
      pe.eval = pe.eval[-1]
    } 
    npe <- length(pe.eval)  # number of prection errors
    obs.eval <- rep(NA,length(yr.eval))
    obs.eval[yr.eval%in%yr] = obs[subset][yr[subset] %in%yr.eval]
    nhc = length(endyrvec)-1
    #naive.eval = log(obs.eval[1:nhc])-log(obs.eval[2:(nhc+1)]) # add log for v1.1   
    #npe <- length(naive.eval[is.na(naive.eval)==F])  # number of prection errors
    #scaler = mean(abs(naive.eval[is.na(naive.eval)==F]))
    
    
    if(length(endyrvec[yr%in%endyrvec])>0 & length(which(yr.eval%in%yr))>1){ # ><>
      if(verbose) cat(paste("\n","Computing MASE with",ifelse(npe<(length(endyrvec)-1),"only","all"),
                            npe,"of",length(endyrvec)-1," prediction residuals for Index",indices2$Fleet_name[1]),"\n")
      if(verbose & npe<(length(endyrvec)-1))cat(paste("\n","Warning:  Unequal spacing of naive predictions residuals may influence the interpretation of MASE","\n"))
      
        plot(0, type = "n", xlim = c(max(min(yr),xmin),min(c(max(yr),max(endyrvec)))), yaxs = yaxs, 
             ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
      
      if(!log & yaxs != "i"){
        abline(h = 0, col = "grey")
      }
      Qtext <- rep("(Q =", nlines)
      
      
      
      if(indexUncertainty){
        polygon(c(yr[subset],rev(yr[subset])),c(lower[subset],rev(upper[subset])),col=shadecol,border=shadecol)
        polygon(c(yr[subset & yr<=min(endyrvec)],rev(yr[subset& yr<=min(endyrvec)])),c(lower[subset& yr<=min(endyrvec)],rev(upper[subset& yr<=min(endyrvec)])),col=shadecol2,border=shadecol2)
      }  
      
      lines(yr[subset],obs[subset],pch=21,lty=2,col="white")
      points(yr[subset],obs[subset],pch=21,cex=1.5,bg="white")
      points(yr.eval[pe.eval],obs.eval[pe.eval],pch=21,cex=1.5,bg=(rev(col))[pe.eval-1])
      
      # Plot Reference
      index.i = unique(indices2$Fleet_name)
      x.ref = indices[indices$imodel==imodel & indices$Yr>=xmin & indices$Fleet_name==index.i,]$Yr
      y.ref = indices[indices$imodel==imodel & indices$Yr>=xmin & indices$Fleet_name==index.i,]$Exp
      lines(x.ref,y.ref,col=col[1],lwd=2,lty=1,type=type,pch=16)
      pred.resid = NULL # Note Prediction Residuals
      for(iline in (2:nlines)[!mcmcVec]){
        imodel <- models[iline]
        subset <- indices2$imodel==imodel & yr <= endyrvec[iline]+1 & yr>=xmin
        subset.ref <- indices2$imodel==imodel
        
        if(endyrvec[iline-1]%in%yr){
          x <- yr[subset]
          y <- exp[subset]
          yobs = obs[subset]
          pred.resid = c(pred.resid,log(y[length(x)])-log(yobs[length(x)])) # add log() for v1.1
          
          
          lines(x, y, lwd=lwd[iline],
                lty=lty[iline], col=col[iline], type="l",cex=0.9)
          
          lines(x[(length(x)-1):(length(x))], y[(length(x)-1):(length(x))], lwd=2,
                col=1,lty=2)
          
            points(x[length(x)], y[length(y)],pch=21,
                 bg=col[iline],col=1, type="p",cex=0.9)
        }  
        }
        
      #}
      
      maepr =  mean(abs(pred.resid))
      #nhc = length(endyrvec)-1
      #naive.eval = log(obs.eval[1:nhc])-log(obs.eval[2:(nhc+1)]) # add log for v1.1   
      #npe <- length(naive.eval[is.na(naive.eval)==F])  # number of prection errors
      naive.eval=log(obs.eval[pe.eval])-log(obs.eval[is.na(obs.eval)==F][-(npe+1)])
      scaler = mean(abs(naive.eval))
      
      mase=maepr/scaler
      mase.adj =maepr/(max(scaler,MAE.base.adj))
      
      MASE.i = NULL
      MASE.i = data.frame(Index=unique(indices2$Fleet_name)[1],Season=Season, MASE=mase,MAE.PR=maepr,MAE.base=scaler,MASE.adj=mase.adj,n.eval=npe)
      
      #legendlabels <- c("Ref",rev(yr.eval))
      if(indexQlabel){
        legendlabels2 <- paste(legendlabels, Qtext,
                               format(meanQ, digits=indexQdigits), ")")
      }
      if(legend){
        # add legend if requested
      
        legendfun(legendlabels)
      }
      if(mase==mase.adj | show.mase.adj==FALSE) legend("top",paste0(unique(indices2$Fleet_name)[1],ifelse(length(unique(hcruns$indices$Seas))>1,paste0(".S",Season),""), ": MASE = ",round(mase,2)),bty="n",y.intersp=-0.2,cex=legendcex+0.1)
      if(mase.adj<mase & show.mase.adj==TRUE) legend("top",paste0(unique(indices2$Fleet_name)[1],ifelse(length(unique(hcruns$indices$Seas))>1,paste0(".S",Season),""), ": MASE = ",round(mase,2)," (",round(mase.adj,2),")"),bty="n",y.intersp=-0.2,cex=legendcex+0.1)
      
      
        axis(1, at=c(max(xmin,min(yr)):max(endyrvec)))
        if(tickEndYr) axis(1, at=max(endyrvec))
      
        axis(2)
        box()
    
    } else {
      if(verbose) cat(paste0("\n","No observations in evaluation years to compute prediction residuals for Index ",indices2$Fleet_name[1]),"\n")
      MASE.i = NULL
      MASE.i = data.frame(Index=unique(indices2$Fleet_name)[1],Season=Season, MASE=NA,MAE.PR=NA,MAE.base=NA,MASE.adj=NA,n.eval=0)
      if(png==FALSE & add==FALSE) dev.off()
    }
    return(list(MASE=MASE.i))
  } # End of plot_hcxval function  
  #------------------------------------------------------------
  
  if(verbose) cat("Plotting Hindcast Cross-Validation (one-step-ahead) \n")
  if(plot){ 
    # LOOP through fleets
    nfleets=length(unique(hcruns$indices$Fleet))
    if(print){
      
      MASE = NULL
      for(fi in 1:nfleets){
        legend=F
        if(fi%in%legendindex) legend=TRUE
        indexfleets = unique(hcruns$indices$Fleet)[fi] 
        pngfun(paste0("hcxval_",unique(hcruns$indices$Fleet)[fi],".png",sep=""))
        par(par)
        get_mase = plot_hcxval(indexfleets)$MASE   
        dev.off()
        MASE = rbind(MASE,get_mase)
      } # End of Fleet Loop
    }
    
    
    MASE = NULL
    for(fi in 1:nfleets){
      legend=F
      if(fi%in%legendindex) legend=TRUE
      indexfleets = unique(hcruns$indices$Fleet)[fi] 
      if(!add)(par)
      get_mase = plot_hcxval(indexfleets)$MASE   
      MASE = rbind(MASE,get_mase)
    } # End of Fleet Loop
  }   
  
  
  if(verbose) cat(paste0("\n","MASE stats by Index:","\n"))
  return(MASE)
} # end of SSplotHCxal()
#-----------------------------------------------------------------------------------------
