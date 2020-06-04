#' Function to do runs.test and 3 x sigma limits
#'
#' runs test is conducted with library(snpar)
#' @param x residuals from CPUE fits
#' @param type only c("resid","observations")
#' @return runs p value and 3 x sigma limits
#' @export
SSruns_sig3 <- function(x,type=NULL) {
  if(is.null(type)) type="resid"
  if(type=="resid"){mu = 0}else{mu = mean(x, na.rm = TRUE)}
  # Average moving range
  mr  <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  # Calculate control limits
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  if(nlevels(factor(sign(x)))>1){
    runstest = snpar::runs.test(x)
    pvalue = round(runstest$p.value,3)} else {
      pvalue = 0.001
    }
  
  return(list(sig3lim=c(lcl,ucl),p.runs= pvalue))
}

#' plot function for runs test plot 
#'
#' Residual diagnostics with runs test p-value and 3xsigma limits for Indices and meanL
#'
#' @param ss3rep from r4ss::SSgetoutput()$replist1
#' @param subplots optional use of cpue and comp data (only tested for length) 
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param miny  minimum abs values of ylim
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param pdf not tested for TRUE
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param tickEndYr TRUE/FALSE switch to turn on/off extra axis mark at final
#' year in timeseries plots.
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' @param xaxs Choice of xaxs parameter (see ?par for more info)
#' @param yaxs Choice of yaxs parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param legend Add a legend?
#' @param legendloc Location of legend. Either a string like "topleft" or a vector
#' @param legendcex=1 Allows to adjust legend cex
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
#' @param new Create new empty plot window
#' @param add surpresses par() to create multiplot figs
#' @author Henning Winker
#' @export

SSplotRunstest <- function(ss3rep, subplots=c("cpue","comps")[1],
                             plot=TRUE,print=FALSE,png=print,pdf=FALSE,
                             indexselect = NULL,
                             miny = 1,
                             pch=21, lty=1, lwd=2,
                             tickEndYr=FALSE,
                             xlim="default", ylimAdj=1.4,
                             xaxs="i", yaxs="i",
                             xylabs=TRUE,
                             type="o", 
                             legend=TRUE, legendloc="top",
                             legendcex=1,
                             pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
                             plotdir=NULL,
                             filenameprefix="",
                             par=list(mar=c(5,4,1,1)+.1),
                             verbose=TRUE,
                             new=TRUE,
                             add=FALSE){
  
    #-------------------------------------------
    # r4ss plotting functions and coding style
    #-------------------------------------------
    # subfunction to write png files
    if(!add) graphics.off()
  
  
    if(subplots=="cpue"){
    cpue = ss3rep$cpue
    cpue$residuals = ifelse(is.na(cpue$Obs),NA,log(cpue$Obs)-log(cpue$Exp))
    if(is.null(cpue$Fleet_name)){ # Deal with Version control
    cpue$Fleet_name = cpue$Name}
    res = cpue
    }
    
    pngfun <- function(file){
    # if extra text requested, add it before extention in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  
  # subset if indexselect is specified
  if(is.null(indexselect) ==F & is.numeric(indexselect)){
    iname =  unique(res$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    res = res[res$Fleet_name%in%iname,]
  }

    # Define indices
    indices = unique(res$Fleet_name)
    n.indices = length(indices)
    series = 1:n.indices
    
    
    #---------------------------------------
    plot_runs <- function(resid){  
      
      
      
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
      
        
      
      labels=c("Year",             #1
               "Residuals",        #2
               "Log index")        #3
      
      #-------------------------------------------------------------
      # plot_hcxal function
      #-------------------------------------------------------------
      # get stuff from summary output (minimized)
      
    
      # open new window if requested
      if(plot & png==FALSE){
        if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
        
      } else {
        
        if(!add) par(par)
      }
      
      
      # get quantities for plot
      yr <- resid$Yr
      ylab=labels[2]
      
      ### make plot of index fits
      
      # Do runs test
      runstest = runs_sig3(x=as.numeric(resid$residuals),type="resid")
      
      # if no values included in subset, then set ylim based on all values
      ylim=c(min(-miny,runstest$sig3lim[1]*ylimAdj),max(miny,runstest$sig3lim[2]*ylimAdj))
      
      if(xlim[1]=="default") xlim = range(yr)
      
        plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
             ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
        
        
        lims = runstest$sig3lim
        cols =  c(rgb(1,0,0,0.5),rgb(0,1,0,0.5))[ifelse(runstest$p.runs<0.05,1,2)]
        rect(min(resid$Yr-1),lims[1],max(resid$Yr+1),lims[2],col=cols,border=cols) # only show runs if RMSE >= 0.1
        
        abline(h=0,lty=2)
        for(j in 1:length(resid$Yr)){
          lines(c(resid$Yr[j],resid$Yr[j]),c(0,resid$residuals[j]))
        }
        points(resid$Yr,resid$residuals,pch=pch,bg=ifelse(resid$residuals < lims[1] | resid$residuals > lims[2],2,"white"),cex=1)
        if(legend){
        legend(legendloc,paste(resid$Fleet_name[1]),bty="n",y.intersp = -0.2,cex=legendcex)
        }
        #legend("topright", bty='n',
        #       c("Passed","Failed"),pch=15,col=c(rgb(0,1,0,0.5),rgb(1,0,0,0.5)),pt.cex=2,cex=legendcex,y.intersp=0.9 )
        
        axis(1, at=resid$Yr)
        if(tickEndYr) axis(1, at=max(resid$Yr))
        
        axis(2)
        box()
        
      return(runstest)
    } # End of plot_runs function  
    #------------------------------------------------------------
    
    
    if(verbose) cat("Plotting Residual Runs Tests \n")
    if(plot){ 
      # LOOP through fleets
      nfleets=n.indices
      if(print){
        
        runs = NULL
        for(fi in 1:nfleets){
          resid = res[res$Fleet_name==indices[fi],]
          pngfun(paste0("residruns_",indices[fi],".png",sep=""))
          par(par)
          get_runs = plot_runs(resid)    
          dev.off()
          runs = rbind(runs,c(get_runs$p.runs,get_runs$sig3lim))
        } # End of Fleet Loop
      }
      
      
      runs = NULL
      for(fi in 1:nfleets){
        resid = res[res$Fleet_name==indices[fi],]
        if(!add)(par)
        get_runs = plot_runs(resid)    
        runs = rbind(runs,c(get_runs$p.runs,get_runs$sig3lim))
        # End of Fleet Loop
    }   
    }
    runstable = data.frame(Index=indices,runs.p=as.matrix(runs)[,1],Test=ifelse(as.matrix(runs)[,1]<0.05,"Failed","Passed"),sigma3.lo=as.matrix(runs)[,2],sigma3.hi=as.matrix(runs)[,3]) 
    colnames(runstable) = c("Index","runs.p","test","sigma3.lo","sigma3.hi")
    if(verbose) cat(paste0("\n","Runs Test stats by Index:","\n"))
    return(runstable)
} # end of SSplotRuns()
#-----------------------------------------------------------------------------------------

    
    
    
    
    
    
    
    
    
    
    