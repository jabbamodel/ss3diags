#' Function to do runs.test and 3 x sigma limits
#'
#' runs test is conducted with library(randtests)
#' @param x residuals from CPUE fits
#' @param type only c("resid","observations")
#' @param mixing c("less","greater","two.sided"). Default less is checking for postive autocorrelation only    
#' @return runs p value and 3 x sigma limits
#' @export
#' @author Henning Winker (JRC-EC) and Laurence Kell (Sea++)
ssruns_sig3 <- function(x,type=NULL,mixing="less") {
  if(is.null(type)) type="resid"
  if(type=="resid"){
    mu = 0}else{mu = mean(x, na.rm = TRUE)}
  alternative=c("two.sided","left.sided")[which(c("two.sided", "less")%in%mixing)]
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
    # Make the runs test non-parametric
    runstest = randtests::runs.test(x,threshold = 0,alternative = alternative)
    if(is.na(runstest$p.value)) p.value =0.001
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
#' @param mixing c("less","greater","two.sided"). Default less is checking for postive autocorrelation only    
#' @param subplots optional use of c("cpue","len","age"), yet to be tested for age.
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param miny  minimum abs values of ylim
#' @param plot plot to active plot device?
#' @param print print to PNG files? Deprecated. Please use print_plot.
#' @param print_plot Option to print to PNG files
#' @param png png plots. Deprecated, please use use_png
#' @param use_png Draw plots in PNG format
#' @param pdf PDF plots (not tested for TRUE). Deprecated. Please use use_pdf.
#' @param use_pdf option for pdf plots (not tested for TRUE)
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
#' @param legendcex Allows to adjust legend cex
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
#' @param xlim xlim TODO TODO
#' @param xylabs draw x-axis and y-axis TODO TODO
#' @return Runs Test p-values and sig3 limits
#' @author Henning Winker (JRC-EC) and Laurance Kell (Sea++)
#' @importFrom lifecycle deprecated
#' @export

SSplotRunstest <- function(ss3rep=ss3diags::ss3sma,
                           mixing="less",
                           subplots=c("cpue","len","age")[1],
                           plot=TRUE,
                           print=deprecated(),
                           print_plot=FALSE,
                           png=deprecated(),
                           use_png=print_plot,
                           pdf=deprecated(),
                           use_pdf=FALSE,
                           indexselect = NULL,
                           miny = 1,
                           pch=21, 
                           lty=1, 
                           lwd=2,
                           tickEndYr=FALSE,
                           xlim="default", 
                           ylimAdj=1.4,
                           xaxs="i", 
                           yaxs="i",
                           xylabs=TRUE,
                           type="o", 
                           legend=TRUE, 
                           legendloc="top",
                           legendcex=1,
                           pwidth=6.5,
                           pheight=5.0,
                           punits="in",
                           res=300,
                           ptsize=10,
                           cex.main=1,
                           plotdir=NULL,
                           filenameprefix="",
                           par=list(mar=c(5,4,1,1)+.1),
                           verbose=TRUE,
                           new=TRUE,
                           add=FALSE){

  #Parameter DEPRECATION checks 
  if (lifecycle::is_present(print)){
    lifecycle::deprecate_warn("1.0.9","SSplotRunstest(print)","SSplotRunstest(print_plot)")
    print_plot <- print
  }
  
  if(lifecycle::is_present(png)){
    lifecycle::deprecate_warn("1.0.9", "SSplotRunstest(png)","SSplotRunstest(use_png)")
    use_png <- png
  }
  
  if(lifecycle::is_present(pdf)){
    lifecycle::deprecate_warn("1.0.9", "SSplotRunstest(pdf)","SSplotRunstest(use_pdf)")
    use_pdf <- pdf
  }
  
  if(!isTRUE(plot)){
    lifecycle::deprecate_warn(
      when = "1.0.9",
      what = "SSplotRunsTest(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be removed in a future version"
    )
  }
  
  if(!isTRUE(new)){
    lifecycle::deprecate_warn(
      when = "1.0.9",
      what = "SSplotJABBAres(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be removed in a future version"
    )
  }
  
    
    #-------------------------------------------
    # r4ss plotting functions and coding style
    #-------------------------------------------
    # subfunction to write png files
    if(!add) graphics.off()
    if(add){
      print_plot=F
      use_png=F
    }
  
    subplots = subplots[1]
    datatypes= c("Index","Mean length","Mean age")
    ylabel = datatypes[which(c("cpue","len","age")%in%subplots)]
    if(verbose) cat('\n',"Running Runs Test Diagnosics for",datatypes[which(c("cpue","len","age")%in%subplots)],'\n')
    if(subplots=="cpue"){
    cpue = ss3rep$cpue
    cpue$residuals = ifelse(is.na(cpue$Obs) | is.na(cpue$Like),NA,log(cpue$Obs)-log(cpue$Exp))
    
    if(is.null(cpue$Fleet_name)){ # Deal with Version control
    cpue$Fleet_name = cpue$Name}
    Res = cpue
    }
    
    if(subplots=="len" | subplots=="age"){
      comps = SScompsTA1.8(ss3rep,fleet=NULL,type=subplots,plotit = FALSE)$runs_dat
      comps$residuals = ifelse(is.na(comps$Obs),NA,log(comps$Obs)-log(comps$Exp))
      if(is.null(comps$Fleet_name)){ # Deal with Version control
      comps$Fleet_name = comps$Name}
      Res = comps
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
    iname =  unique(Res$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    Res = Res[Res$Fleet_name%in%iname,]
  }

    # Define indices
    indices = unique(Res$Fleet_name)
    n.indices = length(indices)
    series = 1:n.indices
    
    
    
    if(use_png) print_plot <- TRUE
    if(use_png & is.null(plotdir))
      stop("to print PNG files, you must supply a directory as 'plotdir'")
    
    # check for internal consistency
    if(use_pdf & use_png){
      stop("To use 'use_pdf', set 'print_plot' or 'use_png' to FALSE.")
    }
    if(use_pdf){
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
    
    #---------------------------------------
    plot_runs <- function(resid){  
      
      
      
        
      
      labels=c("Year",             #1
               "Residuals",        #2
               "Log index")        #3
      
      
    
      # open new window if requested
      if(plot & use_png==FALSE){
        if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
        
      } else {
        
        if(!add) par(par)
      }
      
      
      # get quantities for plot
      yr <- resid$Yr
      ti <- resid$Time  
      ylab= paste(ylabel,"residuals")
      
      ### make plot of index fits
      
      # Do runs test
      runstest = ssruns_sig3(x=as.numeric(resid$residuals),type="resid",mixing=mixing)
      
      # if no values included in subset, then set ylim based on all values
      ylim=c(min(-miny,runstest$sig3lim[1]*ylimAdj),max(miny,runstest$sig3lim[2]*ylimAdj))
      
      if(xlim[1]=="default") xlim = c(floor(min(ti,yr)-.1),ceiling(max(ti,yr)+0.1))
      
        plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
             ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
        
        
        lims = runstest$sig3lim
        cols =  c(rgb(1,0,0,0.5),rgb(0,1,0,0.5))[ifelse(runstest$p.runs<0.05,1,2)]
        rect(min(resid$Yr-1),lims[1],max(resid$Yr+1),lims[2],col=cols,border=cols) # only show runs if RMSE >= 0.1
        
        abline(h=0,lty=2)
        for(j in 1:length(resid$Yr)){
          lines(c(resid$Time[j],resid$Time[j]),c(0,resid$residuals[j]))
        }
        points(resid$Time,resid$residuals,pch=pch,bg=ifelse(resid$residuals < lims[1] | resid$residuals > lims[2],2,"white"),cex=1)
        if(legend){
        legend(legendloc,paste(resid$Fleet_name[1]),bty="n",y.intersp = -0.2,cex=legendcex+0.1)
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
      if(print_plot){
        
        runs = NULL
        for(fi in 1:nfleets){
          resid = Res[Res$Fleet_name==indices[fi],]
          pngfun(paste0("residruns_",indices[fi],".png",sep=""))
          par(par)
          if(nrow(resid)>3 & (max(resid$Time)-min(resid$Time))>3){
          get_runs = plot_runs(resid)    
          dev.off()
          runs = rbind(runs,c(get_runs$p.runs,get_runs$sig3lim))
          } else {
            runs = rbind(runs,c(NA,NA,NA))}
          
        } # End of Fleet Loop
      }
      
      
      runs = NULL
      for(fi in 1:nfleets){
        resid = Res[Res$Fleet_name==indices[fi],]
        if(nrow(resid)>3 & (max(resid$Time)-min(resid$Time))>3){
        if(!add)(par)
        get_runs = plot_runs(resid)    
        runs = rbind(runs,c(get_runs$p.runs,get_runs$sig3lim))
        # End of Fleet Loop
        } else {
        runs = rbind(runs,c(NA,NA,NA))
        }
    }   
    }
    
    runstable = data.frame(Index=indices,runs.p=as.matrix(runs)[,1],Test=ifelse(is.na(as.matrix(runs)[,1]),"Excluded",ifelse(as.matrix(runs)[,1]<0.05,"Failed","Passed")),sigma3.lo=as.matrix(runs)[,2],sigma3.hi=as.matrix(runs)[,3],type=subplots) 
    colnames(runstable) = c("Index","runs.p","test","sigma3.lo","sigma3.hi","type")
    if(verbose) cat(paste0("\n","Runs Test stats by ",datatypes[which(c("cpue","len","age")%in%subplots)],":","\n"))
    return(runstable)
} # end of SSplotRuns()
#-----------------------------------------------------------------------------------------

#' runs test  
#'
#' Residual diagnostics with runs test p-value and 3xsigma limits for Indices, mean length and mean age
#'
#' @param ss3rep from r4ss::SSgetoutput()$replist1
#' @param mixing c("less","greater","two.sided"). Default less is checking for postive autocorrelation only    
#' @param quants optional use of c("cpue","len","age"), yet to be tested for age.
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param verbose Report progress to R GUI?
#' @return Runs Test p-values and sig3 limits
#' @author Henning Winker (JRC-EC) and Laurance Kell (Sea++)
#' @export

SSrunstest <- function(ss3rep=ss3diags::ss3sma,
                       mixing="less",
                       quants=c("cpue","len","age")[1],
                       indexselect = NULL,
                       verbose=TRUE){
  
  
  datatypes= c("Index","Mean length","Mean age")
  subplots = quants
  ylabel = datatypes[which(c("cpue","len","age")%in%subplots)]
  if(verbose) cat('\n',"Running Runs Test Diagnosics for",datatypes[which(c("cpue","len","age")%in%subplots)],'\n')
  if(subplots=="cpue"){
    cpue = ss3rep$cpue
    cpue$residuals = ifelse(is.na(cpue$Obs) | is.na(cpue$Like),NA,log(cpue$Obs)-log(cpue$Exp))
    
    if(is.null(cpue$Fleet_name)){ # Deal with Version control
      cpue$Fleet_name = cpue$Name}
    Res = cpue
  }
  
  if(subplots=="len" | subplots=="age"){
    comps = SScompsTA1.8(ss3rep,fleet=NULL,type=subplots,plotit = FALSE)$runs_dat
    comps$residuals = ifelse(is.na(comps$Obs),NA,log(comps$Obs)-log(comps$Exp))
    if(is.null(comps$Fleet_name)){ # Deal with Version control
      comps$Fleet_name = comps$Name}
    Res = comps
  }  
  
  
  # subset if indexselect is specified
  if(is.null(indexselect) ==F & is.numeric(indexselect)){
    iname =  unique(Res$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    Res = Res[Res$Fleet_name%in%iname,]
  }
  
  # Define indices
  indices = unique(Res$Fleet_name)
  n.indices = length(indices)
  series = 1:n.indices
  
  
  
  
  #---------------------------------------
  doruns <- function(resid){  
    
    
    # get quantities for plot
    yr <- resid$Yr
    ti <- resid$Time  
    ylab= paste(ylabel,"residuals")
    
    ### make plot of index fits
    
    # Do runs test
    runstest = ssruns_sig3(x=as.numeric(resid$residuals),type="resid",mixing=mixing)
    
    # if no values included in subset, then set ylim based on all values
    lims = runstest$sig3lim

    return(runstest)
  } # End of runs function  
  #------------------------------------------------------------
  
  
  if(verbose) cat("Computing Residual Runs Tests \n")
    # LOOP through fleets
    nfleets=n.indices
    runs = NULL
    for(fi in 1:nfleets){
      resid = Res[Res$Fleet_name==indices[fi],]
      if(nrow(resid)>3 & (max(resid$Time)-min(resid$Time))>3){
        get_runs = doruns(resid)    
        runs = rbind(runs,c(get_runs$p.runs,get_runs$sig3lim))
        # End of Fleet Loop
      } else {
        runs = rbind(runs,c(NA,NA,NA))
      }
    }   
  
  runstable = data.frame(Index=indices,runs.p=as.matrix(runs)[,1],Test=ifelse(is.na(as.matrix(runs)[,1]),"Excluded",ifelse(as.matrix(runs)[,1]<0.05,"Failed","Passed")),sigma3.lo=as.matrix(runs)[,2],sigma3.hi=as.matrix(runs)[,3],type=subplots) 
  colnames(runstable) = c("Index","runs.p","test","sigma3.lo","sigma3.hi","type")
  if(verbose) cat(paste0("\n","Runs Test stats by ",datatypes[which(c("cpue","len","age")%in%subplots)],":","\n"))
  return(runstable)
} # end of SSplotRuns()
#-----------------------------------------------------------------------------------------



    
    
    
    
    
    
    
    
    
    