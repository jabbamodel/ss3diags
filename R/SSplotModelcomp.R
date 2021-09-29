#' SSplotHCxal() for one-step ahead hindcasting cross-validations of indices
#'
#' Plots one-step ahead hindcasting cross-validations and computes MASE from prediction redisuals 
#' 
#' @param summaryoutput List created by r4ss::SSsummarize() 
#' @param models Optional subset of the models described in
#' r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param subplots option to "SSB","Bratio","Fvalue","Recruits","Index" 
#' @param brp option to set reference point c("msy","btargs")
#' @param fmsy to specify Fvalue as F/Fmsy if so in starter file setting
#' @param ylabs yaxis labels for quants
#' final year of values to show for each model. By default it is set to the
#' @param endyrvec ending year specified in each model.
#' @param xmin = NULL optional number first year shown in plot (if available)  
#' @param indexselect = Vector of fleet numbers for each model for which to compare
#' @param indexfleets CHECK IF NEEDED or how to adjust indexfleets
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
#' @param legendcex Allows to adjust legend cex
#' @param legendsp Space between legend labels
#' @param legendindex Allows to add lengend for selected indices (plots)
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
#' @param shadealpha Transparency adjustment used to make default shadecol
#' @param new Create new empty plot window
#' @param add surpresses par() to create multiplot figs
#' @param mcmcVec NOT TESTED Vector of TRUE/FALSE values (or single value) indicating
#' @param indexQlabel Add catchability to legend in plot of index fits (TRUE/FALSE)?
#' @param indexQdigits Number of significant digits for catchability in legend
#' @author Mostly adopted from r4ss::SSplotComparisons by Taylor et al
#' @export
SSplotModelcomp<- function(summaryoutput=aspm.sma,
                        plot=TRUE,print=FALSE,png=print,pdf=FALSE,
                        models="all",
                        subplots=c("SSB","Bratio","Fvalue","Recruits","Index","RecDevs"),
                        brp = c("msy","btargs"),
                        fmsy= TRUE,
                        ylabs = c("SSB (t)",expression(SSB/SSB[MSY]),"Fishing mortality F","Recruits ('000s)","Index","Recruitment Deviations"),
                        endyrvec="default",
                        xmin = NULL,
                        indexselect = NULL,
                        indexUncertainty=TRUE,
                        col=NULL, 
                        pch=NULL, lty=1, lwd=2,
                        tickEndYr=FALSE,
                        xlim="default", ylimAdj=1.05,
                        xaxs="i", yaxs="i",
                        xylabs=TRUE,
                        type="l", uncertainty=TRUE, 
                        legend=TRUE, legendlabels="default", legendloc="topright",
                        legendorder="default",legendncol=1,legendcex=1,legendsp=0.9,legendindex = NULL,
                        pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
                        plotdir=NULL,
                        filenameprefix="",
                        par=list(mar=c(5,4,1,1)+.1),
                        verbose=TRUE,
                        shadecol = NULL, shadealpha=0.3,new=TRUE,
                        add=FALSE,mcmcVec=FALSE,indexQlabel=TRUE,
                        indexQdigits=4
                        ){ # plot different fits to a single index of abundance
  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if(!add) graphics.off()
  if(add){
    print=F
    png=F
  }
  quant = subplots[1]
  refplots=c("SSB","Bratio","Fvalue","Recruits","Index","RecDevs")
  refline = refline2 = 1
  if(brp[1]!="msy") ylabs[2]  = expression(SSB/SSB[0])
  if(brp[1]!="msy") refline  = summaryoutput$btargs[1]
  #if(fmsy) refline2  = 1 
  if(fmsy) ylabs[3] = expression(F/F[MSY])
  
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
    iname =  unique(summaryoutput$indices$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    summaryoutput$indices = summaryoutput$indices[summaryoutput$indices$Fleet_name%in%iname,]
  }
  
  
  log=FALSE #(no option to plot on log scale)
  if(is.null(legendindex))  legendindex=1:summaryoutput$n
  if(!legend) legendindex=10000
  
  
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
  
  
  #-------------
  #plot Index 
  #-------------
  
  plot_quants <- function(quant="SSB"){  
    
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
      legend.pch <- pch
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
    
    
    
    #-------------------------------------------------------------
    # plot function
    
    # get stuff from summary output (minimized)
    n             <- summaryoutput$n
    startyrs      <- summaryoutput$startyrs
    endyrs        <- summaryoutput$endyrs
    years         <- min(startyrs):max(endyrs)
    if(quant=="SSB"){
    exp      <- summaryoutput$SpawnBio
    lower <- summaryoutput$SpawnBioLower
    upper <- summaryoutput$SpawnBioUpper
    }
    if(quant=="Bratio"){
    exp        <- summaryoutput$Bratio
    lower <- summaryoutput$BratioLower
    upper <- summaryoutput$BratioUpper
    }
    if(quant=="Fvalue"){
    exp       <- summaryoutput$Fvalue
    lower <- summaryoutput$FvalueLower
    upper <- summaryoutput$FvalueUpper
    }
    if(quant=="Recruits"){
      exp       <- summaryoutput$recruits
      lower <- summaryoutput$recruitsLower
      upper <- summaryoutput$recruitsUpper
    }
    if(quant=="RecDevs"){
      exp       <- summaryoutput$recdevs
      lower <- summaryoutput$recdevsLower
      upper <- summaryoutput$recdevsUpper
      for(r in 1:(ncol(exp)-2)){
      exp[,r] <- ifelse(is.na(exp[,r]),0,exp[,r])  
      lower[,r] <- ifelse(is.na(lower[,r]) & is.na(exp[,r])==F,0.01,lower[,r])  
      upper[,r] <- ifelse(is.na(upper[,r]) & is.na(exp[,r])==F,-0.01,upper[,r])  
      } 
      
      base = summaryoutput$recruits
      if(min(base$Yr)<min(exp$Yr)){
      base = base[base$Yr%in%exp$Yr ==FALSE,] 
      base[,1:(ncol(base)-2)] = 0
      exp = rbind(base,exp)
      lower = rbind(base,lower)
      upper = rbind(base,upper)} else {
      exp = exp[exp$Yr>=min(base$Yr),]
      lower = lower[exp$Yr>=min(base$Yr),]
      upper = upper[exp$Yr>=min(base$Yr),]
    }
    }
    if(models[1]=="all") models <- 1:n    
    nlines <- length(models) 
    
    
    if(endyrvec[1]=="default"){
      endyrvec <- endyrs 
    }
    
    exp = exp[exp$Yr<=max(endyrvec) & exp$Yr>=max(startyrs),]
    lower  = lower[lower$Yr<=max(endyrvec) & lower$Yr>=max(startyrs),]
    upper  = upper[upper$Yr<=max(endyrvec) & upper$Yr>=max(startyrs),]
    
    
    # setup colors, points, and line types
    if(is.null(col) & nlines>3)  col <- rc(nlines+1)[-1]
    if(is.null(col) & nlines<3)  col <- c("blue","green4")
    if(is.null(col) & nlines==3) col <- c("blue","red","green4")
    if(is.null(shadecol)){
      # new approach thanks to Trevor Branch
      shadecol <- adjustcolor(col, alpha.f=shadealpha)
    }
    
    # if line stuff is shorter than number of lines, recycle as needed
    if(length(col) < nlines) col <- rep(col,nlines)[1:nlines]
    if(length(pch) < nlines) pch <- rep(pch,nlines)[1:nlines]
    if(length(lty) < nlines) lty <- rep(lty,nlines)[1:nlines]
    if(length(lwd) < nlines) lwd <- rep(lwd,nlines)[1:nlines]
    
    if(!is.expression(legendlabels[1]) &&
       legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)
    if(legendorder[1]=="default") legendorder <- 1:(nlines)
    
    # open new window if requested
    if(plot & png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    
    # Check if uncertainty is measured
    if(uncertainty ==TRUE & sum(exp[,1]-lower[,1])==0){
      if(verbose) cat("No uncertainty estimates available from the provided")
      uncertainty=FALSE
    }
    
    
    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    #ylim <- c(0,max(ifelse(uncertainty,unlist(upper[,1:nlines])*ylimAdj, ylimAdj*unlist(exp[,1:nlines])*1.05)))
    # if no values included in subset, then set ylim based on all values
    
    yr <- exp$Yr
    
    
    if(is.null(xmin)){
      xmin = min(yr)} else {
        xmin = min(xmin,min(endyrvec)-3)  
      }
    
    if(quant!="RecDevs"){
      ylim <- c(0,max(ifelse(uncertainty,max(unlist(upper[upper$Yr>=xmin,1:nlines]))*ylimAdj, ylimAdj*max(unlist(exp[exp$Yr>=xmin,1:nlines]))*1.05)))
    } else {
      ylims <- max(ifelse(uncertainty,max(abs(unlist(upper[upper$Yr>=xmin,1:nlines])))*ylimAdj, ylimAdj*max(abs(unlist(exp[exp$Yr>=xmin,1:nlines])))*1.05))
      ylim = c(-ylims,ylims) 
    }
    
    plot(0, type = "n", xlim = c(max(min(yr),xmin),max(yr)), yaxs = yaxs, 
         ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylabs[which(refplots%in%quant)],""), axes = FALSE)
    
    if(uncertainty){
    for(iline in 1:nlines){
      
    if(quant%in%c("SSB","Fvalue","Bratio")){  
       polygon(c(yr,rev(yr)),c(lower[,iline],rev(upper[,iline])),col=shadecol[iline],border=shadecol[iline])
    } else {
      adj <- 0.2*iline/nlines - 0.1
      arrows(x0=yr+adj, y0=lower[,iline],
      x1=yr+adj, y1=upper[,iline],
      length=0.02, angle=90, code=3, col=col[iline])
    }}
    }
    
    for(iline in 1:nlines){
      if(quant%in%c("SSB","Fvalue","Bratio")){
        lines(yr,exp[,iline],col=col[iline],pch=pch[iline],lty=lty[iline],lwd=lwd[iline],type="l")
      } else {
        points(yr,exp[,iline],col=col[iline],pch=16,cex=0.8)
      } 
   
    }  
    if(quant == "Bratio") abline(h=refline,lty=2)
    if(quant == "Fvalue" & fmsy) abline(h=refline2,lty=2)
    
    if(legend){
      # add legend if requested
      
      legendfun(legendlabels)
    }
    
    #axis(1, at=c(min(xmin,min(yr)):max(endyrvec)))
    axis(1)
    
    if(tickEndYr) axis(1, at=max(endyrvec))
    
    axis(2)
    box()
  } # End of plot_quant function  
  legend.temp = legend  
  # Do plotting
  if(plot){ 
    # subplots
    for(s in 1:length(subplots)){
    if(print){
    if(subplots[s]!="index"){  
    quant=subplots[s]
    par(par)
    pngfun(paste0("ModelComp_",quant,".png",sep=""))
    plot_quants(quant)
    dev.off()
    }else{  
      if(subplots[s]=="index") nfleets=length(unique(summaryoutput$indices$Fleet))
      for(fi in 1:nfleets){
        legend=F
        if(fi%in%legendindex) legend=TRUE
        indexfleets = unique(summaryoutput$indices$Fleet)[fi] 
        pngfun(paste0("FitsIndex_",unique(summaryoutput$indices$Fleet)[fi],".png",sep=""))
        par(par)
        plot_index(indexfleets)   
        dev.off()
        legend = legend.temp 
      } # End of Fleet Loop
    }}
    }
    # subplots
    for(s in 1:length(subplots)){
      if(verbose) cat(paste0("\n","Plot Comparison of ",subplots[s],"\n"))
    if(subplots[s]!="Index"){  
    if(!add)par(par)
    quant=subplots[s]
    plot_quants(quant)   
    }else{  
       nfleets=length(unique(summaryoutput$indices$Fleet))
      
        for(fi in 1:nfleets){
        legend=F
        if(fi%in%legendindex) legend=TRUE
        indexfleets = unique(summaryoutput$indices$Fleet)[fi] 
        if(!add)par(par)
        plot_index(indexfleets)   
        legend = legend.temp 
      } # End of Fleet Loop
    }
  }  
  } # endplot

} # end of SSplotModelcomp()
#-----------------------------------------------------------------------------------------





#' Plot Index
#'
#' Index of a plot (?)
#' 
#' @param indexfleets indexfleets. Defaults to 1
#' 
#' @importFrom stats dnorm
#' 
#' @export
plot_index <- function(indexfleets=1){  
  
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
  
  #-------------------------------------------------------------
  # plot_index function
  #-------------------------------------------------------------
  # get stuff from summary output (minimized)
  n             <- summaryoutput$n
  nsexes        <- summaryoutput$nsexes
  startyrs      <- summaryoutput$startyrs
  endyrs        <- summaryoutput$endyrs
  indices <- summaryoutput$indices
  
  
  if(models[1]=="all") models <- 1:n    
  nlines <- length(models) 
  
  if(endyrvec[1]=="default"){
    endyrvec <- endyrs 
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
  if(is.null(col) & nlines<3)  col <- c("blue","green4")
  if(is.null(col) & nlines==3) col <- c("blue","red","green4")
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
     legendlabels[1]=="default") legendlabels <- paste("model",1:nlines)
  if(legendorder[1]=="default") legendorder <- 1:nlines
  
  # open new window if requested
  if(plot & png==FALSE){
    if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
    
  } else {
    
    if(!add) par(par)
  }
  
  
  indices2 <- NULL
  for(iline in 1:nlines){
    imodel <- models[iline]
    subset1 <- indices$imodel==imodel & !is.na(indices$Like)
    subset2 <- indices$imodel==imodel
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
  # get quantities for plot
  yr <- indices2$Yr
  obs <- indices2$Obs
  exp <- indices2$Exp
  imodel <- indices2$imodel
  se <- indices2$SE
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
    
    indexSEvec <- indices2$SE
    if(log){
      uppers <- qnorm(.975,mean=y,sd=indexSEvec)
      lower <- qnorm(.025,mean=y,sd=indexSEvec)
    }else{
      upper <- qlnorm(.975,meanlog=log(obs),sdlog=indexSEvec)
      lower <- qlnorm(.025,meanlog=log(obs),sdlog=indexSEvec)
    }
    
  }else{
    upper <- NULL
    lower <- NULL
  }
  
  ### make plot of index fits
  # calculate ylim (excluding dummy observations from observed but not expected)
  sub <- !is.na(indices2$Like)
  ylim <- ylimAdj*range(exp, obs[sub], lower[sub], upper[sub], na.rm=TRUE)
  # if no values included in subset, then set ylim based on all values
  
  
  if(!any(sub)){
    ylim <- ylimAdj*range(exp, obs, lower, upper, na.rm=TRUE)
  }
  if(!log){
    # 0 included if not in log space
    ylim <- range(0,ylim*1.1)
  }
  
  if(is.null(xmin)){
    xmin = min(startyrs)} 
  
  meanQ <- rep(NA,nlines)
  
  
  plot(0, type = "n", xlim = c(max(min(yr),xmin),min(c(max(yr),max(endyrvec)))), yaxs = yaxs, 
       ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
  
  if(!log & yaxs != "i"){
    abline(h = 0, col = "grey")
  }
  Qtext <- rep("(Q =", nlines)
  
  
  for(iline in 1:nlines){
    adj <- 0.2*iline/nlines - 0.1
    imodel <- models[iline]
    subset <- indices2$imodel==imodel & !is.na(indices2$Like) & yr>= xmin
    subexp <- indices2$imodel==imodel  & yr>= xmin
    if(iline==1){
      if(indexUncertainty){
        arrows(x0=yr[subset], y0=lower[subset],
               x1=yr[subset], y1=upper[subset],
               length=0.02, angle=90, code=3, col=1)
      }
      points(yr[subset],obs[subset],pch=21,cex=1,bg="white")
    }
    lines(yr[subexp],exp[subexp],lwd=lwd,col=col[iline])
    
  }
  if(quant=="Bratio") abline(h=1,lty=2)
  # Plot Reference
  
  
  #legendlabels <- c("Ref",rev(yr.eval))
  if(indexQlabel){
    legendlabels2 <- paste(legendlabels, Qtext,
                           format(meanQ, digits=indexQdigits), ")")
  }
  if(legend){
    # add legend if requested
    
    legendfun(legendlabels)
  }
  legend("top",paste0(unique(indices2$Fleet_name)[1]),bty="n",y.intersp=-0.2,cex=legendcex+0.1)
  
  
  
  
  axis(1, at=c(max(xmin,min(yr)):max(endyrvec)))
  if(tickEndYr) axis(1, at=max(endyrvec))
  
  axis(2)
  box()
  
} # End of plot_index function  
#------------------------------------------------------------
