#' SSplotEnsemble() 
#'
#' Plots model ensembles and forecasts with uncertaity represented by MVLN or MCMC posteriors
#' 
#' @param kb SSdeltaMVLN $kb type output    
#' @param subplots option to "Bratio","Fvalue","SSB", "F", "Recr","Catch" 
#' @param models option to manually subset the models in kb$run  
#' @param ylabs yaxis labels for quants
#' final year of values to show for each model. By default it is set to the
#' @param endyrvec ending year specified in each model.
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
#' @param xlim = NULL range of years
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
#' @author Mostly adopted from r4ss::SSplotComparisons by Taylor et al
#' @export
SSplotEnsemble<- function(kb,
                        subplots=c("stock","harvest","SSB","F","Recr","Catch"),
                        models = "all", 
                        quantiles = c(0.025,0.975),
                        ylabs = NULL,
                        endyrvec="default",
                        plot=TRUE,print=FALSE,png=print,pdf=FALSE,
                        col=NULL, 
                        pch=NULL, lty=1, lwd=2,
                        tickEndYr=FALSE,
                        xlim=NULL, ylimAdj=1.05,
                        xaxs="i", yaxs="i",
                        xylabs=TRUE,
                        type="l", uncertainty=TRUE, 
                        legend=TRUE, legendlabels="default", legendloc="topright",
                        legendorder="default",legendncol=1,legendcex=1,legendsp=0.9,
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
  
  if(is.null(ylabs)){
    ylab.default = TRUE
    ylabs =  c(expression(SSB/SSB[MSY]),expression(F/F[MSY]),"SSB (t)","Fishing Mortality (F)","Recruits ('000s)","Catch (t)")
  } else {
    ylab.default = FALSE
  }
  
  
  refquants=c("stock","harvest","SSB","F","Recr","Catch")
  
  # Check time line
  minyr = max(aggregate(year~run,kb,min)[,2])
  maxyr = min(aggregate(year~run,kb,max)[,2])
  kb = kb[kb$year>=minyr & kb$year<=maxyr,]
  
  quants = subplots
  
  pngfun <- function(file){
    # if extra text requested, add it before extention in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  

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
    n             <- length(unique(kb$run))
    startyrs      <- min(kb$year)
    endyrs        <- max(kb$year)
    years         <- unique(kb$year)
    y = kb[,quant]   
    run = kb$run
    year = kb$year
    
    exp      <- aggregate(y~year+run,kb,median)
    lower    <- aggregate(y~year+run,kb,quantile,quantiles[1])
    upper <- aggregate(y~year+run,kb,quantile,quantiles[2])
    exp$Yr = exp$year
    lower$Yr = lower$year
    upper$Yr = upper$year
    
    if(models[1]=="all") models <- 1:n    
    nlines <- length(models) 
    runs = unique(kb$run)[models]
    
    
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
       legendlabels[1]=="default") legendlabels <- runs
    if(legendorder[1]=="default") legendorder <- 1:(nlines)
    
    # open new window if requested
    if(plot & png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    yr <- years
    
    if(is.null(xlim)) xlim = c(max(min(yr)),max(yr)) 
    xmin = min(xlim)
    ylim <- c(0,max(ifelse(uncertainty,max(upper[upper$Yr>=xmin,"y"])*ylimAdj, ylimAdj*max(exp[exp$Yr>=xmin,"y"])*1.05)))
    
    if(ylab.default){
    ylab = ylabs[which(refquants%in%quant)]} else {
    ylab = ylabs[which(subplots%in%quant)]  
    }
    
    
    plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
         ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
    
    if(uncertainty & quant!="Catch"){
    for(iline in nlines:1){
      
    if(quant%in%c("SSB","stock","harvest","F")){  
       polygon(c(yr,rev(yr)),c(lower[lower$run == runs[iline],"y"],rev(upper[upper$run == runs[iline],"y"])),col=shadecol[iline],border=shadecol)
    } else {
      adj <- 0.2*iline/nlines - 0.1
      arrows(x0=yr+adj, y0=lower[lower$run == runs[iline],"y"],
      x1=yr+adj, y1=upper[upper$run == runs[iline],"y"],
      length=0.02, angle=90, code=3, col=col[iline])
    }}
    }
    
    for(iline in 1:nlines){
      if(quant%in%c("SSB","stock","harvest","F","Catch")){
        lines(yr,exp[exp$run == runs[iline],"y"],col=col[iline],pch=pch[iline],lty=lty[iline],lwd=lwd[iline],type="l")
      } else {
        points(yr,exp[exp$run == runs[iline],"y"],col=col[iline],pch=16,cex=0.8)
      } 
   
    }  
    if(quant == "stock") abline(h=1,lty=2)
    if(quant == "harvest") abline(h=1,lty=2)
    
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
    quant=subplots[s]
    par(par)
    pngfun(paste0("ModelComp_",quant,".png",sep=""))
    plot_quants(quant)
    dev.off()
    }
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
