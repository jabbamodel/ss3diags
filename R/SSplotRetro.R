#' SSplotRetro() Retrospective-Forecast with one-step ahead hindcasting 
#'
#' Plots retrospective pattern, including (optional) one-step ahead forecast and computes Mohn's Rho 
#' 
#' @param summaryoutput List created by r4ss::SSsummarize() 
#' @param models Optional subset of the models described in
#' r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param subplots Vector of subplots to be created
#' @param plot plot to active plot device? Deprecated, please use show_plot.
#' @param show_plot Option to draw subplots and plot in the interface.
#' @param print print to PNG files? Deprecated. Please use print_plot.
#' @param print_plot Option to print to PNG files
#' @param png png plots. Deprecated, please use use_png
#' @param use_png Draw plots in PNG format
#' @param pdf PDF plots (not tested for TRUE). Deprecated. Please use use_pdf.
#' @param use_pdf option for pdf plots (not tested for TRUE)
#' @param xlim  optional xlim, which overwrites xmin   
#' @param xmin  optional minimum year shown in plot (default first yr)   
#' @param labels yaxis lable for biomass (bony fish and sharks) 
#' @param ylim option to specify ylim range
#' @param forecast if true one-step ahead forecasts are shown in plot
#' @param forecastrho if true one-step ahead forecast rho value is denoted in plot
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
#' @param shadecol1 uncertainty shading of early years not affected by hindcast
#' @param new Create new empty plot window. Deprecated.
#' @param add surpresses par() to create multiplot figs
#' @param mcmcVec NOT TESTED Vector of TRUE/FALSE values (or single value) indicating
#' @param indexQlabel Add catchability to legend in plot of index fits (TRUE/FALSE)?
#' @param indexQdigits Number of significant digits for catchability in legend
#' @param showrho showrho TODO TODO. Defaults to TRUE
#' @param xylabs Draw x and y axis lables (?) TODO TODO. Defaults to TRUE
#' @param uncertainty uncertainty TODO TODO. Defaults to TRUE.
#' @param shadealpha shadealpha TODO TODO. Defalut to 0.3
#' @author Henning Winker (JRC-EC) and Laurance Kell (Sea++)
#' @importFrom lifecycle deprecated
#' @export
SSplotRetro<- function(summaryoutput, 
                       subplots=c("SSB","F"),
                       plot=deprecated(),
                       show_plot=TRUE,
                       print=deprecated(),
                       print_plot=FALSE,
                       png=deprecated(),
                       use_png=print_plot,
                       pdf=deprecated(),
                       use_pdf=FALSE,
                       models="all",
                       endyrvec="default",
                       xlim = NULL,
                       xmin = NULL,
                       labels =NULL,       
                       ylim = NULL,
                       forecast = TRUE,
                       forecastrho = TRUE,
                       showrho  = TRUE,
                       col=NULL, 
                       pch=NULL, 
                       lty=1, 
                       lwd=2,
                       tickEndYr=TRUE,
                       ylimAdj=1.05,
                       xaxs="i",
                       yaxs="i",
                       xylabs=TRUE,
                       type="o", 
                       uncertainty=TRUE, 
                       legend=TRUE, 
                       legendlabels="default", 
                       legendloc="topright",
                       legendorder="default",
                       legendncol=1,
                       legendcex=1,
                       legendsp=0.7,
                       legendindex = NULL,
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
                       shadecol = grey(0.4,0.6),
                       new=TRUE,
                       add=FALSE,
                       mcmcVec=FALSE,
                       #documented params not in usage -ef
                       shadecol1=grey(0.5,0.4),
                       indexQlabel = TRUE,
                       indexQdigits = 4,
                       shadealpha=0.3
){ 
  
  # Parameter Deprecation Checks
  if(lifecycle::is_present(print)){
    lifecycle::deprecate_warn("1.0.9","SSplotRetro(print)","SSplotRetro(print_plot)")
    print_plot <- print
  }
  
  if(lifecycle::is_present(pdf)){
    lifecycle::deprecate_warn("1.0.9","SSplotRetro(pdf)","SSplorRetro(use_pdf)")
    use_pdf <- pdf
  }
  
  if(lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("1.0.9","SSplotRetro(png)","SSplotRetro(use_png)")
    use_png <- png
  }
  
  if(lifecycle::is_present(plot)){
    lifecycle::deprecate_warn("1.0.9","SSplotRetro(plot)","SSplotRetro(show_plot)")
    show_plot <- plot
  }
  
  if(!isTRUE(new)){
    lifecycle::deprecate_warn(
      when = "1.0.9",
      what = "SSplotRetro(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be removed in a future version"
    )
  }
  
  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if(!add) graphics.off()
  
  
  pngfun <- function(file){
    # if extra text requested, add it before extention in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  
  
  if(is.null(legendindex))  legendindex=1:summaryoutput$n
  if(!legend) legendindex=10000
  
  quant = subplots[1] 
  
  #------------------------------------------------------------------
  plot_retro <- function(quant=quant){  
    
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
    
    if(endyrvec[1]=="default"){
      endyrvec <- endyrs-seq(0,n-1,1) 
    }
    
    years         <- min(startyrs):max(endyrvec)
    
    if(quant=="SSB"){
    mu      <- summaryoutput$SpawnBio
    Lower <- summaryoutput$SpawnBioLower
    Upper <- summaryoutput$SpawnBioUpper
    if(is.null(labels)){
      if(summaryoutput$SpawnOutputUnits[1]=="numbers"){
      labels = "Stock fecundity"
      } else {
      labels = "Spawning biomass (t)"
      }}
    }
    
  if(quant=="F"){
    mu      <- summaryoutput$Fvalue
    Lower <- summaryoutput$FvalueLower
    Upper <- summaryoutput$FvalueUpper
    if(is.null(labels)){
      if(strsplit(summaryoutput$FvalueLabels[1],";")[[1]][1]=="_abs_F"){
        labels = "Fishing mortality F"
      } else if(strsplit(summaryoutput$FvalueLabels[1],";")[[1]][1]=="(F)/(Fmsy)"){
        labels = expression(F/F[MSY])
      } else
        labels = "F ratio"
    }
    }
    

    ylab = labels
      
  if(models[1]=="all"){models <- 1:n}

  
  nlines <- length(models) 
    
    
    
    if(length(endyrvec)==1){
      stop("SSplotRequires requires a minimum of one reference and one retro peel")
    }
    
    
    if(is.null(col) & nlines>3)  col <- rc(nlines+1)[-1]
    if(is.null(col) & nlines<3)  col <- rc(nlines)
    if(is.null(col) & nlines==3) col <- c("blue","red","green3")
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
       legendlabels[1]=="default") legendlabels <- c("Ref",paste(endyrvec[-1]))
    if(legendorder[1]=="default") legendorder <- 1:(nlines)
    
    # open new window if requested
    if(show_plot & use_png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    # get quantities for plot
    
    
    
    # get exp 
    exp = mu[mu$Yr%in%years,]  
    
    # get uncertainty intervals if requested
    lower = Lower[Lower$Yr%in%years,]
    upper = Upper[Upper$Yr%in%years,]
      
    
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
    
        
    if(is.null(ylim)) ylim <- c(0,max(ifelse(uncertainty,max(c(unlist(exp[exp$Yr>=xmin,1:nlines]),unlist(upper[upper$Yr>=xmin,1])))*ylimAdj, ylimAdj*max(unlist(exp[exp$Yr>=xmin,1:nlines]))*1.05)))
    if(is.null(xlim)) xlim <- c(max(min(yr),xmin),min(c(max(yr),max(endyrvec+0.5))))
    
    # hindcast section
    yr.eval <- c(endyrvec)
    yr.eval <- (sort(yr.eval))
    nhc = length(endyrvec)-1
    
    
      plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
           ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
      
      imodel <- models[which(endyrvec==max(endyrvec))[1]]
      
      if(uncertainty){
        polygon(c(yr,rev(yr)),c(lower[,imodel],rev(upper[,imodel])),col=shadecol,border=shadecol)
      }  
      
      # Plot Reference
      x.ref = exp$Yr
      y.ref = exp[,imodel]
      lines(x.ref,y.ref,col=col[1],lwd=2,lty=1,pch=16)
      rho.i = fcrho.i = NULL
      for(iline in (2:nlines)[!mcmcVec]){
        imodel <- models[iline]
        subset <- yr <= endyrvec[iline] 
        subsetfc <- yr <= endyrvec[iline]+1
        x <- yr[subset]
        y <- exp[subset,imodel]
        xfc <- yr[subsetfc]
        yfc <- exp[subsetfc,imodel]
        lines(x, y, lwd=lwd[iline], col=col[iline], type="l",cex=0.9)
        if(forecast){
        lines(xfc[(length(xfc)-1):length(xfc)], yfc[(length(xfc)-1):length(xfc)], lwd=1, col=col[iline], type="l",cex=0.9,lty=2)
        points(xfc[length(xfc)], yfc[length(yfc)],pch=21,
                 bg=col[iline],col=1, type="p",cex=0.9)
        }
        rho.i[iline-1] = (y[length(y)]-y.ref[length(y)])/
          y.ref[length(y)]
        fcrho.i[iline-1] = (yfc[length(yfc)]-y.ref[length(yfc)])/
          y.ref[length(yfc)]
        
        }
        
      
      rho =  mean(rho.i)
      fcrho= mean(fcrho.i)
      rho.table = data.frame(type=quant,peel=c(endyrvec[-1],"Combined"),Rho=c(rho.i,rho),ForecastRho=c(fcrho.i,fcrho))
      
      
      if(legend){
        # add legend if requested
        
        legendfun(legendlabels)
      }
      if(showrho) legend("top", paste0("Mohn's rho = ",round(rho,2),ifelse(forecast & forecastrho,paste0("(",round(fcrho,2),")"),"")),bty="n",y.intersp=-0.2,cex=legendcex+0.1)
      
      #axis(1, at=c(max(xmin,min(yr)):max(endyrvec)))
      axis(1)
      if(tickEndYr) axis(1, at=max(endyrvec))
      
      axis(2)
      box()
      
    return(rho.table)
  } # End of plot_retro function  
  #------------------------------------------------------------
  
  if(verbose) cat("Plotting Retrospective pattern \n")
  if(show_plot){ 
    if(print_plot){
      
        pngfun(paste0("retro_",quant,".png",sep=""))
        par(par)
        get_rho = plot_retro(quant)   
        dev.off()
        
      
    }
    
    
      if(!add)(par)
      get_rho = plot_retro(quant)   
    
    
  }   
  
  
  if(verbose) cat(paste0("\n","Mohn's Rho stats, including one step ahead forecasts:","\n"))
  return(get_rho)
} # end of SSplotRetro()
#-----------------------------------------------------------------------------------------
