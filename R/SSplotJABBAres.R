#' SSplotJABBAres() 
#'
#' plots residuals for all indices as boxplot with a loess showing systematic trends
#' 
#' @param ss3rep from r4ss::SSgetoutput()$replist1
#' @param subplots optional use of cpue and comp data (only tested for length) 
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param pdf not tested for TRUE
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param miny  minimum abs values of ylim
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
#' @param boxcol color boxes 
#' @param new Create new empty plot window
#' @param add surpresses par() to create multiplot figs
#' @author Henning Winker
#' @export
SSplotJABBAres<- function(ss3rep=ss3sma,subplots=c("cpue","comps")[1],
                        plot=TRUE,print=FALSE,png=print,pdf=FALSE,
                        indexselect=NULL,
                        miny = 3,
                        col=NULL, 
                        pch=21, lty=1, lwd=2,
                        tickEndYr=TRUE,
                        xlim="default", ylimAdj=1.1,
                        xaxs="i", yaxs="i",
                        xylabs=TRUE,
                        type="o", 
                        legend=TRUE, legendlabels="default", legendloc="bottomleft",
                        legendorder="default",legendncol=1,legendcex=1,legendsp=0.9,legendindex = NULL,
                        pwidth=6.5,pheight=5.0,punits="in",res=300,ptsize=10,cex.main=1,
                        plotdir=NULL,
                        filenameprefix="",
                        par=list(mar=c(5,4,1,1)+.1),
                        verbose=TRUE,
                        boxcol =  grey(0.8,0.5),new=TRUE,
                        add=FALSE){ 
  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if(!add) graphics.off()
  if(add){
    print=F
    png=F
  }
  
  
  if(subplots=="cpue"){
    cpue = ss3rep$cpue
    cpue$residuals = ifelse(is.na(cpue$Obs),NA,log(cpue$Obs)-log(cpue$Exp))
    if(is.null(cpue$Fleet_name)){ # Deal with Version control
      cpue$Fleet_name = cpue$Name}
    Res = cpue
    
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
  resids = reshape2::dcast(Res,Yr~Fleet_name,value.var="residuals")
  indices = unique(Res$Fleet_name)
  n.indices = length(indices)
  series = 1:n.indices
  
  
  
  log=FALSE #(no option to plot on log scale)
  if(is.null(legendindex))  legendindex=series
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
  
  
  #-----------------
  # start plot
  #----------------
  jabbaresiduals <- function(){
    
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
    
    labels=c("Year",             #1
             "Residuals")         #2
               
    # open new window if requested
    if(plot & png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    
    # get quantities for plot
    yr <- resids[,1]
    Resids = t(resids[,-1])
    ylab=labels[2]
    n.years = length(yr)
    
    # setup colors, points, and line types
    if(is.null(col) & n.indices>3)  col <- rc(n.indices+1)[-1]
    if(is.null(col) & n.indices<3)  col <- rc(n.indices)
    if(is.null(col) & n.indices==3) col <- c("blue","red","green3")
    # set pch values if no input
    
    # if line stuff is shorter than number of lines, recycle as needed
    if(!is.expression(legendlabels[1]) &&
       legendlabels[1]=="default") legendlabels <- c(paste(indices),"Loess")
    if(legendorder[1]=="default") legendorder <- 1:(n.indices+1)
    
    # open new window if requested
    if(plot & png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    
    
    ### make plot of index fits
    yrange = ifelse(rep(max(ifelse(abs(Resids)>miny,0,Resids),na.rm = T),2)>0.5,range(ylimAdj*ifelse(abs(Resids)>miny,0,Resids),na.rm = T),range(c(-0.7,0.5)))
    ylim= c(-max(abs(yrange)),max(abs(yrange)))
    
    if(xlim[1]=="default") xlim = range(yr)
    
    plot(0, type = "n", xlim = xlim, yaxs = yaxs, 
         ylim = ylim, xlab = ifelse(xylabs,"Year",""), ylab = ifelse(xylabs,ylab,""), axes = FALSE)
    
    Resids = ifelse(abs(Resids)>3,NA,Resids)
    boxplot(as.matrix(Resids),add=TRUE,at=c(yr),xaxt="n",col=grey(0.8,0.5),notch=FALSE,outline = FALSE,axes=F)
    abline(h=0,lty=2)
    positions=runif(nrow(Resids),-0.2,0.2)
    
    for(i in 1:n.indices){
      for(t in 1:n.years){
        lines(rep((yr+positions[i])[t],2),c(0,Resids[i,t]),col=col[i])}
      points(yr+positions[i],Resids[i,],col=1,pch=pch,bg=col[i])}
    mean.res = apply(Resids,2,mean,na.rm =TRUE)
    smooth.res = predict(loess(mean.res~yr),data.frame(yr))
    lines(yr,smooth.res,lwd=2)
    # get degree of freedom
    Nobs =length(as.numeric(Resids)[is.na(as.numeric(Resids))==FALSE])
    RMSE = round(100*sqrt(mean(Resids^2,na.rm =TRUE)),1)
    rmse.i =  ni = NULL
    for(i in 1:n.indices){
      res.i =sum(Resids[1,]^2,na.rm =TRUE)
      ni[i] =length(as.numeric(Resids[i,])[is.na(as.numeric(Resids[i,]))==FALSE])
      rmse.i[i] = round(100*sqrt(res.i/ni[i]),1)
    }
      
    legend('topright',c(paste0("RMSE = ",RMSE,"%")),bty="n",cex=legendcex+0.1,y.intersp=0.2,x.intersp = 0)
    if(legend) legend(legendloc,legendlabels,bty="n",col=1,pt.cex=1.1,cex=legendcex,pch=c(rep(21,n.indices),-1),pt.bg=c(col,1),lwd=c(rep(-1,n.indices),2))
     axis(1, at=c(min(yr):max(yr)))
    if(tickEndYr) axis(1, at=max(yr))
    axis(2)
    box()
  
    return(data.frame(indices=c(indices,"Combined"),RMSE.perc=c(rmse.i,RMSE),nobs=c(ni,Nobs)))
  } # jabba residual plot  
  #------------------------------------------------------------
  
  if(verbose) cat("Plotting JABBA residual plot \n")
  if(plot){ 
    if(print){
      
         pngfun(paste0("jabbaresidual.png",sep=""))
         par(par)
         rmse = jabbaresiduals()   
        dev.off()
        
        }

      if(!add)(par)
      rmse = jabbaresiduals()# End of Fleet Loop
       
  }
  
  if(verbose) cat(paste0("\n","RMSE stats by Index:","\n"))
  return(rmse)
} # end of SSplotJABBAresids()
#-----------------------------------------------------------------------------------------
