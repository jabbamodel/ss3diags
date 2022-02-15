#' Residual plot
#'
#' plots residuals for all indices as boxplot (color coded by fleet) with a loess showing systematic trends over time. This function is from the package JABBA. 
#' 
#' @param ss3rep Stock Synthesis output as read by r4SS function SS_output
#' @param subplots string of type of data to plot, 'cpue' for index of abundance data, 'len' or 'age' for length- or age-composition, or 'con' for conditional age-at-length data. 
#' @param seas string indicating how to treat data from multiple seasons
#' 'comb' - combine seasonal data for each year and plot against Yr
#' 'sep' - treat season separately, plotting against Yr.S.
#' If is.null(seas), it is assumed that there is only one season and option 'comb' is used.
#' @param plot Deprecated. Plots (and subplots) are drawn to the active plot device 
#' by default (TRUE), and the option to disable this, via FALSE, is unused.
#' @param print Deprecated. Please use 'print_plot'.
#' @param print_plot Option to print to PNG files
#' @param png Deprecated. please use 'use_png'.
#' @param use_png Draw plots in PNG format
#' @param pdf Deprecated. Please use 'use_pdf'.
#' @param use_pdf option for pdf plots (not tested for TRUE)
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param miny  minimum abs values of ylim
#' @param col Optional vector of colors to be used for lines. Input NULL
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param tickEndYr TRUE/FALSE switch to turn on/off extra axis mark at final
#' year in time series plots.
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' @param xaxs Choice of x-axis parameter (see ?par for more info)
#' @param yaxs Choice of y-axis parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param legend Option to add a legend. TRUE by default.
#' @param legendlabels Optional vector of labels to include in legend.
#' @param legendloc Location of legend. Either a string like "topleft" or a vector
#' of two numeric values representing the fraction of the maximum in the x and y
#' dimensions, respectively. See ?legend for more info on the string options.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param legendcex Allows to adjust legend cex. Defaults to 1.
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
#' @param boxcol color boxes 
#' @param new Deprecated. New plot windows are created by default (TRUE), and the 
#' option to disable this, via FALSE, is unused.
#' @param add supresses par() to create multiplot figs
#' @param xlim Optional, values for x-axis range of years to display on plot. 
#' Default = "default" displays all years of available data. (currently not used)
#' @param xylabs TRUE or FALSE, include x- and y-axis labels
#' 
#' @author Henning Winker (JRC-EC)
#' 
#' @keywords ssplot
#' 
#' @importFrom grDevices grey
#' @importFrom graphics boxplot 
#' @importFrom stats predict loess runif
#' @importFrom lifecycle deprecated
#' 
#' @export
SSplotJABBAres<- function(ss3rep=ss3diags::ss3sma,
                          subplots=c("cpue","len","age", "con")[1],
                          seas = NULL,
                          plot=TRUE,
                          print=deprecated(),
                          print_plot=FALSE,
                          png=deprecated(),
                          use_png=print_plot,
                          pdf=deprecated(),
                          use_pdf=FALSE,
                          indexselect=NULL,
                          miny = 3,
                          col=NULL, 
                          pch=21, 
                          lty=1,
                          lwd=2,
                          tickEndYr=TRUE,
                          xlim="default", 
                          ylimAdj=1.1,
                          xaxs="i", 
                          yaxs="i",
                          xylabs=TRUE,
                          type="o", 
                          legend=TRUE, 
                          legendlabels="default", 
                          legendloc="bottomleft",
                          legendorder="default",
                          legendncol=1,
                          legendcex=1,
                          legendsp=0.9,
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
                          boxcol = grey(0.8,0.5),
                          new=TRUE,
                          add=FALSE){ 

  
  #Parameter DEPRECATION checks 
  if (lifecycle::is_present(print)){
    lifecycle::deprecate_warn("2.0.0","SSplotJABBAres(print)","SSplotJABBAres(print_plot)")
    print_plot <- print
  }
  
  if(lifecycle::is_present(png)){
    lifecycle::deprecate_warn("2.0.0", "SSplotJABBAres(png)","SSplotJABBAres(use_png)")
    use_png <- png
  }
  
  if(lifecycle::is_present(pdf)){
    lifecycle::deprecate_warn("2.0.0", "SSplotJABBAres(pdf)","SSplotJABBAres(use_pdf)")
    use_pdf <- pdf
  }
  
  if(!isTRUE(plot)){
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotJABBAres(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }
  
  if(!isTRUE(new)){
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotJABBAres(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be defunct in a future version"
    )
  }
  
  

  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if(!add) graphics.off()
  if(add){
    print_plot=F
    use_png=F
  }
  
  subplots = subplots[1]
  datatypes= c("Index","Mean length","Mean age", "Conditional Age")
  ylabel = datatypes[which(c("cpue","len","age", "con")%in%subplots)]
  

  if(subplots=="cpue"){
    cpue = ss3rep$cpue
    cpue$residuals = ifelse(is.na(cpue$Obs),NA,log(cpue$Obs)-log(cpue$Exp))
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
  
  if(subplots=="con"){
    cond = SScompsTA1.8(ss3rep,fleet=NULL,type=subplots,plotit = FALSE)$runs_dat
    cond$residuals = ifelse(is.na(cond$Obs),NA,log(cond$Obs)-log(cond$Exp))
    if(is.null(cond$Fleet_name)){ # Deal with Version control
      cond$Fleet_name = cond$Name}
    Res = cond
  }
  
  if(is.null(seas)){
    seas <- 'comb'
    if(length(unique(Res$Seas))>1)
      cat('Warning: combining data from multiple seasons\n')
  }
  
  pngfun <- function(file){
    # if extra text requested, add it before extension in file name
    file <- paste0(filenameprefix, file)
    # open png file
    png(filename=file.path(plotdir,file),
        width=pwidth,height=pheight,units=punits,res=res,pointsize=ptsize)
    # change graphics parameters to input value
    par(par)
  }
  
  if(seas == "comb"){
    Res$Time <- Res$Yr
  }
  
  # subset if indexselect is specified
  if(is.null(indexselect) ==F & is.numeric(indexselect)){
    iname =  unique(Res$Fleet_name)[indexselect]
    if(TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    Res = Res[Res$Fleet_name%in%iname,]
  }
  
  # Define indices
  resids = reshape2::dcast(Res,Time~Fleet,value.var="residuals")
  indices = unique(Res$Fleet_name)
  n.indices = length(indices)
  series = 1:n.indices
  yr = unique(round(resids$Time))
  
  
  log=FALSE #(no option to plot on log scale)
  if(is.null(legendindex))  legendindex=series
  if(!legend) legendindex=10000
  
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
    if(verbose) message("PDF file with plots will be:",pdffile)
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
    if(plot & use_png==FALSE){
      if(!add) dev.new(width=pwidth,height=pheight,pointsize=ptsize,record=TRUE)
      
    } else {
      
      if(!add) par(par)
    }
    
    
    # get quantities for plot
    yr <- resids[,1]
    Resids = t(resids[,-1])
    ylab= paste(ylabel,"residuals")
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
      res.i =sum(Resids[i,]^2,na.rm =TRUE)
      ni[i] =length(as.numeric(Resids[i,])[is.na(as.numeric(Resids[i,]))==FALSE])
      rmse.i[i] = round(100*sqrt(res.i/ni[i]),1)
    }
    
    legend('topright',c(paste0("RMSE = ",RMSE,"%")),bty="n",cex=legendcex+0.1,y.intersp=0.2,x.intersp = 0)
    if(legend) legend(legendloc,legendlabels,bty="n",col=1,pt.cex=1.1,cex=legendcex,pch=c(rep(21,n.indices),-1),pt.bg=c(col,1),lwd=c(rep(-1,n.indices),2))
    axis(1, at=c(min(floor(yr)):max(floor(yr))))
    if(tickEndYr) axis(1, at=max(floor(yr)))
    axis(2)
    box()
    
    return(data.frame(indices=c(indices,"Combined"),RMSE.perc=c(rmse.i,RMSE),nobs=c(ni,Nobs)))
  } # jabba residual plot  
  #------------------------------------------------------------
  
  if(verbose) message("Plotting JABBA residual plot")
  if(plot){ 
    if(print_plot){
      
      pngfun(paste0("jabbaresidual.png",sep=""))
      par(par)
      rmse = jabbaresiduals()   
      dev.off()
      
    }
    
    if(!add)(par)
    rmse = jabbaresiduals()# End of Fleet Loop
    
  }
  
  if(verbose) cat(paste0("RMSE stats by Index:","\n"))
  return(rmse)
} # end of SSplotJABBAresids()
#-----------------------------------------------------------------------------------------
