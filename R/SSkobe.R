#' KOBE phase plot
#'
#' plots the stock status uncertainty over SSB/SSBmsy and F/Fmsy
#'
#' @param kb output from SSdeltaMVLN()$kb
#' @param joint option FALSE shows individual runs
#' @param year option to choose year for kobe, last year is default
#' @param posterior visualization of posterior c("points","kernel")
#' @param xlab graphic parameter
#' @param ylab graphic parameter
#' @param ylim graphic parameter
#' @param xlim graphic parameter
#' @param fill shows color-coded quadrants
#' @param legend graphic parameter
#' @param legendpos legend position 
#' @param legendcex lengend size
#' @param legendruns show legend for run lables
#' @param yr.label show year along trajectory
#' @param yr.int = year interval along trajectory
#' @param verbose Output to R console. Default is TRUE 
#' @return Kobe Quadrant percentages
#' @export
#' @keywords ssplot kobe
#' @importFrom gplots ci2d
#' @importFrom graphics text
#' @importFrom stats median 

SSplotKobe <- function(kb,joint = TRUE,year = NULL,
                       posterior = c("points","kernel"),
                       xlab = expression(SSB/SSB[MSY]),ylab = expression(F/F[MSY]),ylim = NULL,
                       xlim = NULL,fill = TRUE,legend = TRUE,legendpos = "right",legendcex = 0.7, 
                       legendruns = TRUE, yr.label = TRUE, yr.int = 5,verbose=TRUE){
  
  if(is.null(year)) year =max(kb$year)
  trj = aggregate(cbind(stock,harvest)~year,kb,median)
  trj = trj[trj$year<=year,]
  kb = kb[kb$year==year,]
  r = (unique(kb$run)) # runs
  n = length(r)
  
  if(is.null(xlim)) xlim = c(0,max(trj$stock,2.5,quantile(kb$stock,0.99)))
  if(is.null(ylim)) ylim = c(0,max(trj$harvest,2.1,quantile(kb$harvest,0.99)))
  
  plot(1000,1000,type="b", xlim=xlim, ylim=ylim,lty=3,ylab=ylab,xlab=xlab,xaxs="i",yaxs="i")
  c1 <- c(-1,100)
  c2 <- c(1,1)
  # extract interval information from ci2d object
  # and fill areas using the polygon function
  zb2 = c(0,1)
  zf2  = c(1,100)
  zb1 = c(1,100)
  zf1  = c(0,1)
  if(fill){
    polygon(c(zb1,rev(zb1)),c(0,0,1,1),col="green",border=0)
    polygon(c(zb2,rev(zb2)),c(0,0,1,1),col="yellow",border=0)
    polygon(c(1,100,100,1),c(1,1,100,100),col="orange",border=0)
    polygon(c(0,1,1,0),c(1,1,100,100),col="red",border=0)
  }
  lines(c1,c2,lty=3,lwd=0.7)
  lines(c2,c1,lty=3,lwd=0.7)
  
  if(posterior[1]=="kernel"){
    kernelF <- gplots::ci2d(kb$stock,kb$harvest,nbins=151,factor=1.5,ci.levels=c(0.50,0.80,0.75,0.90,0.95),show="none")
    polygon(kernelF$contours$"0.95",lty=2,border=NA,col="cornsilk4")
    polygon(kernelF$contours$"0.8",border=NA,lty=2,col="grey")
    polygon(kernelF$contours$"0.5",border=NA,lty=2,col="cornsilk2")
  } 
  
  # MVN posterior
  if(n>1 & joint==FALSE){
    if(posterior[1]=="points") for(i in 1:n) points(kb$stock[kb$run==r[i]],kb$harvest[kb$run==r[i]],col=sscol(n,0.3)[i],pch=16,cex=0.8)
    for(i in 1:n) points(median(kb$stock[kb$run==r[i]]),median(kb$harvest[kb$run==r[i]]),bg=sscol(n,1)[i],pch=21,cex=1.5,col=1)
  } else {
    if(posterior[1]=="points")  points(kb$stock,kb$harvest,bg=grey(0.6,0.8),pch=21)
  }  
  lines(trj$stock, trj$harvest,lwd=2)
  
  if(yr.label){
    showyr = unique(yr.int*floor(trj$year/yr.int))
    for(i in 1:length(trj$year)){
      if(trj$year[i] %in% showyr){
        points(trj$stock[i], trj$harvest[i],pch=21,bg="white",cex=1)
        text(trj$stock[i]-0.05, trj$harvest[i]-0.07, as.character(trj$year[i]), cex=0.8, col="blue")
      }}
  }
  points(median(kb$stock),median(kb$harvest),bg=0,pch=21,cex=2,lwd=2)
  points(trj$stock[1],trj$harvest[1],bg=0,pch=21,cex=1.7,lwd=2)
  if(legendruns & joint==F){
    legend("topright",paste(r),bty="n",cex=0.8,pch=21,pt.bg=sscol(n,1),col=1,pt.cex=1.5)
  }
  # Get Propability
  b = kb$stock
  f = kb$harvest
  Pr.green = sum(ifelse(b>1 & f<1,1,0))/length(b)*100
  Pr.red = sum(ifelse(b<1 & f>1,1,0))/length(b)*100
  Pr.yellow = sum(ifelse(b<1 & f<1,1,0))/length(b)*100
  Pr.orange = sum(ifelse(b>1 & f>1,1,0))/length(b)*100
  
  out = data.frame(Quadrant=c("Red","Orange","Yellow","Green"),Percent=c(Pr.red,Pr.orange,Pr.yellow,Pr.green))
  
  if(legend==T & fill==T){
    legend(legendpos,
           paste0(round(c(Pr.red,Pr.orange,Pr.yellow,Pr.green),1),"%"),
           lty=c(rep(-1,3)),pch=c(rep(22,3)),pt.bg=c("red","orange","yellow","green"),
           col=1,lwd=1.1,cex=legendcex,pt.cex=c(rep(2.,3)),bty=1,x.intersp = 0.2,y.intersp = 1.4)
  }
  
  if(verbose==TRUE){
    return(out)  
  }
} # end
#}}} 

