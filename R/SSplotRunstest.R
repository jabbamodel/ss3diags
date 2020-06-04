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
#' @param resdat residual data.frame
#' @param output.dir directory to save plots
#' @param as.png save as png file of TRUE
#' @param single.plots if TRUE plot invidual fits else make multiplot
#' @param width plot width
#' @param height plot hight
#' @export
plot_runstest <- function(resdat, output.dir=getwd(),as.png=FALSE,single.plots=FALSE,width=NULL,height=NULL){
  
  if(jabba$settings$CatchOnly==FALSE){
    cat(paste0("\n","><> jbplot_runstest()   <><","\n"))
    
    
    years = jabba$yr
    check.yrs = abs(apply(jabba$residuals,2,sum,na.rm=TRUE))
    cpue.yrs = years[check.yrs>0]
    Resids = jabba$residuals
    n.years = length(years)
    n.indices = jabba$settings$nI
    indices = unique(jabba$diags$name)
    series = 1:jabba$settings$nI
    
    
    if(single.plots==TRUE){
      if(is.null(width)) width = 5
      if(is.null(height)) height = 3.5
      for(i in 1:n.indices){
        Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.5, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
        if(as.png==TRUE){png(file = paste0(output.dir,"/ResRunsTests_",jabba$assessment,"_",jabba$scenario,"_",indices[i],".png"), width = width, height = height,
                             res = 200, units = "in")}
        
        if(as.png==TRUE | i==1) par(Par)
        
        
        
        resid = (Resids[i,is.na(Resids[i,])==F])
        res.yr = years[is.na(Resids[i,])==F]
        runstest = runs_sig3(x=as.numeric(resid),type="resid")
        # CPUE Residuals with runs test
        plot(res.yr,rep(0,length(res.yr)),type="n",ylim=c(min(-1,runstest$sig3lim[1]*1.25),max(1,runstest$sig3lim[2]*1.25)),lty=1,lwd=1.3,xlab="Year",ylab=expression(log(cpue[obs])-log(cpue[pred])))
        abline(h=0,lty=2)
        lims = runstest$sig3lim
        cols =  c(rgb(1,0,0,0.5),rgb(0,1,0,0.5))[ifelse(runstest$p.runs<0.05,1,2)]
        rect(min(years-1),lims[1],max(years+1),lims[2],col=cols,border=cols) # only show runs if RMSE >= 0.1
        for(j in 1:length(resid)){
          lines(c(res.yr[j],res.yr[j]),c(0,resid[j]))
        }
        points(res.yr,resid,pch=21,bg=ifelse(resid < lims[1] | resid > lims[2],2,"white"),cex=1)
        legend('topright',paste(indices[i]),bty="n",y.intersp = -0.2,cex=0.8)
        mtext(paste("Year"), side=1, outer=TRUE, at=0.5,line=1,cex=1)
        mtext(expression(log(cpue[obs])-log(cpue[pred])), side=2, outer=TRUE, at=0.5,line=1,cex=1)
        if(as.png==TRUE){dev.off()}
      } # end of loop
    } else { # single.plot = FALSE
      if(is.null(width)) width = 7
      if(is.null(height)) height = ifelse(n.indices==1,5,ifelse(n.indices==2,3.,2.5))*round(n.indices/2+0.01,0)
      Par = list(mfrow=c(round(n.indices/2+0.01,0),ifelse(n.indices==1,1,2)),mai=c(0.35,0.15,0,.15),omi = c(0.2,0.25,0.2,0) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
      if(as.png==TRUE){png(file = paste0(output.dir,"/ResRunsTests_",jabba$assessment,"_",jabba$scenario,".png"), width = 7, height = ifelse(n.indices==1,5,ifelse(n.indices==2,3.,2.5))*round(n.indices/2+0.01,0),
                           res = 200, units = "in")}
      par(Par)
      for(i in 1:n.indices){
        resid = (Resids[i,is.na(Resids[i,])==F])
        res.yr = years[is.na(Resids[i,])==F]
        runstest = runs_sig3(x=as.numeric(resid),type="resid")
        # CPUE Residuals with runs test
        plot(res.yr,rep(0,length(res.yr)),type="n",ylim=c(min(-1,runstest$sig3lim[1]*1.25),max(1,runstest$sig3lim[2]*1.25)),lty=1,lwd=1.3,xlab="Year",ylab=expression(log(cpue[obs])-log(cpue[pred])))
        abline(h=0,lty=2)
        lims = runstest$sig3lim
        cols =  c(rgb(1,0,0,0.5),rgb(0,1,0,0.5))[ifelse(runstest$p.runs<0.05,1,2)]
        rect(min(years-1),lims[1],max(years+1),lims[2],col=cols,border=cols) # only show runs if RMSE >= 0.1
        for(j in 1:length(resid)){
          lines(c(res.yr[j],res.yr[j]),c(0,resid[j]))
        }
        points(res.yr,resid,pch=21,bg=ifelse(resid < lims[1] | resid > lims[2],2,"white"),cex=1)
        legend('topright',paste(indices[i]),bty="n",y.intersp = -0.2,cex=0.8)
        
      }
      mtext(paste("Year"), side=1, outer=TRUE, at=0.5,line=1,cex=1)
      mtext(expression(log(cpue[obs])-log(cpue[pred])), side=2, outer=TRUE, at=0.5,line=1,cex=1)
      if(as.png==TRUE){dev.off()}
    }
    
    
  }else {
    cat(paste0("\n","><> jbplot_runstest() not available CatchOnly=TRUE <><","\n"))
  }
  
} # end of runstest plot function
