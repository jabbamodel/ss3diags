

#' SSplot_eqcurves() 
#'
#' function to plot equilibrium yield curves
#'
#' @param ss3rep from r4ss::SS_output
#' @param Fref  Choice of Fratio c("MSY","Btgt","SPR","F01"), correponding to F_MSY and F_Btgt                                                               
#' @return base plot
#' @author Henning Winker (GFCM)
#' @export
#' @examples 
#' # MSY refpoints with pretty good yield 80% line
#' SSplot_eqcurves(ss3sma,Fref="MSY",msyline=0.8) 
#' # SSB40 refpoints 
#' SSplot_eqcurves(ss3phk,Fref="Btgt") 
#' # SPR40 refpoints 
#' SSplot_eqcurves(ss3phk,Fref="SPR") 
SSplot_eqcurves = function(ss3rep,Fref=c("MSY","Btgt","SPR","F01")[2],plot=TRUE,verbose=TRUE, msyline=NULL,cex=0.7){ 
  
  eq = ss3rep$equil_yield
  b = eq$SSB
  f = eq$F_report
  y = eq$Tot_Catch

  hat = ss3rep$derived_quants
  b0 =hat[hat$Label%in%c("SSB_unfished","SSB_Unfished"),2]
  r0 = hat[hat$Label%in%c("Recr_unfished","Recr_Unfished"),2]
  spr0 = b0/r0
  fmsy = hat[hat$Label%in%c("Fstd_MSY","annF_MSY"),2]
  if(Fref=="SPR"){
    ftgt = hat[hat$Label%in%c("Fstd_SPR","annF_SPR"),2]
    trg=paste0("SPR",round(ss3rep$sprtarg*100))
    if(ss3rep$sprtarg<0)trg=paste0("SPR",round(40))  
  }
  if(Fref=="Btgt"){
     ftgt = hat[hat$Label%in%c("Fstd_Btgt","annF_Btgt"),2]
     trg =paste0("SB",round(ss3rep$btarg*100))
     if(ss3rep$btarg<0)trg=paste0("SB",round(40)) 
  }  
  if(Fref=="F01"){
    ftgt = hat[hat$Label%in%c("annF_F01"),2]
    if(any(hat[hat$Label%in%c("annF_F01"),2])) stop("F0.1 is not defined in starter.ss")
    trg =paste0("F0.1")
  }
  
    # biomass
  if(Fref!="MSY")btgt=mean(b[min(abs(ftgt-f))==abs(ftgt-f)])
  bmsy=mean(b[min(abs(fmsy-f))==abs(fmsy-f)])
  
  # Yield
  if(Fref!="MSY") ytgt=mean(y[min(abs(ftgt-f))==abs(ftgt-f)])
  ymsy = mean(y[min(abs(fmsy-f))==abs(fmsy-f)])
  
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
  
  
  if(plot){
  cols= rc(6)
  ylim = c(0,max(y)*1.1)
  mai=c(0.55,0.25,0.15,.15)
  omi = c(0.3,0.35,0.2,0.2)
  par(mfrow=c(1,2),mai = mai, mgp =c(2.,0.5,0),omi =omi, tck = -0.02,cex=cex)
  
  
  xlim= c(0,max(f[y>0]))
  plot(f,y,ylab="Yield",xlab="Fishing Mortality",type="l",col=rc(1),lwd=2,ylim=ylim,xlim=xlim)
  lines(c(fmsy,fmsy),c(0,ymsy),col=rc(1))
  text(fmsy*1.15,ymsy*1.05,expression(F[MSY]),cex=cex+0.1)
  if(Fref!="MSY"){
    lines(c(ftgt,ftgt),c(0,ytgt),col=cols[3])
    text(ftgt*0.8,ytgt*1.05,bquote("F"[.(trg)]),cex=cex+0.1)
  }
  if(!is.null(msyline)) abline(h=msyline*ymsy,lty=2,col=2)
  mtext("Yield",outer = T,side=2,cex=cex)

  plot(b,y,ylab="Yield",xlab="SSB",type="l",col=rc(1),lwd=2,ylim=ylim)
  lines(c(bmsy,bmsy),c(0,ymsy),col=rc(1))
  text(bmsy*0.8,ymsy*1.05,expression(B[MSY]),cex=cex+0.1)
  if(Fref!="MSY"){
    lines(c(btgt,btgt),c(0,ytgt),col=cols[3])
    text(btgt*1.15,ytgt*1.05,bquote("B"[.(trg)]),cex=cex+0.1)
  }
  if(!is.null(msyline)) abline(h=msyline*ymsy,lty=2,col=2)
  }
  
  
  refpts = data.frame(Fmsy=fmsy,Bmsy=bmsy,MSY=ymsy,B0=b0,R0=r0)
  if(Fref!="MSY"){
    refpts = cbind(refpts,data.frame(Ftgt=ftgt,Btgt=btgt,Ytgt = ytgt))
  }
  if(verbose) return(refpts)  
  }


