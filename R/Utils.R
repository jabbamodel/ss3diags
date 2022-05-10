#' sspar()
#'
#' Set the par() to options suitable for ss3diags multi plots   
#' @param mfrow determines plot frame set up
#' @param plot.cex cex graphic option
#' @param mai graphical par for plot margins
#' @param labs if TRUE margins are narrow 
#' @export
sspar <- function(mfrow=c(1,1),plot.cex=1,mai=c(0.55,0.6,0.1,.1),omi = c(0.,0.,0.,0)+ 0.1,labs=TRUE){
  if(labs==F){
    mai=c(0.25,0.25,0.15,.15)
    omi = c(0.3,0.35,0.2,0.2)}
  par(list(mfrow=mfrow,mai = mai, mgp =c(2.,0.5,0),omi =omi, tck = -0.02,cex=plot.cex))
}

#' SSdiagsStep2Year()
#'
#' Function to convert non-annual into annual time-steps for retros and cpue residuals   
#' @param ss3out outputs from r4ss::SS_output(), r4ss::SSsummarize() or ss3diags::SSretroComps()
#' @param time.steps  time steps behind yrs e.g. 0.25 for quarterly 
#' @param styr start year step (e.g. 1950)
#' @param season reference Season for retrospective
#' @return Reformatted Rep file outputs
#' @export
SSdiagsStep2Year <-  function(ss3out,time.steps=0.25,styr,season=1){

  make.conv <- function(step1,time.steps){
  y0 = step1-floor(step1)
  intv = floor(1/time.steps)
  Seas = 1:intv 
  Step = c(-2,-1,seq(y0,(200-time.steps)*intv,1))+step1
  Yr = floor(c(-2,-1,seq(y0,200-time.steps,time.steps))+styr)
  Time = c(-2,-1,seq(y0,200-time.steps,time.steps))+styr
  Seas = c(1,1,rep(1:intv,200))
  conv = data.frame(Step,Yr,Time,Seas)
  return(conv)
  }
  
  convTY <- function(indices,conv){
    Steps = indices$Yr
    indices$Yr = conv$Yr[match(Steps,conv$Step)]
    indices$Time = conv$Time[match(Steps,conv$Step)]
    indices$Seas = conv$Seas[match(Steps,conv$Step)]
    return(indices)
  }
  
  
  convQuant <- function(ss3out,quants=c("SpawnBio","Fvalue","Bratio","SPRratio","recdevs","recruits"),conv,season){
    out = ss3out
    for(j in 1:length(quants)){
    quant = quants[j]
    Quant = out[[paste0(quant)]]
    Steps = Quant$Yr
    Quant$Yr = conv$Time[match(Steps,conv$Step)]
    subs = c("","SD","Lower","Upper")
    for(i in 1:length(subs)){
      out[[paste0(quant,subs[i])]]$Yr = Quant$Yr
      out[[paste0(quant,subs[i])]]= out[[paste0(quant,subs[i])]][
        out[[paste0(quant,subs[i])]]$Yr%in%conv$Time[conv$Seas==season],]
      out[[paste0(quant,subs[i])]]$Yr =  floor(out[[paste0(quant,subs[i])]]$Yr) 
    }
    } # end j 
   return(out)
  } 
  
  if(!is.null(ss3out$Data_File)){
    type="rep"
    step1 = ss3out$startyr
    conv = make.conv(step1,time.steps)
    ss3out$cpue = convTY(ss3out$cpue,conv)
    if(!is.null(ss3out$lendbase))
         ss3out$lendbase = convTY(ss3out$lendbase,conv)
    
    ss3out$startyr = conv$Yr[conv$Step%in%step1]
    ss3out$endyr = conv$Yr[conv$Step%in%ss3out$endyr]
  } else if(!is.null(ss3out$modelnames)){
    type ="retro"
    step1 = ss3out$startyrs[1]
    conv = make.conv(step1,time.steps)
    ss3out$indices = convTY(ss3out$indices,conv)
    ss3out = convQuant(ss3out,conv=conv,season =season)
    ss3out$startyrs = rep(conv$Yr[conv$Step%in%step1],length(ss3out$startyrs)) 
    ss3out$endyrs = rep(conv$Yr[conv$Step%in%ss3out$endyrs[1]],length(ss3out$endyrs)) 
    } else if(is.null(ss3out$len)==F | is.null(ss3out$age)==F){
    type = "retrocomps"
    step1 = ss3out$startyrs[1]
    conv = make.conv(step1,time.steps)
    if(!is.null(ss3out$len))
       ss3out$len = convTY(ss3out$len,conv)
    if(!is.null(ss3out$age))
      ss3out$len = convTY(ss3out$age,conv)
    
    ss3out$startyrs = rep(conv$Yr[conv$Step%in%step1],length(ss3out$startyrs)) 
    ss3out$endyrs = rep(conv$Yr[conv$Step%in%ss3out$endyrs[1]],length(ss3out$endyrs)) 
  }

  return(ss3out)
}

#' rc r4ss color palette 
#' @param n number of colors
#' @param alpha transluscency 
#' @return vector of color codes
#' @export
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



#' sscol r4ss color generator 
#' @param n number of colors
#' @param alpha transluscency 
#' @return vector of color codes
#' @export
sscol <- function(n,alpha=1){
if(n>3) col <- rc(n+1)[-1]
if(n<3)  col <- rc(n)
if(n==3) col <- c("blue","red","green3")
if(alpha<1){
  # new approach thanks to Trevor Branch
  cols <- adjustcolor(col, alpha.f=alpha)
} else {
  cols=col
}
return(cols)
}
