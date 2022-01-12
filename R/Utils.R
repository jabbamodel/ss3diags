#' Allow Multi-Plots
#' Set the par() to options suitable for ss3diags multi plots.
#'
#' See \link[graphics]{par} for more details on each parameter.
#' 
#' @param mfrow determines plot frame set up
#' @param plot.cex cex graphic option
#' @param mai graphical par for plot margins
#' @param labs if TRUE margins are narrow 
#' @param omi Outer margins in lines of text.  
#' 
#' @keywords ssplot utils
#' 
#' @export
#' 
sspar <- function(mfrow=c(1,1),plot.cex=1,mai=c(0.55,0.6,0.1,.1),omi = c(0.,0.,0.,0)+ 0.1,labs=TRUE){
  if(labs==F){
    mai=c(0.25,0.25,0.15,.15)
    omi = c(0.3,0.35,0.2,0.2)}
  par(list(mfrow=mfrow,mai = mai, mgp =c(2.,0.5,0),omi =omi, tck = -0.02,cex=plot.cex))
}


#' Convert Time-Steps
#' Function to convert non-annual into annual time-steps for retros and cpue residuals
#'  
#' @param ss3out outputs from r4ss::SS_output() or r4ss::SSsummarize()
#' @param time.steps  time steps behind yrs e.g. 0.25 for quarterly 
#' @param end.time last time step e.g. 2018.75 with a cpue observation
#' 
#' @return Reformatted Rep file outputs
#' 
#' @keywords utils rep retro retrocomps
#' 
#' @export
#' 
SSdiagsTime2Year = function(ss3out,time.steps=0.25,end.time){
  if(is.null(ss3out$len)==F | is.null(ss3out$len)==F){
  type = "retrocomps"} else {  
  type = ifelse(is.null(ss3out$modelnames),"rep","retro")
  }
  # Conversion match function
  convTY <- function(indices,end.time,time.steps){
    steps = (unique(indices$Time))
    nsteps = length(steps)
    Time = rev(rev(seq(0,end.time,time.steps))[1:nsteps])
    Yr = floor(Time)  
    Seas = (Time-Yr)*4+1
    conv = match(indices$Time,steps)
    indices$Yr = Yr[conv]
    indices$Time = Time[conv]
    indices$Seas = Seas[conv]
    return(indices)
  }
  
  if(type == "rep"){ 
    ss3out$cpue =  convTY(ss3out$cpue,end.time,time.steps)
    #if(!is.null(ss3out$lendbase)) ss3out$lendbase =  convTY(ss3out$lendbase,end.time,time.steps)
    # length comps not working
  }
  if(type == "retro"){ 
    ss3out$indices =  convTY(ss3out$indices,end.time,time.steps)
    # SSB
    ssb = ss3out$SpawnBio
    steps = unique(ssb$Yr)  
    nsteps = length(steps)
    Time = (rev(rev(seq(0,end.time,time.steps))[1:nsteps]))
    ssb$Time = Time
    subset = ssb$Time%in%floor(ssb$Time)
    ssb = ssb[subset,] 
    ss3out$SpawnBio =  ssb
    ss3out$SpawnBioLower = ss3out$SpawnBioLower[subset,]
    ss3out$SpawnBioUpper = ss3out$SpawnBioUpper[subset,]
    
    ss3out$SpawnBio$Yr = ssb$Time
    ss3out$SpawnBioLower$Yr = ssb$Time
    ss3out$SpawnBioUpper$Yr = ssb$Time 
    ss3out$startyrs = rep(min(ssb$Time),ss3out$n)
    ss3out$endyrs = rep(max(ssb$Time),ss3out$n)
    # Can add F and Rec if needed
  }
  if(type=="retrocomps"){
    if(!is.null(ss3out$len)) ss3out$len =  convTY(ss3out$len,end.time,time.steps)
    if(!is.null(ss3out$age)) ss3out$len =  convTY(ss3out$age,end.time,time.steps)
    ss3out$startyrs = rep(min(ss3out$len$Time,ss3out$age$Time),ss3out$n)
    ss3out$endyrs = rep(max(ss3out$len$Time,ss3out$age$Time),ss3out$n)
  }
  return(ss3out)
}

#' r4ss color palette 
#' @param n number of colors
#' @param alpha transluscency 
#' @return vector of color codes
#' @keywords ssplot utils
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



#' r4ss color generator 
#' @param n number of colors
#' @param alpha transluscency 
#' @return vector of color codes
#' @keywords ssplot utils
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
