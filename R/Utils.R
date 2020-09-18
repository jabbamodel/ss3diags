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

#' SSdiagsTime2Year()
#'
#' Function to convert non-annual into annual time-steps for retros and cpue residuals   
#' @param ss3out outputs from r4ss::SS_output() or r4ss::SSsummarize()
#' @param time.steps  time steps behind yrs e.g. 0.25 for quarterly 
#' @param end.time last time step e.g. 2018.75 with a cpue observation
#' @return Reformatted Rep file outputs
#' @export
SSdiagsTime2Year = function(ss3out,time.steps=0.25,end.time){
  type = ifelse(is.null(ss3out$modelnames),"rep","retro")
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
  return(ss3out)
}
