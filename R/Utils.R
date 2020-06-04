#' sspar()
#'
#' Set the par() to options suitable for ss3diags multi plots   
#' @param mfrow determines plot frame set up
#' @param plot.cex cex graphic option
#' @param mai graphical par for plot margins
#' @param labs if TRUE margins are narrow 
#' @export
sspar <- function(mfrow=c(1,1),plot.cex=1,mai=c(0.35,0.15,0,.15),labs=TRUE){
  if(labs)  mai=c(0.25,0.25,0.15,.15)
  par(list(mfrow=mfrow,mai = mai, mgp =c(2.,0.5,0),omi = c(0.3,0.3,0.2,0) + 0.1, tck = -0.02,cex=plot.cex))
}

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


