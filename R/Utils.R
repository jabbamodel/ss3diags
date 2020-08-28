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


