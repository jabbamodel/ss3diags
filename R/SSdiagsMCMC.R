#' SSdiagsMCMC() 
#'
#' function to read mcmc file outputs for Kobe and SSplotEnsemble() plotting
#'
#' @param mcmcdir file path for folder with the derived_posteriors.sso file
#' @param Bref  Choice of reference point for stock SSB/X c("MSY","Btrg")
#' @param Fref  Choice of reference point for stock SSB/XFref=c("MSY","Ftrg")                                                                      
#' @param run qualifier for model run
#' @param Fstarter starter settings for  c("_abs_F","(F)/(Fmsy)","Fstd_Btgt") see SSsettingsBratioF()
#' @param forecast option to include forecasts TRUE/FALSE
#' @param Plot option to plot results with SSplotEnsemble()
#' @param thin  option to use additional thinning
#' @param biomass the function is only tested to run with default biomass = "SSB" 
#' @param refs required reference quantaties
#' @return  list of (1) all quataties "sims" (2) object of SSplotEnsemble() "kb" and settings
#' @author Henning Winker (JRC-EC), Massimiliano and Laurence Kell (Sea++)
#' @export


SSdiagsMCMC <- function (mcmcdir,Bref=c("MSY","Btrg"),Fref=c("MSY","Ftrg"),run="MCMC",Fstarter =  c("_abs_F","(F)/(Fmsy)","Fstd_Btgt"),forecast=FALSE,plot=TRUE,thin = 1, biomass = "SSB", 
                         refs = c("SSB_unfished","SSB_MSY","SSB_Btgt","SSB_SPR","SPR_MSY",
                                  "Fstd_MSY","Fstd_SPR","Fstd_Btgt","Recr_unfished", "B_MSY.SSB_unfished",
                                 "Dead_Catch_MSY", "Ret_Catch_MSY")) {
    
   x = paste0(mcmcdir,"/derived_posteriors.sso")
    nms = names(read.csv(x, sep = " ", nrows = 1))
    yrs = nms[substr(nms, 1, 3) == "Bra"]
    yrs = as.numeric(substr(yrs, 8, nchar(yrs)))

    nms = names(read.csv(x, sep = " ", nrows = 1))
    pts = nms[substr(nms, 1, 3) == "For"]
    pts = min(as.numeric(substr(pts, 11, nchar(pts)))) - 
      1
  
  Fs = paste("F", yrs, sep = "_")
  Bs = paste("Bratio", yrs, sep = "_")
  Rs = paste("Recr", yrs, sep = "_")
  FCs = paste("ForeCatch", tail(yrs, n=3), sep = "_")
  SSBs = paste("SSB", yrs, sep = "_")
  ops = options()
  options(warn = -1)
  hd = names(read.csv(x, sep = " ", nrows = 1, header = T))
  res = read.csv(x, sep = " ")
  dat = read.csv(file = x, colClasses = rep("double", length(hd)), 
                 col.names = hd, sep = " ", skip = 2)
  
  res <- dat[, ]
  rfs <- res[, c("Iter", refs)]
  names(rfs)[1] = "iter"
  res = res[, c("Iter", SSBs, Fs,Rs,FCs)]
  res = res[seq(1, dim(res)[1], thin), ]
  res = reshape2::melt(res[, c("Iter", SSBs, Fs,Rs,FCs)], id.vars = "Iter")
  res$year = as.numeric(gsub("SSB_", "", as.character((res$variable))))
  res$year[is.na(res$year)] = as.numeric(gsub("SSB_", "", 
                                                as.character(res[is.na(res$year), "variable"])))
  res$var = substr(res$variable, 1, 2)
  options(ops)
    res1 = data.frame(res[res$var == "SS", c("Iter", "year", 
                                             "value")], harvest = res[res$var == "F_", "value"], recr = res[res$var == "Re", "value"])
    res2 = data.frame(res[res$var == "SS" & res$year %in% tail(yrs, n=3), c("Iter", "year", 
                                                                            "value")], fcat =res[res$var == "Fo", "value"])
    res = merge(res1,res2,all = TRUE)
  
  names(res)[c(1, 3)] = c("iter", "stock")
  resFc = res$fcat
  res[is.na(res)] = 0
  res$fcat =resFc
  sims = merge(res, rfs, by = "iter")
  
  SSB = sims$stock
  if(Bref[1]=="MSY") stock = SSB/sims$SSB_MSY 
  if(Bref[1]=="Btrg") stock = SSB/sims$SSB_Btgt 
  #if(Bref[1]=="B0") stock = SSB/sims$SSB_unfished 
  Fout = sims$harvest
  #if(Fstarter[1]=="_abs_F"){ Fabs = Fout} else if(Fstarter[1] == "(F)/(Fmsy)"){Fabs = Fout} else {Fabs = Fout*sims$Fstd_Btgt}
  if(Fstarter[1] == "(F)/(Fmsy)"){Fabs=Fout*sims$Fstd_MSY} else {Fabs=Fout}
  if(Fref[1]=="MSY"){harvest = Fabs/sims$Fstd_MSY}else{harvest = Fabs/sims$Fstd_Btgt}
  simout = data.frame(sims[,1:2],run=run,SSB,Fabs,BB0 = SSB/sims$SSB_unfished,stock=stock,harvest=harvest,Recr=sims$recr,sims[,6:ncol(sims)])
  if(forecast==FALSE) simout = simout[simout$year<=pts,] 
  
  kb = data.frame(year=simout$year,run=run,iter=simout$iter,stock=simout$stock,harvest=simout$harvest,SSB=simout$SSB,Recr=simout$Recr)
  
  # some house cleaning
  bb = c("MSY","Btrg","B0")
  fb=c("MSY","Ftrg")
  xlab = c(expression(SSB/SSB[MSY]),bquote("SSB/SSB"["trg"]),expression(SSB/SSB[MSY]))[which(bb%in%Bref[1])] 
  ylab = c(expression(F/F[MSY]),bquote("F/F"["trg"]))[which(fb%in%Fref[1])] 
  
  if(plot==TRUE){
    sspar(mfrow=c(2,2),plot.cex = 0.8)
    SSplotEnsemble(kb,add=T,legend = F,ylabs=c(xlab,ylab,"SSB","Recruitment"))  
  }
  
  return(list(sims = simout, kb = kb,Bref=Bref,Fref=Fref,Fstarter=Fstarter,labels=c(xlab,ylab,"SSB","Recruitment")))

}

