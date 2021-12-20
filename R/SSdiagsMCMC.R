#' A function to read mcmc file outputs from a Stock Synthesis model
#'
#'
#' @param mcmcdir file path for folder with the derived_posteriors.sso file
#' @return Stock Synthesis mcmc output file
#' @author Henning Winker (JRC-EC) & Laurence Kell (Sea++)
#' @export
#' @importFrom utils read.csv
#' 
SSreadMCMC <- function(mcmcdir){
  
  x = paste0(mcmcdir,"/derived_posteriors.sso")
  hd = names(read.csv(x, sep = " ", nrows = 1, header = T))
  res = read.csv(x, sep = " ")
  mcmcdat = read.csv(file = x, colClasses = rep("double", length(hd)), 
                     col.names = hd, sep = " ", skip = 2)
  return(mcmcdat)  
}



#' A function to generate joint distributions for stock status ratios (SSB/SSBref and F/Fref where ref can be MSY, SSB40, F40, etc.) using MCMC. The function produces a Kobe plot, maximum likelihood estimates and the MVLN Monte-Carlo distributions of the Kobe values which can be input into SSplotEnsemble().
#'
#' @param mcmc file path for folder with the derived_posteriors.sso file
#' @param ss3rep from r4ss::SS_output
#' @param Fref  Choice of Fratio c("MSY","Btgt), correponding to F_MSY and F_Btgt                                                               
#' @param years single year or vector of years for mvln   
#' @param run qualifier for model run
#' @param thin  option to use additional thinning
#' @param plot option to show plot
#' @param ymax ylim maximum
#' @param xmax xlim maximum
#' @param addprj include forecast years
#' @param legendcex Allows to adjust legend cex
#' @param verbose Report progress to R GUI?
#' @return output list maximum likelihood estimates and the MCMC posterier distributions of the Kobe values and a Kobe plot
#' @seealso [SSdeltaMVLN()], [SSplotEnsemble()], [SSkobe()]
#' @author Henning Winker (JRC-EC), Massimiliano and Laurence Kell (Sea++)
#' @export

SSdiagsMCMC = function(mcmc,ss3rep,Fref = NULL,years=NULL,run="MCMC",thin = 1,plot=TRUE,
                       addprj=FALSE,ymax=NULL,xmax=NULL,legendcex=1,verbose=TRUE){
  
  
  dat = mcmc
  
  # check ss3 version
  ssver = ifelse("annF_MSY"%in%ss3rep$derived_quants$Label,"new","old")
  # Define refs
  if(ssver=="new"){
    refs = c("SSB_unfished","SSB_MSY","SSB_Btgt","SSB_SPR","SPR_MSY",
             "annF_MSY","annF_SPR","annF_Btgt","Recr_unfished", "B_MSY.SSB_unfished")  
  } else {
    refs = c("SSB_unfished","SSB_MSY","SSB_Btgt","SSB_SPR","SPR_MSY",
             "Fstd_MSY","Fstd_SPR","Fstd_Btgt","Recr_unfished", "B_MSY.SSB_unfished"
    )  
  }  
  # VERY OLD
  if(is.null(dat$SSB_unfished)){
    refs = c("SSB_Unfished","SSB_MSY","SSB_Btgt","SSB_SPRtgt","SPR_MSY",
             "Fstd_MSY","Fstd_SPRtgt","Fstd_Btgt","Recr_Unfished"
    )  
    
  }
  
  # Some checks
  nms = names(dat)
  yrs = nms[substr(nms, 1, 3) == "Bra"]
  yrs = as.numeric(substr(yrs, 8, nchar(yrs)))
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
  
  res <- dat[, ]
  rfs <- res[, c("Iter", refs)]
  names(rfs)[1] = "iter"
  res = res[, c("Iter", SSBs,Bs, Fs,Rs,FCs)]
  res = res[seq(1, dim(res)[1], thin), ]
  res = reshape2::melt(res[, c("Iter", SSBs,Bs, Fs,Rs,FCs)], id.vars = "Iter")
  res$year = as.numeric(gsub("SSB_", "", as.character((res$variable))))
  res$year[is.na(res$year)] = as.numeric(gsub("SSB_", "", 
                                              as.character(res[is.na(res$year), "variable"])))
  res$var = substr(res$variable, 1, 2)
  options(ops)
  res1 = data.frame(res[res$var == "SS", c("Iter", "year", 
                                           "value")],stock =res[res$var == "Br", "value"] ,harvest = res[res$var == "F_", "value"], recr = res[res$var == "Re", "value"])
  res2 = data.frame(res[res$var == "SS" & res$year %in% tail(yrs, n=3), c("Iter", "year", 
                                                                          "value")], fcat =res[res$var == "Fo", "value"])
  res = merge(res1,res2,all = TRUE)
  
  names(res)[c(1, 3)] = c("iter", "SSB")
  resFc = res$fcat
  res[is.na(res)] = 0
  res$fcat =resFc
  sims = merge(res, rfs, by = "iter")
  
  status=c('Bratio','F')
  quants =c("SSB","Recr")
  
  hat = ss3rep$derived_quants # from rep file
  allyrs = yrs
  if(is.null(years) & addprj==TRUE) yrs = allyrs 
  if(is.null(years) & addprj==FALSE) yrs = allyrs[allyrs<=ss3rep$endyr]
  if(is.null(years)==FALSE) yrs = years[years%in%allyrs==TRUE]
  
  
  # brp checks for starter file setting
  refyr = max(yrs)
  bt = hat[hat$Label==paste0("SSB_",refyr),2]
  b0 =hat[hat$Label%in%c("SSB_unfished","SSB_Unfished"),2]
  btrg = hat[hat$Label==paste0("SSB_Btgt"),2]
  bmsy = hat[hat$Label==paste0("SSB_MSY"),2]
  bb.check = c(bt/b0,bt/bmsy,bt/btrg)
  
  # bratio definition
  bratio = hat[hat$Label==paste0("Bratio_",refyr),2]
  bb = which(abs(bratio-bb.check)==min(abs(bratio-bb.check)))   
  if(bb%in%c(1:2)==F) stop("This Bratio is not [yet] defined, please rerun Stock Synthesis with starter.ss option for Depletion basis: 1 or 2")
  
  bbasis  = c("SSB/SSB0","SSB/SSBMSY","SSB/SSBtrg")[bb]
  fbasis = strsplit(ss3rep$F_report_basis,";")[[1]][1]
  gettrg = strsplit(fbasis,"%")[[1]][1]
  gettrg = as.numeric(strsplit(gettrg,"B")[[1]][2])
  
  if(fbasis%in%c("_abs_F","(F)/(Fmsy)",paste0("(F)/(F_at_B",ss3rep$btarg*100,"%)"),paste0("(F)/(F",ss3rep$btarg*100,"%SPR)"))){
    fb = which(c("_abs_F","(F)/(Fmsy)",paste0("(F)/(F_at_B",ss3rep$btarg*100,"%)"),
                 paste0("(F)/(F",ss3rep$btarg*100,"%SPR)"))%in%fbasis)
  } else { stop("F_report_basis is not defined, please rerun Stock Synthesis with recommended starter.ss option for F_report_basis: 1")}
  if(is.null(Fref) & fb%in%c(1,2)) Fref = "MSY"
  if(is.null(Fref) & fb%in%c(3)) Fref = "Btgt"
  if(is.null(Fref) & fb%in%c(4)) Fref = "SPR"
  
  if(verbose) cat("\n","starter.sso with Bratio:",bbasis,"and F:",fbasis,"\n","\n")
  bref  = ifelse(ss3rep$btarg<0,gettrg/100,ss3rep$btarg)
  if(is.na(bref)) bref = 0.4
  
  if(fb==4 & Fref[1] %in% c("Btgt","MSY")) stop("Fref = ",Fref[1]," option conflicts with ",fbasis," in starter.sso, please choose Fref = SPR")
  if(fb==2 & Fref[1] %in% c("Btgt","SPR")) stop("Fref = ",Fref[1]," option conflicts with ",fbasis," in starter.sso, please choose Fref = MSY")
  if(fb==3 & Fref[1] %in% c("Btgt","MSY")) stop("Fref = ",Fref[1]," option conflicts with ",fbasis,", in starter.sso, please choose Fref = Btgt")
  if(fb%in%c(1,2) &  Fref[1] =="MSY") Fquant = "MSY"
  if(fb%in%c(1,3) & Fref[1] =="Btgt") Fquant = "Btgt"
  if(fb%in%c(1,4) & Fref[1] =="SPR") Fquant = "SPR"
  
  SSB = sims$SSB
  if(bb==2) stock = sims$stock
  if(bb==1) stock = sims$stock/bref
  #if(Bref[1]=="B0") stock = SSB/sims$SSB_unfished 
  Fout = sims$harvest
  #if(Fstarter[1]=="_abs_F"){ Fabs = Fout} else if(Fstarter[1] == "(F)/(Fmsy)"){Fabs = Fout} else {Fabs = Fout*sims$Fstd_Btgt}
  if(ssver=="new"){ # new version
    if(fb==1) {Fabs=Fout}
    if(fb==2) {Fabs=Fout*sims$annF_MSY}
    if(fb==3) {Fabs=Fout*sims$annF_Btgt}
    if(fb==4) {Fabs=Fout*sims$annF_SPR}
    if(Fref[1]=="MSY"){harvest = Fabs/sims$annF_MSY}
    if(Fref[1]=="Btgt"){harvest = Fabs/sims$annF_Btgt}
    if(Fref[1]=="SPR"){harvest = Fabs/sims$annF_SPR}
  } else { # Old version
    if(fb==1) {Fabs=Fout}
    if(fb==2) {Fabs=Fout*sims$Fstd_MSY}
    if(fb==3) {Fabs=Fout*sims$Fstd_Btgt}
    if(fb==4) {Fabs=Fout*sims$Fstd_SPR}
    if(Fref[1]=="MSY"){harvest = Fabs/sims$Fstd_MSY}
    if(Fref[1]=="Btgt"){harvest = Fabs/sims$Fstd_Btgt}
    if(Fref[1]=="SPR"){harvest = Fabs/sims$Fstd_SPR}
  }
  
  simout = data.frame(sims[,1:2],run=run,SSB,Fabs,BB0 = SSB/sims$SSB_unfished,stock=stock,harvest=harvest,Recr=sims$recr,sims[,6:ncol(sims)])
  
  kb = data.frame(year=simout$year,run=run,type=ifelse(simout$year<=ss3rep$endyr,"fit","forecast"),
                  iter=simout$iter,stock=simout$stock,harvest=simout$harvest,SSB=simout$SSB,
                  F = simout$Fabs, Recr=simout$Recr)
  
  
  kb = kb[kb$year%in%yrs,]
  # Sort
  kb = kb[order(kb$year, kb$iter),]
  iter = nrow(kb[kb$year==yrs[1],])
  kb$iter = rep(1:iter,length(yrs))
  
  
  
  # Add catch
  C_obs = aggregate(Obs~Yr,ss3rep$catch,sum)
  Cobs = C_obs[C_obs$Yr%in%yrs,]
  foreyrs = unique(as.numeric(gsub(paste0("ForeCatch_"),"",hat$Label[grep(paste0("ForeCatch_"), hat$Label)])))
  Cfore = data.frame(Yr=foreyrs,Obs=hat$Value[hat$Label%in%paste0("ForeCatch_",foreyrs)] )
  Catch = rbind(Cobs,Cfore)
  Catch = Catch[Catch$Yr%in%yrs,]
  kb$Catch = rep(Catch$Obs,each=iter)
  trg =round(bref*100,0)
  
  # House keeping
  xlab = c(bquote("SSB/SSB"[.(trg)]),expression(SSB/SSB[MSY]))[bb] 
  ylab = c(expression(F/F[MSY]),
           bquote("F/F"[SB~.(trg)]),
           bquote("F/F"[SPR~.(trg)])
  )[which(c("MSY","Btgt","SPR")%in%Fquant)] 
  
  labs = ifelse(quants=="Recr","Recruits",quants)
  if(plot==TRUE){
    sspar(mfrow=c(3,2),plot.cex = 0.7)
    SSplotEnsemble(kb,add=T,legend = F,ylabs=c(xlab,ylab,labs[1],"F",labs[2],"Catch"))  
  }
  
  labs = ifelse(quants=="Recr","Recruits",quants)
  return(list(kb=kb,quants=c("stock","harvest","SSB","F","Recr","Catch"),
              labels=c(xlab,ylab,labs[1],"F",labs[2],"Catch"),iter=iter))
  
}

