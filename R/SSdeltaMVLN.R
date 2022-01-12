#' Joint Distributions for Stock Status Ratios
#'
#' A function to generate joint distributions for stock status ratios (SSB/SSBref and F/Fref where ref can be MSY, SSB40, F40, etc.) using a Multivariate Log-Normal Distribution. The function produces a Kobe plot, maximum likelihood estimates and the MVLN Monte-Carlo distributions of the Kobe values which can be input into SSplotEnsemble().
#'
#' @param ss3rep from r4ss::SS_output
#' @param Fref  Choice of Fratio c("MSY","Btgt"), correponding to F_MSY and F_Btgt                                                               
#' @param years single year or vector of years for mvln   
#' @param mc number of monte-carlo simulations   
#' @param weight weighting option for model ensembles weight*mc 
#' @param run qualifier for model run
#' @param plot option to show plot
#' @param ymax ylim maximum
#' @param xmax xlim maximum
#' @param addprj include forecast years
#' @param legendcex Allows to adjust legend cex
#' @param verbose Report progress to R GUI?
#' 
#' @return output list of maximum likelihood estimates and the MVLN Monte-Carlo distributions of the Kobe values, and kobe plot
#' 
#' @author Henning Winker (JRC-EC)
#' 
#' @keywords diags kobe lognormal
#' 
#' @importFrom stats rlnorm aggregate
#' @importFrom graphics rect points lines legend
#' 
#' @export
SSdeltaMVLN = function(ss3rep,Fref = NULL,years=NULL,mc=5000,weight=1,run="MVLN",plot=TRUE,
                       addprj=FALSE,ymax=NULL,xmax=NULL,legendcex=1,verbose=TRUE){
  
  status=c('Bratio','F')
  quants =c("SSB","Recr")
  mc = round(weight*mc,0)
  hat = ss3rep$derived_quants
  cv = cv1 = ss3rep$CoVar
  
  
  if(is.null(cv)) stop("CoVar from Hessian required")
  # Get years
  allyrs = unique(as.numeric(gsub(paste0(status[1],"_"),"",hat$Label[grep(paste0(status[1],"_"), hat$Label)])))[-1]
  allyrs = allyrs[!is.na(allyrs)] 
  
  if(is.null(years) & addprj==TRUE) yrs = allyrs   
  if(is.null(years) & addprj==FALSE) yrs = allyrs[allyrs<=ss3rep$endyr]
  if(is.null(years)==FALSE) yrs = years[years%in%allyrs==TRUE]
  estimate = ifelse(yrs<=ss3rep$endyr,"fit","forecast")
  
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
  
  
  # check ss3 version
  if("Fstd_MSY"%in%hat$Label){Fname = "Fstd_"} else {Fname="annF_"}
  cv <- cv[cv$label.i %in% paste0(status,"_",yrs),]
  cv1 = cv1[cv1$label.i%in%paste0(Fname,Fquant) & cv1$label.j%in%paste0(status,"_",yrs),]
  fref = hat[hat$Label==paste0(Fname,Fquant),]
  cv$label.j[cv$label.j=="_"] <- cv$label.i[cv$label.j=="_"]
  
  if(is.null(hat$Label)){ylabel = hat$LABEL} else {ylabel=hat$Label}
  kb=mle = NULL
  for(yi in 1:length(yrs)){ 
    yr = yrs[yi]
    x <- cv[cv$label.j %in% paste0(status[2],"_",c(yr-1,yr,yr+1)) & cv$label.i %in% paste0(status[1],"_",c(yr-1,yr,yr+1)),]
    x1 = cv1[cv1$label.j %in% paste0(status[1],"_",c(yr-1,yr,yr+1)),] 
    x2 = cv1[cv1$label.j %in% paste0(status[2],"_",c(yr-1,yr,yr+1)),] 
    y = hat[ylabel %in% paste0(status,"_",yr),] # old version Label not LABEL
    y$Value[1] = ifelse(y$Value[1]==0,0.001,y$Value[1])
    varF = log(1+(y$StdDev[1]/y$Value[1])^2) # variance log(F/Fmsy)  
    varB = log(1+(y$StdDev[2]/y$Value[2])^2) # variance log(SSB/SSBmsy)  
    varFref = log(1+(fref$StdDev[1]/fref$Value)^2) # variance log(F/Fmsy)  
    cov = log(1+mean(x$corr)*sqrt(varF*varB)) # covxy
    cov1 = log(1+mean(x1$corr)*sqrt(varB*varFref)) # covxy
    cov2 = log(1+mean(x2$corr)*sqrt(varF*varFref)) # covxy
    # MVN means of SSB/SBBmsy, Fvalue and Fref (Ftgt or Fmsy) 
    mvnmu = log(c(y$Value[2],y$Value[1],fref$Value)) # Assume order F_ then Bratio_ 
    # Create MVN-cov-matrix
    mvncov = matrix(NA,ncol=3,nrow=3)
    diag(mvncov) = c(varB,varF,varFref)
    mvncov[1,2] = mvncov[2,1] = cov 
    mvncov[2,3] = mvncov[3,2] = cov1 
    mvncov[1,3] = mvncov[3,1] = cov2 
    kb.temp = data.frame(year=yr,run=run,type=estimate[yi],iter=1:mc,exp(mvtnorm::rmvnorm(mc ,mean = mvnmu,sigma = mvncov,method=c( "svd")))) # random  MVN generator
    colnames(kb.temp) = c("year","run","type","iter","stock","harvest","F")
    if(length(quants)>0){
    quant=NULL
    for(qi in 1:length(quants)){
        qy = hat[ylabel %in% paste0(quants[qi],"_",yr),]
        qsd = sqrt(log(1+(qy$StdDev[1]/qy$Value[1])^2))
        quant = cbind(quant,rlnorm(mc,log(qy$Value[1])-0.5*qsd*qsd,qsd))
        }     
    colnames(quant) = quants    
    kb.temp = cbind(kb.temp,quant)
    }
    kb = rbind(kb,cbind(kb.temp))
    mle = rbind(mle,data.frame(year=yr,run=run,type=estimate[yi],stock=y$Value[2],harvest=y$Value[1],F=fref$Value[1])) 
  }
  # add mle quants
  qmles = NULL
  for(qi in 1:length(quants)){
    qmles = cbind(qmles, hat[ylabel %in% paste0(quants[qi],"_",yrs),]$Value)
  } 
   colnames(qmles) = quants    
   mle = cbind(mle,qmles)
  
  mle = mle[,c(1:5,7,6,8)]
  kb = kb[,c(1:6,8,7,9)]
  
  # Take ratios
  if(bb==1){
  kb[,"stock"] = kb[,"stock"]/bref  
  mle[,"stock"] = mle[,"stock"]/bref
  }
  
   if(fb> 1){
    kb[,"F"] = kb[,"F"]*kb[,"harvest"] 
    mle[,"F"] = mle[,"F"]*mle[,"harvest"] 
    
  } else {
  fi = kb[,"harvest"]
  fm = mle[,"harvest"]
  kb[,"harvest"] = kb[,"harvest"]/kb[,"F"]
  kb[,"F"] = fi
  mle[,"harvest"] = mle[,"harvest"]/mle[,"F"]
  mle[,"F"] = fm
}    

  
  # Add catch
  C_obs = aggregate(Obs~Yr,ss3rep$catch,sum)
  #colnames(C_obs) = c("Yr","Obs")
  Cobs = C_obs[C_obs$Yr%in%yrs,]
  foreyrs = unique(as.numeric(gsub(paste0("ForeCatch_"),"",hat$Label[grep(paste0("ForeCatch_"), hat$Label)])))
  Cfore = data.frame(Yr=foreyrs,Obs=hat$Value[hat$Label%in%paste0("ForeCatch_",foreyrs)] )
  Catch = rbind(Cobs,Cfore)
  Catch = Catch[Catch$Yr%in%yrs,]
  kb$Catch = rep(Catch$Obs,each=max(kb$iter))
  mle$Catch = Catch$Obs
  trg =round(bref*100,0)
  xlab = c(bquote("SSB/SSB"[.(trg)]),expression(SSB/SSB[MSY]))[bb] 
  ylab = c(expression(F/F[MSY]),
           bquote("F/F"[SB~.(trg)]),
           bquote("F/F"[SPR~.(trg)])
  )[which(c("MSY","Btgt","SPR")%in%Fquant)] 
  
  if(plot==TRUE){
    sh =c("stock","harvest")
    if(is.null(xmax)) xmax= max(c(2,kb[kb$year==max(kb$year),sh[1]],mle[,sh[1]]))
    if(is.null(ymax)) ymax = max(c(2,kb[kb$year==max(kb$year),sh[2]],mle[,sh[2]]))
    stock = kb[kb$year==max(kb$year),sh[1]]
    harvest = kb[kb$year==max(kb$year),sh[2]]
    plot(kb[kb$year==max(kb$year),sh]  ,type="n",ylab=ylab,xlab=xlab,xlim=c(0,xmax),ylim=c(0,ymax),yaxs="i",xaxs="i")  
    rect(1,0,100,1,col="green",border=0)
    rect(0,0,1,1,col="yellow",border=0)
    rect(0,1,1,100,col="red",border=0)
    rect(1,1,100,100,col="orange",border=0)
    points(kb[kb$year==max(kb$year),sh],pch=21,bg="grey",cex=0.7)
    if(nrow(mle)>3){
      lines(mle[,sh])  
      points(mle[,sh],pch=21,cex=0.9,bg="white")
      points(mle[1,sh],pch=24,bg="white",cex=1.7)
      } 
    points(mle[nrow(mle),sh],pch=21,bg="white",cex=2)
    
    # Get Propability
    Pr.green = sum(ifelse(stock>1 & harvest<1,1,0))/length(stock)*100
    Pr.red = sum(ifelse(stock<1 & harvest>1,1,0))/length(stock)*100
    Pr.yellow = sum(ifelse(stock<1 & harvest<1,1,0))/length(stock)*100
    Pr.orange = sum(ifelse(stock>1 & harvest>1,1,0))/length(stock)*100
    ## Add legend
    legend('topright',
           c(paste0(round(c(Pr.red,Pr.yellow,Pr.orange,Pr.green),1),"%")),
           pch=22,pt.bg=c("red","yellow","orange","green"),
           col=1,cex=legendcex,pt.cex=2.2,bty="n")
  }
  labs = ifelse(quants=="Recr","Recruits",quants)
  return(list(kb=kb,mle=mle,quants=c("stock","harvest","SSB","F","Recr","Catch"),
                labels=c(xlab,ylab,labs[1],"F",labs[2],"Catch")))
  }


