#' SSdeltaMVLN() 
#'
#' function to generate kobe pdfs from a Multivariate Log-Normal Distribution
#' including plotting option
#'
#' @param ss3rep from r4ss::SSgetoutput()$replist1
#' @param status covarying stock status quantaties to extract from Hessian
#' @param quants additional stock quantaties to extract from Hessian
#' @param years single year or vector of years for mvln   
#' @param mc number of monte-carlo simulations   
#' @param weight weighting option for model ensembles weight*mc 
#' @param run qualifier for model run
#' @param plot option to show plot
#' @param ymax ylim maximum
#' @param xmax xlim maximum
#' @param legendcex=1 Allows to adjust legend cex
#' @param verbose Report progress to R GUI?
#' @return output list of kobe objects and mle's
#' @author Henning Winker (JRC-EC)
#' @export
SSdeltaMVLN = function(ss3rep,status=c('Bratio','F'),quants =c("SSB","Recr"),years=NULL,mc=5000,weight=1,run="MVLN",plot=TRUE,
                       addtrj=TRUE,ymax=NULL,xmax=NULL,legendcex=1,verbose=TRUE){
  mc = round(weight*mc,0)
  hat = ss3rep$derived_quants
  cv = ss3rep$CoVar
  if(is.null(cv)) stop("CoVar from Hessian required")
  # Get years
  allyrs = unique(as.numeric(gsub(paste0(status[1],"_"),"",hat$Label[grep(paste0(status[1],"_"), hat$Label)])))[-1]
  allyrs = allyrs[!is.na(allyrs)] 
  if(is.null(years) & addtrj==FALSE) yrs = ss3rep$endyr   
  if(is.null(years) & addtrj==TRUE) yrs = allyrs[allyrs<=ss3rep$endyr]
  if(is.null(years)==FALSE) yrs = years[years%in%allyrs==TRUE]
  
  cv <- cv[cv$label.i %in% paste0(status,"_",yrs),]
  cv$label.j[cv$label.j=="_"] <- cv$label.i[cv$label.j=="_"]
  
  if(is.null(hat$Label)){ylabel = hat$LABEL} else {ylabel=hat$Label}
  kb=mle = NULL
  for(yi in 1:length(yrs)){ 
    yr = yrs[yi]
    x <- cv[cv$label.j %in% paste0(status[2],"_",c(yr-1,yr,yr+1)) & cv$label.i %in% paste0(status[1],"_",c(yr-1,yr,yr+1)),]
    y = hat[ylabel %in% paste0(status,"_",yr),] # old version Label not LABEL
    varF = log(1+(y$StdDev[1]/y$Value[1])^2) # variance log(F/Fmsy)  
    varB = log(1+(y$StdDev[2]/y$Value[2])^2) # variance log(SSB/SSBmsy)  
    cov = log(1+mean(x$corr)*sqrt(varF*varB)) # covxy
    # MVN means of SSB/SBBmsy and F/Fsmy
    mvnmu = log(c(y$Value[2],y$Value[1])) # Assume order F_ then Bratio_ 
    # Create MVN-cov-matrix
    mvncov = matrix(c(varB,rep(cov,2),varF),ncol=2,nrow=2)
    kb.temp = data.frame(year=yr,run=run,iter=1:mc,exp(mvtnorm::rmvnorm(mc ,mean = mvnmu,sigma = mvncov,method=c( "svd")))) # random  MVN generator
    colnames(kb.temp) = c("year","run","iter","stock","harvest")
    if(length(quants)>0){
    quant=NULL
    for(qi in 1:length(quants)){
        qy = hat[ylabel %in% paste0(quants[qi],"_",yr),]
        qsd = sqrt(log(1+(qy$StdDev[1]/qy$Value[1])^2))
        quant = cbind(quant,rlnorm(mc,log(qy$Value[1])-0.5*qsd*qsd,qsd))}     
    colnames(quant) = quants    
    kb.temp = cbind(kb.temp,quant)
    }
    kb = rbind(kb,cbind(kb.temp))
    mle = rbind(mle,data.frame(year=yr,run=run,stock=y$Value[2],harvest=y$Value[1])) 
  }
  # add mle quants
  qmles = NULL
  for(qi in 1:length(quants)){
    qmles = cbind(qmles, hat[ylabel %in% paste0(quants[qi],"_",yrs),]$Value)
  } 
   colnames(qmles) = quants    
   mle = cbind(mle,qmles)

  
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
  if(bb%in%c(1:2)==F) stop("Bratio in starter.sso must specified as either 1 or 2")
  bbasis  = c("SSB/SSB0","SSB/SSBMSY","SSB/SSBtrg")[bb]
  fbasis = strsplit(ss3rep$F_report_basis,";")[[1]][1]
  fb = which(c("_abs_F","(F)/(Fmsy)",paste0("(F)/(F_at_B",ss3rep$btarg*100,"%)"))%in%fbasis)
  if(verbose) cat("\n","starter.sso with Bratio:",bbasis,"and F:",fbasis)
  
  
  bref  = ss3rep$btarg
  # Take ratios
  if(bb==1){
  kb[,"stock"] = kb[,"stock"]/bref  
  mle[,"stock"] = mle[,"stock"]/bref
  }
  if(fb==1) warning("stater.sso specifies Fratio as abs_F. To derive F/Fmsy, Fmsy is represented by the MLE without error")
  
  if(fb==1){
    kb[,"harvest"] = kb[,"harvest"]/hat[hat$Label=="Fstd_MSY",2]  
    mle[,"harvest"] = mle[,"harvest"]/hat[hat$Label=="Fstd_MSY",2]
  }
  
  
  trg =round(ss3rep$btarg*100,0)
  xlab = c(bquote("SSB/SSB"[.(trg)]),expression(SSB/SSB[MSY]))[bb] 
  ylab = c(expression(F/F[MSY]),expression(F/F[MSY]),bquote("F/F"[.(trg)]))[bb] 
  
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
    if(addtrj & nrow(mle)>3){
      lines(mle[,sh])  
      points(mle[,sh],pch=21,cex=0.9,bg="white")
      points(mle[1,sh],pch=24,bg="white",cex=1.7)
      } 
    points(mle[nrow(mle),3:4],pch=21,bg="white",cex=2)
    
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
  return(list(kb=kb,mle=mle,labels=c(xlab,ylab,labs)))
  }


