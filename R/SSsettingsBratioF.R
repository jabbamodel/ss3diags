#' SSsettingsBratioF 
#'
#' function to get Bratio and F settings
#'
#' @param ss3rep from r4ss::SSgetoutput()
#' @param status covarying stock status quantaties to extract from Hessian
#' @param verbose Report progress to R GUI?
#' @return output list with Bratio and F settings
#' @author Henning Winker (JRC-EC)
#' @export

SSsettingsBratioF = function(ss3rep,status=c('Bratio','F'),verbose=TRUE){
  hat = ss3rep$derived_quants
  # Get years
  allyrs = unique(as.numeric(gsub(paste0(status[1],"_"),"",hat$Label[grep(paste0(status[1],"_"), hat$Label)])))[-1]
  allyrs = allyrs[!is.na(allyrs)] 
  yrs = ss3rep$endyr   
  
  
  
  
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
  gettrg = strsplit(fbasis,"%")[[1]][1]
  gettrg = as.numeric(strsplit(gettrg,"B")[[1]][2])
  
  if(fbasis%in%c("_abs_F","(F)/(Fmsy)",paste0("(F)/(F_at_B",ss3rep$btarg*100,"%)"))){
    fb = which(c("_abs_F","(F)/(Fmsy)",paste0("(F)/(F_at_B",ss3rep$btarg*100,"%)"))%in%fbasis)
  } else {fb=3}
  bref  = ifelse(ss3rep$btarg<0.2,gettrg/100,ss3rep$btarg)
  if(is.na(bref)) bref= 0.4
  
  
  return(list(Bratio = bbasis,F = fbasis,Bref=bref))
}


