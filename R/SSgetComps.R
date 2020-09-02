#' SSgetComps
#' 
#' Get observed and expected mean length/age from multiple Stock Synthesis models.
#' Wrapper that uses r4ss::SS_output() and SScompsTA1.8()
#' 
#' @param dirvec A vector of directories (full path or relative to working
#' @param ncols Maximum number of columns in Report.sso (same input as for
#' @param covar Choice to read or not read covar.sso output (saves time and
#' @param verbose Print various messages to the command line as the function
#' @author Henning Winker
#' @return list of observed and expected mean Length/age comps (c.f. ss3rep$cpue)
#' @export

SSgetComps <- function(dirvec=getwd(),ncols=400,covar=FALSE,verbose=FALSE){
  summarycomps = NULL
  endyrs = startyrs = NULL 
  for(i in 1:length(dirvec)){
    rep = r4ss::SS_output(dir=dirvec[i],covar=covar,ncols = ncols,verbose = verbose)
    lenc = SScompsTA1.8(rep,type="len",plotit = F)$runs_dat     
    agec = SScompsTA1.8(rep,type="age",plotit = F)$runs_dat
    if(is.null(lenc)==FALSE) lenc$type="len"
    if(is.null(agec)==FALSE) lenc$type="age"
    comps = rbind(lenc,agec)
    comps$name = paste0("replist",i)
    comps$imodel = i
    startyrs = c(startyrs,rep$endyr)
    endyrs = c(endyrs,rep$endyr)
    summarycomps = rbind(summarycomps,comps)
  }
  compsout=list()
  compsout$comps <- summarycomps 
  compsout$n    <- length(unique(summarycomps$imodel))
  compsout$startyrs      <- summaryoutput$startyrs
  compsout$endyrs        <- summaryoutput$endyrs
  
  return(compsout)
}
