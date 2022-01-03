#' Retrieve Composition Data from Multiple Models
#' 
#' Get observed and expected mean length/age from multiple Stock Synthesis models.
#' Wrapper that uses r4ss::SSgetoutput() and SScompsTA1.8()
#' 
#' @param retroModels object list of replists from r4ss::SSgetoutput() 
#' @author Henning Winker
#' @return list of observed and expected mean Length/age comps (c.f. ss3rep$cpue)
#' @export

SSretroComps <- function(retroModels){
  
  if(is.null(retroModels$replist1)){
    stop("Require input as object list() from r4ss::SSgetoutput()")
  }
  # check replist
  replist = rownames(summary(retroModels))
  # get excluded ghost from base run
  ref = retroModels[[paste(replist[1])]]
  hccomps = list()
  hccomps$len = NULL
  hccomps$age = NULL
  hccomps$n = length(replist)
  hccomps$startyrs = rep(ref$startyr,length(replist))
  hccomps$endyrs = rep(ref$endyr,length(replist))
  
  if(nrow(ref$lendbase)>0){
    lencomps = NULL
    for(i in 1:length(replist)){
      rep.temp = retroModels[[paste(replist[i])]]
      rep.temp$lendbase =rbind(rep.temp$ghostagedbase,rep.temp$agedbase[,1:ncol(rep.temp$ghostagedbase)]) 
      lencomps = rbind(lencomps,SScompsTA1.8(rep.temp,type="len",plotit = F)$runs_dat)
    }
    hccomps$len = lencomps 
  }
  
  if(nrow(ref$agedbase)>0){
    agecomps = NULL
    for(i in 1:length(replist)){
      rep.temp = retroModels[[paste(replist[i])]]
      rep.temp$agedbase =rbind(rep.temp$ghostagedbase,rep.temp$agedbase[,1:ncol(rep.temp$ghostagedbase)]) 
      agecomps = rbind(agecomps,SScompsTA1.8(rep.temp,type="age",plotit = F)$runs_dat)
      }
    hccomps$age = agecomps
  }
  
  return(hccomps)
}

