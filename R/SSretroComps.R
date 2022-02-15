
#' Retrieve Composition Data from Multiple SS Models
#' 
#' Wrapper to get observed and expected mean length/age from multiple Stock Synthesis models.
#' 
#' @param retroModels object list of replists from r4ss::SSgetoutput() 
#' @author Henning Winker
#' @return list of observed and expected mean Length/age comps (c.f. ss3rep$cpue)
#' @keywords diags retrocomps
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
  hccomps$con = NULL
  hccomps$n = length(replist)
  hccomps$startyrs = rep(ref$startyr,length(replist))
  hccomps$endyrs = rep(ref$endyr,length(replist))
  
  if(nrow(ref$lendbase)>0){
    lencomps = NULL
    for(i in 1:length(replist)){
      rep.temp = retroModels[[paste(replist[i])]]
      rep.temp$lendbase =rbind(rep.temp$ghostlendbase,rep.temp$lendbase[,1:ncol(rep.temp$ghostlendbase)]) 
      rep.temp$lendbase =data.frame(rep.temp$lendbase,imodel=i) 
      lencomps = rbind(lencomps,data.frame(SScompsTA1.8(rep.temp,type="len",plotit = F)$runs_dat,imodel=i))
      
    }
    lencomps=lencomps[order(lencomps$imodel,lencomps$Fleet, lencomps$Time),]
    
    hccomps$len = lencomps 
  }
  
  if(nrow(ref$agedbase)>0){
    agecomps = NULL
    for(i in 1:length(replist)){
      rep.temp = retroModels[[paste(replist[i])]]
      rep.temp$agedbase =rbind(rep.temp$ghostagedbase,rep.temp$agedbase[,1:ncol(rep.temp$ghostagedbase)]) 
      agecomps = rbind(agecomps,data.frame(SScompsTA1.8(rep.temp,type="age",plotit = F)$runs_dat,imodel=i))
      }
    agecomps=agecomps[order(agecomps$imodel,agecomps$Fleet, agecomps$Time),]
    hccomps$age = agecomps
  }
  
  if(nrow(ref$condbase) > 0){
    concomps = NULL
    for(i in 1:length(replist)){
      rep.temp = retroModels[[paste(replist[i])]]
      rep.temp$condbase =rbind(rep.temp$ghostcondbase,rep.temp$condbase[,1:ncol(rep.temp$ghostcondbase)]) 
      concomps = rbind(concomps,data.frame(SScompsTA1.8(rep.temp,type="con",plotit = F)$runs_dat,imodel=i))
      
  }
    concomps=concomps[order(concomps$imodel,concomps$Fleet, concomps$Time),]
    hccomps$con = concomps
  }
  return(hccomps)
}

