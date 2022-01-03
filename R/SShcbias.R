#' Mohn's Rho and Forecast Bias 
#'
#' This function produces the statistics from retrospective analysis (Mohn's rho and forecast bias). To visualize the retrospective forecasts from a Stock Synthesis model, use SSplotRetro().
#' 
#' @param summaryoutput List created by r4ss::SSsummarize() 
#' @param models Optional subset of the models described in
#' r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param quants SSB or F quantity
#' @param verbose Report progress to R GUI?
#' @author Henning Winker (JRC-EC) and Laurance Kell (Sea++)
#' @export
#' @seealso [SSplotRetro()]
SShcbias<- function(summaryoutput, quants=c("SSB","F"),
                        models="all",
                        endyrvec="default",
                        verbose=TRUE){ 
  
  # get stuff from summary output (minimized)
  n             <- summaryoutput[["n"]]
  startyrs      <- summaryoutput[["startyrs"]]
  endyrs        <- summaryoutput[["endyrs"]]
  years         <- min(startyrs):max(endyrs)
  
  rhos <- function(quant=quants[1]){ 

  
  if(quant=="SSB"){
  
    mu    <- summaryoutput[["SpawnBio"]]
    Lower <- summaryoutput[["SpawnBioLower"]]
    Upper <- summaryoutput[["SpawnBioUpper"]]
  
    if(is.null(labels)){
      if(summaryoutput$SpawnOutputUnits[1]=="numbers"){
        labels = "Stock fecundity"
      } else {
        labels = "Spawning biomass (t)"
      }
    }
  } #end SSB quant
  
  if(quant=="F"){
    
    mu    <- summaryoutput[["Fvalue"]]
    Lower <- summaryoutput[["FvalueLower"]]
    Upper <- summaryoutput[["FvalueUpper"]]
  
    if(is.null(labels)){
      if(strsplit(summaryoutput$FvalueLabels[1],";")[[1]][1]=="_abs_F"){
        labels = "Fishing mortality F"
      } else if(strsplit(summaryoutput$FvalueLabels[1],";")[[1]][1]=="(F)/(Fmsy)"){
        labels = expression(F/F[MSY])
      } else {
        labels = "F ratio"
      }
    }
  } #end F quant
  
  ylab = labels
    
  if(models[1]=="all") models <- 1:n    
  nlines <- length(models) 
    
  
  if(endyrvec[1]=="default"){
    endyrvec <- endyrs-seq(0,n-1,1) 
  }
  if(length(endyrvec)==1){
    stop("SSplotRequires requires a minimum of one reference and one retro peel")
  }
  
  # get exp 
  exp = mu[mu$Yr%in%years,]  
  uncertainty=FALSE  
    
  yr <- exp[["Yr"]]
  
  # hindcast section
  yr.eval <- c(endyrvec)
  yr.eval <- (sort(yr.eval))
  nhc = length(endyrvec)-1
  imodel <- models[which(endyrvec==max(endyrvec))[1]]
  
  # Reference peels
  x.ref = exp[["Yr"]]
  y.ref = exp[,imodel]
  rho.i = fcrho.i = NULL

  for(iline in (2:nlines)){
    
    imodel <- models[iline]
    subset <- yr <= endyrvec[iline] 
    subsetfc <- yr <= endyrvec[iline]+1
    
    x <- yr[subset]
    y <- exp[subset,imodel]
    xfc <- yr[subsetfc]
    yfc <- exp[subsetfc,imodel]
    
    rho.i[iline-1] = (y[length(y)]-y.ref[length(y)])/
      y.ref[length(y)]
    fcrho.i[iline-1] = (yfc[length(yfc)]-y.ref[length(yfc)])/
      y.ref[length(yfc)]
    
    }
      
    
    rho =  mean(rho.i)
    fcrho= mean(fcrho.i)
    rho.table = data.frame(type=quant,
                           peel=c(endyrvec[-1],"Combined"),
                           Rho=c(rho.i,rho),
                           ForcastRho=c(fcrho.i,fcrho))
    
   return(rho.table)
  } # End of plot_retro function  
  #------------------------------------------------------------
  get_rho = NULL
  
  for(j in 1:length(quants)){
    get_rho = rbind(get_rho,rhos(quant=quants[j]))   
  }  

  
  if(verbose) message("Mohn's Rho stats, including one step ahead forecasts:")
  return(get_rho)
} # end of SSplotRetro()
#-----------------------------------------------------------------------------------------
