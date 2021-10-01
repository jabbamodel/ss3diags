#' @name pac.hke
#' @title 2020 Pacific Hake Stock Synthesis model
#' @description Information relevant to Pacific Hake (HKE) example, including
#' Stock Synthesis (V3.30.14) base-case run, list of retrospective runs,
#' and list of base-case and aspm runs.
#' @docType data
#' @format Individual list objects generated using \pkg{r4ss}:
#' \describe{
#'  \item{ss3phk}{output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}}
#'  \item{retro.phk}{list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}}
#'  \item{aspm.phk}{list of Stock Synthesis referece and aspm created with \code{\link[r4ss]{SSsummarize}()}}
#' }
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting
#' Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. Prepared by the Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada. 273 p.
#' 
#' @examples
#' \dontrun{
#' data(pac.hke, package = "ss3diags")
#' # List the three objects returned by the line above
#' ls(pattern = "phk")
#' # The list of retrospective runs, which contains 8 lists, one for each peel
#' str(retro.phk, max.level = 1)
#' }
NULL


#' @name natl.sma
#' @title ICCAT 2017 North Atlantic Shortfin Mako Stock Synthesis model run 3
#' @description Information relevant to North Atlantic Shortfin Mako (SMA), including
#' Stock Synthesis (V3.30.15) reference run 3, list of retrospective runs,
#' and list of reference run 3 and aspm runs.
#' @docType data
#' @format Individual list objects generated using \pkg{r4ss}
#' \describe{
#'  \item{ss3sma}{output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}}
#'  \item{retro.sma}{list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}}
#'  \item{aspm.sma}{list of Stock Synthesis referece and aspm created with \code{\link[r4ss]{SSsummarize}()}}
#' }
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' @examples
#' \dontrun{
#' data(natl.sma, package = "ss3diags")
#' # List the three objects returned by the line above
#' ls(pattern = "sma")
#' }
NULL


#' ICCAT 2017 North Atlantic Shortfin Mako ASPM Stock Synthesis model summarization
#' 
#' List of Stock Synthesis reference run #3 and aspm created with \code{\link[r4ss]{SSsummarize}()}
#' 
#' @docType data
#' 
#' @format Large list of 54 objects generated using \pkg{r4ss}
#' \describe{
#'  \item{n}{n}
#'  \item{npars}{number of pars}
#'  \item{modelnames}{modelnames}
#'  \item{maxgrad}{maxgrad} 
#'  \item{nsexes}{nsexes} 
#'  \item{startyrs}{startyrs} 
#'  \item{endyrs}{endyrs} 
#'  \item{pars}{pars} 
#'  \item{parsSD}{parsSD} 
#'  \item{parphases}{parphases} 
#'  \item{quants}{quants} 
#'  \item{quantsSD}{quantsSD} 
#'  \item{likelihoods}{likelihoods} 
#'  \item{likelambdas}{likelambdas} 
#'  \item{likelihoods_by_fleet}{likelihoods_by_fleet} 
#'  \item{SpawnBio}{SpawnBio} 
#'  \item{SpawnBioSD}{SpawnBioSD} 
#'  \item{SpawnBioLower}{SpawnBioLower} 
#'  \item{SpawnBioUpper}{SpawnBioUpper} 
#'  \item{Bratio}{Bratio} 
#'  \item{BratioSD}{BratioSD} 
#'  \item{BratioLower}{BratioLower} 
#'  \item{BratioUpper}{BratioUpper} 
#'  \item{SPRratio}{SPRratio} 
#'  \item{SPRratioSD}{SPRratioSD} 
#'  \item{SPRratioLower}{SPRratioLower} 
#'  \item{SPRratioUpper}{SPRratioUpper} 
#'  \item{SPRratioLabels}{SPRratioLabels} 
#'  \item{Fvalue}{Fvalue} 
#'  \item{FvalueSD}{FvalueSD} 
#'  \item{FvalueLower}{FvalueLower} 
#'  \item{FvalueUpper}{FvalueUpper} 
#'  \item{FvalueLabels}{FvalueLabels} 
#'  \item{sprtargs}{sprtargs} 
#'  \item{btargs}{btargs} 
#'  \item{minbthreshs}{minbthreshs} 
#'  \item{recruits}{recruits} 
#'  \item{recruitsSD}{recruitsSD} 
#'  \item{recruitsLower}{recruitsLower} 
#'  \item{recruitsUpper}{recruitsUpper} 
#'  \item{recdevs}{recdevs} 
#'  \item{recdevsSD}{recdevsSD} 
#'  \item{recdevsLower}{recdevsLower} 
#'  \item{recdevsUpper}{recdevsUpper} 
#'  \item{growth}{growth} 
#'  \item{sizesel}{sizesel} 
#'  \item{agesel}{agesel} 
#'  \item{indices}{indices} 
#'  \item{InitAgeYrs}{InitAgeYrs} 
#'  \item{lowerCI}{lowerCI} 
#'  \item{upperCI}{upperCI} 
#'  \item{SpawnOutputUnits}{SpawnOutputUnits} 
#'  \item{FleetNames}{FleetNames}
#'  \item{mcmc}{mcmc}
#' }
#' 
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'
#' @author Courtney, C., D., Carvalho, C. Winker, H., and LT Kell. (2020). Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
#' 
#'
"aspm.phk" 

