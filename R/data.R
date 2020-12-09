#' @name pac.hke
#' @title 2020 Pacific Hake Stock Synthesis model 
#' @description Stock Synthesis (V3.30.14) base-case run, list of retrospective runs,
#'  list of base-case and aspm runs  
#' @docType data
#' @usage pac.hke
#' @format \code{r4ss} list objects
#' \describe{
#'  \item{ss3phke}{output from Stock Synthesis as read by r4ss::SS_output()}
#'  \item{retro.phk}{list of retrospective runs with r4ss::SS_doRetro() as read by r4ss::SSgetoutput()}
#'  \item{aspm.phk}{list of Stock Synthesis referece and aspm created with r4ss::SSsummarize()}
#' }
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting 
#' Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. Prepared by the Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada. 273 p.
NULL

#' @name natl.sma
#' @title ICCAT 2017 North Atlantic Shortfin Mako (SMA) SS3 Model Run 3
#' @description Stock Synthesis (V3.30.15) reference run 3, list of retrospective runs,
#'  list of reference and aspm runs 
#' @docType data
#' @usage natl.sma
#' @format \code{r4ss} list objects 
#' \describe{
#'  \item{ss3sma}{output from Stock Synthesis as read by r4ss::SS_output()}
#'  \item{retro.sma}{list of retrospective runs with r4ss::SS_doRetro() as read by r4ss::SSgetoutput()}
#'  \item{aspm.sma}{list of Stock Synthesis referece and aspm created with r4ss::SSsummarize()}
#' }
#' @source ICCAT 2017. Report of the 2017 Shortfin Mako Assessment Meeting (Madrid, Spain 12–16 June 2017).Collect. Vol. Sci. Pap.
#'  
#' @author C Courtney, D., Carvalho, C. Winker, H., and LT Kell. (2020) Examples of Stock Synthesis diagnostic methods and results implemented for previously completed North Atlantic shortfin mako Stock Synthesis model runs. Collect. Vol. Sci. Pap. ICCAT, 76(10): 173-234.
NULL
