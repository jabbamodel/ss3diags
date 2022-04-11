#' @name pac.hke
#' @title 2020 Pacific Hake Stock Synthesis model
#' @description Information relevant to Pacific Hake (HKE) example, including
#' Stock Synthesis (V3.30.14) base-case run, list of retrospective runs,
#' and list of base-case and aspm runs.
#' @docType data
#' @usage pac.hke
#' @format Individual list objects generated using \pkg{r4ss}:
#' \describe{
#'  \item{ss3phk}{output from Stock Synthesis as read by \code{\link[r4ss]{SS_output}()}}
#'  \item{retro.phk}{list of retrospective runs with \code{\link[r4ss]{SS_doRetro}()} as read by \code{\link[r4ss]{SSgetoutput}()}}
#'  \item{aspm.phk}{list of Stock Synthesis referece and aspm created with \code{\link[r4ss]{SSsummarize}()}}
#' }
#' @source 2020 Pacific hake/whiting Stock Synthesis assessment. Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting
#' Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada
#' @author Grandin, C.J., K.F. Johnson, A.M. Edwards, and A.M. Berger. (2020). Status of the Pacific Hake (whiting) stock in U.S. and Canadian waters in 2020. Prepared by the Joint Technical Committee of the U.S. and Canada Pacific Hake/Whiting Agreement, National Marine Fisheries Service and Fisheries and Oceans Canada. 273 p.
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
#' @usage natl.sma
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
