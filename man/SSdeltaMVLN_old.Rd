% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSdeltaMVLN_old.R
\name{SSdeltaMVLN_old}
\alias{SSdeltaMVLN_old}
\title{SSdeltaMVLN_old()}
\usage{
SSdeltaMVLN_old(
  ss3rep,
  status = c("Bratio", "F"),
  quants = c("SSB", "Recr"),
  Fref = c("MSY", "Ftrg"),
  years = NULL,
  mc = 5000,
  weight = 1,
  run = "MVLN",
  plot = TRUE,
  addtrj = TRUE,
  ymax = NULL,
  xmax = NULL,
  legendcex = 1,
  verbose = TRUE
)
}
\arguments{
\item{ss3rep}{from r4ss::SSgetoutput()$replist1}

\item{status}{covarying stock status quantaties to extract from Hessian}

\item{quants}{additional stock quantaties to extract from Hessian}

\item{Fref}{Choice of reference point for stock SSB/XFref=c("MSY","Ftrg"),only if F_report_basis: 0 or 3}

\item{years}{single year or vector of years for mvln}

\item{mc}{number of monte-carlo simulations}

\item{weight}{weighting option for model ensembles weight*mc}

\item{run}{qualifier for model run}

\item{plot}{option to show plot}

\item{ymax}{ylim maximum}

\item{xmax}{xlim maximum}

\item{verbose}{Report progress to R GUI?}

\item{legendcex=1}{Allows to adjust legend cex}
}
\value{
output list of kobe objects and mle's
}
\description{
function to generatbe kobe pdfs from a Multivariate Log-Normal Distribution
including plotting option
}
\author{
Henning Winker (JRC-EC)
}
