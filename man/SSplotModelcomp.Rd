% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSplotModelcomp.R
\name{SSplotModelcomp}
\alias{SSplotModelcomp}
\title{SSplotHCxal() for one-step ahead hindcasting cross-validations of indices}
\usage{
SSplotModelcomp(
  summaryoutput = aspm.sma,
  plot = TRUE,
  print = FALSE,
  png = print,
  pdf = FALSE,
  models = "all",
  subplots = c("SSB", "Bratio", "Fvalue", "Recruits", "Index", "RecDevs"),
  brp = c("msy", "btargs"),
  fmsy = TRUE,
  ylabs = c("SSB (t)", expression(SSB/SSB[MSY]), "Fishing mortality F",
    "Recruits ('000s)", "Index", "Recruitment Deviations"),
  endyrvec = "default",
  xmin = NULL,
  indexselect = NULL,
  indexUncertainty = TRUE,
  col = NULL,
  pch = NULL,
  lty = 1,
  lwd = 2,
  tickEndYr = FALSE,
  xlim = "default",
  ylimAdj = 1.05,
  xaxs = "i",
  yaxs = "i",
  xylabs = TRUE,
  type = "l",
  uncertainty = TRUE,
  legend = TRUE,
  legendlabels = "default",
  legendloc = "topright",
  legendorder = "default",
  legendncol = 1,
  legendcex = 1,
  legendsp = 0.9,
  legendindex = NULL,
  pwidth = 6.5,
  pheight = 5,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  plotdir = NULL,
  filenameprefix = "",
  par = list(mar = c(5, 4, 1, 1) + 0.1),
  verbose = TRUE,
  shadecol = NULL,
  shadealpha = 0.3,
  new = TRUE,
  add = FALSE,
  mcmcVec = FALSE,
  indexQlabel = TRUE,
  indexQdigits = 4
)
}
\arguments{
\item{summaryoutput}{List created by r4ss::SSsummarize()}

\item{plot}{plot to active plot device?}

\item{print}{print to PNG files?}

\item{pdf}{not tested for TRUE}

\item{models}{Optional subset of the models described in
r4ss function summaryoutput().  Either "all" or a vector of numbers indicating
columns in summary tables.}

\item{subplots}{option to "SSB","Bratio","Fvalue","Recruits","Index"}

\item{brp}{option to set reference point c("msy","btargs")}

\item{fmsy}{to specify Fvalue as F/Fmsy if so in starter file setting}

\item{ylabs}{yaxis labels for quants
final year of values to show for each model. By default it is set to the}

\item{endyrvec}{ending year specified in each model.}

\item{xmin}{= NULL optional number first year shown in plot (if available)}

\item{indexselect}{= Vector of fleet numbers for each model for which to compare}

\item{indexUncertainty}{Show fixed uncertainty intervals on index (not estimated)}

\item{col}{Optional vector of colors to be used for lines. Input NULL}

\item{pch}{Optional vector of plot character values}

\item{lty}{Optional vector of line types}

\item{lwd}{Optional vector of line widths}

\item{tickEndYr}{TRUE/FALSE switch to turn on/off extra axis mark at final
year in timeseries plots.}

\item{ylimAdj}{Multiplier for ylim parameter. Allows additional white space}

\item{xaxs}{Choice of xaxs parameter (see ?par for more info)}

\item{yaxs}{Choice of yaxs parameter (see ?par for more info)}

\item{type}{Type parameter passed to points (default 'o' overplots points on
top of lines)}

\item{legend}{Add a legend?}

\item{legendlabels}{Optional vector of labels to include in legend.}

\item{legendloc}{Location of legend. Either a string like "topleft" or a vector
of two numeric values representing the fraction of the maximum in the x and y
dimensions, respectively. See ?legend for more info on the string options.}

\item{legendorder}{Optional vector of model numbers that can be used to have
the legend display the model names in an order that is different than that
which is represented in the summary input object.}

\item{legendncol}{Number of columns for the legend.}

\item{legendsp}{Space between legend labels}

\item{legendindex}{Allows to add lengend for selected indices (plots)}

\item{pwidth}{Width of plot}

\item{pheight}{Height of plot}

\item{punits}{Units for PNG file}

\item{res}{Resolution for PNG file}

\item{ptsize}{Point size for PNG file}

\item{cex.main}{Character expansion for plot titles}

\item{plotdir}{Directory where PNG or PDF files will be written. By default
it will be the directory where the model was run.}

\item{filenameprefix}{Additional text to append to PNG or PDF file names.
It will be separated from default name by an underscore.}

\item{par}{list of graphics parameter values passed to par() function}

\item{verbose}{Report progress to R GUI?}

\item{shadecol}{uncertainty shading of hcxval horizon}

\item{shadealpha}{Transparency adjustment used to make default shadecol}

\item{new}{Create new empty plot window}

\item{add}{surpresses par() to create multiplot figs}

\item{mcmcVec}{NOT TESTED Vector of TRUE/FALSE values (or single value) indicating}

\item{indexQlabel}{Add catchability to legend in plot of index fits (TRUE/FALSE)?}

\item{indexQdigits}{Number of significant digits for catchability in legend}

\item{indexfleets}{CHECK IF NEEDED or how to adjust indexfleets}

\item{legendcex=1}{Allows to adjust legend cex}
}
\description{
Plots one-step ahead hindcasting cross-validations and computes MASE from prediction redisuals
}
\author{
Mostly adopted from r4ss::SSplotComparisons by Taylor et al
}
