library(testthat)
library(ss3diags)


ss3diags::retro.sma
retroI.sma <- r4ss::SSsummarize(retro.sma, verbose = FALSE)

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## Hindcast plotting of indices

test_that("Hindcast plot is created for sma indices", {
  
  
  SSplotHCxval(retroI.sma,
               add=T,
               verbose=F,
               ylimAdj = 1.3,
               legendcex = 0.7, 
               png = TRUE,
               print = TRUE, 
               plotdir = path)
  
  fleets <- unique(retroI.sma$indices$Fleet)
  plot_names <- paste0(rep(path, length(fleets)), 
                       "/", 
                       rep("hcxval", length(fleets)), 
                       "_", 
                       fleets, 
                       ".png")
  
  ## Check that each plot exists individually
  expect_true(file.exists(plot_names[1]))
  expect_true(file.exists(plot_names[2]))
  expect_true(file.exists(plot_names[3]))
  expect_true(file.exists(plot_names[4]))
  expect_true(file.exists(plot_names[5]))
  expect_true(file.exists(plot_names[6]))
  
})



ss3diags::retro.phk
retrosum.phk <- r4ss::SSsummarize(retro.phk, verbose = FALSE)


test_that("Hindcast plot is created for phk index", {
  SSplotHCxval(retrosum.phk,
               add=T,
               verbose=F,
               ylimAdj = 1.3,
               legendcex = 0.7, 
               png = TRUE,
               print = TRUE, 
               plotdir = getwd())
  
  fleets <- unique(retrosum.phk$indices$Fleet)
  plot_names <- paste0(rep(getwd(), length(fleets)), 
                       "/", 
                       rep("hcxval", length(fleets)), 
                       "_", 
                       fleets, 
                       ".png")
  expect_true(file.exists(plot_names))

})

## Test that MASE table is correct 

