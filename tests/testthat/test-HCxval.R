library(testthat)
library(ss3diags)


ss3diags::retro.sma
retroI.sma <- r4ss::SSsummarize(retro.sma)

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
retrosum.phk <- r4ss::SSsummarize(retro.phk)


test_that("Hindcast plot is created for phk index", {
  SSplotHCxval(retrosum.phk,
               add=T,
               verbose=F,
               ylimAdj = 1.3,
               legendcex = 0.7, 
               png = TRUE,
               print = TRUE, 
               plotdir = path)
  
  fleets <- unique(retrosum.phk$indices$Fleet)
  plot_names <- paste0(rep(path, length(fleets)), 
                       "/", 
                       rep("hcxval", length(fleets)), 
                       "_", 
                       fleets, 
                       ".png")
  expect_true(file.exists(plot_names))

})

## Test that MASE table is correct 
#### note: function was too complicated to replicate in the test script so used values directly from running the function. If code for calculations changes, the values will change and it will error or if the SS3 input files change, the values will be different and it will error. 

test_that("MASE table gives expected values for sma", {
  
  mase <- SSplotHCxval(retroI.sma, add = T, verbose = F)
  
  expect_match(mase$Index[1], "CPUE_1")
  expect_equal(round(mase$MASE[1], 7), 0.8905631)
  expect_equal(round(mase$MAE.PR[1], 8), 0.09920418)

  
})

test_that("MASE table gives expected values for phk", {
  
  mase <- SSplotHCxval(retrosum.phk, add = T, verbose = F)
  
  expect_match(mase$Index[1], "Acoustic_Survey")
  expect_equal(round(mase$MASE[1], 6), 1.064745)
  expect_equal(round(mase$MAE.PR[1], 7), 0.3022749)
  
  
  
})


test_that("SSretroComps returns the correct comp data for sma", {
  
  retro_comps <- SSretroComps(retro.sma)
  
  expect_equal(retro_comps$n, 6)
  expect_equal(retro_comps$startyrs, rep(1950, 6))
  expect_equal(retro_comps$endyrs, rep(2015, 6))
  expect_equal(round(retro_comps$len$Obs[1], 4), 121.3780)
  
})


test_that("SSmase base.adj changes", {
  
  ssmase <- SSmase(retro.sma, MAE.base.adj = 0.15)
  
  expect_equal(round(ssmase$MASE.adj[1], 7), 0.6613612)
  
})

