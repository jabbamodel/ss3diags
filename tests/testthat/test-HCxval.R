## Test script for hindcast cross validation and MASE

test_example_path <- system.file("data", package = "ss3diags")
#ss3diags::retro_simple
load("C:/Users/Megumi.Oshima/Documents/ss3diags/data/retro_simple.rda")

retrosum.simple <- r4ss::SSsummarize(retroSimple)

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## Hindcast plotting of indices

test_that("Hindcast plot is created for sma indices", {
  
  
  SSplotHCxval(retrosum.simple,
               add=T,
               verbose=F,
               legendcex = 0.7, 
               png = TRUE,
               print = TRUE, 
               plotdir = path)
  
  fleets <- unique(retrosum.simple$indices$Fleet)
  plot_names <- paste0(rep(path, length(fleets)), 
                       "/", 
                       rep("hcxval", length(fleets)), 
                       "_", 
                       fleets, 
                       ".png")
  
  ## Check that each plot exists individually
  expect_true(file.exists(plot_names[1]))

  
})


## Test that MASE table is correct 
#### note: function was too complicated to replicate in the test script so used values directly from running the function. If code for calculations changes, the values will change and it will error or if the SS3 input files change, the values will be different and it will error. 

test_that("MASE table gives expected values for sma", {
  
  mase <- SSplotHCxval(retrosum.simple, add = T, verbose = F)
  
  expect_match(mase$Index[1], "Survey")
  expect_equal(round(mase$MASE[1], 7), 0.5836339)
  expect_equal(round(mase$MAE.PR[1], 7), 0.1856207)

  
})


test_that("SSretroComps returns the correct comp data for simple model", {
  
  retro_comps <- SSretroComps(retroSimple)
  
  expect_equal(retro_comps$n, 6)
  expect_equal(retro_comps$startyrs, rep(26, 6))
  expect_equal(retro_comps$endyrs, rep(100, 6))
  expect_gt(nrow(retro_comps$con), 1)
  expect_gt(nrow(retro_comps$len), 1)
  expect_gt(nrow(retro_comps$age), 1)
  
  
})


test_that("SSmase base.adj changes", {
  
  ssmase <- SSmase(retrosum.simple, MAE.base.adj = 0.15)
  
  expect_equal(round(ssmase$MASE.adj[1], 7), 0.8503379)
  
})

