library(testthat)
library(ss3diags)

#test_example_path <- system.file("extdata", package = "ss3diags")
#load(file.path(test_example_path, "pac.hke.rdata"))

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

ss3diags::retro.phk
retrosum.phk <- r4ss::SSsummarize(retro.phk, verbose = FALSE)

## SSB
test_that("Retrospective plot is created for SSB", {
  
  SSplotRetro(retrosum.phk, 
              subplots = "SSB", 
              png = TRUE, 
              print = T,
              plotdir = path)
  
  expect_true(file.exists(file.path(path, "retro_SSB.png")))
    
  })

## F
test_that("Retrospective plot is created for F", {
  

  SSplotRetro(retrosum.phk, 
              subplots = "F", 
              png = TRUE, 
              print = T,
              plotdir = path)
  
  expect_true(file.exists(file.path(path, "retro_F.png")))
  
})

## Mohn's Rho table using SShcbias function. It's the same code used in SSplotRetro to make the table.
## SSB
test_that("Mohn's Rho table is created for SSB", {
  
  n <- retrosum.phk$n
  endyrs <- retrosum.phk$endyrs - seq(0,n-1,1)
  exp <- retrosum.phk$SpawnBio
  y.ref <- exp[, 1]
  subset <- exp$Yr <= endyrs[2] 
  subsetfc <- exp$Yr <= endyrs[2]+1
  x <- exp$Yr[subset]
  y <- exp[subset,2]
  xfc <- exp$Yr[subsetfc]
  yfc <- exp[subsetfc,2]
  
  rho.i = (y[length(y)]-y.ref[length(y)])/
    y.ref[length(y)]
  fcrho.i = (yfc[length(yfc)]-y.ref[length(yfc)])/
    y.ref[length(yfc)]

  mohn.rho <- SShcbias(retrosum.phk, quants = "SSB")
  
  expect_equal(round(mohn.rho$Rho[1], 7), rho.i)
  expect_equal(round(mohn.rho$ForcastRho[1], 8), fcrho.i)
  expect_match(mohn.rho$type, "SSB")
  
  
})

## F
test_that("Mohn's Rho table is created for F", {
  
  n <- retrosum.phk$n
  endyrs <- retrosum.phk$endyrs - seq(0,n-1,1)
  exp <- retrosum.phk$Fvalue
  y.ref <- exp[, 1]
  subset <- exp$Yr <= endyrs[2] 
  subsetfc <- exp$Yr <= endyrs[2]+1
  x <- exp$Yr[subset]
  y <- exp[subset,2]
  xfc <- exp$Yr[subsetfc]
  yfc <- exp[subsetfc,2]
  
  rho.i = (y[length(y)]-y.ref[length(y)])/
    y.ref[length(y)]
  fcrho.i = (yfc[length(yfc)]-y.ref[length(yfc)])/
    y.ref[length(yfc)]
  
  mohn.rho <- SShcbias(retrosum.phk, quants = "F")

  expect_equal(round(mohn.rho$Rho[1], 7), rho.i)
  expect_equal(round(mohn.rho$ForcastRho[1], 8), fcrho.i)
  expect_match(mohn.rho$type, "F")
  
})