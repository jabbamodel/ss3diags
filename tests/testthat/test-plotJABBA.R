library(testthat)
library(ss3diags)

test_example_path <- system.file("extdata", package = "ss3diags")

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

load(file.path(test_example_path, "natl.sma.rdata"))
load(file.path(test_example_path, "pac.hke.rdata"))
load(file.path(test_example_path, "gob.her.rdata"))

## SMA
test_that("file of sma_cpue_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3sma, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "sma_cpue2")
  
  expect_true(file.exists(file.path(path, "sma_cpue2jabbaresidual.png")))
  
  SSplotJABBAres(ss3sma, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 indexselect = c(1, 3:5),
                 plotdir = path, 
                 filenameprefix = "sma_cpue_")
  
  expect_true(file.exists(file.path(path, "sma_cpue_jabbaresidual.png")))
  
})

test_that("file of sma_len_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3sma, 
                 png = TRUE, 
                 print = T, 
                 subplots = "len", 
                 indexselect = 2:3,
                 plotdir = path, 
                 filenameprefix = "sma_len23_")
  
  expect_true(file.exists(file.path(path, "sma_len23_jabbaresidual.png")))
  
})

## PHK
test_that("file of phk_cpue_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3phk, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 plotdir = path, 
                 filenameprefix = "phk_cpue_")
  
  expect_true(file.exists(file.path(path, "phk_cpue_jabbaresidual.png")))
  
})

test_that("file of phk_age_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3phk, 
                 png = TRUE, 
                 print = T, 
                 subplots = "age", 
                 indexselect = 1,
                 plotdir = path, 
                 filenameprefix = "phk_age_")
  
  expect_true(file.exists(file.path(path, "phk_age_jabbaresidual.png")))
  
})

## HER
test_that("file of her_cpue_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3her, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "her_cpue_")
  
  expect_true(file.exists(file.path(path, "her_cpue_jabbaresidual.png")))
  
})

test_that("file of her_age_jabbaresiduals plot exists", {
  
  SSplotJABBAres(ss3her, 
                 png = TRUE, 
                 print = T, 
                 subplots = "age", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "her_age_")
  
  expect_true(file.exists(file.path(path, "her_age_jabbaresidual.png")))
  
})







