## Test script for JABBA residual plot

simple <- ss3diags::simple

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

#ss3diags::diags_simple

## Simple
test_that("file of simple_cpue_jabbaresiduals plot exists", {

  
  SSplotJABBAres(simple, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 plotdir = path, 
                 filenameprefix = "simple_cpue")
  
  expect_true(file.exists(file.path(path, "simple_cpuejabbaresidual.png")))
  
  
})

test_that("file of simple_len_jabbaresiduals plot exists", {
  
  SSplotJABBAres(simple, 
                 png = TRUE, 
                 print = T, 
                 subplots = "len", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "simple_len2_")
  
  expect_true(file.exists(file.path(path, "simple_len2_jabbaresidual.png")))
  
})

##CAAL uncomment when con option is finished in function
# test_that("file of simple_con_jabbaresiduals plot exists", {
#   
#   SSplotJABBAres(simple, 
#                  png = TRUE, 
#                  print = T, 
#                  subplots = "con",
#                  plotdir = path, 
#                  filenameprefix = "simple_con_")
#   
#   expect_true(file.exists(file.path(path, "simple_con_jabbaresidual.png")))
#   
# })


