library(ss3diags)
library(testthat)
library(dplyr)

ss3diags::ss3sma

mvln.msy <- SSdeltaMVLN(ss3sma, Fref = "MSY")

test_that("mle table values are as expected for SMA with Fref = MSY", {
  
  expect_equal(round(mvln.msy$mle$harvest[1], 7), 0.0417581)
  expect_equal(round(mvln.msy$mle$stock[1], 5), 1.91848)
  expect_equal(round(mvln.msy$mle$F[1], 10), 0.0023503797)
  
})


test_that("Kobe plot values are correct", {
  
  kb <- mvln.msy$kb %>% 
    filter(year == 2015) %>% 
    mutate(green = ifelse(stock > 1 & harvest < 1, 1, 0), 
           red = ifelse(stock < 1 & harvest > 1, 1, 0), 
           yellow = ifelse(stock < 1 & harvest < 1, 1, 0),
           orange = ifelse(stock > 1 & harvest > 1, 1, 0)) %>% 
    summarise(pgreen = sum(green)/nrow(.)*100,
              pred = sum(red)/nrow(.)*100,
              pyellow = sum(yellow)/nrow(.)*100,
              porange = sum(orange)/nrow(.)*100)
  
  kb.p <- SSplotKobe(mvln.msy$kb)
  
  expect_equal(kb.p$Percent[1], kb$pred)
  expect_equal(kb.p$Percent[2], kb$porange)
  expect_equal(kb.p$Percent[3], kb$pyellow)
  expect_equal(kb.p$Percent[4], kb$pgreen)
  
})

