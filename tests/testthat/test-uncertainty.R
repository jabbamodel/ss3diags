# Test script for evaluating model uncertainty

simple <- ss3diags::simple

mvln.msy <- SSdeltaMVLN(simple, Fref = "MSY")

test_that("mle table values are as expected for Simple with Fref = MSY", {
  
  expect_equal(round(mvln.msy$mle$harvest[1], 7), 0.8845945)
  expect_equal(round(mvln.msy$mle$stock[1], 7), 2.0921525)
  expect_equal(round(mvln.msy$mle$F[1], 7), 0.1009570)
  
})


test_that("Kobe plot values are correct", {
  
  kb <- mvln.msy$kb[which(mvln.msy$kb$year == 100),] #last year of model
    
  kb$green = ifelse(kb$stock > 1 & kb$harvest < 1, 1, 0) 
  kb$red = ifelse(kb$stock < 1 & kb$harvest > 1, 1, 0) 
  kb$yellow = ifelse(kb$stock < 1 & kb$harvest < 1, 1, 0)
  kb$orange = ifelse(kb$stock > 1 & kb$harvest > 1, 1, 0) 
  pgreen = sum(kb$green)/nrow(kb)*100
  pred = sum(kb$red)/nrow(kb)*100
  pyellow = sum(kb$yellow)/nrow(kb)*100
  porange = sum(kb$orange)/nrow(kb)*100
  
  kb.p <- SSplotKobe(mvln.msy$kb)
  
  expect_equal(kb.p$Percent[1], pred)
  expect_equal(kb.p$Percent[2], porange)
  expect_equal(kb.p$Percent[3], pyellow)
  expect_equal(kb.p$Percent[4], pgreen)
  
})

