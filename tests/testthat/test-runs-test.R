#library(ss3diags)
#library(testthat)

#test_example_path <- system.file("extdata", package = "ss3diags")

#### Shortfin Mako Model ###############################################################
########################################################################################

## First test to make sure 3 objects load with the data files
test_that("sma.Rdata has correct files", {
  
  load(file.path(test_example_path, "natl.sma.rdata"))
  expect_equal(length(ls(pattern = "sma")), 3)
  expect_match(ls(pattern = "sma"), c("ss3|aspm|retro"))
  
})

## SSrunstest function
test_that("runs test works with shortfin mako", {
  
  load(file.path(test_example_path, "natl.sma.rdata"))
  
  test.resids <- ss3sma$cpue[which(ss3sma$cpue$Fleet_name == "CPUE_1"), c("Fleet_name", "Yr", "Obs", "Exp")]
  test.resids$residuals = log(test.resids$Obs) - log(test.resids$Exp)
  
  mu <- 0 
  mr <- abs(diff(test.resids$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  runstest <- randtests::runs.test(test.resids$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  ## for cpue
  n.cpue <- length(unique(ss3sma$cpue$Fleet))
  run_cpue <- SSrunstest(ss3sma, quants = "cpue")
  
  ## testing structure of dataframe
  expect_match(run_cpue$Index[1], "CPUE_1")
  expect_equal(nrow(run_cpue), n.cpue)
  ## testing values in the first row
  expect_equal(run_cpue$runs.p[1], test.p)
  expect_equal(run_cpue$sigma3.lo[1], lcl)
  expect_equal(run_cpue$sigma3.hi[1], ucl)
  
  ## specifying cpue index
  run_cpue <- SSrunstest(ss3sma, quants = "cpue", indexselect = 4)
  expect_match(run_cpue$Index, "CPUE_4")
  run_cpue <- SSrunstest(ss3sma, quants = "cpue", indexselect = 3:5)
  expect_equal(run_cpue$Index, c("CPUE_3", "CPUE_4", "CPUE_5"))
  
  ## for length comp
  len.test.resids <- ss3sma$lendbase[which(ss3sma$lendbase$Fleet == 1),]
  len.test.resids$indx = paste(len.test.resids$Fleet, len.test.resids$Yr, len.test.resids$Seas)
  
  uind <- unique(len.test.resids$indx)
  pldat <- matrix(0,length(uind),13,
                  dimnames=list(uind,
                                c('Obsmn','Obslo','Obshi','semn','Expmn','Like','Std.res',
                                  'ObsloAdj','ObshiAdj','Fleet','Yr','Time','Seas')))
  
  for(i in 1:length(uind)){  
    subdbase <- len.test.resids[which(len.test.resids$indx == uind[i]),]
     
    if(is.null(subdbase$Nsamp_adj)) subdbase$Nsamp_adj = subdbase$N 
    xvar <- subdbase$Bin
    pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
    pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
    pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                               pldat[i,'Expmn']^2)/mean(subdbase$Nsamp_adj))
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Yr'] <- mean(subdbase$Yr) 
    pldat[i,'Time'] <- mean(subdbase$Time)
    pldat[i,'Seas'] <- mean(subdbase$Seas)
    pldat[i,'Like'] <- mean(subdbase$Like)
    
  }
  
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)
  
  for(i in 1:length(uind)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }
  
  pldat <- data.frame(pldat)
  yrs <- pldat$Yr
  
  runs_dat <- data.frame(Fleet=pldat$Fleet,
                         Fleet_name=ss3sma$FleetNames[pldat$Fleet],
                         Yr=yrs,
                         Time=pldat$Time,
                         Seas=pldat$Seas,
                         Obs=pldat$Obsmn,
                         Exp=pldat$Expmn,
                         SE=((pldat$Obsmn-pldat$ObsloAdj)/1.96)/pldat$ObsloAdj,
                         Like=pldat$Like)
  
  runs_dat$residuals <- log(runs_dat$Obs) - log(runs_dat$Exp)
  runstest <- randtests::runs.test(runs_dat$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  mu <- 0 
  mr <- abs(diff(runs_dat$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  
  
  n.fish <- nrow(ss3sma$Length_Comp_Fit_Summary)
  run_fish <- SSrunstest(ss3sma, quants = "len")
  
  ## testing structure of dataframe
  expect_match(run_fish$Index[1], "Fishery_1")
  expect_equal(nrow(run_fish), n.fish)
  ## testing values in first row
  expect_equal(run_fish$runs.p[1], test.p)
  expect_equal(run_fish$sigma3.lo[1], lcl)
  expect_equal(run_fish$sigma3.hi[1], ucl)
  
})


#### Pacific Hake ######################################################################
########################################################################################

## First test to make sure 3 objects load with the data files
test_that("pac.hke.Rdata has correct files", {
  
  load(file.path(test_example_path, "pac.hke.rdata"))
  expect_equal(length(ls(pattern = "phk")), 3)
  expect_match(ls(pattern = "phk"), c("ss3|aspm|retro"))
  
})

## SSrunstest function
test_that("runs test works with pacific hake", {
  
  load(file.path(test_example_path, "pac.hke.rdata"))
  
 
  test.resids <- ss3phk$cpue[,c("Fleet_name", "Yr", "Obs", "Exp", "Like")]
  test.resids$residuals <- ifelse(is.na(test.resids$Obs) | is.na(test.resids$Like),NA, log(test.resids$Obs)-log(test.resids$Exp))
  
  mu <- 0
  mr <- abs(diff(test.resids$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  runstest <- randtests::runs.test(test.resids$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  ## for cpue
  n.cpue <- length(unique(ss3phk$cpue$Fleet))
  run_cpue <- SSrunstest(ss3phk, quants = "cpue")
  
  ## testing structure of dataframe
  expect_match(run_cpue$Index[1], "Acoustic_Survey")
  expect_equal(nrow(run_cpue), n.cpue)
  ## testing values in the first row
  expect_equal(run_cpue$runs.p[1], test.p)
  expect_equal(run_cpue$sigma3.lo[1], lcl) 
  expect_equal(run_cpue$sigma3.hi[1], ucl) 
  
  
  ## for age comp
  age.test.resids <- ss3phk$agedbase[which(ss3phk$agedbase$Fleet == 2),]
  age.test.resids$indx <- paste(age.test.resids$Fleet, age.test.resids$Yr, age.test.resids$Seas)
  
  uind <- unique(age.test.resids$indx)
  pldat <- matrix(0,length(uind),13,
                  dimnames=list(uind,
                                c('Obsmn','Obslo','Obshi','semn','Expmn','Like','Std.res',
                                  'ObsloAdj','ObshiAdj','Fleet','Yr','Time','Seas')))
  
  for(i in 1:length(uind)){  
    subdbase <- age.test.resids[which(age.test.resids$indx == uind[i]),]
    
    if(is.null(subdbase$Nsamp_adj)) subdbase$Nsamp_adj = subdbase$N 
    xvar <- subdbase$Bin
    pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
    pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
    pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                               pldat[i,'Expmn']^2)/mean(subdbase$Nsamp_adj))
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Yr'] <- mean(subdbase$Yr) 
    pldat[i,'Time'] <- mean(subdbase$Time)
    pldat[i,'Seas'] <- mean(subdbase$Seas)
    pldat[i,'Like'] <- mean(subdbase$Like)
    
  }
  
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)
  
  for(i in 1:length(uind)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }
  
  pldat <- data.frame(pldat)
  yrs <- pldat$Yr
  
  runs_dat <- data.frame(Fleet=pldat$Fleet,
                         Fleet_name=ss3sma$FleetNames[pldat$Fleet],
                         Yr=yrs,
                         Time=pldat$Time,
                         Seas=pldat$Seas,
                         Obs=pldat$Obsmn,
                         Exp=pldat$Expmn,
                         SE=((pldat$Obsmn-pldat$ObsloAdj)/1.96)/pldat$ObsloAdj,
                         Like=pldat$Like)
  
  runs_dat$residuals <- log(runs_dat$Obs) - log(runs_dat$Exp)
  runstest <- randtests::runs.test(runs_dat$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  mu <- 0 
  mr <- abs(diff(runs_dat$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  
  
  n.fish <- nrow(ss3phk$Age_Comp_Fit_Summary)
  run_fish <- SSrunstest(ss3phk, quants = "age")
  
  ## testing structure of dataframe
  expect_match(run_fish$Index[1], "Fishery")
  expect_equal(nrow(run_fish), n.fish)
  ## testing values in first row
  expect_equal(run_fish$runs.p[2], test.p)
  expect_equal(run_fish$sigma3.lo[2], lcl)
  expect_equal(run_fish$sigma3.hi[2], ucl)
  
})


#### GOB Herring Model #################################################################
########################################################################################

## First test to make sure 3 objects load with the data files
test_that("gob.her.Rdata has correct files", {
  
  load(file.path(test_example_path, "gob.her.rdata"))
  expect_equal(length(ls(pattern = "her")), 2)
  expect_match(ls(pattern = "her"), c("ss3|mcmc"))
  
})

## SSrunstest function
test_that("runs test works with GOB Herring", {
  
  load(file.path(test_example_path, "gob.her.rdata"))
  
  test.resids <- ss3her$cpue[which(ss3her$cpue$Fleet_name == "Acoustics"), c("Fleet_name", "Yr", "Obs", "Exp")]
  test.resids$residuals = log(test.resids$Obs) - log(test.resids$Exp)
  
  mu <- 0 
  mr <- abs(diff(test.resids$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  runstest <- randtests::runs.test(test.resids$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  ## for cpue
  n.cpue <- length(unique(ss3her$cpue$Fleet))
  run_cpue <- SSrunstest(ss3her, quants = "cpue")
  
  ## testing structure of dataframe
  expect_match(run_cpue$Index[1], "Acoustics")
  expect_equal(nrow(run_cpue), n.cpue)
  ## testing values in the first row
  expect_equal(run_cpue$runs.p[1], test.p)
  expect_equal(run_cpue$sigma3.lo[1], lcl)
  expect_equal(run_cpue$sigma3.hi[1], ucl)
  
  ## specifying cpue index
  run_cpue <- SSrunstest(ss3her, quants = "cpue", indexselect = 2)
  expect_match(run_cpue$Index, "Trapnet")
  
  age.test.resids <- ss3her$agedbase[which(ss3her$agedbase$Fleet == 3),]
  age.test.resids$indx <- paste(age.test.resids$Fleet, age.test.resids$Yr, age.test.resids$Seas)
  
  uind <- unique(age.test.resids$indx)
  pldat <- matrix(0,length(uind),13,
                  dimnames=list(uind,
                                c('Obsmn','Obslo','Obshi','semn','Expmn','Like','Std.res',
                                  'ObsloAdj','ObshiAdj','Fleet','Yr','Time','Seas')))
  
  for(i in 1:length(uind)){  
    subdbase <- age.test.resids[which(age.test.resids$indx == uind[i]),]
    
    if(is.null(subdbase$Nsamp_adj)) subdbase$Nsamp_adj = subdbase$N 
    xvar <- subdbase$Bin
    pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
    pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
    pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                               pldat[i,'Expmn']^2)/mean(subdbase$Nsamp_adj))
    pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
    pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
    pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
    pldat[i,'Fleet'] <- mean(subdbase$Fleet)
    pldat[i,'Yr'] <- mean(subdbase$Yr) 
    pldat[i,'Time'] <- mean(subdbase$Time)
    pldat[i,'Seas'] <- mean(subdbase$Seas)
    pldat[i,'Like'] <- mean(subdbase$Like)
    
  }
  
  Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)
  
  for(i in 1:length(uind)){
    pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
    pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
  }
  
  pldat <- data.frame(pldat)
  yrs <- pldat$Yr
  
  runs_dat <- data.frame(Fleet=pldat$Fleet,
                         Fleet_name=ss3sma$FleetNames[pldat$Fleet],
                         Yr=yrs,
                         Time=pldat$Time,
                         Seas=pldat$Seas,
                         Obs=pldat$Obsmn,
                         Exp=pldat$Expmn,
                         SE=((pldat$Obsmn-pldat$ObsloAdj)/1.96)/pldat$ObsloAdj,
                         Like=pldat$Like)
  
  runs_dat$residuals <- log(runs_dat$Obs) - log(runs_dat$Exp)
  runstest <- randtests::runs.test(runs_dat$residuals, 
                                   threshold = 0, 
                                   alternative = "left.sided")
  test.p <- round(runstest$p.value, 3)
  
  mu <- 0 
  mr <- abs(diff(runs_dat$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  
  
  n.fish <- nrow(ss3her$Age_Comp_Fit_Summary)
  run_fish <- SSrunstest(ss3her, quants = "age")
  
  ## testing structure of dataframe
  expect_match(run_fish$Index[1], "Fleet")
  expect_equal(nrow(run_fish), n.fish)
  ## testing values in first row
  expect_equal(run_fish$runs.p[3], test.p)
  expect_equal(run_fish$sigma3.lo[3], lcl)
  expect_equal(run_fish$sigma3.hi[3], ucl)
  
})


#### Snapshot plots for all 3 stocks ###################################################
########################################################################################


## Testing plotting function
#### right now it only checks if a plot file is saved in the temp directory, would like to use expect_snapshot_file to actually compare new plots to the original but need to be able to save the plot object not just the dataframe from SSplotRunstest() ####

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## SMA
test_that("snapshot of sma_cpue", {
  
  SSplotRunstest(ss3sma, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 indexselect = 3, 
                 plotdir = path, 
                 filenameprefix = "sma_")
  
  expect_true(file.exists(file.path(path, "sma_residruns_CPUE_3.png")))
  
})


test_that("snapshot of sma_len", {
  
  SSplotRunstest(ss3sma, 
                 png = TRUE, 
                 print = T, 
                 subplots = "len", 
                 indexselect = c(2:3), 
                 plotdir = path, 
                 filenameprefix = "sma_")
  
  expect_true(file.exists(file.path(path, "sma_residruns_Fishery_2.png")))
  expect_true(file.exists(file.path(path, "sma_residruns_Fishery_3.png")))
  
})

## PHK
test_that("snapshot of phk_cpue", {
  
  SSplotRunstest(ss3phk, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 plotdir = path, 
                 filenameprefix = "phk_")
  
  expect_true(file.exists(file.path(path, "phk_residruns_Acoustic_Survey.png")))
  
})

test_that("snapshot of phk_age", {
  
  SSplotRunstest(ss3phk, 
                 png = TRUE, 
                 print = T, 
                 subplots = "age", 
                 indexselect = 1,
                 plotdir = path, 
                 filenameprefix = "phk_")
  
  expect_true(file.exists(file.path(path, "phk_residruns_Fishery.png")))
  
})

## HER
test_that("snapshot of her_cpue", {
  
  SSplotRunstest(ss3her, 
                 png = TRUE, 
                 print = T, 
                 subplots = "cpue", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "her_")
  
  expect_true(file.exists(file.path(path, "her_residruns_Trapnet.png")))
  
})

test_that("snapshot of her_age", {
  
  SSplotRunstest(ss3her, 
                 png = TRUE, 
                 print = T, 
                 subplots = "age", 
                 indexselect = 2,
                 plotdir = path, 
                 filenameprefix = "her_")
  
  expect_true(file.exists(file.path(path, "her_residruns_Acoustics.png")))
  
})
