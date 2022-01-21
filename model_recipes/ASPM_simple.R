##
# ASPM_simple.R 
# Example application: 
# ss3diags::simple Stock Synthesis model
# Stock Synthesis (version 3_30_18 Windows) 
# r4ss (version 1.43.0)  
# R (version 4.1.1 64 bit)
##

library(r4ss)
library(ss3diags)

# Step 1. Copy and paste files from reference run to ASPM directory
dirname.completed.model.run <- "./model_recipes/reference_run_orig"

# Step 2. Create a "ASPM" subdirectory
dirname.aspm <- "./model_recipes/ASPM"
dir.create(path=dirname.aspm, showWarnings = TRUE, recursive = TRUE)

# Step 3. Create a "Figures_Tables" subdirectory
plotdir=paste0(dirname.aspm, "/Figures & Tables")
dir.create(path=plotdir, showWarnings = TRUE, recursive = TRUE)

# Step 4. Copy completed base model output to "ASPM" directory
list_of_files <- list.files(dirname.completed.model.run) 
file.copy(file.path(dirname.completed.model.run, list_of_files), dirname.aspm)

# Step 5. Change rec devs in ss3.pars to 0 
pars <- SS_readpar_3.30(file.path(dirname.aspm, "ss3.par"), 
                        datsource = file.path(dirname.aspm, "data.ss_new"),
                        ctlsource = file.path(dirname.aspm, "control.ss_new"))

pars$recdev_early[,2] <- 0
pars$recdev1[,2] <- 0
pars$recdev_forecast[,2] <- 0
SS_writepar_3.30(pars, outfile = file.path(dirname.aspm, "ss3.par"))

# Step 6. Change starter to read from par file and dat and ctl file names to .ss_new versions
starter <- SS_readstarter(file = file.path(dirname.aspm, "starter.ss"))
starter$init_values_src <- 1
starter$datfile <- "data.ss_new"
starter$ctlfile <- "control.ss_new"
SS_writestarter(starter, dir = dirname.aspm, overwrite = TRUE)

SS_changepars(dir = dirname.aspm, 
              strings = c("steep", "sigmaR"),
              newphs = c(-4, -5))

# Step 7. Change control file to fix rec devs at value read from par file (change phase to negative (recdev phase =-3, recdev_early_phase = -4))

control <- SS_readctl(file = file.path(dirname.aspm, "control.ss_new"), 
                      datlist = file.path(dirname.aspm, "data.ss_new"))


control$recdev_early_phase <- -4
control$recdev_phase <- -3
SS_writectl_3.30(control, outfile = file.path(dirname.aspm, "control.ss_new"), overwrite = TRUE)
# Manually turn off all length comp data (likelihood lambda to 0) and penalty for rec dev estimation in liklihood (lambda = 0 for recruitment)
# If there are already lambda adjustments you can do this through R by:
# control$lambdas$value <- 0
# 4     1     1   0   1
# 4     2     1   0   1
# 10    1     1   0   1

# Step 8. Run original and ASPM models 

# Step 9. Summarize results
aspm.mods <- SSgetoutput(dirvec = c(dirname.completed.model.run, dirname.aspm))
aspm.summary <- SSsummarize(aspm.mods)

# plot comparisons 
SSplotComparisons(aspm.summary, 
                  legendlabels = c("Reference", "ASPM"), 
                  print = TRUE, 
                  plotdir = plotdir)

SSplotModelcomp(aspm.summary, subplots = "Index", add = TRUE, legendlabels = c("Full Model","ASPM"))
SSplotModelcomp(aspm.summary, subplots = "SSB", add = TRUE)
SSplotModelcomp(aspm.summary, subplots = "RecDevs", add = TRUE)


