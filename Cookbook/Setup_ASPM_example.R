##
# Setup_ASPM_example.R 
# Example original application: 
# 2017 ICCAT North Atlantic shortfin mako (SMA) Stock Synthesis model run 3
# Stock Synthesis (version 3_30_08 Windows) 
# r4ss (version 1.35.1)  
# R (version 3.3.2 64 bit)
##

library(r4ss)
# Step 1) copy and paste files from Full to ASPM directory
# control.ss_new data.ss_new forecast.ss_new SS3fast_Win64.exe SS3fast_Win64.par starter.ss_new 
# rename *.ss_new to *.ss 
# rename SS3fast_Win64.par* to SS3.* 
# Step 2) change SS3.par from original model run to set all rec dev = 0 
# recdev_early:
#0.00 0.00 0.00 0.00 0.00
# recdev1:
#0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
# Fcast_recruitments:
#0.000000000000 0.000000000000 0.000000000000
#
# Step 3) change starter.ss from original model run to read from par file 
# 1 # 0=use init values in control file; 1=use SS3.par
#
# Step 4) change control.ss from original model run to fix selectivity parameters at the value read from par file (change phase to negative for all estimated paramters)
#
# Step 5) change control.ss from original model run to turn off fit to all length comp data (change likelihood lambda to 0 for all length  data; 4=length)
#4 1 1 0 1
#4 2 1 0 1
#4 3 1 0 1
#4 4 1 0 1
#4 5 1 0 1
#
# Step 6) change control.ss from original model run to fix rec devs at the value read from par file (change phase to negative for all recdev phases)
#-3 #_recdev phase 
#-4 #_recdev_early_phase
#         [not sure next step makes any difference] 
# Step 7) change control.ss from original model run to turn off penalty for rec dev estimation in likelihood (change likelihood lambda to 0 for recruitment; 10=recrdev)
#10 1 1 0 1
#         
# Step 8) manually run the original and ASPM
#
# Step 9) summarize results in R using the r4ss package 
library(r4ss)
rootdir <- "C:/Users/Felipe.Carvalho/Desktop/MS_Diagnostics/ASPM"

# directories where models were run need to be defined
dir1 <- "C:/Users/Felipe.Carvalho/Desktop/MS_Diagnostics/ASPM/ASPM"
dir2 <- "C:/Users/Felipe.Carvalho/Desktop/MS_Diagnostics/ASPM/Reference_run"

# read two models
mod1 <- SS_output(dir=dir1)
mod2 <- SS_output(dir=dir2)

# create list summarizing model results
mod.sum <- SSsummarize(list(mod1, mod2))

# plot comparisons
SSplotComparisons(mod.sum, legendlabels=c("ASPM", "Reference"),print = T, plotdir = paste0(rootdir,"/Plots"))
 



