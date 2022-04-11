##
# Run Retrospective analysis example
# Example original application: 
# 2017 ICCAT North Atlantic shortfin mako (SMA) Stock Synthesis model run 3
# Stock Synthesis (version 3_30_15 Windows) 
# r4ss (version 1.35.1)  
# R (version 3.3.2 64 bit)
##
#devtools::install_github('r4ss/r4ss')
library(r4ss)
# test


# Step 1. Identify restrospective period
# e.g., for end.yr.vec   <- c(2015,2014,2013,2012,2011,2010)
start.retro <- 0    #end year of model e.g., 2015
end.retro   <- 5    #number of years for retrospective e.g., 2014,2013,2012,2011,2010

# Step 2. Identify the base directory
dirname.base <- "C:\\Users\\Felipe.Carvalho\\Desktop\\Ms_Diagnostics\\Retrospective\\SMA\\"
dirname.base

# Step 3. Identify the directory where a completed model run is located
dirname.completed.model.run <- paste0(dirname.base,'/Reference_run')
dirname.completed.model.run

# Step 4. Create a subdirectory for the Retrospectives
dirname.Retrospective <- paste0(dirname.base,'/Retrospective')
dir.create(path=dirname.Retrospective, showWarnings = TRUE, recursive = TRUE)
setwd(dirname.Retrospective)
getwd()

# Step 5. Create a subdirectory for the Plots
dirname.plots <- paste0(dirname.Retrospective,"/plots_1")
dir.create(dirname.plots)

#----------------- copy model run files ----------------------------------------
file.copy(paste(dirname.completed.model.run,       "starter.ss_new", sep="/"),
          paste(dirname.Retrospective, "starter.ss", sep="/"))
file.copy(paste(dirname.completed.model.run,       "control.ss_new", sep="/"),
          paste(dirname.Retrospective, "CONTROL.SS", sep="/"))
file.copy(paste(dirname.completed.model.run,       "data.ss_new", sep="/"),
          paste(dirname.Retrospective, "DATA.SS", sep="/"))	
file.copy(paste(dirname.completed.model.run,       "forecast.ss", sep="/"),
          paste(dirname.Retrospective, "forecast.ss", sep="/"))
file.copy(paste(dirname.completed.model.run,       "SS.exe", sep="/"),
          paste(dirname.Retrospective, "SS.exe", sep="/"))
# Required for assessments with wtatage.ss files
file.copy(paste(dirname.completed.model.run,       "wtatage.ss", sep="/"),
          paste(dirname.Retrospective, "wtatage.ss", sep="/"))

#------------Make Changes to the Starter.ss file (DC Example) ------------------------------- 
starter <- readLines(paste(dirname.Retrospective, "/starter.ss", sep=""))

# 1) Starter File changes to speed up model runs
# Run Display Detail
#[8] "2 # run display detail (0,1,2)" 
linen <- grep("# run display detail", starter)
starter[linen] <- paste0( 1 , " # run display detail (0,1,2)" )
write(starter, paste(dirname.Retrospective, "starter.ss", sep="/"))

#------------ r4SS retrospective calculations------------------------------- 

# Step 6. Run the retrospective analyses with r4SS function "SS_doRetro"
SS_doRetro(masterdir=dirname.Retrospective, oldsubdir="", newsubdir="retrospectives", years=start.retro:-end.retro)

# Step 7. Read "SS_doRetro" output
retroModels <- SSgetoutput(dirvec=file.path(dirname.Retrospective, "retrospectives",paste("retro",start.retro:-end.retro,sep="")))

# Step 8. Summarize "SS_doRetro" output
retroSummary <- SSsummarize(retroModels)

