### Create Retrospective for Simple example model and plots
library(r4ss)
library(ss3diags)
# Step 1. Identify restrospective period
# e.g., for end.yr.vec   <- c(2015,2014,2013,2012,2011,2010)
start.retro <- 0    #end year of model e.g., 2015
end.retro   <- 5    #number of years for retrospective e.g., 2014,2013,2012,2011,2010

# Step 2. Identify the base directory
dirname.base <- "./model_recipes"
dirname.base

# Step 3. Identify the directory where a completed model run is located
dirname.completed.model.run <- paste0(dirname.base, "reference_run_orig")
dirname.completed.model.run

# Step 4. Create a subdirectory for the Retrospectives
dirname.Retrospective <- paste0(dirname.base,'/Retrospective')
dir.create(path=dirname.Retrospective, showWarnings = TRUE, recursive = TRUE)
setwd(dirname.Retrospective)
getwd()

# Step 5. Create a subdirectory for the Plots
dirname.plots <- paste0(dirname.Retrospective,"/retro_plots")
dir.create(dirname.plots)

#----------------- copy model run files ----------------------------------------
file.copy(paste(dirname.completed.model.run, "starter.ss_new", sep="/"),
          paste(dirname.Retrospective, "starter.ss", sep="/"))
file.copy(paste(dirname.completed.model.run, "control.ss_new", sep="/"),
          paste(dirname.Retrospective, "CONTROL.SS", sep="/"))
file.copy(paste(dirname.completed.model.run, "data.ss_new", sep="/"),
          paste(dirname.Retrospective, "DATA.SS", sep="/"))	
file.copy(paste(dirname.completed.model.run, "forecast.ss", sep="/"),
          paste(dirname.Retrospective, "forecast.ss", sep="/"))
file.copy(paste(dirname.completed.model.run, "SS.exe", sep="/"),
          paste(dirname.Retrospective, "SS.exe", sep="/"))
# Required for assessments with wtatage.ss files
file.copy(paste(dirname.completed.model.run, "wtatage.ss", sep="/"),
          paste(dirname.Retrospective, "wtatage.ss", sep="/"))

#------------Make Changes to the Starter.ss file (DC Example) ------------------------------- 
starter <- SS_readstarter(paste(dirname.Retrospective, "/starter.ss", sep=""))
# 1) Starter File changes to speed up model runs
# Run Display Detail
#[8] "2 # run display detail (0,1,2)" 
starter$run_display_detail
starter$datfile <- "DATA.SS"
starter$ctlfile <- "CONTROL.SS"
SS_writestarter(starter, dirname.Retrospective, overwrite = TRUE)

#------------ r4SS retrospective calculations------------------------------- 

# Step 6. Run the retrospective analyses with r4SS function "SS_doRetro"
SS_doRetro(masterdir=dirname.Retrospective, oldsubdir="", newsubdir="retrospectives", years=start.retro:-end.retro)

# Step 7. Read "SS_doRetro" output
retroSimple <- SSgetoutput(dirvec=file.path(dirname.Retrospective, "retrospectives",paste("retro",start.retro:-end.retro,sep="")))

# Step 8. Summarize "SS_doRetro" output
retroSum <- SSsummarize(retroSimple)
# summarize compositional data
retroComps <- SSretroComps(retroSimple)
# Step 9. Save retrospective output as a .rda object 
save(retroSimple, file='./data/retroSimple.rda')

# Step 10. Create retrospective plots using ss3diags functions
SSplotRetro(retroSum, 
            subplots = "SSB", 
            print = TRUE, 
            png = TRUE,
            plotdir = dirname.plots)

SSplotRetro(retroSum, 
            subplots = "F",
            print = TRUE, 
            png = TRUE,
            plotdir = dirname.plots)

SSplotRetro(retroSum, 
            subplots = "F", 
            forecast = TRUE, 
            xlim = c(94,100), 
            ylim = c(0,0.16),
            uncertainty = FALSE,
            print = TRUE, 
            png = TRUE,
            filenameprefix = "forecast_",
            plotdir = dirname.plots)

SSplotRetro(retroSum, 
            subplots = "SSB", 
            forecast = TRUE, 
            xlim = c(94,100), 
            uncertainty = FALSE,
            print = TRUE, 
            png = TRUE,
            filenameprefix = "forecast_",
            plotdir = dirname.plots)

SSplotHCxval(retroSum, 
             subplots = "cpue",
             add = TRUE,
             print = TRUE, 
             png = TRUE,
             filenameprefix = "cpue",
             plotdir = dirname.plots)

SSplotHCxval(retroComps, 
             subplots = "len", 
             add = TRUE,
             print = TRUE, 
             png = TRUE,
             filenameprefix = "length_comp",
             plotdir = dirname.plots)

SSplotHCxval(retroComps, 
             subplots = "age", 
             indexselect = 2,
             add = TRUE,
             print = TRUE, 
             png = TRUE,
             filenameprefix = "age_comp",
             plotdir = dirname.plots)

