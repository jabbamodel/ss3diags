##
# Likelihood_profile_R0_example.R 
# Example original application: 
# 2017 ICCAT North Atlantic shortfin mako (SMA) Stock Synthesis model run 3
# Stock Synthesis (version 3_30_08 Windows) 
# r4ss (version 1.35.1)  
# R (version 3.3.2 64 bit)
##

#rm(list=ls())
#devtools::install_github('r4ss/r4ss')
library(r4ss)
library(doParallel)
registerDoParallel(8)

# Step 1. Identify a directory for the profile likelihood model run(s)
dirname.base <- "./model_recipes"

# Step 2. Identify a directory where the completed base model run is located
dirname.completed.model.run <- "./model_recipes/reference_run_orig"

# Step 3. Create a "R0_profile" subdirectory and set as the working directory
dirname.R0.profile <- paste0(dirname.base,'/Likelihood profiles/R0')
dir.create(path=dirname.R0.profile, showWarnings = TRUE, recursive = TRUE)

# Step 4. Create a "Figures_Tables" subdirectory
plotdir=paste0(dirname.R0.profile, "/Figures & Tables")
dir.create(path=plotdir, showWarnings = TRUE, recursive = TRUE)

# Step 5. Copy completed base model output to "R0_profile" directory
list_of_files <- list.files(dirname.completed.model.run) 
file.copy(file.path(dirname.completed.model.run, list_of_files), dirname.R0.profile)

# Step 6. Edit "control.ss" in the "R0_profile" working directory to estimate at least one parameter in each phase
# E.g., 
control.file <- readLines(paste(dirname.R0.profile, "/control.ss_new", sep=""))
linen <- NULL
linen <- grep("#_recdev phase", control.file)
control.file[linen] <- paste0("1 #_recdev phase")
write(control.file, paste(dirname.R0.profile, "/control.ss_new", sep=""))

# Step 7. Edit "starter.ss" in the "R0_profile" working directory to read from init values from control_modified.ss
starter.file <- SS_readstarter(file.path(dirname.R0.profile, "/starter.ss", sep=""))
#make sure names for control and data file are correct
starter.file$ctlfile <- "control_modified.ss"
starter.file$datfile <- "data.ss_new"
#for non-estimated quantities
starter.file$init_values_src <- 0
# Make sure the prior likelihood is calculated for non-estimated quantities
starter.file$prior_like <- 1    
SS_writestarter(starter.file, dir = dirname.R0.profile, overwrite = TRUE)

###############

# Step 8. Begin Likelihood profile_R0_example.R

#########################################################
### R0 or any other parameter profile
#########################################################

# vector of values to profile over
R0.vec <- seq(18.0,19.0,0.1)     
Nprof.R0 <- length(R0.vec)
#Define directory
#mydir <- mydir

#Run SS_profile command
profile <- SS_profile(dir=dirname.R0.profile, # directory
                      model="ss",
                      masterctlfile="control.ss_new",
                      newctlfile="control_modified.ss",
                      string="SR_LN(R0)",
                      profilevec=R0.vec)

# read the output files (with names like Report1.sso, Report2.sso, etc.)
prof.R0.models <- SSgetoutput(dirvec=c, keyvec=1:Nprof.R0, getcovar = FALSE) # 

# Step 9.  summarize output
prof.R0.summary <- SSsummarize(prof.R0.models)

#add base model into summary
MLEmodel <- SS_output(dirname.R0.profile)
prof.R0.models$MLE <- MLEmodel
prof.R0.summary <- SSsummarize(prof.R0.models)


# Likelihood components 
mainlike_components <- c('TOTAL',"Survey", "Discard", 'Length_comp',"Age_comp",'Recruitment') 

mainlike_components_labels <- c('Total likelihood','Index likelihood',"Discard",'Length likelihood',"Age likelihood",'Recruitment likelihood') 

# END OPTIONAL COMMANDS

# plot profile using summary created above
SSplotProfile(prof.R0.summary,           # summary object
              profile.string = "R0",     # substring of profile parameter
              profile.label=expression(log(italic(R)[0])), 
              print = TRUE,
              plotdir=plotdir 
              )

Baseval <- round(Base$parameters$Value[grep("R0",Base$parameters$Label)],2)
#Baselab <- paste(Baseval,sep="")
#axis(1,at=Baseval,label=Baselab)
abline(v = Baseval, lty=2)


# make timeseries plots comparing models in profile
labs <- paste("SR_Ln(R0) = ",R0.vec)
labs[which(round(R0.vec,2)==Baseval)] <- paste("SR_Ln(R0) = ",Baseval,"(Base model)")

SSplotComparisons(prof.R0.summary,
                  legendlabels=labs,
                  pheight=4.5,
                  png=TRUE,
                  plotdir=plotdir,
                  legendloc='bottomleft')

###Piner plot
par(mfrow = c(2,2))
PinerPlot(prof.R0.summary, 
          profile.string = "R0", 
          component = "Length_like",
          main = "Changes in length-composition likelihoods by fleet",
          add_cutoff = TRUE,
          cutoff_prob = 0.95)
Baseval <- round(Base$parameters$Value[grep("SR_LN",Base$parameters$Label)],2)
#Baselab <- paste(Baseval,sep="")
#axis(1,at=Baseval,label=Baselab)
abline(v = Baseval, lty=2)
dev.off()

png(file.path(plotdir,"R0_profile_plot_Survey_like.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
PinerPlot(prof.R0.summary, profile.string = "R0", component = "Surv_like",main = "Changes in Index likelihoods by fleet",
          add_cutoff = TRUE,
          cutoff_prob = 0.95, legendloc="topleft")
Baseval <- round(Base$parameters$Value[grep("SR_LN",Base$parameters$Label)],2)
#Baselab <- paste(Baseval,sep="")
#axis(1,at=Baseval,label=Baselab)
abline(v = Baseval, lty=2)
dev.off()
