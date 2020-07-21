##
# Likelihood_profile_R0_example.R 
# Example original application: 
# 2017 ICCAT North Atlantic shortfin mako (SMA) Stock Synthesis model run 3
# Stock Synthesis (version 3_30_08 Windows) 
# r4ss (version 1.35.1)  
# R (version 3.3.2 64 bit)
# written by Felipe Carvalho
##

#rm(list=ls())
#devtools::install_github('r4ss/r4ss')
library(r4ss)
library(doParallel)
registerDoParallel(8)

# Step 1. Identify a directory for the profile likelihood model run(s)
dirname.base <- "C:\\Users\\Felipe.Carvalho\\Desktop\\Ms_Diagnostics\\R0_profile\\SMA\\"

# Step 2. Identify a directory where the completed base model run is located
dirname.completed.model.run <- paste0(dirname.base,'/Reference_run')

# Step 3. Create a "R0_profile" subdirectory and set as the working directory
dirname.R0.profile <- paste0(dirname.base,'/Likelihood profiles/R0')
dir.create(path=dirname.R0.profile, showWarnings = TRUE, recursive = TRUE)

mydir <- dirname.R0.profile
setwd(dirname.R0.profile)
getwd()

# Step 4. Create a "Figures_Tables" subdirectory
plotdir=paste0(dirname.R0.profile, "/Figures & Tables")
dir.create(path=plotdir, showWarnings = TRUE, recursive = TRUE)


# Step 5. Create a "Reference_run" subdirectory and copy completed base model output to this directory
reference.dir <- paste0(mydir,'/Reference_run') 
dir.create(path=reference.dir, showWarnings = TRUE, recursive = TRUE)
file.copy(Sys.glob(paste(dirname.completed.model.run, "*.*", sep="/"), dirmark = FALSE),reference.dir)


# Step 6. Copy necassary files from the "Reference_run" subdirectory to the "R0_profile" working directory 
copylst <-  c("control.ss_new", "data.ss",  "forecast.ss",  "ss.exe", "starter.ss")
for(nn in copylst){file.copy(  paste(reference.dir,"/", nn, sep='')  ,     file.path(dirname.R0.profile))}

# Step 7. Edit "control.ss" in the "R0_profile" working directory to estimate at least one parameter in each phase
# E.g., 
control.file <- readLines(paste(dirname.R0.profile, "/control.ss_new", sep=""))
linen <- NULL
linen <- grep("#_recdev phase", control.file)
control.file[linen] <- paste0("1 #_recdev phase")
write(control.file, paste(dirname.R0.profile, "/control.ss_new", sep=""))

# Step 8. Edit "starter.ss" in the "R0_profile" working directory to read from init values from control_modified.ss
starter.file <- readLines(paste(dirname.R0.profile, "/starter.ss", sep=""))
linen <- NULL
linen <- grep("# 0=use init values in control file; 1=use ss.par", starter.file)
starter.file[linen] <- paste0("0 # 0=use init values in control file; 1=use ss.par")
write(starter.file, paste(dirname.R0.profile, "/starter.ss", sep=""))
###############

# Step 9. Begin Likelihood profile_R0_example.R

###Working directory
setwd(dirname.R0.profile)

####Set the plotting directory
plotdir=paste0(dirname.R0.profile, "/Figures & Tables")
Base <- SS_output(dir=paste0(reference.dir,"\\"),covar=T)

#########################################################
### R0 or any other parameter profile
#########################################################

# vector of values to profile over
R0.vec <- seq(4.8,6.8,0.1)     
Nprof.R0 <- length(R0.vec)
#Define directory
#mydir <- mydir

#Define the starter file
starter <- SS_readstarter(file.path(mydir, "starter.ss"))

#Change control file name in the starter file
starter$ctlfile <- "control_modified.ss" 

# Make sure the prior likelihood is calculated for non-estimated quantities
starter$prior_like <- 1                                 

SS_writestarter(starter, dir=mydir, overwrite=TRUE)

#Run SS_profile command
profile <- SS_profile(dir=mydir, # directory
                      model="SS",
                      masterctlfile="control.ss_new",
                      newctlfile="control_modified.ss",
                      string="SR_LN(R0)",
                      profilevec=R0.vec)

# read the output files (with names like Report1.sso, Report2.sso, etc.)
prof.R0.models <- SSgetoutput(dirvec=mydir, keyvec=1:Nprof.R0, getcovar = FALSE) # 

# Step 10.  summarize output
prof.R0.summary <- SSsummarize(prof.R0.models)

# Likelihood components 
mainlike_components         <- c('TOTAL',"Survey", "Discard", 'Length_comp',"Age_comp",'Recruitment') 

mainlike_components_labels  <- c('Total likelihood','Index likelihood',"Discard",'Length likelihood',"Age likelihood",'Recruitment likelihood') 

# END OPTIONAL COMMANDS

# plot profile using summary created above
png(file.path(plotdir,"R0_profile_plot.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))

SSplotProfile(prof.R0.summary,           # summary object
              profile.string = "R0",     # substring of profile parameter
              profile.label=expression(log(italic(R)[0])), ymax=150,minfraction = 0.001,
              pheight=4.5, 
              print=FALSE, 
              plotdir=plotdir, 
              components = mainlike_components, 
              component.labels = mainlike_components_labels,
              add_cutoff = TRUE,
              cutoff_prob = 0.95)

Baseval <- round(Base$parameters$Value[grep("R0",Base$parameters$Label)],2)
#Baselab <- paste(Baseval,sep="")
#axis(1,at=Baseval,label=Baselab)
abline(v = Baseval, lty=2)
dev.off()

# make timeseries plots comparing models in profile
labs <- paste("SR_Ln(R0) = ",R0.vec)
labs[which(round(R0.vec,2)==Baseval)] <- paste("SR_Ln(R0) = ",Baseval,"(Base model)")

SSplotComparisons(prof.R0.summary,legendlabels=labs,
pheight=4.5,png=TRUE,plotdir=plotdir,legendloc='bottomleft')

dev.off()

###Piner plot
png(file.path(plotdir,"R0_profile_plot_Length_like.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
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

