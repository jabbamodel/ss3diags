


#-----------------------------------------------------------
# This code is used with r4ss to extract retrospective runs
#----------------------------------------------------------
#library(r4ss)
#workdir = "C:/Users/henni/Dropbox/ss3diags/ss3hcxval/data/Mako"
#start.retro <- 0    #end year of model e.g., 2018
#end.retro   <- 5    #number of years for retrospective e.g., 2017,2016,2015,2014,2013
#subdirs = paste0("retrospectives/retro",start.retro:-end.retro) # Difine SS3 model folders

### Compile models
#retroModels <- SSgetoutput(dirvec=file.path(workdir,subdirs))
### Summarize outputs
#retroSummary <- SSsummarize(retroModels)

#----------------------------------------------
# retroSummary are available for:
# (1) N.Atl. Shortfin Mako: retro.sma
# (2) Med (Sci) Hake: retro.hke
# in data(retro.runs)
#---------------------------------------------

# Install ss3diags
#devtools::install_github("jabbamodel/ss3diags")

# load pakage
library(ss3diags)
# load data
data("retroruns")
# Plot SMA multiplot
sspar(mfrow=c(3,2),labs=F,plot.cex=0.8)
SSplotHCxval(retro.sma,add=T,legendcex=0.7,legend=T,legendindex = 1)
mtext("Year",side=1,outer=T,cex=1)
mtext("Index",side=2,outer=T,line=1,cex=1)

# Add hake
sspar(mfrow=c(3,3),labs=F,plot.cex=0.7)
SSplotHCxval(retro.sma,add=T,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=F)
plot(0,0,type="n",axes=F)
SSplotHCxval(retro.hke,add=T,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=F)
mtext("Year",side=1,outer=T,cex=1)
mtext("Index",side=2,outer=T,line=1,cex=1)

# Plot full time series
SSplotHCxval(retro.hke,add=F,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=T,xmin=1)
# Change col
SSplotHCxval(retro.hke,add=F,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=T,xmin=1,col=c(1,rainbow(5)))

