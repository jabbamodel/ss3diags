#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Example code for library(ss3diags) 
#
# github.com/jabbamodel/ss3diags 
#
# Install ss3diags
# devtools::install_github("jabbamodel/ss3diags")
#
# Henning Winker (henning.winker@gmail.com) 
# Joint Research Centre of the European Commission
# Work Email: Henning.Winker@ec.europa.eu 
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

# load pakage
library(ss3diags)
library(r4ss)
# set working directory for saving the plots
setwd("C:/Work/Research/MS_diags/testplots")

# Data 
help("natl.sma") 
help("pac.hke")

#--------------------
# Make Cookplots 
#--------------------
l = 1.2 # size of plot label a), b), c)
# PLOTS FOR MS COOKBOOK
graphics.off()
res=300 # resolution
# JABBA-Residual plots
# Requires input from r4ss::SS_output()
plname = "Fig3_jointresiduals"
pwidth=8
pheight=7

if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
sspar(mfrow=c(2,2),labs=T,plot.cex=0.9)
SSplotJABBAres(ss3sma,add=T,legendcex=0.8,tickEndYr=T,xylabs=T,indexselect = c(1,3,4,5,6),legendsp = 1)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3phke,add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3sma,subplots = "len", add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3phke,subplots = "age",add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.79))
#save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

pwidth = 8
pheight = 9
plname = "Fig4_runsfig_sma"
if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
# set graphic options
sspar(mfrow=c(3,2),labs=T,plot.cex=0.9)
for(i in 1:3){
SSplotRunstest(ss3sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,3,4)[i])
legend("topleft",paste0(letters[c(1,3,5)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRunstest(ss3sma,subplots = "len",add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,2,4)[i])
legend("topleft",paste0(letters[c(1,2,6)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}  
# save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

pwidth = 4
pheight = 9
res=300
plname = "Fig5_runsfig_hke"
if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
sspar(mfrow=c(3,1),labs=T,plot.cex=0.9)
for(i in 1:1){
  SSplotRunstest(ss3phke,add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = i)
  legend("topleft",paste0(letters[c(1)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
for(i in 1:2){
  SSplotRunstest(ss3phke,subplots="age",add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(2:1)[i])
  legend("topleft",paste0(letters[c(2:3)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
#save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# ASPM plots 
# Requires input from r4ss::SSsummarize()
plname = "Fig7_aspmplot"
pwidth = 8
pheight = 9
res=300
if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}

sspar(mfrow=c(3,2),labs=F,plot.cex=1)
SSplotModelcomp(aspm.sma,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM","ASPMdev"))
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Index",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phke,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM","ASPMdev"))
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "SSB",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("SSB (t)",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phke,subplots = "SSB",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "RecDevs",ylimAdj=0.9,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[5],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Recruitment Deviations",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phke,subplots = "RecDevs",ylimAdj=1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[6],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.2,cex=1.1,c(0.27,0.77))
#save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


#------------------------------------------------------------
# Prepare retro + HCxval data objects
#------------------------------------------------------------
# convert replist into summary object
retroI.sma = r4ss::SSsummarize(retro.sma)
# convert replist into summary object with mean size/age
hccomps.sma = ss3diags::SSretroComps(retro.sma)
# convert replist into summary object
retroI.phke = r4ss::SSsummarize(retro.phke)
# convert replist into summary object with mean size/age
hccomps.phke = ss3diags::SSretroComps(retro.phke)

#--------------------------------------------------
# Retrospective plots with one-step ahead forecasts
#--------------------------------------------------
pwidth=8; pheight=8
if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
plname = "Fig8_retrowithforecast"
sspar(mfrow=c(2,2),labs=F,plot.cex=1.)
SSplotRetro(retroI.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",uncertainty = T,showrho = F,forecast = T,labels="SSB (t)",legendsp=0.9)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retroI.phke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "topright",uncertainty = T,showrho = F,forecast = T,labels="SSB (t)",legendsp=0.9)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("SSB (t)",side=2,outer=T,line=0.5,cex=1)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.77))
SSplotRetro(retroI.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",xmin=2005,uncertainty = T,legend = F,forecast = T,legendsp = 0.9,forcastrho=T)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retroI.phke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomright",xmin=2005,legend = F,forecast = T,legendsp = 0.9,forcastrho=T)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
#save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

#--------------------------------------------------
# HCxval Index + mean size/age
#--------------------------------------------------

pwidth = 8
pheight = 9
res=300
plname = "Fig9_hcxval_sma"

if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
sspar(mfrow=c(3,2),labs=T,plot.cex=0.9)
for(i in 1:3){
  SSplotHCxval(retroI.sma,add=T,legendcex=0.8,legend=F,legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="topright",indexselect = c(1,3,4)[i])
  legend("topleft",paste0(letters[c(1,3,5)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
  #mtext("Index",2,line=1.5)
  SSplotHCxval(hccomps.sma,subplots = "len",add=T,legendcex=0.8,legend=T,legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="bottomright",indexselect = c(1,2,4)[i])
  #mtext("Mean Length",2,line=1.5)
  legend("topleft",paste0(letters[c(2,4,6)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}  
#save
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


pwidth = 4
pheight = 9
res=300
plname = "Fig10_hcxval_hke"

if(grepl("win",tolower(Sys.info()['sysname']))) {windows(width=pwidth,height=pheight)}
sspar(mfrow=c(3,1),labs=T,plot.cex=0.9)

for(i in 1:1){
  SSplotHCxval(retroI.phke,add=T,legendcex=0.8,legend=F,legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="topright",indexselect = c(1)[i])
  legend("topleft",paste0(letters[c(1)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
for(i in 1:2){
  SSplotHCxval(hccomps.phke,subplots = "age",add=T,legendcex=0.8,legend=ifelse(i==2,T,F),legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="bottomleft",indexselect = c(2,1)[i])
  legend("topleft",paste0(letters[c(2:3)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")







