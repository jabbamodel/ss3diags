#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Example code for library(ss3diags) 
#
# github.com/jabbamodel/ss3diags 
#
# Install ss3diags
# devtools::install_github("jabbamodel/ss3diags")
# Requires r4ss installation
# devtools::install_github("r4ss/r4ss")
# Henning Winker (henning.winker@gmail.com) 
# Joint Research Centre of the European Commission
# Work Email: Henning.Winker@ec.europa.eu 
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>


# load packages
library(ss3diags)
library(r4ss)

# Set workdirectory to save output 
setwd("C:/Work/Research/MS_diags/Revision2")
assessments = c("Mako","PacificHake")
dir.create("output",showWarnings = F)

# Load inbuilt data
data("natl.sma")
data("pac.hke")
# Data documentation
help("natl.sma") 
help("pac.hke")

# Save Dianostic Statistics  
# Runs Test
runssmaI = SSrunstest(ss3sma)
runssmaL = SSrunstest(ss3sma,quants =  "len")
runshkeI = SSrunstest(ss3phk)
runshkeA = SSrunstest(ss3phk,quants =   "age")
runstest = rbind(data.frame(stk="sma",runssmaI),data.frame(stk="sma",runssmaL),data.frame(stk="phk",runshkeI),data.frame(stk="pke",runshkeA))
write.csv(runstest,paste0(getwd(),"/output/","runstests.csv"),row.names = F)
# Summarize do.retro output
retroI.sma = SSsummarize(retro.sma) # for hindcast Index + Retro
hccomps.sma = SSretroComps(retro.sma) # summarize hindcasts for composition data
retroI.phk = SSsummarize(retro.phk) 
hccomps.phk = SSretroComps(retro.phk)

# Get MASE as metric of prediction skill
hcsmaI = SSmase(retroI.sma)
hcsmaL = SSmase(hccomps.sma,quants = "len")
hcphkI = SSmase(retroI.phk)
hcphkA = SSmase(hccomps.phk,quants = "age")
hcxval =rbind(data.frame(stk="sma",hcsmaI),data.frame(stk="sma",hcsmaL),data.frame(stk="phk",hcphkI),data.frame(stk="pke",hcphkA))
write.csv(hcxval,paste0(getwd(),"/output/","hcxval.csv"),row.names = F)
# Retrospective bias (Mohn's Rho) and Forecast bias
retrosma = SShcbias(retroI.sma)
retrophk = SShcbias(retroI.phk)
retro =rbind(data.frame(stk="sma",retrosma),data.frame(stk="phk",retrophk))
write.csv(retro,paste0(getwd(),"/output/","retro.csv"),row.names = F)

#--------------------
# Make Cookbook plots 
#--------------------
# rename fleets for MS plots

l = 1.2 # size of plot label a), b), c)
# PLOTS FOR MS COOKBOOK
graphics.off()

#----------------------------
# Runs Test Plots
#----------------------------
# Requires input from r4ss::SS_output()

pwidth = 8
pheight = 9
res=300
plname = "Fig4_runsfig_sma"

jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(3,2),labs=T,plot.cex=0.9)
for(i in 1:3){
SSplotRunstest(ss3sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,3,4)[i])
legend("topleft",paste0(letters[c(1,3,5)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRunstest(ss3sma,subplots = "len",add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,2,4)[i])
legend("topleft",paste0(letters[c(2,4,6)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}  

dev.off()

pwidth = 4
pheight = 9
res=300
plname = "Fig5_runsfig_hke"
jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(3,1),labs=T,plot.cex=0.9)
for(i in 1:1){
  SSplotRunstest(ss3phk,add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = i)
  legend("topleft",paste0(letters[c(1)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
for(i in 1:2){
  SSplotRunstest(ss3phk,subplots="age",add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(2:1)[i])
  legend("topleft",paste0(letters[c(2:3)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
dev.off()


#-------------------------------------
# JABBA(Joint)-Residual plots
#-------------------------------------
# Requires input from r4ss::SS_output()

plname = "Fig3_jointresiduals"
pwidth=8; pheight=7
jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(2,2),labs=T,plot.cex=0.9)
SSplotJABBAres(ss3sma,add=T,legendcex=0.8,tickEndYr=T,xylabs=T,indexselect = c(1,3,4,5,6),legendsp = 1)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3phk,add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3sma,subplots = "len", add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3phk,subplots = "age",add=T,legendcex=0.8,tickEndYr=T,xylabs=T,legendsp = 1)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.79))
dev.off()

#--------------------------------------------
# ASPM plots 
# Plot function for comparing model runs
#--------------------------------------------
# Requires input from r4ss::SSsummarize()
plname = "Fig7_aspmplot"
pwidth = 8
pheight = 9
res=300
jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(3,2),labs=F,plot.cex=1)
SSplotModelcomp(aspm.sma,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM","ASPMdev"))
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Index",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phk,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM","ASPMdev"))
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "SSB",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("SSB (t)",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phk,subplots = "SSB",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "RecDevs",ylimAdj=0.9,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[5],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Recruitment Deviations",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.phk,subplots = "RecDevs",ylimAdj=1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[6],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.2,cex=1.1,c(0.27,0.77))
dev.off()

#--------------------------------------------------
# Retrospective plots with one-step ahead forecasts
#--------------------------------------------------
# Requires list of retrospective runs with SS_doRetro() as read by SSgetoutput()

pwidth=8; pheight=8
plname = "Fig8_retrowithforecast"

jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(2,2),labs=F,plot.cex=1.)
SSplotRetro(retroI.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",uncertainty = T,showrho = F,forecast = T,labels="SSB (t)",legendsp=0.9,ylim=c(0,1500))
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retroI.phk,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "topright",uncertainty = T,showrho = F,forecast = T,labels="SSB (t)",legendsp=0.9,ylim=c(0,6.5E6))
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("SSB (t)",side=2,outer=T,line=0.5,cex=1)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.77))
SSplotRetro(retroI.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",xmin=2005,uncertainty = T,legend = F,forecast = T,legendsp = 0.9,forecastrho=T,ylim=c(0,1100))
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retroI.phk,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomright",xmin=2005,legend = F,forecast = T,legendsp = 0.9,forecastrho=T)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
dev.off()

#--------------------------------------------------
# Hindcast Cross-Validation (HCXval) plots with one-step ahead forecasts
#--------------------------------------------------
# Requires list of retrospective runs with SS_doRetro() as read by SSgetoutput()


pwidth = 8
pheight = 9
res=300
plname = "Fig9_hcxval_sma"
retroI.sma = SSsummarize(retro.sma) # summarize index fits
hccomps.sma = ss3diags::SSretroComps(retro.sma) # summarize composition fits

jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(3,2),labs=T,plot.cex=0.9)
for(i in 1:3){
  SSplotHCxval(retroI.sma,add=T,legendcex=0.8,legend=ifelse(i==0,T,F),legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="bottomleft",indexselect = c(1,3,4)[i],MAE.base.adj=0)
  legend("topleft",paste0(letters[c(1,3,5)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
  
  SSplotHCxval(hccomps.sma,subplots = "len",add=T,legendcex=0.95,legend=ifelse(i==3,T,F),legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="bottomright",indexselect = c(1,2,4)[i],MAE.base.adj=0)
  
  legend("topleft",paste0(letters[c(2,4,6)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}  
dev.off()

pwidth = 4
pheight = 9
res=300
plname = "Fig10_hcxval_hke"
retroI.phk = SSsummarize(retro.phk) # summarize index fits
hccomps.phk = ss3diags::SSretroComps(retro.phk) # summarize composition fits

jpeg(file = paste0(getwd(),"/output/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")
sspar(mfrow=c(3,1),labs=T,plot.cex=0.9)

for(i in 1:1){
  SSplotHCxval(retroI.phk,add=T,legendcex=0.8,legend=F,legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="topright",indexselect = c(1)[i])
  legend("topleft",paste0(letters[c(1)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
for(i in 1:2){
  SSplotHCxval(hccomps.phk,subplots = "age",add=T,legendcex=0.8,legend=ifelse(i==2,T,F),legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=T,legendloc="bottomleft",indexselect = c(2,1)[i])
  legend("topleft",paste0(letters[c(2:3)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}
dev.off()

# End
