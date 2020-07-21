#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Example code for library(ss3diags) 
#
# github.com/jabbamodel/ss3diags 
#
# Henning Winker (henning.winker@gmail.com) 
# Joint Research Centre of the European Commission
# Work Email: Henning.Winker@ec.europa.eu 
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

#----------------------------------------------------
# data(ss3diags) contains
# ss3sma # SMA base-case model object from r4ss::SS_output()
# ss3hke # HAKE base-case model object from r4ss::SS_output()
# retro.sma # SMA retrospective models from r4ss::SSgetoutput()
# retro.hke # HAKE retrospective models from r4ss::SSgetoutput()
# aspm.sma # SMA base + aspm model from r4ss::SSgetoutput()
# aspm.hke # HAKE base + aspm model from r4ss::SSgetoutput()
#---------------------------------------------

setwd("C:/Work/Research/MS_diags/Plots")
# Install ss3diags
#devtools::install_github("jabbamodel/ss3diags")

# load pakage
library(ss3diags)
# load data
data("ss3diags")
# TEST FUNCTIONS
SSplotRunstest()
SSplotJABBAres()
SSplotModelcomp(fmsy=F,brp = "msy")
SSplotModelcomp(aspm.sma,fmsy=T,brp = "msy")
SSplotHCxval()

#--------------------
# Make Cookplots 
#--------------------
# rename fleets for MS plots
inames = c(paste0("SMA-CPUE_",c(1,1.1,3,4,5,6)))
retro.sma$indices$Fleet_name  =    inames[unclass(factor(retro.sma$indices$Fleet_name))] 
retro.hke$indices$Fleet_name = "HAKE-Survey"
ss3sma$cpue$Fleet_name  =    inames[unclass(factor(ss3sma$cpue$Fleet_name))] 
ss3hke$cpue$Fleet_name = "HAKE-Survey"
aspm.sma$indices$Fleet_name  =    c(paste0("CPUE_",c(1,1.1,3,4,5,6)))[unclass(factor(aspm.sma$indices$Fleet_name))] 
aspm.hke$indices$Fleet_name = "Survey"

l = 1.2 # size of plot label a), b), c)
# PLOTS FOR MS COOKBOOK
graphics.off()
# Hindcast Cross-validation
# Requires input from r4ss::SSsummarize()

pwidth = 8
pheight = 9
res=300
plname = "hcfig"
windows(width=pwidth,height=pheight)

sspar(mfrow=c(3,2),labs=F,plot.cex=1)
for(i in 1:5){
SSplotHCxval(retro.sma,add=T,legendcex=0.8,legend=T,legendsp = 0.8,legendindex = 1,tickEndYr=F,xylabs=F,legendloc="topright",indexselect = c(1,3,4,5,6)[i])
legend("topleft",paste0(letters[i],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
}  
i=6
SSplotHCxval(retro.hke,add=T,legendcex=0.8,legend=T,legendindex = 1,tickEndYr=F,xylabs=F,legendsp=0.8)
legend("topleft",paste0(letters[i],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Index",side=2,outer=T,line=0.5,cex=1)
#safe
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Runs Tests
# Requires input from r4ss::SS_output()

plname = "runsfig"
windows(width=pwidth,height=pheight)

sspar(mfrow=c(3,2),labs=F,plot.cex=1)
for(i in 1:5){
SSplotRunstest(ss3sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1,3,4,5,6)[i])
legend("topleft",paste0(letters[i],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)}  
i=6
SSplotRunstest(ss3hke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F)
legend("topleft",paste0(letters[i],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Residuals",side=2,outer=T,line=0.5,cex=1)
#safe
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# JABBA-Residual plots
# Requires input from r4ss::SS_output()
plname = "jabbaresiduals"
pwidth=8; pheight=5
windows(width=pwidth,height=pheight)

sspar(mfrow=c(1,2),labs=F,plot.cex=1)
SSplotJABBAres(ss3sma,add=T,legendcex=0.8,tickEndYr=T,xylabs=F,indexselect = c(1,3,4,5,6),legendsp = 1)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotJABBAres(ss3hke,add=T,legendcex=0.8,tickEndYr=T,xylabs=F,legendsp = 1)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Residuals",side=2,outer=T,line=0.5,cex=1)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.77))
#save
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Retrospective plots with one-step ahead forecasts
# Ref lastyrplus1
# Requires input from r4ss::SSsummarize()

lastyrplus1 = TRUE
start.retro= 0
end.retro =5
eyh <- retro.hke$endyrs + start.retro:-end.retro+ifelse(lastyrplus1,1,0)

pwidth=8; pheight=8
windows(width=pwidth,height=pheight)
plname = "retrowithforecast"
sspar(mfrow=c(2,2),labs=F,plot.cex=1.)
SSplotRetro(retro.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",uncertainty = F,showrho = F,forecast = T,labels="SSB (t)",legendsp=0.9)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retro.hke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",showrho = F,forecast = T,labels="SSB (t)",endyrvec = eyh,legendsp=0.9)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("SSB (t)",side=2,outer=T,line=0.5,cex=1)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.77))
SSplotRetro(retro.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",xmin=2005,uncertainty = F,legend = F,forecast = T,legendsp = 0.9,forcastrho=T)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retro.hke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomright",xmin=2005,legend = F,forecast = T,endyrvec = eyh,legendsp = 0.9,forcastrho=T)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
#save
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


# Retrospective plots without forecasts
# Requires input from r4ss::SSsummarize()

pwidth=8; pheight=8
windows(width=pwidth,height=pheight)

plname = "retrosimple"
sspar(mfrow=c(2,2),labs=F,plot.cex=1.)
SSplotRetro(retro.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",uncertainty = F,showrho = F,forecast = F,labels="SSB (t)",legendsp = 0.9)
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retro.hke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",showrho = F,forecast = F,labels="SSB (t)",endyrvec = eyh,legendsp = 0.9)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)

mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("SSB (t)",side=2,outer=T,line=0.5,cex=1)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.4,cex=1.1,c(0.27,0.77))
SSplotRetro(retro.sma,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomleft",xmin=2005,uncertainty = F,legend = F,forecast = F)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRetro(retro.hke,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,legendloc = "bottomright",xmin=2005,legend = F,forecast = F,endyrvec = eyh)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)

# save
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


# ASPM plots 
# Requires input from r4ss::SSsummarize()
plname = "aspmplot"
pwidth = 8
pheight = 9
res=300
windows(width=pwidth,height=pheight)

sspar(mfrow=c(3,2),labs=F,plot.cex=1)
SSplotModelcomp(aspm.sma,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM"))
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Index",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "SSB",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("SSB (t)",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "SSB",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotModelcomp(aspm.sma,subplots = "Recruits",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[5],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Recruits (1000s)",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "Recruits",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[6],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.2,cex=1.1,c(0.27,0.77))
#safe
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")



# ASPM plots 
# Requires input from r4ss::SSsummarize()
plname = "aspmplot_bbmsy"
pwidth = 8
pheight = 9
res=300
windows(width=pwidth,height=pheight)

sspar(mfrow=c(3,2),labs=F,plot.cex=1)
SSplotModelcomp(aspm.sma,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legendlabels = c("Full Model","ASPM"))
legend("topleft",paste0(letters[1],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Index",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "Index",ylimAdj=1.1,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[2],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)

SSplotModelcomp(aspm.sma,subplots = "Bratio",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[3],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
text(1955,1.15,expression(SSB[MSY]),cex=0.9)
mtext(expression(SSB/SSB[MSY]),side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "Bratio",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[4],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
text(1952,1.3,expression(SSB[MSY]),cex=0.9)
SSplotModelcomp(aspm.sma,subplots = "Recruits",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[5],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Recruits (1000s)",side=2,outer=F,line=1.5,cex=1)
SSplotModelcomp(aspm.hke,subplots = "Recruits",ylimAdj=1.15,add=T,legendcex=0.8,tickEndYr=F,xylabs=F,indexselect = c(1),legend=F)
legend("topleft",paste0(letters[6],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext(c("SMA","HAKE"), side=3, outer=T,line= -0.2,cex=1.1,c(0.27,0.77))
#safe
dev.print(tiff,paste0(getwd(),"/",plname,"_hires.tiff"), width = pwidth, height = pheight, res = res, units = "in")
dev.print(jpeg,paste0(getwd(),"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")



