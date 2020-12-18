
# load pakage
library(ss3diags)
library(r4ss)
# Data documentation
help("natl.sma") 
help("pac.hke")

# retro run
retroI.sma = SSsummarize(retro.sma)
retroL.sma = SSretroComps(retro.sma)
retroI.phk = SSsummarize(retro.phk)
retroA.phk = SSretroComps(retro.phk)

SSmase(retroI.sma)
SSmase(retroL.sma,quants = "len")
SSmase(retroI.phk)
SSmase(retroA.phk,quants = "age")

# New retro-forecast options (ylim added too)
sspar(mfrow=c(2,2),plot.cex=0.7)
SSplotRetro(retroI.sma,subplots = "SSB",ylim=c(0,1500),add=T,legend=F)
SSplotRetro(retroI.phk,subplots = "SSB",add=T,legend=F,ylim=c(0,7*10^6))
SSplotRetro(retroI.sma,subplots = "F",add=T,legendloc = "left",legendcex = 0.8)
SSplotRetro(retroI.phk,subplots = "F",add=T,legendloc = "left",legendcex = 0.85)

# Compute without plotting
SShcbias(retroI.sma)
SShcbias(retroI.phk)


