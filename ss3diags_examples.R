
sspar(mfrow=c(3,2),labs=F,plot.cex=0.8)
SSplotHCxval(retro.sma,add=T,legendcex=0.7,legend=T,legendindex = 1)
mtext("Year",side=1,outer=T,cex=1)
mtext("Index",side=2,outer=T,line=1,cex=1)

sspar(mfrow=c(3,3),labs=F,plot.cex=0.7)
SSplotHCxval(retro.sma,add=T,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=F)
plot(0,0,type="n",axes=F)
SSplotHCxval(retro.hke,add=T,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=F)
mtext("Year",side=1,outer=T,cex=1)
mtext("Index",side=2,outer=T,line=1,cex=1)


SSplotHCxval(retro.hke,add=F,legendcex=0.7,legend=T,legendindex = 1,tickEndYr=F,xylabs=T,xmin=1)
