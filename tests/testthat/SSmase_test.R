
library(ss3diags)
# Tests 
#sma = r4ss::SSsummarize(retro.sma)
# Check joint MASE for indices
SSmase(retro.sma)
# select indices
SSmase(retro.sma,indexselect=c(4))
# check for length comps
#smaL = ss3diags::SSretroComps(retro.sma)
SSmase(retro.sma,quants="len",indexselect = 2:4)

# check manually
mase = SSmase(retro.sma,residuals = T)
# Joint mase from index 1, 3,4
mae = apply(mase$Residuals[c("Pred.Res","Naive.Res")],2,
            function(x)mean(abs(x)))
# MASE
mae[1]/mae[2]
# compare
SSmase(retro.sma)

# Check hake
#phk= r4ss::SSsummarize(retro.phk)
SSmase(retro.phk,verbose=T,indexselect = 1)
# check for age comps
phkA = ss3diags::SSretroComps(retro.phk)
SSmase(retro.phk,quants="age")

