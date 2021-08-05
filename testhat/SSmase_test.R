
library(ss3diags)
# Tests 
sma = r4ss::SSsummarize(retro.sma)
sma = SSmase(sma,residuals = TRUE)
mae = apply(sma$Residuals[sma$Residuals$Index=="CPUE_1",4:5],2,function(x)mean(abs(x)))
# MASE
mae[1]/mae[2]
# compare
sma$MASE[1,]

retroSummary = r4ss::SSsummarize(retro.phk)
phk = SSmase(phk,residuals = TRUE)
mae = apply(phk$Residuals[,4:5],2,function(x)mean(abs(x)))
# MASE
mae[1]/mae[2]
# compare
phk$MASE[1,]

