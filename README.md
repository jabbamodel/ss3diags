## ss3diags

### Build Status

[![R-CMD-check](https://github.com/r4ss/r4ss/workflows/R-CMD-check/badge.svg)](https://github.com/PIFSCstockassessments/ss3diags/actions)

The R package `ss3diags` enables users to apply advanced diagnostics to evaluate a Stock Synthesis model. Diagnostics include residual analyses, hindcasting and cross-validation techniques, and retrospective analyses. Functions also allow users to reproduce the key model diagnostics plots that presented in the paper 'A Cookbook for Using Model Diagnostics in Integrated Stock Assessments'. 
<br><br>
A handbook with detailed [User guidelines for Advanced Model Diagnostics with ss3diags](https://github.com/jabbamodel/ss3diags/blob/master/Vignette/ss3diags_handbook.pdf) is currently being finalized. 


In addition, the ss3diags Github respository provides fully commented step-by-step R recipes on how to:  

- [Do log-likelood  profiling for R0](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Likelihood_profile_R0_example.R)
- [Run the ASPM diagnostic](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Setup_ASPM_example.R)
- [Conduct iterative hindcasts for restrospective analysis with forecasts](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Run_Retrospective_example.R)
- [Do Jitter tests](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Jitter_test_example.R)

with Stock Synthesis by making use of a comprehensive collection of R functions available in the R package [`r4ss`](https://github.com/r4ss/r4ss)

## Installation 

ss3diags is not currently supported on CRAN. To install `ss3diags` directly from the GitHub repository install an R version >= 3.5 and then: 
```S
install.packages("devtools")
library(devtools)
install_github("jabbamodel/ss3diags")
```
Once the package is installed it can be loaded by:
```S
library(ss3diags)
```
## Model Diagnostics
The functions within ss3diags can be grouped by purpose. 
For residual diagnostics use:
```S
SSrunstest()
SSplotRunstest()
SSplotJABBAres()
```
For retrospective and forecast bias use: 
```S
SSplotRetro()
SShcbias()
```
For hindcast cross-validation and prediction skill use:

```S
SSplotHCxval()
SSmase()
``` 
For model uncertainty use:
```S
SSdeltaMVLN()
SSplotEnsemble()
SSplotKobe()
```
### Reference

Carvalho, F., Winker, H., Courtney, D., Kapur, M., Kell, L., Cardinale, M., Schirripag, M., Kitakado, T., Yemane, D., Piner, K.R., Maunder, M.N., Taylor, I., Wetzel, C.R., Doering, K., Johnsonm, K.F., Methot, R.D., in press. A Cookbook for Using Model Diagnostics in Integrated Stock Assessments. Fisheries Research.


