
#' SSnewrun() 
#'
#' creates a subfolder with relevant ss3 file for retros and forecasts 
#' 
#' @param model model name folder to copy from  
#' @param dat data file name, default "data.ss"
#' @param ctl control file name, default "control.ss"
#' @param ss.exe name of ss.exe file, default "control.exe"
#' @param newdir file path to copy the ss3 files to
#' @param overwrite if TRUE existingg files are overwritten
#' @param pars if TRUE the "ss.par" is copied
#' @author Henning Winker
#' @export

SSnewrun <- function(model,dat,ctl,ss.exe="SS.exe",newdir,overwrite=TRUE,pars=TRUE){
  dir.create(path = newdir , showWarnings = F)
  model.run = model
  # Copy files
  file.copy(file.path(model.run, "starter.ss_new"), file.path(newdir, "starter.ss"),overwrite=overwrite)
  file.copy(file.path(model.run, "control.ss_new"), file.path(newdir, ctl),overwrite=overwrite)
  file.copy(file.path(model.run, dat), file.path(newdir, dat),overwrite=overwrite)
  file.copy(file.path(model.run, "forecast.ss"), file.path(newdir, "forecast.ss"),overwrite=overwrite)
  file.copy(file.path(model.run, ss.exe), file.path(newdir, ss.exe),overwrite=overwrite)
  # Automatically ignored for models without wtatage.ss
  file.copy(file.path(model.run, "wtatage.ss"), file.path(newdir, "wtatage.ss"),overwrite=overwrite)
  # add parameters
  if(pars) file.copy(file.path(model.run, "ss.par"), file.path(newdir, "ss.par"),overwrite=overwrite)
} # end function
