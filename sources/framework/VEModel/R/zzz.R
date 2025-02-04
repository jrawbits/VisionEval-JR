#' @include environment.R models.R results.R query.R
# zzz.R - visioneval package .onLoad function

# This file contains an .onAttach function that loads the VisionEval
# runtime environment

# .onAttach is called when a library is attached to the search path
.onAttach <- function(libname, pkgname) {
  initLog(Save=FALSE,Threshold=Sys.getenv("VE_LOGLEVEL",unset="warn")) # Set default logging threshold
  if ( ! "package:VEBase" %in% search() ) { # running without VEBase
    packageStartupMessage("Welcome to the VisionEval 4.0!")
    getSetup(reload=TRUE)   # with no arguments, reload the ve.runtime configuration and return ve.env$RunParam_ls
  } # else all of this will have been done in VEBase and will get set up when VEBase loads VEModel
}

.onDetach <- function(libpath) {
  # If attaching the package put us in a different working directory,
  # then detaching should put us back where we came from.
  if ( !is.na(ve.env$start.dir) ) setwd(ve.env$start.dir)
}
