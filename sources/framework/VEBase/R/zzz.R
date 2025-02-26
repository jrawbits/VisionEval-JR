#' @include VisionEval.R
# zzz.R - VEBase package .onLoad function

# .onAttach is called when a package is attached to an R session
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to VisionEval 4.0 with online installation!")
  packageStartupMessage("If VisionEval does not start automatically, please run this command:")
  packageStartupMessage("startVisionEval()")
}
