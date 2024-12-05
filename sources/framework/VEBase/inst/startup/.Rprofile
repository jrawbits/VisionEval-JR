# Set up and run VisionEval

# Unload VEModel and VEBase if already present
if ( "package:VEModel" %in% search() ) detach("package:VEModel")
unloadNamespace("VEModel")
if ( "package:VEBase" %in% search() ) detach("package:VEBase")
unloadNamespace("VEBase")

# Load the installation driver
if ( ! require(VEBase,quietly=TRUE) ) {
  message("Please run:")
  message("install.packages('VEBase',repos=c('https://visioneval.org/packages','https://cloud.r-project.org'))")
  base <- FALSE
}
VEBase::ve.start()
