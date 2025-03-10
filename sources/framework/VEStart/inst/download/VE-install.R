# Set up user's VE_HOME directory
#   Bare directory, or with an installer zip file
#   Or look for Github structure (VE-Bootstrap.R plus sources)
# Create ve-lib
# Prompt for installer.
# Options:
#   0. Locally available .zip installer in VE_HOME
#   1. Any standard available installer at the Github
#      Standard name pattern
#      look for assets in latest release to download
#      Filter based on user's R version
#   2. Alternative Github from ve-install.cnf (YAML format)
#      Look for assets in latest release to download
#   3. Alternative to Clone and build from VisionEval-dev Github
#   4. Alternative Github for CLone from ve-install.cnf (YAML format)
# Download (if a release) or Clone (for development)
#   Both require VE_HOME set and empty
# If Download:
#   Unzip the installer file into "install" directory of VE_HOME
#   Look for type of install in installation manifest
#      (pre-installed, win.binary install, source install)
#   Pre-install
#     Copy to ve-lib (this is the existing approach - big download)
#     win.binary install - install.packages from contriburl
#       If dependencies are not present, load them from online
#     source install - install packages from contriburl
#       Probably requires RTools, especially if getting all
#       dependencies; get dependencies online if not present in
#       installer
#   Load VEStart
#     Then run startVisionEval()
#     Will prompt user to set startup location for models
#     (VE_RUNTIME)
#     Create startup files in VE_HOME and VE_RUNTIME
#     Then change to that directory and load VEModel
# If Clone
#    Go to selected Github and do git2r clone into VE_HOME
#      (reject and prompt for new VE_HOME not empty)
#    Set VE_BUILD, VE_HOME
#    Launch VE-Boostrap.R once complete
#    ve.build() then ve.run()


ve.home <- getwd()
ve.bootstrap <- "VE-Bootstrap.R"
this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
two.digit.R <- tools::file_path_sans_ext(this.R)
minimal.ve <- c("VEStart","VEBuild","VEModel","visioneval")
ve.lib.name <- "ve-lib"
ve.lib <- file.path(ve.home,ve.lib.name,two.digit.R)

# Change the path when going live
# repository.path <- "https://github.com/visioneval/VisionEval-dev/releases/latest/download"
repository.path <- "C:/Git-Repos/VisionEval-dev-VE40/built/ve-pkg-repo/bin/windows/contrib/4.4"

# Check for presence of VisionEval library
start <- character(0)
start <- if ( dir.exists( ve.lib ) ) {
  if ( file.exists(file.path(ve.home,ve.bootstrap)) ) {
    # Developer bootstrap start
    ve.bootstrap
  } else {
    # Check if VE minimal packages are present
    inst.pkgs <- utils::installed.packages(lib.loc=ve.lib)[,"Package"]
    if ( all ( minimal.ve %in% inst.pkgs ) ) {
      # If so, just do a regular startup
      "VEStart"
    } else {
      # if missing packages, do an install
      "install"
    }
  }
} else {
  # Make sure ve.lib is present
  dir.create(ve.lib,recursive=TRUE)
  "install"
}
if ( start == ve.bootstrap ) {
  # Developer bootstrap start
  source(file.path(ve.home,ve.bootstrap))
  return(invisible(getwd()))
} else if ( start == "install" ) {
  # Install VEStart from Github assets
  # Look for properly formed files 
  pkgType <- .Platform$pkgType
  if ( pkgType == "win.binary" ) {
    vestart.file <- paste0("VEStart_R",two.digit.R,".zip") # VEStart_R4.4.zip
    vestart.file <- "VEStart_4.0.0.zip"
  } else {
    vestart.file <- paste0("VEStart.tgz") # VEStart.tgz built source package for other versions of R or non-Windows
  }
  ve.release <- file.path(repository.path,vestart.file)
  cat("VE Release:",ve.release,"\n")
  install.packages(ve.release,repos=NULL,lib=ve.lib,type=pkgType)
}
if ( ! require("VEStart",lib.loc=ve.lib,quietly=TRUE) ) {
  stop("VEStart is not present in ",ve.lib)
}
startVisionEval()  
# startVisionEval will check library integrity and do either an online or offline installation
# depending on what is present in VE_HOME.
