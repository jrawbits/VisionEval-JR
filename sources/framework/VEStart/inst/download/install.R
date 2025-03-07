# Script to scope out the user's candidate for VE_HOME

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
