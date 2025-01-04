# Setup script for bootstrap installation

# The following are just for testing updated versions of VEBase
lib.loc <- "N:/Git-Repos/VisionEval-built-4.0/built/VE-4.0/build-test/ve-lib-install"
if ( ! dir.exists(lib.loc) ) dir.create(lib.loc)
if ( dir.exists( prev.base <- file.path(lib.loc,"VEBase") ) ) unlink(prev.base,recursive=TRUE)
if ( dir.exists( prev.ve.lib <- file.path(dirname(lib.loc),"ve-lib") ) ) unlink(prev.ve.lib,recursive=TRUE)

inst.pkgs <- utils::installed.packages(lib.loc=lib.loc)
if ( "VEBase" %in% inst.pkgs[,"Package"] ) {
  message("Reinstalling VEBase over ",oldVEBase <- file.path(inst.pkgs["VEBase","LibPath"],"VEBase"))
  unlink(oldVEBase,recursive=TRUE)
}

# Fix the repos location based on your local build installation
utils::install.packages("VEBase",lib=lib.loc,repos="file:N:/Git-Repos/VisionEval-built-4.0/built/VE-4.0/ve-pkg-repo")
if ( require(VEBase,lib.loc=lib.loc,quietly=TRUE) ) VEBase::startVisionEval() else stop("VEBase is still unavailable")
