# Setup script for bootstrap installation

# Change to create a library in the current directory
lib.loc <- "N:/Git-Repos/VisionEval-built-4.0/built/VE-4.0/build-test/ve-lib-install"

# Uncomment the following to simulate a clean install
# if ( ! dir.exists(lib.loc) ) dir.create(lib.loc)

inst.pkgs <- utils::installed.packages(lib.loc=lib.loc)
if ( "VEBase" %in% inst.pkgs[,"Package"] ) {
  message("VEBase is installed\n")
  message("Removing ",oldVEBase <- file.path(inst.pkgs["VEBase","LibPath"],"VEBase"),"\n")
  unlink(oldVEBase,recursive=TRUE)
}

# Fix the repos location based on your local build installation
utils::install.packages("VEBase",lib=lib.loc,repos="file:N:/Git-Repos/VisionEval-built-4.0/built/VE-4.0/ve-pkg-repo")
.libPaths(lib.loc)
if ( require(VEBase,quietly=TRUE) ) VEBase::startVisionEval() else stop("VEBase is still unavailable")
