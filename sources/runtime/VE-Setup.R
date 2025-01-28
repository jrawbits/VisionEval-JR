# Setup script for bootstrap installation
# This files and is neighbors are created by VEBuild/makeInstaller
# This script provide a lightweight way of installing VEBase, which then reinstalls itself into the
# permanent ve-lib and replaces these files with the complete startup.

# Change to create a library in the current directory
lib.loc <- file.path(getwd(),"ve-setup")
if ( dir.exists(lib.loc ) ) unlink(lib.loc,recursive=TRUE)
dir.create(lib.loc,recursive=TRUE) # recursive won't give error if directory still exists...

# Default repository list
ve.repos <- c("https://packages.visioneval.org","https://cloud.r-project.org")
local.repos <- Sys.getenv("VE_REPOS","ve-repos.cnf") # Override for development
if ( ! file.exists(local.repos) ) {
  warning("VE_REPOS is defined but file ",local.repos," does not exist; trying default.")
  local.repos <- "re-repos.cnf" # Fall back to default repository locations
}
if ( file.exists(local.repos) ) {
  ve.repos <- c(grep("(^\\s*#\\s*)|(^\\s*$)",readLines(local.repos),invert=TRUE,value=TRUE),ve.repos)
} # the contents of ve-repos.cnf is one url per line suitable for use with install.packages or update.packages
# The first URL in ve-repos.cnf will be checked first, then the rest
# in order, followed finally by the online default URL

# Check that VEBase is up to date
inst.pkgs <- utils::installed.packages(lib.loc=lib.loc)
if ( "VEBase" %in% inst.pkgs[,"Package"] ) {
  if ( "VEBase" %in% old.packages(lib.loc=lib.loc,repos=ve,repos)[,"Package"] ) {
    update.packages("VEBase",lib.loc=lib.loc,repos=ve.repos)
  }
} else {
  utils::install.packages("VEBase",lib=lib.loc,repos=ve.repos) # "file:N:/Git-Repos/VisionEval-built-4.0/built/VE-4.0/ve-pkg-repo"
}

# Load and run VisionEval
if ( require(VEBase,lib.loc=lib.loc,quietly=TRUE) ) VEBase::startVisionEval() else stop("VEBase is still unavailable")
