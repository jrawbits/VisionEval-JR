# These functions will load the build scripts from VEBuild systemdata build-scripts folder

# We don't just make the build functions elements of the package namespace, because rebuilding the
# VEBuild package itself may require detaching it so it can be rebuilt and reinstalled into the VE
# library.

# Dependencies in the loaded files (e.g. yaml, miniCRAN) are identified for the package
# DESCRIPTION file.

# Note: may still want to use the import package so that private objects can be hidden from
# the environment.

.onAttach <- function(libname, pkgname) {

  ve.env <- if ( ! "ve.env" %in% search() ) {
    attach(NULL,name="ve.env")
  } else {
    as.environment("ve.env")
  }

  # Load the ve.builder scripts so VEBuild itself can be unloaded and rebuilt
  running <- Sys.getenv("VE_BUILD_RUNNING",NA) # Don't reload scripts if one of them might be rebuilding VEBuild
  if ( is.na(running) ) {
    # It's on the script to set and unset VE_BUILD_RUNNING
    packageStartupMessage("Bootstrapping VisionEval...")
    VEBuild.scripts <- system.file("build-scripts",package="VEBuild")
    build.loader <- file.path(VEBuild.scripts,"load-builder.R")
    if ( ! file.exists(build.loader) ) {
      packageStartupMessage("No build.loader at ",VEBuild.scripts)
      stop("VEBuild is missing load-builder.R.")
    }
    # Create an environment to hold build functions (if not already present)
    env.build <- if ( ! "ve.builder" %in% search() ) {
      attach(NULL,name="ve.builder")
    } else {
      as.environment("ve.builder")
    }
    sys.source(build.loader,envir=env.build) # Imports ve.build and related functions
    env.build$load.builder(ve.scripts=VEBuild.scripts,CRAN.mirror=CRAN.mirror)
  }
}

# ve.build will build VisionEval from local sources
# This function stub will probably never be called, but it is maintained here to generate
#  function documentation.
#' Build VisionEval from source code in local directories.
#' The VEBuild package loads a separate searchable environment and namespace which contains the
#'   true machinery of ve.build. The function here exists for documentation purposes and will just
#'   call the ve.build function in the "ve.builder" pseudo-package.
#' Packages to be built are provided as a list of regular expressions that are used to search the
#'   directories and subdirectories listed in PackageSources (in ve-config.yml). Any dependencies
#'   will be loaded into the target ve-lib and (depending on any configured Installer type)
#'   possibly into dependencies-repo (either as source or binary, depending on the package type
#'   being built
#' @param packages a character vector of regular expressions naming VE packages to build; default
#'   is an empty character string, which will match all packages; see description above
#' @param reset a logical; if TRUE, then remove any package artifacts before rebuilding matched
#'   packages; default is FALSE (up to date packages will be skipped)
#' @param check a logical; if TRUE, run R CMD check; otherwise skip those tests
#' @param confirm a logical; if TRUE (default for interactive use), ask user to confirm prior to
#'   (re-)building each package.
#' @param config a list of configuration elements that replace iems in the ve-config.yml file (see
#'   documentation for that file elsewhere)
#' @return data.frame of packages and status (unchanged, built, failed)
#' @name ve.build
NULL

# ve.build <- function(packages="",reset=FALSE,check=TRUE,confirm=interactive(),config=list()) {
#   do.it <- get("ve.build",envir=as.environment("ve.builder")) # NOTE: will throw an error if not present
#   do.it(packages=packages,reset=reset,check=check,confirm=confirm,config=config) 
# }
