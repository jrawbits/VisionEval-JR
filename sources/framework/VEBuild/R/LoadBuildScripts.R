# These functions will load the build scripts from VEBuild systemdata build-scripts folder

# We don't just make the build functions as elements of the package namespace, because
# rebuilding the VEBuild package itself may require detaching it so it can be rebuilt and
# reinstalled into the VE library.

# Dependencies in the loaded files (e.g. yaml, miniCRAN) are identified for the package
# DESCRIPTION file.

# Note: may still want to use the import package so that private objects can be hidden from
# the environment.

.onAttach <- function(libname, pkgname) {

  # Create an environment on the search path to hold build functions
  env.build <- if ( ! "ve.builder" %in% search() ) {
    attach(NULL,name="ve.builder")
  } else {
    as.environment("ve.builder")
  }

  # Load VE_HOME (input locations) and VE_BUILD (output locations)
  eval(
    {
      ve.home <- Sys.getenv("VE_HOME",getwd())
      ve.build <- Sys.getenv("VE_BUILD",file.path(ve.home,"built"))
    },
    envir=env.build

  # Load the ve.builder scripts so VEBuild itself can be unloaded and rebuilt
  # TODO: go back to using the import package to make just the exposed names public
  running <- Sys.getenv("VE_BUILD_RUNNING",NA) # Don't reload scripts if one of them might be rebuilding VEBuild
  if ( is.na(running) ) {
    packageStartupMessage("Bootstrapping VisionEval...")
    build.scripts <- system.file("build-scripts",package="VEBuild")
    script.files <- file.path(build.scripts,dir(build.scripts,pattern="\\.R$"),fsep="/")
    for ( sf in script.files ) {
      packageStartupMessage("Loading script file: ",sf)
      sys.source(sf,envir=env.build)
    }
  }
}

# ve.build will build VisionEval from local sources
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
#' @param confirm a logical; if TRUE (default for interactive use), ask user to confirm prior to
#'   (re-)building each package.
#' @param config a list of configuration elements that replace iems in the ve-config.yml file (see
#'   documentation for that file elsewhere)
#' @return data.frame of packages and status (unchanged, built, failed)
ve.build <- function() {
  do.it <- get("ve.build",envir=as.environment("ve.builder")) # NOTE: will throw an error if not present
  do.it() 
}
