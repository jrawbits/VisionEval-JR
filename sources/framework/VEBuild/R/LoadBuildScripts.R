# These functions will load the build scripts from VEBuild systemdata build-scripts folder

# We don't just make the build functions part of the package namespace, because rebuilding the
# VEBuild package itself may require detaching it so it can be rebuilt and reinstalled into the VE
# library.

# Dependencies in the loaded files (e.g. yaml, miniCRAN) are identified in the package
# DESCRIPTION file.

# In general, requiring VEBuild as a library will not be used to build VEBuild itself.
# To rebuild from scratch, it is better to start VE from the Github root.

loadRuntimeEnvironment <- function() { # Keep this synchronized with VE-Bootstrap.R environment setup
  ve.env <- if ( ! "ve.env" %in% search() ) {
    attach(NULL,name="ve.env")
  } else {
    as.environment("ve.env")
  }

  # Set up file locations and R version
  ve.env$CRAN.mirror <- Sys.getenv("VE_CRAN_MIRROR","https://cloud.r-project.org")

  # User-adjustable names and defauls
  ve.env$build.config <- "ve-build-config.yml"
  ve.env$ve.lib.name <- "ve-lib"
  ve.env$ve.home <- normalizePath(Sys.getenv("VE_HOME",getwd()),winslash="/",mustWork=FALSE)
  ve.env$ve.build.dir <- Sys.getenv("VE_BUILD",NA)
  ve.env$ve.runtime <- Sys.getenv("VE_RUNTME",NA)
  if ( is.na(ve.env$ve.build.dir) ) {
    if ( getwd() != ve.env$ve.home ) {
      # If ve.env$ve.home is somewhere else than working directory, we presume it's because
      # the user previously did an end-user (VEStart) installation at that location
      # The working directory is the fresh source code location.
      # We'll try to rebuild into the end-user location
      ve.env$ve.build.dir <- ve.env$ve.home
      ve.env$ve.home <- getwd()
    } else {
      # Park the artifacts in "built" subdirectory
      # ve-lib itself will go in ve.home
      ve.env$ve.build.dir <- file.path(ve.env$ve.home,"built")
    }
  } else if ( is.na(ve.env$ve.runtime) ) {
    if ( getwd() != ve.env$ve.home ) {
      # If ve.env$ve.home is somewhere else than working directory, we presume we
      # are in the runtime directory
      ve.env$ve.runtime <- getwd()
    } else {
      ve.env$ve.runtime <- file.path(ve.env$ve.home,"runtime")
    }
  }
  ve.env$ve.sources <- normalizePath(Sys.getenv("VE_SOURCE",file.path(ve.env$ve.build.dir,"sources")),winslash="/",mustWork=FALSE)

  # ve.test() should be available in VEBuild after build ; provide a package name to search in src
  #   folder and load from there to allow dynamic changes. Use pkgload as in current debug setup.
  #   Aimed mostly at framework code - top-level estimation nonsense makes module packages
  #   "inconvenient".

  # Do the rest of the work reading sources etc from VE_HOME and putting build artifacts in VE_BUILD.
  if ( ! dir.exists(ve.env$ve.runtime) ) dir.create(ve.env$ve.runtime,recursive=TRUE)
  if ( ! dir.exists(ve.env$ve.build.dir) ) dir.create(ve.env$ve.build.dir,recursive=TRUE)

  Sys.setenv(VE_BUILD=ve.env$ve.build.dir)
  Sys.setenv(VE_RUNTIME=ve.env$ve.runtime)

  # Construct a ve-lib in ve.build.dir
  # These can be ignored/re-done when a full build happens, based on ve-build-config.yml
  # Generally with the default names and locations, these will end up in the right place
  this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
  two.digit.R <- tools::file_path_sans_ext(this.R)
  ve.env$ve.lib <- file.path(ve.env$ve.home,ve.lib.name,two.digit.R)
  if ( ! dir.exists(ve.env$ve.lib) ) {
    dir.create(ve.env$ve.lib,recursive=TRUE)
    # if ( ! ve.env$ve.lib %in% .libPaths() ) .libPaths(ve.env$ve.lib,.libPaths())
  }
  return(ve.env)
}

.onAttach <- function(libname, pkgname) {

  # Load the ve.builder scripts so VEBuild itself can be unloaded and rebuilt
  running <- Sys.getenv("VE_BUILD_RUNNING",NA) # Don't reload scripts if one of them might be rebuilding VEBuild
  if ( is.na(running) ) {
    # The build script loaded below will set and unset VE_BUILD_RUNNING during ve.build()
    packageStartupMessage("Bootstrapping VisionEval...")
    loadRuntimeEnvironment()
    VEBuild.scripts <- system.file("build-scripts",package="VEBuild")
    build.loader <- file.path(VEBuild.scripts,"load-builder.R")
    if ( ! file.exists(build.loader) ) {
      packageStartupMessage("No build.loader at ",VEBuild.scripts)
      stop("VEBuild is missing load-builder.R.")
    }

    # Create an environment to hold build functions (re-create if it exists)
    if ( "ve.builder" %in% search() ) {
      # blow it away and start again
      detach("ve.builder")
    }
    env.build <- attach(NULL,name="ve.builder")

    # Load the build scripts
    sys.source(build.loader,envir=env.build) # Imports ve.build and related functions
    env.build$load.builder(ve.scripts=VEBuild.scripts)
    # The load-builder.R script gives the user instructions about how to proceed
  }
}

# Function documentation for ve.build.
# The block of roxygen code below should be kept consistent wit ve.build in the build-scripts folder

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

#' Imports the build scripts into a pre-created attached environment called "ve.builder".
#' This function uses the import package to load build functions into an attached environment ve.builder.
#' The script calling load.build should have created and attached the ve.builder environment.
#' See \code{VE-Bootstrap.R} at the root of the source tree, or \code{VEBuild::.onAttach()}
#' @param ve.scripts is the directory in which to seek the builder scripts (usually "inst/build-scripts" within VEBuild)
#' @name load.builder
NULL
