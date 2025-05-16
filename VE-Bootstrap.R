#!/usr/bin/env RScript VE-Bootstrap.R

# Bootstrap loading of ve.build() and ve.run() and related functions
# Requires RTools and R set up in desired version; Rstudio optional

# Run this entire block in a local environment so variables are not saved

# Keep this function strictly synchronized with the same name function in VEBuild::LoadBuildScripts.R
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
  ve.env$ve.sources <- normalizePath(Sys.getenv("VE_SOURCE",ve.env$ve.home),winslash="/",mustWork=FALSE)

  # ve.test() should be available in VEBuild after build ; provide a package name to search in src
  #   folder and load from there to allow dynamic changes. Use pkgload as in current debug setup.
  #   Aimed mostly at framework code - top-level estimation nonsense makes module packages
  #   "inconvenient".

  # Do the rest of the work reading sources etc from VE_HOME and putting build artifacts in VE_BUILD.
  if ( ! dir.exists(ve.env$ve.build.dir) ) dir.create(ve.env$ve.build.dir,recursive=TRUE)

  Sys.setenv(VE_BUILD=ve.env$ve.build.dir)

  # Construct a ve-lib in ve.build.dir
  # These can be ignored/re-done when a full build happens, based on ve-build-config.yml
  # Generally with the default names and locations, these will end up in the right place
  this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
  two.digit.R <- tools::file_path_sans_ext(this.R)
  ve.env$ve.lib <- file.path(ve.env$ve.home,ve.lib.name,two.digit.R)
  if ( ! dir.exists(ve.env$ve.lib) ) {
    dir.create(ve.env$ve.lib,recursive=TRUE)
  }
  if ( ! ve.env$ve.lib %in% .libPaths() ) .libPaths(ve.env$ve.lib)
  return(ve.env)
}

local(
  # How to package this so VEBuild can use the same code?
  {
    # Load the builder environment from the source tree
    # We won't use VEBuild itself.
    # If we start a runtime VE and then require(VEBuild) it will re-initialize the loader
    #   just like this file. Running ve.build will always detach VEBuild itself if it is loaded.
    # The use case for requiring VEBuild is to rebuild a couple of local packages without having
    #   to iterate over building the entire core VE (so e.g. for updating PUMS or PTaF).
    ve.env <- loadRuntimeEnvironment()
    VEBuild.scripts <- file.path(ve.sources,"sources","framework","VEBuild","inst","build-scripts")
    build.loader <- file.path(VEBuild.scripts,"load-builder.R")
    if ( ! file.exists(build.loader) ) {
      message("No build.loader at ",build.loader)
      stop("VisionEval source tree has unexpected structure.")
    } else message("Loading ve.build from ",VEBuild.scripts,"...")

    # Create an environment to hold build functions (if not already present)
    if ( "ve.builder" %in% search() ) {
      # blow it away and start again
      detach("ve.builder")
    }
    env.build <- attach(NULL,name="ve.builder")

    # Load the builder scripts
    sys.source(build.loader,envir=env.build)
    env.build$load.builder(VEBuild.scripts)
  }
)
