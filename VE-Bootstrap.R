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

local(
  # TODO: VEBuild LoadBuildScripts should also do the setup below
  # How to package this so VEBuild can use the same code?
  {
    # Load the builder environment from the source tree
    # We won't use VEBuild itself.
    # If we start a runtime VE and then require(VEBuild) it will re-initialize the loader
    #   just like this file. Running ve.build will always detach VEBuild itself if it is loaded.
    # The use case for requiring VEBuild is to rebuild a couple of local packages without having
    #   to iterate over building the entire core VE (so e.g. for updating PUMS or PTaF).
    ve.env <- loadRuntimeEnvironment()
    VEBuild.scripts <- file.path(ve.env$ve.home,"sources","framework","VEBuild","inst","build-scripts")
    build.loader <- file.path(VEBuild.scripts,"load-builder.R")
    if ( ! file.exists(build.loader) ) {
      message("No build.loader at ",build.loader)
      stop("VisionEval source tree has unexpected structure.")
    } else message("Loading ve.build...")

    # Create an environment to hold build functions (if not already present)
    if ( "ve.builder" %in% search() ) {
      # blow it away and start again
      detach("ve.builder")
    }
    env.build <- attach(NULL,name="ve.builder")

    # Load the builder scripts
    sys.source(build.loader,envir=env.build)
    env.build$load.builder(ve.scripts=VEBuild.scripts)

    # Generate .Renviron with default locations
    renv.file <- file.path(ve.env$ve.home,".Renviron")
    renv.txt <- c(
      # NOTE: use wildcard for library R version, so the same .Renviron works for future R versions
      paste0("R_LIBS_USER=",file.path(ve.env$ve.home,ve.lib.name,"%v")), # 2-digit R versions
      paste0("VE_HOME=",ve.env$ve.home),
      paste0("VE_BUILD=",ve.env$ve.build.dir),
      paste0("VE_RUNTIME=",ve.env$ve.runtime)
    )
    if ( ! file.exists(renv.file) ) {
      writeLines(renv.txt,renv.file)
      message("\nCreated default .Renviron")
    }

    # Give the user instructions for optional configuration
    message("\nEdit VE_HOME in .Renviron to set root location for source code")
    message("  (VE_HOME is currently '",ve.env$ve.home,"')\n")
    message("Edit VE_BUILD in .Renviron to set the target location for the build.")
    message("  (VE_BUILD is currently '",ve.env$ve.build.dir,"')\n")
    message("Edit ve-build-config.yml to set locations of package files that might reside")
    message("  outside the VE_HOME directory tree.\n")
    message("ve.build() to build a full VisionEval installation.\n")
    if ( suppressPackageStartupMessages(require("VEStart",lib.loc=ve.env$ve.lib,quietly=TRUE)) ) {
      message("ve.run() to start VisionEval.\n")
    }
  }
)
