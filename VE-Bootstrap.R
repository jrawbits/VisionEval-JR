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
  ve.env$build.config <- "ve-build-config.yml" # Where to look for VE packages to build
  ve.env$ve.lib.name <- "ve-lib" # Where to install the VE R library (within ve.home)

  # ve.home (VE_HOME in .Renviron) is the location of ve-lib (and typically the location of this VE-Bootstrap.R file)
  ve.env$ve.home <- normalizePath(Sys.getenv("VE_HOME",getwd()),winslash="/",mustWork=FALSE)
  # ve.build.dir (VE_BUILD in .Renviron) is the location in which to put the intermediate artifacts of building VE
  # (including installers if you later choose to make those)
  ve.env$ve.build.dir <- Sys.getenv("VE_BUILD",NA)
  # ve.runtime (VE_RUNTIME in .Renviron) is the location of your "models" folder
  ve.env$ve.runtime <- Sys.getenv("VE_RUNTIME",NA)

  # If VE_BUILD or VE_RUNTIME are not present in .Renviron when this script runs, look for them
  # in useful places.
  if ( is.na(ve.env$ve.build.dir) ) {
    if ( getwd() != ve.env$ve.home ) {
      ve.env$ve.build.dir <- file.path(ve.home,"built") # See if we can use 'built' folder in ve.home
    }
    if ( is.na(ve.env$ve.build.dir) || ! dir.exists(ve.env$ve.build.dir) ) {
      ve.env$ve.build.dir <- file.path(getwd(),"built") # put ve.build.dir in the current directory
    }
  }
  if ( is.na(ve.env$ve.runtime) ) {
    if ( getwd() != ve.env$ve.home ) {
      # If ve.env$ve.home is somewhere else than working directory, we presume we
      # are in the runtime directory
      ve.env$ve.runtime <- getwd()
    } else {
      # Create a default runtime within ve.home
      ve.env$ve.runtime <- file.path(ve.env$ve.home,"runtime")
    }
  }

  # VE_SOURCE identifies the VE source files. VE_SOURCE is used internally when you run the the
  # online installation script at visioneval.org/categories/download.html and choose to download a
  # snapshot of the Github code or point the installer to your own local clone of the VisionEval
  # Github code. VE_SOURCE, if set, should already include the "sources" subdirectory.
  ve.env$ve.sources <- normalizePath(Sys.getenv("VE_SOURCE",file.path(ve.env$ve.home,"sources")),winslash="/",mustWork=FALSE)

  # Do the rest of the work reading sources etc from VE_HOME and putting build artifacts in VE_BUILD.
  if ( ! dir.exists(ve.env$ve.build.dir) ) dir.create(ve.env$ve.build.dir,recursive=TRUE)

  # Set VE_BUILD so we can find it again later during the build process.
  Sys.setenv(VE_BUILD=ve.env$ve.build.dir)

  # Set up ve-lib to contain the built R packages that comprise VisionEval
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
  {
    # Load the builder environment from the source tree
    # We won't use VEBuild itself.
    # If we start a runtime VE and then require(VEBuild) it will re-initialize the loader
    #   just like this file. Running ve.build will always detach VEBuild itself if it is loaded.
    # The use case for requiring VEBuild is to rebuild a couple of local packages without having
    #   to iterate over building the entire core VE (so e.g. for updating PUMS or PTaF).
    ve.env <- loadRuntimeEnvironment()
    VEBuild.scripts <- file.path(ve.env$ve.sources,"framework","VEBuild","inst","build-scripts")
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
