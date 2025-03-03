# Bootstrap loading of ve.build() and ve.run() and related functions
# Requires RTools and R set up in desired version; Rstudio optional

# Run this entire block in a local environment so variables are not saved

# Create an environment to hold ve.home, ve.env$ve.build.dir and ve.runtime, ve.lib
ve.env <- if ( ! "ve.env" %in% search() ) {
  attach(NULL,name="ve.env")
} else {
  as.environment("ve.env")
}

local(
  {
    # Set up file locations and R version
    ve.env$CRAN.mirror <- "https://cloud.r-project.org"

    # User-adjustable names and defauls
    build.config <- "ve-build-config.yml"
    ve.lib.name <- "ve-lib"
    ve.env$ve.home <- normalizePath(Sys.getenv("VE_HOME",getwd()),winslash="/",mustWork=FALSE)
    ve.env$ve.build.dir <- Sys.getenv("VE_BUILD",NA)
    if ( is.na(ve.env$ve.build.dir) ) {
      if ( getwd() != ve.env$ve.home ) {
        # If ve.env$ve.home is somewhere else than working directory, we presume it's because
        # the user previously did an end-user (VEBase) installation at that location
        # The working directory is the fresh source code location.
        ve.env$ve.build.dir <- ve.env$ve.home
        ve.env$ve.home <- getwd()
      } else {
        ve.env$ve.build.dir <- file.path(ve.env$ve.home,"built")
      }
    }

    # Look for trigger to initiate build (presence of ve-build-config.yml), under these use cases:
    #   If config is found and VEBuild is available, load VEBuild instead of VEBase
    #   Configure ve.build environment from the YAML config
    #
    # If no ve-build-config.yml, acquire VEBase and do an end-user installation from one of these places:
    #   1. Local pre-installed ve-lib (like old installer)
    #   2. Local pre-installec ve-pkg (all installable packages and dependencies as source)
    #      TODO: contrib.url for specific R version
    #   3. Local pre-installed ve-pkg-repo (VE + BioC); dependencies from CRAN
    #      TODO: contrib.url for specific R version
    #   4. Local no installed pacakges: VE/BioC from online package repo; dependencies from CRAN
    # 
    # After installing runtime VE via VEBase, switch to build environment
    #   Switch by loading VEBuild and running ve.build(), which does this:
    #   Construct a default ve-build-config.yml.
    #   Work in VE_HOME - set up "built" directory and work there to download and build from source.
    #   If we build a default ve-build-config.yml, we don't just go ahead and run it.
    #   Instead, make the user edit it with their directory and parameter preferences.
    #   Default will just pull core VE from Github with git2r via https into "sources"
    #   Build will then proceed.
    #   Don't provide too many options (but do allow individual package rebuilds).
    #   Keep the shell-based build scripts going for the time being as well.
    #
    # Provide ve.run() in ve.builder functions, and also in VEBase. Will retreat to end-user installation if
    #   VE_HOME lacks ve-lib. If VE is installed, ve.run() will change to VE_RUNTIME and load VEModel.
    #
    # ve.test() is available in VEBuild; provide a package name to search in src folder and load from
    #   there to allow dynamic changes. Use pkgload as in current debug setup. Aimed mostly at framework
    #   code - top-level estimation nonsense makes module packages "inconvenient".

    # Do the rest of the work reading sources etc from VE_HOME and putting build artifacts in VE_BUILD.
    if ( ! dir.exists(ve.env$ve.build.dir) ) {
      message("Creating VE_BUILD directory: '",ve.env$ve.build.dir,"'")
      dir.create(ve.env$ve.build.dir,recursive=TRUE)
    }
    Sys.setenv(VE_BUILD=ve.env$ve.build.dir)
    setwd(ve.env$ve.home) # Bootstrap starts in ve.home

    # Construct a ve-lib in ve.build.dir
    # These can be ignored/re-done when a full build happens, based on ve-build-config.yml
    # Generally with the default names and locations, these will end up in the right place
    this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
    ve.env$ve.lib <- file.path(ve.env$ve.build.dir,ve.lib.name,tools::file_path_sans_ext(this.R))
    if ( ! dir.exists(ve.env$ve.lib) ) {
      dir.create(ve.env$ve.lib,recursive=TRUE)
      # if ( ! ve.env$ve.lib %in% .libPaths() ) .libPaths(ve.env$ve.lib,.libPaths())
    }

    # Load the builder environment from the source tree
    # We won't use VEBuild itself.
    # If we start a runtime VE and then require(VEBuild) it will re-initialize the loader
    #   just like this file. Running ve.build will always detach VEBuild itself if it is loaded.
    # The use case for requiring VEBuild is to rebuild a couple of local packages without having
    #   to iterate over building the entire core VE (so e.g. for updating PUMS or PTaF).
    VEBuild.scripts <- file.path(ve.env$ve.home,"sources","framework","VEBuild","inst","build-scripts")
    build.loader <- file.path(VEBuild.scripts,"load-builder.R")
    if ( ! file.exists(build.loader) ) {
      message("No build.loader at ",build.loader)
      stop("VisionEval source tree has unexpected structure.")
    } else message("Loading ve.build...")
    # Create an environment to hold build functions (if not already present)
    env.build <- if ( ! "ve.builder" %in% search() ) {
      attach(NULL,name="ve.builder")
    } else {
      as.environment("ve.builder")
    }
    sys.source(build.loader,envir=env.build) # creates ve.builder environment and load.builder function
    env.build$load.builder(
      ve.scripts=VEBuild.scripts,
      CRAN.mirror=ve.env$CRAN.mirror
    )

    # Generate .Renviron with default locations
    renv.file <- file.path(ve.env$ve.home,".Renviron")
    renv.txt <- c(
      # NOTE: use wildcard for library R version,
      # so the same .Renviron will work for future versions of R.
      paste0("R_LIBS_USER=",file.path(ve.env$ve.build.dir,ve.lib.name,"%v")), # 2-digit R versions
      paste0("VE_HOME=",ve.env$ve.home),
      paste0("VE_BUILD=",ve.env$ve.build.dir),
      paste0("VE_RUNTIME=",ve.env$ve.home)
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
    message("When ready, run ve.build() to build a full VisionEval installation.\n")
  }
)
