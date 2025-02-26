# Bootstrap loading of ve.build() and ve.run() and related functions
# Requires RTools and R set up in desired version; Rstudio optional

# Run this entire block in a local environment so variables are not saved

local(
  {
    # Set up file locations and R version
    CRAN.mirror <- "https://cloud.r-project.org"

    # User-adjustable names and defauls
    build.config <- "ve-build-config.yml"
    ve.lib.name <- "ve-lib"
    ve.home <- normalizePath(Sys.getenv("VE_HOME",getwd()),winslash="/",mustWork=FALSE)
    ve.build <- Sys.getenv("VE_BUILD",NA)
    if ( is.na(ve.build) ) {
      if ( getwd() != ve.home ) {
        # If ve.home is somewhere else than working directory, we presume it's because
        # the user previously did an end-user (VEBase) installation at that location
        # The working directory is the fresh source code location.
        ve.build <- ve.home
        ve.home <- getwd()
      } else {
        ve.build <- file.path(ve.home,"built")
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
    if ( ! dir.exists(ve.build) ) {
      message("Creating VE_BUILD directory: '",ve.build,"'")
      dir.create(ve.build,recursive=TRUE)
    }
    Sys.setenv(VE_BUILD=ve.build)
    setwd(ve.build)

    # TODO: the remainder here will build VEBuild and set up for ve.build()

    # NOTE: the code to set up the environment in VEBuild relies on certain scripts that we could
    # just load into "ve.builder" environment/pseudo-package using the "import" package. Then we can
    # retain consistency on what the build process is. We'd dip down into VEBuild/inst/build-scripts
    # and then import them from there. So at the end, we have working ve.builder, without ever
    # having to have built VEBuild as a package (though we will later). That will simplify the
    # dependencies.

    # Create a VE R library that might be the "live" ve-lib
    this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
    ve.lib <- file.path(ve.build,ve.lib.name,tools::file_path_sans_ext(this.R))

    if ( ! dir.exists(ve.lib) ) {
      dir.create(ve.lib,recursive=TRUE) # no patch level on R version
    }
    .libPaths(c(ve.lib,.libPaths())) # add ve.lib to front of .libPaths()

    # Use VEBuild itself if present to install builder functons
    installed <- FALSE
    ve.build.loaded <- if (
      is.na(Sys.getenv("VE_FORCE_NEW")) &&
      (installed <- "VEBuild" %in% utils::installed.packages(lib.loc=ve.lib)[,"Package"])
    ) {
      message("Using installed version of VEBuild.")
      suppressWarnings(require("VEBuild",lib.loc=ve.lib,quietly=TRUE))
    } else if ( ! installed ) {
      message("Forcing new load.")
      FALSE
    }
    if ( ve.build.loaded ) {
      # build functions should now be loaded.
      detach("package:VEBuild")
      unloadNamespace("VEBuild")
    } else {
      # VEBuild is not present, so reach into the source code and load the build functions
      # This should be the same operation performed when VEBuild itself is attached.
      VEBuild.scripts <- file.path(ve.home,"sources","framework","VEBuild","inst","build-scripts")
      build.loader <- file.path(VEBuild.scripts,"load-builder.R")
      if ( ! file.exists(build.loader) ) {
        message("No build.loader at ",build.loader)
        stop("VisionEval source tree has unexpected structure.")
      } else message("Sourcing build.loader")
      source(build.loader) # creates ve.builder environment and load.builder function
      load.builder(ve.scripts=VEBuild.scripts,CRAN.mirror=CRAN.mirror)
    }

    # Generate .Renviron with default locations
    renv.file <- file.path(ve.home,".Renviron")
    renv.txt <- c(
      # NOTE: use wildcard for library R version,
      # so the same .Renviron will work for future versions of R.
      paste0("R_LIBS_USER=",file.path(ve.build,ve.lib.name,"%v")), # 2-digit R versions
      paste0("VE_HOME=",ve.home),
      paste0("VE_BUILD=",ve.build)
    )
    if ( ! file.exists(renv.file) ) {
      writeLines(renv.txt,renv.file)
      message("\nCreated default .Renviron")
    }

    # Give the user instructions for optional configuration
    message("\nEdit VE_HOME in .Renviron to set root location for source code")
    message("  (VE_HOME is currently '",ve.home,"')\n")
    message("Edit VE_BUILD in .Renviron to set the target location for the build.")
    message("  (VE_BUILD is currently '",ve.build,"')\n")
    message("Edit ve-build-config.yml to set locations of package files that might reside")
    message("  outside the VE_HOME directory tree.\n")
    message("When ready, run ve.build() to build a full VisionEval installation.\n")
  }
)
