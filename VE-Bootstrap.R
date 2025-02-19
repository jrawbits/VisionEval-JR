# Bootstrap VEBuild from Github repository, then run ve.build
# Requires RTools and R set up in desired version; Rstudio optionalF

# Run this entire block in a local environment so variables are not saved

local(
  {
    # Set up file locations and R version
    CRAN.mirror <- "https://cloud.r-project.org"

    # User-adjustable names and defauls
    ve.lib.name <- "ve-lib"
    ve.src.name <- "ve.src"
    temp.build <- "temp-build"
    ve.home <- Sys.getenv("VE_HOME",getwd())
    ve.build <- Sys.getenv("VE_BUILD",file.path(ve.home,"built"))
    VEBuild.package <- file.path(ve.home,"sources","framework","VEBuild")

    # Look for trigger to initiate build (presence of VEBuild-config.yml), under these use cases:
    #   If config is found and VEBuild is available, load VEBuild instead of VEBase
    #   Configure ve.build environment from the YAML config
    #
    # TODO: how to do a true bootstrap for when we start from a real source installation.
    #   Keep this version of VE-Setup.R (perhaps call it VE-Bootstrap.R). Perhaps put it in the
    #   Github build directory. Then put a .Rprofile in the Github root that sources that file.
    #   So basically, that's the same as what we do now for bootstrap, just with some different
    #   names and machinery - building VEBuild bootstrapper to load ve.build() environment.
    #
    # If no VEBuild-config.yml, acquire VEBase and do an end-user installation from one of these places:
    #   1. Local pre-installed ve-lib (like old installer)
    #   2. Local pre-installec ve-pkg (all installable packages and dependencies as source)
    #      TODO: contrib.url for specific R version
    #   3. Local pre-installed ve-pkg-repo (VE + BioC); dependencies from CRAN
    #      TODO: contrib.url for specific R version
    #   4. Local no installed pacakges: VE/BioC from online package repo; dependencies from CRAN
    # 
    # After installing runtime VE via VEBase, switch to build environment
    #   Switch by loading VEBuild and running ve.build(), which does this:
    #   Construct a default VEBuild-config.yml.
    #   Work in VE_HOME - set up "built" directory and work there to download and build from source.
    #   If we build a default VEBuild-config.yml, we don't just go ahead and run it.
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

    if ( ! dir.exists(ve.build) ) {
      message("Creating VE_BUILD directory: '",ve.build,"'")
      dir.create(ve.build,recursive=TRUE)
    }
    setwd(ve.build)

    # Create a VE R library that might be the "live" ve-lib
    # (Unless user opts to move VE_BUILD to a different location.)
    this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
    ve.lib <- file.path(ve.build,ve.lib.name,tools::file_path_sans_ext(this.R))

    if ( ! dir.exists(ve.lib) ) {
      dir.create(ve.lib,recursive=TRUE) # no patch level on R version
    }
    .libPaths(c(ve.lib,.libPaths())) # add ve.lib to front of .libPaths()

    # locate the VEBuild package in the source tree
    # keep this up to date with Github repository structure
    message("package: ",VEBuild.package," exists ",dir.exists(VEBuild.package))
    if ( ! dir.exists(VEBuild.package) || ! "DESCRIPTION" %in% dir(VEBuild.package) ) {
      message("No VEBuild at ",VEBuild.package)
      stop("VisionEval source tree has unexpected structure.")
    }

    # Create the full package source directory from which to build packages
    ve.src <- file.path(ve.build,ve.src.name)
    ve.src.VEBuild <- file.path(ve.src,"VEBuild")
    if ( dir.exists(ve.src.VEBuild) ) unlink(ve.src.VEBuild,recursive=TRUE) # blow away temp source directory
    dir.create(ve.src.VEBuild,recursive=TRUE)
    file.copy(from=VEBuild.package,to=ve.src,recursive=TRUE)

    # Install packages required for building
    # Note that RTools in a suitable version also needs to be installed
    requireNamespace("desc",quietly=TRUE)
    if ( ! suppressWarnings(requireNamespace("desc",quietly=TRUE)) ) {
      utils::install.packages("desc", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    }
    if ( ! suppressWarnings(requireNamespace("devtools",quietly=TRUE)) ) {
      utils::install.packages("devtools", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    }
    if ( ! suppressWarnings(requireNamespace("roxygen2",quietly=TRUE)) ) {
      utils::install.packages("roxygen2", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    }
    if ( ! suppressWarnings(requireNamespace("rcmdcheck",quietly=TRUE)) ) {
      utils::install.packages("rcmdcheck", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    }
    if ( ! suppressWarnings(requireNamespace("withr",quietly=TRUE)) ) {
      utils::install.packages("withr", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    }

    # Find and install dependencies specifically for VEBuild (some are not part of development
    # environment)
    deps <- pkgload::pkg_desc(ve.src.VEBuild)$get_deps()
    deps <- deps[deps$package != "R", ]$package
    for ( pkg in deps ) {
      if ( ! requireNamespace(pkg,quietly=TRUE) ) {
        utils::install.packages( pkg,lib=ve.lib,repos=CRAN.mirror,type=.Platform$pkgType )
      }
    }

    # Update all existing packages in case of R Repository updates in dependencies
    utils::update.packages(lib=ve.lib,repos=CRAN.mirror,type=.Platform$pkgType,ask=FALSE)

    # Construct package supports using collate, rd and namespace
    # Build the package not into ve-pkg but rather temp-build so we don't confuse things later
    # VEBuild will be rebuilt in the correct final place when full VE is built
    # NOTE: Will always rebuild, even if already present to ensure correct build process updating
    ve.pkg.built <- file.path(ve.build,temp.build)
    if ( ! dir.exists(ve.pkg.built) ) dir.create(ve.pkg.built,recursive=TRUE)
    withr::with_dir(ve.src.VEBuild,roxygen2::roxygenise(roclets=c("collate","namespace","rd")))
    ve.pkg.zip <- devtools::build(ve.src.VEBuild,path=ve.pkg.built,binary=TRUE)
    if ( ! file.exists(ve.pkg.zip) ) stop("Failed to build VEBuild in ",ve.pkg.built)
    if ( "package:VEBuild" %in% search() ) devtools::unload("VEBuild")
    utils::install.packages(ve.pkg.zip,lib.loc=ve.lib,type=.Platform$pkgType)

    # Load VEBuild (which creates ve.builder functions), then unload it again
    # so we can rebuild it as part of the full installation.
    message("Load VEBuild")
    if ( ! require("VEBuild",quietly=TRUE) ) {
      stop("Failed to build VEBuild")
    }
    if ( ! file.exists(file.path(ve.home,"VEBuild-config.yml")) ) {
      file.copy( system.file(file.path("build-scripts","ve-config.yml"),package="VEBuild"), ve.home )
      # This file uses locations relative to VE_HOME (for source code) and VE_BUILD (for build destination)
    }
    if ( "ve.builder" %in% search() ) {
      devtools::unload("VEBuild")
      detach("devtools_shims") # left over from roxygenize/pkgload
    } else {
      stop("VEBuild failed to load ve.builder functions.")
    }

    # Generate .Renviron with default locations
    renv.file <- file.path(ve.home,".Renviron")
    renv.txt <- c(
      paste0("VE_HOME=",ve.home),
      paste0("VE_BUILD=",ve.build)
    )
    if ( ! file.exists(renv.file) ) {
      writeLines(renv.txt,renv.file)
    } else message(".Renviron exists and is unchanged.")

    message("Edit VEBuild-config.yml to pick source files to build.")
    message("Edit VE_BUILD in .Renviron to set the target location for the build.")
    message("  (VE_BUILD is currently '",ve.build,"')\n")
    message("When ready, run ve.build() to build a full VisionEval installation.\n")
  }
)
