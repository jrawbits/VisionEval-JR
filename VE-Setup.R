# Bootstrap VEBuild from Github repository, then run ve.build
# Requires RTools and R set up in desired version; Rstudio optionalF

# TODO: Can we use the save VE-Setup.R for VEBase and VEBuild
# Key off the presence of ve-lib containing VEBase and VEBuild
# Also consider whether we are starting from a Github repository
# Identifying features of the repository are named directories and
#   ve-lib existing in a sub-directory. If there is ve-lib within
#   VE_HOME (or getwd()), presume we're doing a VEBase installation
# VEBuild can then be loaded later (including code to clone the
#   repository).

# Run this entire block in a local environment so variables are not saved

local(
  {
    # Set up file locations and R version
    CRAN.mirror <- "https://cloud.r-project.org"

    ve.lib.name <- "ve-lib"
    ve.home <- Sys.getenv("VE_HOME",getwd())

    ve.build <- Sys.getenv("VE_BUILD",file.path(ve.home,"built"))
    if ( ! dir.exists(ve.build) ) dir.create(ve.build,recursive=TRUE)

    this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")

    setwd(ve.build)

    # Create VE R library
    ve.lib <- file.path(ve.build,ve.lib.name,tools::file_path_sans_ext(this.R))

    if ( ! dir.exists(ve.lib) ) {
      dir.create(ve.lib,recursive=TRUE) # no patch level on R version
    }
    .libPaths(c(ve.lib,.libPaths())) # add ve.lib to front of .libPaths()

    # locate the VEBuild package in the source tree
    # keep this up to date with Github repository structure
    VEBuild.package <- file.path(ve.home,"sources","framework","VEBuild")
    message("package: ",VEBuild.package," exists ",dir.exists(VEBuild.package))
    if ( ! dir.exists(VEBuild.package) || ! "DESCRIPTION" %in% dir(VEBuild.package) ) {
      stop("VisionEval source tree has unexpected strucure")
    }

    # Create the full package source directory from which to build packages
    ve.src <- file.path(ve.build,"ve-src") #
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

    # Update all existing packages in case of R Repository updates in dependencies
    utils::update.packages(lib=ve.lib,repos=CRAN.mirror,type=.Platform$pkgType,ask=FALSE)

    # TODO: determine if these are, in fact, still needed.
    # # We're not using 'markdown', but some vignette creation fails if it's not present.
    # # even though we're using knitr as the vignette engine, which gets dragged in from
    # # elsewhere...
    # if ( ! suppressWarnings(requireNamespace("markdown",quietly=TRUE)) ) {
    #   utils::install.packages("markdown", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    # }
    # if ( ! suppressWarnings(requireNamespace("rmarkdown",quietly=TRUE)) ) {
    #   utils::install.packages("rmarkdown", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    # }

    # Find and install dependencies specifically for VEBuild (some are not part of development
    # environment)
    deps <- pkgload::pkg_desc(ve.src.VEBuild)$get_deps()
    deps <- deps[deps$package != "R", ]$package
    for ( pkg in deps ) {
      if ( ! requireNamespace(pkg,quietly=TRUE) ) {
        utils::install.packages( pkg,lib=ve.lib,repos=CRAN.mirror,type=.Platform$pkgType )
      }
    }

    # Construct package supports using collate, rd and namespace
    # Build the package not into ve-pkg but rather temp-build so we don't confuse things later
    # VEBuild will be rebuilt in the correct final place when full VE is built
    ve.pkg.built <- file.path(ve.build,"temp-build")
    if ( ! dir.exists(ve.pkg.built) ) dir.create(ve.pkg.built,recursive=TRUE)
    message("Roxygenize")
    withr::with_dir(ve.src.VEBuild,roxygen2::roxygenise(roclets=c("collate","namespace","rd")))
    message("Build")
    ve.pkg.zip <- devtools::build(ve.src.VEBuild,path=ve.pkg.built,binary=TRUE)
    if ( ! file.exists(ve.pkg.zip) ) stop("Failed to build VEBuild in ",ve.pkg.built)
    message("Unloading VEBuild")
    if ( "package:VEBuild" %in% search() ) devtools::unload("VEBuild")
    message("Install VEBuild")
    utils::install.packages(ve.pkg.zip,lib.loc=ve.lib,type=.Platform$pkgType)

    message("Load VEBuild")
    if ( ! require("VEBuild",quietly=TRUE) ) {
      stop("Failed to build VEBuild")
    }
    if ( "ve.builder" in search() ) {
      devtools:unload("VEBuild")
    } else {
      stop("VEBuild failed to load ve.builder functions.")
    }

    # TODO: generate a .Renviron with the necessary elements
    # VE_HOME
    # VE_BUILD (as VE_HOME/built)
    # R_LIBS_USER (as VE_HOME/built/ve-lib)
    # VE_RUNTIME (as VE_HOME/built/runtime)
    # And VE_HOME will also contain "ve-src" and "temp-build"

    # VEBuild's startup message should explain what to do next.
  }
)