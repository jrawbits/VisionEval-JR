#!/bin/env Rscript

# Author: Jeremy Raw

# ve.build should reload .Renviron as it starts for VE_HOME and VE_BUILD

# TEMPORARY: basic VEBuild process
    # locate the VEBuild package in the source tree
    # keep this up to date with Github repository structure
    # Create the full package source directory from which to build packages
    ve.src <- file.path(ve.build,ve.src.name)
    ve.src.VEBuild <- file.path(ve.src,"VEBuild")
    if ( dir.exists(ve.src.VEBuild) ) unlink(ve.src.VEBuild,recursive=TRUE) # blow away temp source directory
    dir.create(ve.src.VEBuild,recursive=TRUE)
    file.copy(from=VEBuild.package,to=ve.src,recursive=TRUE)

    # Install packages required for building
    # Note that RTools in a suitable version also needs to be installed
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
    if ( ! file.exists(file.path(ve.home,"ve-build-config.yml")) ) {
      file.copy( system.file(file.path("build-scripts","ve-build-config.yml"),package="VEBuild"), ve.home )
      # This file uses locations relative to VE_HOME (for source code) and VE_BUILD (for build destination)
    }
    if ( "ve.builder" %in% search() ) {
      devtools::unload("VEBuild")
      detach("devtools_shims") # left over from roxygenize/pkgload
    } else {
      stop("VEBuild failed to load ve.builder functions.")
    }



# Don't break ve.build up into elements (can have helpers)
# Actions:
#   - VEBuild can take a list of package patterns (RE's on package directory names)
#     - Reset (delete ve-src folder for matching packages and rebuild
#       regardless of up to date)
#     - Directories (overrides configuration file) - also a default "sources" within VE_HOME
#     - Installer types
#   - load configuration file:
#     - Directories to search for packages
#     - What to build (local install, various levels of zip installers)
#     - reconcile ve.build arguments, configuration file and defaults
#   - Find all the packages to build
#     - Look for DESCRIPTION
#     - Load package name, dependencies, VEModules (for estimation etc build)
#     - Discard folders that don't match pattern
#     - Report and abort on package directory not matching DESCRIPTION
#   - Load dependencies for current R version
#     - Figure out what R repositories to use and whether to do online or offline
#     - Check if dependencies are up to date
#     - Install external dependencies (CRAN + BioC)
#     - Save dependencies in local repository (if building installer)
#     - Update dependencies if already present (install missing, then update all)morrimo
#   - Order packages to build (based on unsatisfied dependencies)
#     - Check that unsatisfied dependencies are in the package list
#     - Get them in order so we build in this order
#       - No dependencies
#       - Packages with dependencies in the no-dependency list
#       - Packages with dependencies in either earlier list
#       - Iterate all packages are ordered
#   - Build the packages
#     - Report if requested is up to date and stop unless "clean" build
#     - Copy to ve-src by package name (from DESCRIPTION)
#       - Overwrite
#       - Clean ve-src first
#     - Standard build process as we did before (and reproduce in VEBuilder bootstrap)
#     - Built results (source and possibly binary) into local repository
#   - Install the packages
#   - Make installers
#     - Offline installer requires R version (so we just do contriburl)
#     - Create a different kind of directory name for VEBase to seek
#       - all of those located in VE_BUILD or VE_HOME
#       - ve-pkg-repo versus ve-pkg-contrib
#       - dependency-repo versus dependency-contrib
#     - build directory, then zip it

# TODO: keep this documentation up to date with the stub in the package R functions
# Build the "targets", which call functions from the named .build.functions list
# @param packages a character vector of regular expressions naming VE packages to build; default
#   is an empty character string, which will match all packages; see description above
# @param reset a logical; if TRUE, then remove any package artifacts before rebuilding matched
#   packages; default is FALSE (up to date packages will be skipped)
# @param confirm a logical; if TRUE (default for interactive use), ask user to confirm prior to
#   (re-)building each package.
# @param config a list of configuration elements that replace iems in the ve-config.yml file (see
#   documentation for that file elsewhere)
# @return data.frame of packages and status (unchanged, built, failed)
ve.build <- function(targets="",reset=FALSE,confirm=interactive(),config=list()) {
  # Load ve-config.yml and update from config parameter
  # Find PackageSources and expand to normalized directories
  # - subdirectory in ve.home
  # - absolute path anywhere
  # Find all DESCRIPTION files and identify their containing directory as a package to build.
}

