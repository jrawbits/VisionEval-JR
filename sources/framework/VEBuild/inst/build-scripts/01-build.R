#!/bin/env Rscript

# Author: Jeremy Raw

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

