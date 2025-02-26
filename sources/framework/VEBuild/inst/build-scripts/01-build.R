#!/bin/env Rscript

# Author: Jeremy Raw

# ve.build should reload .Renviron as it starts for VE_HOME and VE_BUILD

tool.contents <- c(
  "ve.build"
)

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

# TEMPORARY: basic VEBuild process
keep.around <- function() {
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
}

#=================== HELPER FUNCTIONS

# Helper functions (loaded into ve.builder and callable from build scripts
# TODO 4.0: note that we're allowing builds from multiple repositories.
# We probably need a YAML configuration file that lists the repositories and source
# code locations (essentially a lighter/reworked version of VE-config.yml).

localBranch <- function(repopath) {
  localbr <- git2r::branches(repopath,flags="local")
  hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
  return( localbr[[hd]]$name )
}

# Helper function to get roots and branches
checkBranchOnRoots <- function(roots,branches) {
  #  rtb <- names(branches)
  for ( rt in roots ) {
    # It's okay if there is not branch (i.e. not github)
    # but it's an error if there is a local branch name and it doesn't match
    if ( length(branches) > 0 ) {
      br <- branches[rt]
      if ( length(br)==1 && nzchar(br) ) {
        repopath <- get(rt)
        #       cat("Examining branch for",rt,"which should be",paste0("'",br,"'"),"\n")
        if ( dir.exists(repopath) && git2r::in_repository(repopath) ) {
          # Find the currently checked out branch by looking at HEAD for local branches
          # cat("Need branch",paste0("<",br,">"),"on repository",repopath,"\n")
          hd <- localBranch(repopath)
          # cat("Have branch",paste0("<",hd,">"),"on repository",repopath,"\n")
          cat("hd:",hd,"\n")
          cat("br:",br,"\n")
          if ( hd != br) {
            cat(paste("Root",rt,"wants branch",paste0("<",br,">"),"but has",paste0("<",hd,">")),"\n")
            return(FALSE)
          }
        } else {
          end.message <- if ( ! dir.exists(repopath) ) {
            "does not exist"
          } else if ( ! git2r::in_repository(repopath) ) {
            "is not a Git repository"
          } else {
            "is bad for an unknown reason"
          }
          cat("Branch",paste0("'",br,"'"),"specified, but",repopath,end.message,".")
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}

# Helper function for other scripts, to verify situational awareness

checkVEEnvironment <- function() {
  # Check for situational awareness, and report if we are lost
  # Returns 0 or 1
  if ( ! exists("ve.installer") || is.na(ve.installer) || ! file.exists(ve.installer) ) {
    message("Missing ve.installer; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.repository") || is.na(ve.repository) ) {
    message("Missing ve.repository definition; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.dependencies") || is.na(ve.dependencies) ) {
    message("Missing ve.dependencies definition; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.runtime") || is.na(ve.runtime) ) {
    message("Missing ve.runtime definition; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.pkgs") || is.na(ve.pkgs) ) {
    message("Missing ve.pkgs definition; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.lib") || is.na(ve.lib) ) {
    message("Missing ve.lib definition; run build-config.R")
    return(FALSE)
  } else if ( ! exists("ve.roots") || ! exists("ve.branches") || ! checkBranchOnRoots(ve.roots,ve.branches) ) {
    message("Missing roots, or incorrect branches")
    return(FALSE)
  }
  return(TRUE)
}

# The following two helpers extract modules from built packages
# Used in the scripts to detect whether a module has been built yet.
modulePath <- function( module, path ) {
  # determine which module in a vector of names is present in the
  # path
  #
  # Args:
  #   module: a character vector of module names to look for
  #   path: a file system path to look for the modules
  #
  # Returns:
  #   A character vector of the file system names (include version
  #   number strings) for the corresponding packages in module,
  #   if any
  mods <- dir(path)
  # result <- mods[grep(paste("^", basename(module), "_", sep=""), mods)]
  matching <- paste("^", basename(module), "_", sep="")
  test<-sapply(matching,FUN=function(x){ grep(x,mods) },simplify=TRUE,USE.NAMES=FALSE)
  if ( class(test)=="list" ) test <- integer(0) # weirdness of sapply(simplify=TRUE) when empty
  result <- mods[test]
}

moduleExists <- function( module, path ) {
  # determine if modulePath found any 'modules' in 'path'
  #
  # Args:
  #   module: a character vector of module names to look for
  #   path: a file system path to look for the modules
  #
  # Returns:
  #   TRUE if any matching modules were found in path, else FALSE
  #
  # Let us genuflect briefly toward a coding standard that calls for
  # a dozen lines of documentation for a one line "alias"
  found <- modulePath(module,path)
  found.test <- length(found)>0
}

# Helper function to compare package path (source) to a built target (modification date)
newerThan <- function( srcpath, tgtpath, pkg.files=character(0), quiet=TRUE ) {
  # Compare modification time for a set of files to a target file
  #
  # Args:
  #   srcpath - a single folder containing a bunch of files that might be newer, or a vector of files
  #   tgtpath - one (or a vector) of files that may be older, or may not exist
  #   pkg.files - if provided, make sure the same files are present in both places
  #   quiet - if TRUE, then print a message about what is being tested
  #
  # Value: TRUE if the most recently modified source file is newer
  #        than the newest target file
  if (!quiet) cat("Comparing",srcpath,"to",paste(tgtpath,collapse="\n"),"\n")
  if ( any(is.null(srcpath)) || any(is.na(srcpath)) || any(nchar(srcpath))==0 || ! file.exists(srcpath) ) return(TRUE)
  if ( any(is.null(tgtpath)) || any(is.na(tgtpath)) || any(nchar(tgtpath))==0 || ! file.exists(tgtpath) ) return(TRUE)
  if ( dir.exists(srcpath) ) {
    srcfiles <- dir(srcpath,recursive=TRUE,all.files=TRUE)
    if ( length(pkg.files)>0 ) {
      srcfiles <- srcfiles[ srcfiles %in% pkg.files ]
    }
    srcpath <- file.path(srcpath,srcfiles)
  }
  if ( dir.exists(tgtpath) ) {
    tgtfiles <- dir(tgtpath,recursive=TRUE,all.files=TRUE)
    if ( length(pkg.files)>0 ) {
      tgtfiles <- tgtfiles[ tgtfiles %in% pkg.files ]
    }
    tgtpath <- file.path(tgtpath,tgtfiles)
  }
  if ( length(tgtpath) < 1 ) {
    if (!quiet) cat("Newer: target files do not exist\n")
    return(TRUE)
  }
  if ( length(pkg.files)>0 && length(srcpath) > length(tgtpath) ) {
    # Only check for same length file list if pkg.files is provided
    if (!quiet) {
      cat("Newer: target files different length than source\n")
      print( srcfiles[ ! srcfiles %in% tgtfiles ] )
    }
    return(TRUE)
  }
  source.time <- file.mtime(srcpath)
  target.time <- file.mtime(tgtpath)
  source.newest <- order(source.time,decreasing=TRUE)
  target.newest <- order(target.time,decreasing=TRUE)
  if (!quiet) cat("Source:",srcpath[source.newest[1]],strftime(source.time[source.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
  if (!quiet) cat("Target:",tgtpath[target.newest[1]],strftime(target.time[target.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
  newer <- source.time[source.newest[1]] > target.time[target.newest[1]]
  if (!quiet) cat("Newer:",newer,"\n")
  return(newer)
}

# ========== DONE WITH HELPER FUNCTIONS ==========

