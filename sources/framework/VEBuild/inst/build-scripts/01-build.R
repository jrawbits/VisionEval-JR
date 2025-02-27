#!/bin/env Rscript

# Author: Jeremy Raw

# ve.build should reload .Renviron as it starts for VE_HOME and VE_BUILD

script.contents <- c( "ve.build" )

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

# IMPORTANT:
#   Also maintain this documentation on the stub in the package R functions.
#   This file is not visited by Roxygen during the build processlk

# Build the "targets", which call functions from the named .build.functions list
# Expects that getwd() == VE_BUILD and ve.home is defined on the search path
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

  # NOTE: this function expects ve.home, ve.build.dir set in searchable environment "ve.env"
  # VE-Bootstrap.R will set that up, as will VEBase for deeper end-user builds

  # Load ve-build-config.yml and update from config parameter
  if ( ! suppressWarnings(requireNamespace("yaml",quietly=TRUE)) ) {
    utils::install.packages("yaml", lib=ve.env$ve.lib, repos=ve.env$CRAN.mirror, type=.Platform$pkgType )
    suppressWarnings(requireNamespace("yaml",quietly=TRUE))
  }
  build.config.file <- "ve-build-config.yml"
  if ( exists("ve.home") ) { # look here for build configuration
    build.config.file <- file.path(ve.home,build.config.file)
  }
  if ( file.exists(build.config.file) ) {
    message("Config from config file: ",build.config.file)
    build.config <- yaml::yaml.load_file(build.config.file)
  } else {
    message("No usable ve.home; Config from built-in default")
    build.config <- list(
      # bare defaults
      Output = "Build",
      InstallerType = "Online",
      BuildTargets = c(
        ve.lib = "ve-lib",
        ve.src = "ve-src",
        ve.repository = "ve-pkg-repo",
        ve.dependencies = "dependencies-repo"
      ),
      PackageSources = c( "sources", "external" )
    )
  }
  # BuildTargets are names for things like ve-lib or ve-src (see the sample)
  if ( "BuildTargets" %in% names(build.config) && is.list(build.config$BuildTargets) ) {
    # YAML brings BuildTargets in as a named list; make it a named character vector
    build.config$BuildTargets <- unlist(build.config$BuildTargets)
  }
  if ( ! "CRAN.mirror" %in% names(build.config) ) {
    build.config$CRAN.mirror <- "https://cloud.r-project.org"
  }

  if ( is.list(config) ) {
    # TODO: handle degenerate config (e.g. not a named list) better
    build.config[names(config)] <- config
  }

  # Construct Build-Targets
  this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")

  ve.lib <- file.path(ve.env$ve.build.dir,build.config$BuildTargets["ve.lib"],tools::file_path_sans_ext(this.R))
  if ( ! dir.exists(ve.lib) ) dir.create(ve.lib,recursive=TRUE)
  ve.src <- file.path(ve.env$ve.build.dir,build.config$BuildTargets["ve.src"])

  if ( dir.exists(ve.src) ) unlink(ve.src,recursive=TRUE) # DEBUG

  if ( ! dir.exists(ve.src) ) dir.create(ve.src)
  ve.repository <- file.path(ve.env$ve.build.dir,build.config$BuildTargets["ve.repository"])
  if ( ! dir.exists(ve.repository) ) dir.create(ve.repository)
  ve.dependencies <- file.path(ve.env$ve.build.dir,build.config$BuildTargets["ve.dependencies"])
  if ( ! dir.exists(ve.dependencies) ) dir.create(ve.dependencies)

  if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib,.libPaths())) # add ve.lib to front of .libPaths()

  # Find the packages to build from within folders named in build.config$PackageSources
  package.paths <- normalizePath(build.config$PackageSources,winslash="/",mustWork=FALSE)
  if ( any( missing.paths <- ! file.exists(package.paths) ) ) {
    package.paths[missing.paths] <- file.path(ve.env$ve.home,build.config$PackageSources[missing.paths])
  }
  if ( any( missing.paths <- ! file.exists(package.paths) ) ) {
    message("Can't locate these build paths:")
    print(build.config$PackageSources[missing.paths])
  }
  message("\nBuilding packages in these directories:")
  all.packages <- dir(package.paths,pattern="^DESCRIPTION$",recursive=TRUE,full.name=TRUE)
  target.packages <- character(0)
  for ( tgt in targets ) {
    target.packages <- c(target.packages,grep(tgt,all.packages,value=TRUE))
  }
  target.packages <- dirname(unique(target.packages))
  package.names <- basename(target.packages)
  print(package.names)

  # Load build-helper dependency packages (these will be present if VEBuild has previously been loaded)
  message("\nLoading package dependencies...")
  # Install helper package
  if ( ! suppressWarnings(requireNamespace("desc",quietly=TRUE)) ) {
    utils::install.packages("desc", lib=ve.lib, repos=ve.env$CRAN.mirror, type=.Platform$pkgType )
    suppressWarnings(requireNamespace("desc",quietly=TRUE))
  }
  if ( ! suppressWarnings(requireNamespace("dplyr",quietly=TRUE)) ) {
    utils::install.packages("dplyr", lib=ve.lib, repos=ve.env$CRAN.mirror, type=.Platform$pkgType )
    suppressWarnings(requireNamespace("dplyr",quietly=TRUE))
  }

  # Process descriptions of the target packages...
  pkg.desc <- lapply(
    target.packages,
    function(pkg) {
      ds <- desc::description$new(pkg)
      deps <- ds$get_deps()
      deps <- deps[ deps$package!="R" & deps$type != "Suggests", ]
      list(
        Package=ds$get("Package"),
        Description=ds,
        Dependencies=deps,
        Folder=pkg
      )
    }
  )
  names(pkg.desc) <- pkg.names <- as.character(sapply(pkg.desc,FUN=function(pkg) pkg$Package))

  # Assemble a complete list of dependencies that are not currently being built
  # Mark out BaseR packages
  base.lib <- dirname(find.package("base")) # looking for recommended packages
  pkgs.BaseR <- as.vector(installed.packages(lib.loc=base.lib, priority=c("base", "recommended"))[,"Package"])
  message("BaseR Packages:")
  print(pkgs.BaseR)

  # Add support.packages to the list in case no one else asks for them
  support.packages <- c("desc","devtools","dplyr","rcmdcheck","roxygen2","withr","yaml","BiocManager","miniCRAN")
  pkg.deps <- unique(dplyr::bind_rows(lapply(pkg.desc,function(pkg) pkg$Dependencies), .id = "BuildPackage")$package)
  pkg.deps <- unique(c(support.packages,pkg.deps))
  pkg.deps <- pkg.deps[ ! pkg.deps %in% c(pkg.names,pkgs.BaseR) ]

  # Prepare to copy dependencies into a local repository
  if ( ! suppressWarnings(requireNamespace("BiocManager",quietly=TRUE)) ) {
    utils::install.packages("BiocManager", lib=ve.lib, repos=ve.env$CRAN.mirror, type=.Platform$pkgType )
    suppressWarnings(requireNamespace("BiocManager",quietly=TRUE))
  }
  repos <- unique(as.character(c(build.config$CRAN.mirror,BiocManager::repositories())))
  if ( ! suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE)) ) {
    utils::install.packages("miniCRAN", lib=ve.lib, repos=repos, type=.Platform$pkgType )
    suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE))
  }
  # Build local repository file tree if not present
  src.contrib <- contrib.url(ve.dependencies, .Platform$pkgType)
  if ( ! dir.exists(src.contrib) ) {
    miniCRAN::makeRepo(c("desc","dplyr","miniCRAN","BiocManager","yaml"), path = ve.dependencies, repos=repos, type = .Platform$pkgType)
  }

  # Get full set of dependencies
  expanded.deps <- miniCRAN::pkgDep( pkg.deps, repos=repos, suggests=FALSE)
  missing.packages <- findMissingPackages(expanded.deps, repos=ve.dependencies, repo.type=.Platform$pkgType )

  # Make sure the repository is complete (and if it is, then try updating it)
  if ( length(missing.packages) > 0 ) {
    miniCRAN::addPackage(missing.packages, path=ve.dependencies, repos=repos, type=.Platform$pkgType, deps=TRUE)
  } else {
    miniCRAN::updatePackage(oldPkgs=expanded.deps, path=ve.dependencies, repos=repos, type=.Platform$pkgType, ask=FALSE)
  }

  # Now install the ones that are not already installed from the local dependency repository
  deps.missing <- pkg.deps[ ! pkg.deps %in% installed.packages(fields="Package") ]
  if ( length(deps.missing) > 0 ) {
    message("Installing dependencies:")
    print(deps.missing)
    utils::install.packages(deps.missing, lib=ve.lib, repos=ve.dependencies, type=.Platform$pkgType )
  }

  # Now load the installed support packages (needed for doing the package build)
  for ( pkg in support.packages ) {
    if ( ! suppressWarnings(requireNamespace(pkg,quietly=TRUE)) ) {
      utils::install.packages(pkg, lib=ve.lib, repos=ve.dependencies, type=.Platform$pkgType )
      suppressWarnings(requireNamespace(pkg,quietly=TRUE))
    }
  }

  stop("Testing")
  
  # Remove any that are in the build list
  # Install any externals that are not already in ve.lib
  # Perhaps update any externals that ARE in ve.lib
  #   Stop with report on any non-build-list that can't be found
  #     (most will be VE packages available but not built or asked to be built)
  #   Need to allow for BiocManager packages versus CRAN
  #   Use available.packages() to locate the installable versions

  # Run through the build list and build packages all of whose
  #   dependencies are present. Just look for dependencies in
  #   installed.packages (and update that list after each build)

  # If any package build fails, stop right there

#   pkg.built <- logical(length(pkg.desc)) # fills all with FALSE
#   names(pkg.built) <- names(pkg.desc)
#   unbuilt <- length(which(!pkg.built))
#   last.unbuilt <- 0
#   while ( unbuilt > 0 && unbuilt != last.unbuilt ) {
#     for ( pkg in 1:length(pkg.desc) ) {
#       ip <- row.names(installed.packages())
#       deps <- pkg.desc[[pkg]]$Dependencies
#       # TODO: 
# 
#       good.deps <- deps %in% ip
#       if ( all(good.deps) ) {
#         pending.deps <- deps %in% names(pkg.built)[!pkg.built]
#         ve.install.deps(deps[!good.deps & !pending.deps])
#         pkg.built[pkg] <- ve.build.package( pkg.desc[[pkg]]$Folder, ve.src, ve.repository=ve.repository )
#       }
#     }
#     last.unbuilt <- unbuilt
#     unbuilt <- length(which(!pkg.built))
#   }
#   if ( last.unbuilt == unbuilt ) {
#     message("Unable to build packages due to missing dependencies:")
#     print(names(pkg.built)[unbuilt])
#     message("Missing dependencies:")
#     print(sapply(pkg.desc[unbuilt],function(pkg) pkg$Dependencies[ ! pkg$Dependencies %in% installed.packages()))
#     stop("\nBuild failed!")
#   }

  # TODO: grab dependencies for the target packages
  #       (install any that are missing; update any that are present)
  # TODO: internal dependencies (on packages not present in ve.lib) should
  #       induce re-sorting target.packages
  # TODO: Reconcile inconsistencies between package folder and name in DESCRIPTION
  # TODO: Don't allow duplicate packages (at the DESCRIPTION name level to be built).

  # Perform the build on each package
  oldwd <- getwd()
  message("\nRunning package build")
  Sys.setenv(VE_BUILD_RUNNING="Yes")
  for ( pkg in target.packages ) {
    # TODO: push this down into a function
    # TODO: ve.pkg.built should be the binary contriburl
    # TODO: option to do source build instead/as well, depending on pkgType and build.config
    ve.build.package(pkg,ve.src,ve.repository=ve.repository)
  }
  Sys.unsetenv("VE_BUILD_RUNNING")
  setwd(oldwd)
}

# pkg is an absolute path to a directory containing a package
# ve.src is where to assemble the package to build
# ve.repository is the root of the CRAN-like repository to receive the built package
ve.build.package <- function(pkg,ve.src,ve.repository,build.type="binary") {
  pkg.name <- basename(pkg) # Change to use name from DESCRIPTION
  setwd(pkg)
  # TODO: check if pkg is newer than ve.src/pkg.name
  file.copy(from=pkg,to=ve.src,recursive=TRUE)
  pkg.src <- file.path(ve.src,basename(pkg))
  message("Building from ",pkg.src," ",dir.exists(pkg.src))
  ve.pkg.built <- contrib.url(ve.repository,build.type)
  if ( ! dir.exists(ve.pkg.built) ) dir.create(ve.pkg.built,recursive=TRUE)
  withr::with_dir(pkg.src,roxygen2::roxygenise(roclets=c("collate","namespace","rd")))
  ve.pkg.zip <- devtools::build(pkg.src,path=ve.pkg.built,binary=(build.type=="binary"))
  if ( ! file.exists(ve.pkg.zip) ) stop("Failed to build ",pkg.name," in ",ve.pkg.built)
  # TODO: reconcile package folder name with the name that is built (from DESCRIPTION)
  # TODO: in next line, may need to check loaded namespaces as well to determine if unload is needed
  if ( paste0("package:",pkg.name) %in% search() || pkg.name %in% loadedNamespaces() ) devtools::unload(pkg.name)
  utils::install.packages(ve.pkg.zip,lib=ve.env$ve.lib,repos=NULL,type=.Platform$pkgType)
  return(TRUE)
}

findMissingPackages <- function( required.packages, repos, repo.type=.Platform$pkgType ) {
  # Determine if any packages are missing from the pkg-repository
  # compared to the required.packages passed in.
  #
  # Args:
  #   required.packages: a character vector containing names of packages
  #                      we hope to find in pkg-repository
  #
  # Returns:

  #   A character vector of package names that are missing from the
  #   ve.build.type section of the pkg-repository compared to the
  #   required.packages
  
  apb <- available.packages(repos=repos, type=repo.type)
  return( setdiff( required.packages, apb[,"Package"]) )
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
    utils::install.packages("desc", lib=ve.lib, repos=build.config$CRAN.mirror, type=.Platform$pkgType )
  }
  if ( ! suppressWarnings(requireNamespace("devtools",quietly=TRUE)) ) {
    utils::install.packages("devtools", lib=ve.lib, repos=build.config$CRAN.mirror, type=.Platform$pkgType )
  }
  if ( ! suppressWarnings(requireNamespace("roxygen2",quietly=TRUE)) ) {
    utils::install.packages("roxygen2", lib=ve.lib, repos=build.config$CRAN.mirror, type=.Platform$pkgType )
  }
  if ( ! suppressWarnings(requireNamespace("rcmdcheck",quietly=TRUE)) ) {
    utils::install.packages("rcmdcheck", lib=ve.lib, repos=build.config$CRAN.mirror, type=.Platform$pkgType )
  }
  if ( ! suppressWarnings(requireNamespace("withr",quietly=TRUE)) ) {
    utils::install.packages("withr", lib=ve.lib, repos=build.config$CRAN.mirror, type=.Platform$pkgType )
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

