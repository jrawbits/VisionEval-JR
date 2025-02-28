#!/bin/env Rscript

# Author: Jeremy Raw

script.contents <- c( "ve.build" ) # for "import" package to make a pseudo package

# Don't break ve.build up into elements (can have helpers)
# Actions:
#   - Find all the packages to build
#     - Load package name, dependencies, VEModules (for estimation etc build)
#     - Report and abort on package directory not matching DESCRIPTION
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
#   Keep the documentation below in sync with the stub ve.build in VEBuild since
#   that is what people will consult for function documentation.

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
  build.config <- ve.build.config(reset=reset,config=config)

  pkg.desc <- ve.get.targets(targets,build.config)

  ve.load.dependencies(pkg.desc,build.config)

  ve.build.packages(pkg.desc,build.config)
}

ve.build.config <- function(config=list(),reset=FALSE) {
  # Prepare Configuration and setup from ve-build-config.yml and update from config parameter
  # reset will blow away ve.src for a clean package build

  # NOTE: this function expects ve.home, ve.build.dir, CRAN.mirror, and ve.lib set in "ve.env"
  # Usually ve.env$ve.lib will be the same location as ve.lib later set from configuration file; if
  # the user alters the configuration after starting R, it is possible that the new ve.lib will
  # lead to re-downloading stuff when the full dependencies are built. Unlikely to be a problem in
  # practice./
  # VE-Bootstrap.R sets that up, as does VEBuild for deeper end-user builds

  build.config <- list()
  within( build.config,
    # ve.build.config returns a copy of build.config with elements added for each of the objects
    # created in the expression block below, and accessible as e.g. build.config$config.file
    {
      message("Setting up Build environment...")
      build.type <- .Platform$pkgType
      if ( ! suppressWarnings(requireNamespace("yaml",quietly=TRUE)) ) {
        # Used to read configuration files - always get from online source
        utils::install.packages("yaml", lib=ve.env$ve.lib, repos=ve.env$CRAN.mirror, type=build.type )
        suppressWarnings(requireNamespace("yaml",quietly=TRUE))
      }

      config.file <- "ve-build-config.yml"
      if ( exists("ve.home") ) { # look here for build configuration
        config.file <- file.path(ve.home,config.file)
      }
      if ( file.exists(config.file) ) {
        message("Configuration file: ",config.file)
        raw.config <- yaml::yaml.load_file(config.file)
      } else {
        message("No usable ve.home; Config from built-in default")
        raw.config <- list(
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
      if ( "BuildTargets" %in% names(raw.config) && is.list(raw.config$BuildTargets) ) {
        # YAML brings BuildTargets in as a named list; make it a named character vector
        raw.config$BuildTargets <- unlist(raw.config$BuildTargets)
      }
      if ( ! "CRAN.mirror" %in% names(raw.config) ) {
        raw.config$CRAN.mirror <- "https://cloud.r-project.org"
      }

      if ( is.list(config) ) {
        # TODO: handle degenerate config (e.g. not a named list) better
        raw.config[names(config)] <- config
      }

      # Construct Build-Targets
      this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")

      # NOTE: this ve.lib may not be the same as ve.env$ve.lib
      ve.lib <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.lib"],tools::file_path_sans_ext(this.R))
      if ( ! dir.exists(ve.lib) ) dir.create(ve.lib,recursive=TRUE)

      ve.src <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.src"])
      if ( reset && dir.exists(ve.src) ) unlink(ve.src,recursive=TRUE) # Clear ve.src to a full from-scratch build
      if ( ! dir.exists(ve.src) ) dir.create(ve.src)

      ve.repository <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.repository"])
      if ( ! dir.exists(ve.repository) ) dir.create(ve.repository)
      ve.repository.url <- paste0("file:///",ve.repository)
      build.contriburl <- contrib.url(ve.repository, build.type)
      if ( ! dir.exists(build.contriburl) ) dir.create(build.contriburl,recursive=TRUE)

      ve.dependencies <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.dependencies"])
      if ( ! dir.exists(ve.dependencies) ) dir.create(ve.dependencies)
      ve.dependencies.url <- paste0("file:///",ve.dependencies)
      dependencies.contriburl <- contrib.url(ve.dependencies, build.type)

      if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib,.libPaths())) # add ve.lib to front of .libPaths() if not present

      CRAN.mirror <- raw.config$CRAN.mirror # to simplify access when we start downloading dependencies

      # Find the packages to build from within folders named in raw.config$PackageSources
      # Start by looking for absolute paths or relative to getwd()
      # getwd() will be VE_BUILD and may differ from ve.home(aka VE_HOME)
      package.paths <- normalizePath(raw.config$PackageSources,winslash="/",mustWork=FALSE)
      if ( any( missing.paths <- ! dir.exists(package.paths) ) ) {
        # retry package paths lokoing for subdirectories of ve.home explicitly
        package.paths[missing.paths] <- file.path(ve.env$ve.home,raw.config$PackageSources[missing.paths])
      }
      if ( any( missing.paths <- ! dir.exists(package.paths) ) ) {
        # Report failed paths as they appear in the ve-build-config.yml, not the expanded path
        message("Can't locate these build paths:")
        print(raw.config$PackageSources[missing.paths])
      }
    }
  )
}

ve.get.targets <- function(targets,build.config) {
  # Create pkg.desc as a named list of information about each package
  # The names are the base names of their folders, not the Package in Description
  # We'll investigate and report mismatches (and duplicate Package names) later

  with(
    build.config,
    {
      message("\nBuilding VisionEval packages from these directories:")
      all.packages <- dir(package.paths,pattern="^DESCRIPTION$",recursive=TRUE,full.name=TRUE)
      target.packages <- character(0)
      for ( tgt in targets ) {
        target.packages <- c(target.packages,grep(tgt,all.packages,value=TRUE))
      }
      target.packages <- dirname(unique(target.packages))
      # package.names <- basename(target.packages) # the "real" name is the Package: in DESCRIPTION
      print(target.packages)

      # Process descriptions of the target packages...
      if ( ! suppressWarnings(requireNamespace("desc",quietly=TRUE)) ) {
        # Used to read DESCRIPTION file for Package name and Dependencies
        utils::install.packages("desc", lib=ve.lib, repos=CRAN.mirror, type=build.type )
        suppressWarnings(requireNamespace("desc",quietly=TRUE))
      }
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
      names(pkg.desc) <- as.character(sapply(pkg.desc,FUN=function(pkg) pkg$Package))

      pkg.desc # return list of packages to build for further processing
    }
  )
}

ve.load.dependencies <- function(pkg.desc,build.config) {
  # Download and install the R package dependencies
  message("Loading dependencies...")
  with(
    build.config, # avoid having to write stuff like build.config$ve.dependencies
    {
      support.packages <- c("BiocManager","desc","devtools","dplyr","miniCRAN","rcmdcheck","roxygen2","withr","yaml")
      if ( ! suppressWarnings(requireNamespace("dplyr",quietly=TRUE)) ) {
        # Used to easily assemble the dependencies into a single list of packages
        utils::install.packages("dplyr", lib=ve.lib, repos=CRAN.mirror, type=build.type )
        suppressWarnings(requireNamespace("dplyr",quietly=TRUE))
      }
      if ( ! suppressWarnings(requireNamespace("BiocManager",quietly=TRUE)) ) {
        # The only current (early 2025) use of BioConductor is for the rhdf5 package. Does anyone still use HDF5?
        utils::install.packages("BiocManager", lib=ve.lib, repos=CRAN.mirror, type=build.type )
        suppressWarnings(requireNamespace("BiocManager",quietly=TRUE))
      }
      if ( ! suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE)) ) {
        # Used to build local repository to support later building an offline installer
        utils::install.packages("miniCRAN", lib=ve.lib, repos=CRAN.mirror, type=build.type )
        suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE))
        # https://cran.r-project.org/web//packages/miniCRAN/vignettes/miniCRAN-introduction.html
      }

      repos.online <- c(CRAN.mirror,BiocManager::repositories())

      # Assemble a complete list of dependencies that are not currently being built
      # Mark out BaseR packages (even if explicitly listed, they will always be there)
      base.lib <- dirname(find.package("base")) # looking for recommended packages
      pkgs.BaseR <- installed.packages(lib.loc=base.lib, priority=c("base", "recommended"))[,"Package"]
      #   message("BaseR Packages:")
      #   print(pkgs.BaseR)

      # Add support.packages to the list in case no one else asks for them
      pkg.deps <- unique(dplyr::bind_rows(lapply(pkg.desc,function(pkg) pkg$Dependencies), .id = "BuildPackage")$package)
      pkg.deps <- unique(c(support.packages,pkg.deps))
      pkg.deps <- pkg.deps[ ! pkg.deps %in% c(names(pkg.desc),pkgs.BaseR) ]
      # ignore any that are not available in repos.online
      available.online <- available.packages(repos=repos.online,type=build.type)[,"Package"] # will take a while...
      available.online <- pkg.deps %in% available.online
      pkg.deps.online <- pkg.deps[ available.online ]
      if ( any( ! available.online ) ) {
        # Make sure offline dependencies (VE or locally built packages) are either already built or
        # scheduled to be built (i.e. present in pkg.desc list of targets)
        local.deps <- pkg.deps[ ! available.online ]
        available.local <- available.packages(repos=ve.repository.url,type=build.type)[,"Package"]
        available.local <- local.deps %in% c(available.local,names(pkg.desc)) # either built already or scheduled to build
        if ( any( ! available.local ) ) {
          message("Required package(s) are not built and not scheduled to build:")
          stop("Re-run ve.build being sure to include those targets")
          print( local.deps[ ! available.local ] )
        }
      }
      
      # Prepare to copy dependencies into a local repository
      # We do it this way to make it easier later to build an offline installer where all the
      # downloaded dependency packages get zipped up with the VE stuff

      # Build local repository file tree if not present to receive packages
      if ( ! dir.exists(dependencies.contriburl) ) {
        # Grab the build support packages as the basis for the repository since they are needed
        # independently of any particular VE package dependencies.
        miniCRAN::makeRepo(support.packages, path = ve.dependencies, repos=repos.online, type = build.type)
      }

      # Get full set of dependencies (recursively, dependencies of dependencies)
      # This makes a loooong list...
      pkg.deps.online <- 
      expanded.deps <- miniCRAN::pkgDep( pkg.deps, repos=repos.online, suggests=FALSE)
      missing.packages <- findMissingPackages(expanded.deps, repos=ve.dependencies.url, repo.type=build.type )

      # Make sure the repository is complete (and if it is, try updating it)
      if ( length(missing.packages) > 0 ) {
        miniCRAN::addPackage(missing.packages, path=ve.dependencies, repos=repos.online, type=build.type, deps=TRUE)
      } else {
        miniCRAN::updatePackages(oldPkgs=expanded.deps, path=ve.dependencies, repos=repos.online, type=build.type, ask=FALSE)
      }

      # Complete installing those downloaded packages into ve-lib for runtime use
      inst.pkgs <- installed.packages(lib.loc=ve.lib)[,"Package"]
      deps.missing <- pkg.deps[ ! pkg.deps %in% inst.pkgs ]
      if ( length(deps.missing) > 0 ) {
        message("Installing missing dependencies...")
        print(deps.missing)
        utils::install.packages(deps.missing, lib=ve.lib, contriburl=paste0("file:///",dependencies.contriburl),type=build.type )
      }

      # Now load the installed support packages (needed for doing the package build)
      for ( pkg in support.packages ) {
        if ( ! suppressWarnings(requireNamespace(pkg,quietly=TRUE)) ) {
          utils::install.packages(pkg, lib=ve.lib, contriburl=paste0("file:///",dependencies.contriburl), type=build.type )
          suppressWarnings(requireNamespace(pkg,quietly=TRUE))
        }
      }
    }
  )
}

ve.build.packages <- function(pkg.desc,build.config) {
  # Process pkg.desc so we cumulatively build VE packages that depend on earlier VE packages

  # Perform the build on each package
  oldwd <- getwd()
  on.exit(
    {
      setwd(oldwd)
      Sys.unsetenv("VE_BUILD_RUNNING")
    }
  )

  cat("\nRunning package build")
  Sys.setenv(VE_BUILD_RUNNING="Yes")
  pkg.built <- logical(length(pkg.desc)) # fills all with FALSE
  last.pkg.built <- 0
  while ( any( ! pkg.built ) ) {
    for ( i in seq_along(pkg.desc) ) {
      pkg.built[i] <- ve.build.one.package(pkg.desc[[i]],build.config) # TRUE if it was built successfully
    }
    built.this.time <- length(which(pkg.built))
    if ( built.this.time > last.pkg.built ) {
      last.pkg.built <- built.this.time
    } else {
      cat("Still missing packages that can't be built:")
      print( names(pkg.desc)[ ! pkg.built ] )
      stop("Scroll up through the build messages to figure out why they failed.")
    }
  }
  # Finalize the ve.repository
  with ( build.config,
    {
      cat("\nFinalizing VisionEval package bundle.\n")
      tools::write_PACKAGES(build.contriburl, type=build.type)
    }
  )
}

# pkg is an absolute path to a directory containing a package
# ve.src is where to assemble the package to build
# ve.repository is the root of the CRAN-like repository to receive the built package
ve.build.one.package <- function(pkg,build.config) {
  # ve.src,ve.repository,build.type="binary")
  with(
    build.config,
    {
      folder <- pkg$Folder
      pkg.name <- basename(folder) # Change to use name from DESCRIPTION
      pkg.src <- file.path(ve.src,pkg$Package)

      # Prepare package to build in ve.src
      # Allows auto-generation of namespace plus VE data estimation and documentation if required
      if ( pkg.name != pkg$Package ) {
        message("Building Package Name",pkg$Package," differs from Folder ",folder)
        message("Output will be in ",pkg.src)
        pkg.name <- pkg$Package
      }
      cat("Building",pkg$Package)

      # Check that all the dependencies are installed, otherwise gracefully return FALSE
      available.dependencies <- installed.packages(lib.loc=ve.lib,)[,"Package"]
      pkg.deps <- pkg$Dependencies$package
      if ( any( missing.deps <- (! pkg.deps %in% available.dependencies) ) ) {
        # Already screened for missing dependencies that are not in the list to build
        # missing.deps will either appear in a later build, or eventually we notice that we've
        # been through the build list and they didn't get finished
        cat(" Still missing dependencies:\n") # DEBUG - don't really need to see this until we've been over the list a few times
        print(pkg.deps[missing.deps])         # DEBUG
        return(FALSE) # hopefully try again after building more possible dependencies
      }

      # Gracefully return TRUE if the package is up to date and installed
      if ( ! newerThan(folder,pkg.src ) && pkg.name %in% installed.packages(lib.loc=ve.lib)[,"Package"] ) {
        cat(": INSTALLED.\n")
        return(TRUE)
      }

      # try to continue the overall build even if this package fails
      cat(" from",folder,"\n\n")
      te <- try( silent=TRUE,
        {
          file.copy(from=folder,to=ve.src,recursive=TRUE)
          withr::with_dir(
            pkg.src,
            {
              roxygen2::roxygenise(roclets=c("collate","namespace","rd"))
              ve.pkg.zip <- devtools::build(".",path=build.contriburl,binary=any(grepl("binary",build.type)))
              status <- if ( file.exists(ve.pkg.zip) ) {
                if ( paste0("package:",pkg.name) %in% search() || pkg.name %in% loadedNamespaces() ) devtools::unload(pkg.name)
                utils::install.packages(ve.pkg.zip,lib=ve.lib,repos=NULL,type=build.type)
                TRUE
              } else {
                stop("Failed to build ",pkg.name)
              }
            }
          )
        }
      )
      return( class(te) != "try-error" ) # errors should be manifest in the console log
    }
  )
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
  # Value: TRUE if the most recently modified srcpath file is newer
  #        than the newest tgtpath file
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

# ========== DONE WITH HELPER FUNCTIONS ==========

