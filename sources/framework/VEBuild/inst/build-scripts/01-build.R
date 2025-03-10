#!/bin/env Rscript

# Author: Jeremy Raw

script.contents <- c( "ve.build" ) # for "import" package to make a pseudo package

# TODO:
#   - Add the installer interpretation
#     - Need to grab suitable versions of dependencies if doing offline
#     - Need a repository for online VE packages themselves if we want to do online
#     - handle installing from "repos" versus from "contriburl" in config
#   - Document how to layer VEBuild or VEBootstrap.R on top of an existing VE_HOME
#     - just need ve-lib and ve-build-config (but defaults should work)
#     - will create ve-src plus the various package downloads (just for package
#       being newly built)
#     - need it to use the existing ve-lib from VE_HOME, so navigating ve-lib
#       when we start up VEBuild and sticking with that will be important.
#   - Install the packages
#   - Make installers
#     - Offline installer requires R version (so we just do contriburl)
#     - Create a different kind of directory name for VEStart to seek
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
#   packages; default is FALSE (up to date packages will be minimally rebuilt)
# @param check a logical; if TRUE, run R CMD check; otherwise skip those tests
# @param confirm a logical; if TRUE (default for interactive use), ask user to confirm prior to
#   (re-)building each package.
# @param config a list of configuration elements that replace iems in the ve-config.yml file (see
#   documentation for that file elsewhere)
# @param debug if TRUE or numeric non-zero, issue additional debugging messages during build
# @param list if TRUE just report what packages would be built and exit
# @return data.frame of packages and status (unchanged, built, failed)
ve.build <- function(
  targets="",
  reset=FALSE,
  check=reset,
  confirm=interactive(),
  config=list(),
  debug=FALSE,
  listtargets=FALSE
) {

  build.config <- ve.build.config(config=config,debug=debug)

  pkg.desc <- ve.get.targets(targets,build.config,debug=debug)
  if ( listtargets ) {
    return(pkg.desc)
  }

  ve.load.dependencies(pkg.desc,build.config,debug=debug)

  ve.build.packages(pkg.desc,build.config,reset=reset,check=check,debug=debug)
}

ve.build.config <- function(config=list(),debug=FALSE) {
  # Prepare Configuration and setup from ve-build-config.yml and update from config parameter

  # NOTE: this function expects ve.home, ve.build.dir, CRAN.mirror, and ve.lib set in "ve.env"
  # Usually ve.env$ve.lib will be the same location as ve.lib later set from configuration file; if
  # the user alters the configuration after starting R, it is possible that the new ve.lib will
  # lead to re-downloading stuff when the full dependencies are built. Unlikely to be a problem in
  # practice. VE-Bootstrap.R sets that environment up, as does VEBuild for deeper end-user builds.

  ve.env <- try( silent=TRUE, as.environment("ve.env") )
  if ( ! is.environment(ve.env) ) {
    stop("VisionEval environment is unavailable. Use VE-Bootstrap.R to begin.", call. = FALSE)
  }

  build.config <- list()
  within( build.config,
    {
      # ve.build.config returns a copy of build.config with elements added for each of the objects
      # created in the expression block below, and accessible as e.g. build.config$config.file
      # The build.config list is used later as an environment for the build sub-steps.
      cat("Loading Build environment...\n")
      build.type <- .Platform$pkgType
      if ( ! suppressWarnings(requireNamespace("yaml",quietly=TRUE)) ) {
        # Used to read configuration files - always get from online source
        utils::install.packages("yaml", lib=ve.env$ve.lib, repos=ve.env$CRAN.mirror, type=build.type, quiet=!debug )
        suppressWarnings(requireNamespace("yaml",quietly=TRUE))
      }

      ve.wantdocs <- TRUE

      config.file <- "ve-build-config.yml"
      if ( exists("ve.home") ) { # look here for build configuration
        config.file <- file.path(ve.home,config.file)
      }
      raw.config <- if ( file.exists(config.file) ) {
        # if (debug)
        cat("Configuration file:",config.file,"\n")
        yaml::yaml.load_file(config.file)
      } else {
        # if (debug)
        cat("No usable",config.file,": Config from built-in default\n")
        list()
      }

      default.config <- list(
        # bare defaults
        Output = "Build",          # or "Install" in which case add the step to make an installer
        InstallerType = "Online",  # Type of installer to make, ignored if Output is "Build"
        BuildTargets = c(          # Standard names for folders in VE_BUILD
          ve.lib = "ve-lib",                      # Where VE packages are installed
          ve.src = "ve-src",                      # Where package to build is developed
          ve.repository = "ve-pkg-repo",          # Repository for built packages (always source and binary)
          ve.dependencies = "dependencies-repo"   # Repository for dependencies (downloaded, only for platform package type)
        ),
        PackageSources = c( "sources", "external" ) # Directories (absolute or relative to VE_HOME) with packages to build
        # Can be a single package directory or the parent of many package
        # directories (sought recursively)
      )
      if ( length(raw.config) == 0 || is.null(names(raw.config)) ) {
        raw.config <- default.config
      } else {
        # Force raw.config to have at least the names in default.config
        missing.names <- ! names(default.config) %in% names(raw.config) 
        default.names <- names(default.config)[ missing.names ]
        raw.config[ default.names ] <- default.config[ missing.names ]
      }

      # BuildTargets are names for things like ve-lib or ve-src (see the sample)
      if ( "BuildTargets" %in% names(raw.config) && is.list(raw.config$BuildTargets) ) {
        # YAML brings BuildTargets in as a named list; make it a named character vector
        raw.config$BuildTargets <- unlist(raw.config$BuildTargets)
      }
      if ( ! "CRAN.mirror" %in% names(raw.config) ) {
        raw.config$CRAN.mirror <- "https://cloud.r-project.org"
      }

      # Add command-line configuration parameters (e.g. replacement PackageSources)
      if ( is.list(config) && !is.null(names(config)) ) {
        raw.config[names(config)] <- config
      }

      # Construct actual directory names from Build-Targets
      this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")

      # NOTE: this ve.lib may not be the same as ve.env$ve.lib
      # It won't matter if they differ, but there may be a few rendundant downloads
      # Put ve.lib in VE_HOME to interoperate between developer and end-user installations
      ve.lib <- file.path(ve.env$ve.home,raw.config$BuildTargets["ve.lib"],tools::file_path_sans_ext(this.R))
      if ( ! dir.exists(ve.lib) ) dir.create(ve.lib,recursive=TRUE)

      # This is the location where the VE packages are built up prior to being built into R packages
      # That will include steps like creating the "data" directory, building module_docs, etc.
      ve.src <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.src"])
      if ( ! dir.exists(ve.src) ) dir.create(ve.src)

      ve.repository <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.repository"])
      if ( ! dir.exists(ve.repository) ) dir.create(ve.repository)
      ve.repository.url <- paste0("file:///",ve.repository)
      build.contriburl <- utils::contrib.url(ve.repository, build.type)
      build.contriburl.src <- utils::contrib.url(ve.repository, "source") # Always build VE source package too
      if ( ! dir.exists(build.contriburl) ) dir.create(build.contriburl,recursive=TRUE)
      if ( ! dir.exists(build.contriburl.src) ) dir.create(build.contriburl.src,recursive=TRUE)

      ve.dependencies <- file.path(ve.env$ve.build.dir,raw.config$BuildTargets["ve.dependencies"])
      if ( ! dir.exists(ve.dependencies) ) dir.create(ve.dependencies)
      ve.dependencies.url <- paste0("file:///",ve.dependencies)
      dependencies.contriburl <- utils::contrib.url(ve.dependencies, build.type)

      # Obscure error message if ve.lib is already in .libPaths() so we need to test
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
        cat("Can't locate certain build paths. These will be ignored:")
        print(raw.config$PackageSources[missing.paths])
      }
      if ( debug ) {
        cat("Building packages from these paths:\n")
        print(package.paths)
      }
      # The following shortcuts get used during build to find obsolete installed packages
      pkgs.info <- utils::installed.packages(lib.loc=ve.lib)
      if ( nrow(pkgs.info) == 0 ) 
      pkgs.info <- utils::installed.packages(lib.loc=ve.lib)[,c("Package","Version")]
      pkgs.installed <- pkgs.info[,"Package"] # list of installed package names (including dependencies)
      pkgs.version <- pkgs.info[,"Version"]   # versions of the packages (only checked later for VE packages)
      rm(pkgs.info)
    }
  )
}

ve.get.targets <- function(targets,build.config,debug=FALSE) {
  # Determine the specific packages to build (from PackageSources)
  # Create pkg.desc as a named list of information about each package
  # The names are the base names of their folders, not the Package in DESCRIPTION
  # This function (or one of the later ones) will report mismatches (and duplicate Package names)

  with(
    build.config,
    {
      all.packages <- dir(package.paths,pattern="^DESCRIPTION$",recursive=TRUE,full.name=TRUE)
      target.packages <- character(0)
      for ( tgt in targets ) {
        target.packages <- c(target.packages,grep(tgt,all.packages,value=TRUE))
      }
      target.packages <- dirname(unique(target.packages))
      # package.names <- basename(target.packages) # the "real" name is the Package: in DESCRIPTION
      if ( length(target.packages)>0 ) {
        cat("\nBuilding VisionEval packages found in these directories:\n")
        print(target.packages)
      } else {
        stop("No VisionEval packages were found to build. Check PackageSources in ve-build-config.yml.\n")
      }

      # Load descriptions of the target packages...
      if ( ! suppressWarnings(requireNamespace("desc",quietly=TRUE)) ) {
        # Used to read DESCRIPTION file for Package name and Dependencies
        utils::install.packages("desc", lib=ve.lib, repos=CRAN.mirror, type=build.type, quiet=TRUE )
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

ve.load.dependencies <- function(pkg.desc,build.config,debug=FALSE) {
  # Download and install the R package dependencies
  cat("\nLoading dependencies...\n")
  with(
    build.config, # as an environment for these commands, providing configured locations
    {
      support.packages <- c("BiocManager","desc","devtools","dplyr","miniCRAN","rcmdcheck","roxygen2","withr","gert","yaml")
      if ( ! suppressWarnings(requireNamespace("dplyr",quietly=TRUE)) ) {
        # Used to easily assemble the dependencies into a single list of packages
        utils::install.packages("dplyr", lib=ve.lib, repos=CRAN.mirror, type=build.type, quiet=TRUE )
        suppressWarnings(requireNamespace("dplyr",quietly=TRUE))
      }
      if ( ! suppressWarnings(requireNamespace("BiocManager",quietly=TRUE)) ) {
        # The only current (early 2025) use of BioConductor is for the rhdf5 package. Does anyone still use HDF5?
        utils::install.packages("BiocManager", lib=ve.lib, repos=CRAN.mirror, type=build.type, quiet=TRUE )
        suppressWarnings(requireNamespace("BiocManager",quietly=TRUE))
      }
      if ( ! suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE)) ) {
        # Used to build local repository to support later building an offline installer
        utils::install.packages("miniCRAN", lib=ve.lib, repos=CRAN.mirror, type=build.type, quiet=TRUE )
        suppressWarnings(requireNamespace("miniCRAN",quietly=TRUE))
        # https://cran.r-project.org/web//packages/miniCRAN/vignettes/miniCRAN-introduction.html
      }

      # BiocManager::repositories() will return appropriate download locations for this version of R
      repos.online <- c(CRAN.mirror,BiocManager::repositories())

      # Assemble a complete list of dependencies that are not currently being built
      # Mark out BaseR packages (even if explicitly listed, they will always be there)
      base.lib <- dirname(find.package("base")) # looking for recommended packages
      pkgs.BaseR <- utils::installed.packages(lib.loc=base.lib, priority=c("base", "recommended"))[,"Package"]
      #   message("BaseR Packages:")
      #   print(pkgs.BaseR)

      # Add support.packages to the list in case no one else asks for them
      # That's needed if VEBuild (which has them as dependencies) is not itself being built
      pkg.deps <- unique(dplyr::bind_rows(lapply(pkg.desc,function(pkg) pkg$Dependencies), .id = "BuildPackage")$package)
      pkg.deps <- unique(c(support.packages,pkg.deps))
      pkg.deps <- pkg.deps[ ! pkg.deps %in% c(names(pkg.desc),pkgs.BaseR) ]
      # ignore any BaseR packages that were explicitly listed as dependencies
      available.online <- utils::available.packages(repos=repos.online,type=build.type)[,"Package"] # will take a while...
      available.online <- pkg.deps %in% available.online
      pkg.deps.online <- pkg.deps[ available.online ]
      if ( any( ! available.online ) ) {
        # Make sure offline dependencies (VE or locally built packages) are either already installed or
        # scheduled to be built (i.e. present in pkg.desc list of targets)
        local.deps <- pkg.deps[ ! available.online ]
        available.local.names <- utils::available.packages(repos=ve.repository.url,type=build.type)[,"Package"]
        available.local <- local.deps %in% c(available.local.names,names(pkg.desc)) # either built already or scheduled to build
        if ( any( ! available.local ) ) {
          cat("Required package(s) are not built and not scheduled to build:\n")
          print( local.deps[ ! available.local ] )
          stop("Re-run ve.build being sure to include those targets")
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

      # Remove from pkg.deps any that are installed.
      # If a dependency was installed or built outside the current request, we're okay with that.
      # However, be sure to do a complete build with "reset=TRUE" before building an Installer
      inst.pkgs <- utils::installed.packages(lib.loc=ve.lib)[,"Package"]
      pkg.deps <- pkg.deps[ ! pkg.deps %in% inst.pkgs ]

      # Get full set of dependencies (recursively, dependencies of dependencies)
      # This makes a loooong list...
      if ( length(pkg.deps) > 0 ) {
        expanded.deps <- miniCRAN::pkgDep( pkg.deps, repos=repos.online, suggests=FALSE)
        missing.packages <- findMissingPackages(expanded.deps, repos=ve.dependencies.url, repo.type=build.type )
      } else {
        missing.packages <- character(0)
        expanded.deps <- character(0)
      }

      # Make sure the repository is complete (and if it is, try updating it)
      if ( length(missing.packages) > 0 ) {
        miniCRAN::addPackage(missing.packages, path=ve.dependencies, repos=repos.online, type=build.type, deps=TRUE)
      } else if ( length(expanded.deps) > 0 ) {
        miniCRAN::updatePackages(oldPkgs=expanded.deps, path=ve.dependencies, repos=repos.online, type=build.type, ask=FALSE)
      }

      # Complete installing those downloaded packages into ve-lib for runtime use
      deps.missing <- pkg.deps[ ! pkg.deps %in% inst.pkgs ]
      if ( length(deps.missing) > 0 ) {
        cat("Installing missing dependencies...\n")
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

ve.build.packages <- function(pkg.desc,build.config,reset=FALSE,check=TRUE,debug=FALSE) {
  # Process pkg.desc so we cumulatively build VE packages that depend on earlier VE packages

  # Set up to return to original working directory and remove VE_BUILD_RUNNING semaphore
  oldwd <- getwd()
  on.exit(
    {
      setwd(oldwd)
      Sys.unsetenv("VE_BUILD_RUNNING")
    }
  )

  cat("\nRunning package build...\n\n")
  Sys.setenv(VE_BUILD_RUNNING="Yes") # Semaphore that suppresses certain module code when loading Roxygen etc

  pkg.built <- logical(length(pkg.desc)) # fills all with FALSE - we'll loop multiple times over pkg.desc until all is built
  last.pkg.built <- 0
  while ( any( ! pkg.built ) ) {
    for ( i in seq_along(pkg.desc) ) {
      # Build each package one by one; will return FALSE if missing dependencies and will stop on build failure
      pkg.built[i] <- ve.build.one.package(pkg.desc[[i]],build.config,reset=reset,check=check,debug=debug) # TRUE if it was built successfully
    }
    built.this.time <- length(which(pkg.built))
    if ( built.this.time > last.pkg.built ) {
      last.pkg.built <- built.this.time # This number should get bigger on every loop through
    } else {
      cat("Still missing packages that can't be built:\n")
      print( names(pkg.desc)[ ! pkg.built ] )
      stop("Scroll up through the build messages to figure out why they failed.")
    }
  }
  # Finalize the ve.repository
  with ( build.config,
    {
      # Packages get built into a local repository; this step updates the Package index
      # so the repository stays well-formed.
      cat("\nFinalizing VisionEval package bundle.\n")
      tools::write_PACKAGES(build.contriburl, type=build.type)
    }
  )
}

# pkg is a description object from pkg.desc list
# ve.src (from build.config) is where to assemble the package to build
# ve.repository is the root of the CRAN-like repository to receive the built package
# if reset is TRUE, blow away all traces of the package source before rebuilding
# if check is TRUE, run R CMD Check
# if debug is TRUE/greater than zero, produce more debugging information
ve.build.one.package <- function(pkg,build.config,reset=FALSE,check=TRUE,debug=0) {
  # ve.src,ve.repository,build.type="binary")
  with(
    build.config,
    {
      # The folder containing the package
      pkg.folder <- pkg$Folder
      pkg.name <- basename(pkg.folder)         # Changed below with warning if pkg.folder != pkg$Package
      pkg.src <- file.path(ve.src,pkg$Package) # Where to build the package

      # Prepare package to build in ve.src
      # Allows auto-generation of namespace plus VE data estimation and documentation if required
      cat("Building",pkg$Package)
      if ( pkg.name != pkg$Package ) { # Location in PackageSources does not correspond to DESCRIPTION package name
        cat("\nBuilding Package Name",pkg$Package," differs from Folder ",pkg.folder,"\n")
        cat("Output will be in ",pkg.src,"\n")
        pkg.name <- pkg$Package
      }

      # Check that all the dependencies are installed, otherwise gracefully return FALSE
      # Need to look at all libraries as some system packages slip through the cracks (e.g. methods)
      # The dependencies of interest are those like "visioneval" itself, upon which the module
      # packages all depend. If we try building one of the module packages before "visioneval", we'll
      # just skip it on the theory that we'll eventually do "visioneval" itself, then loop back.
      available.dependencies <- utils::installed.packages()[,"Package"]
      pkg.deps <- pkg$Dependencies$package
      if ( any( missing.deps <- (! pkg.deps %in% available.dependencies) ) ) {
        # Already screened for missing dependencies that are not in the list to build
        # missing.deps will either appear in a later build, or eventually we notice that we've
        # been through the build list and they didn't get finished (e.g. because they failed due to errors)
        if ( debug ) {
          cat(" Still missing dependencies:\n") # DEBUG - don't really need to see this until we've been over the list a few times
          print(pkg.deps[missing.deps])         # DEBUG
        }
        return(FALSE) # hopefully try again after building more possible dependencies
      }

      # Gracefully return TRUE if the package is up to date and installed
      # TODO: might want to check pkg version here as well...
      if ( ! reset &&
           ! newerThan(pkg.folder,pkg.src ) &&
           pkg.name %in% utils::installed.packages(lib.loc=ve.lib)[,"Package"] ) {
        cat(": already INSTALLED\n")
        return(TRUE)
      }

      # 
      cat(" from",pkg.folder,"\n\n")

      # Step 1: Determine package status (built, installed)
      built.path.src <- utils::contrib.url(ve.repository, type="source")
      built.path.binary <- utils::contrib.url(ve.repository, type=build.type)
      binary.build <- built.path.binary != built.path.src
      src.module <- file.path(built.path.src,modulePath(pkg.name,built.path.src))
      if (reset) {
        cat("+++++++++++++Removing Previous Build Files\n")
        local({
          module.src <- src.module
          if ( length(module.src)>0 ) { # built source package exists
            module.src <- file.path(built.path.src,module.src)
          } else module_src <- character(0)
          if ( length(module.src)>0 ) {
            unlink(module.src);
            cat(module.src,"\n")
          } else if ( debug ) cat(pkg.name,": No Source Package.\n",sep="")

          if ( binary.build ) {
            module.bin <- modulePath(pkg.name,built.path.binary)
            if ( length(module.bin)>0 ) { # built binary package
              module.bin <- file.path(built.path.binary,module.bin)
            } else module.bin <- character(0)
            if ( length(module.bin)>0 ) {
              unlink(module.bin);
              cat(module.bin,"\n")
            } else if ( debug ) cat(pkg.name,": No Binary Package.\n",sep="")
          }
        })
        if ( dir.exists( pkg.src) ) {
          unlink(pkg.src,recursive=TRUE)
          cat("Removed",pkg.src,"\n")
        } else if ( debug ) {
          cat("No Build Directory.\n")
        }
      }

      # Construct list of pkg.files
      cat("+++++++++++++ Identifying Build Elements\n")
      all.files <- dir(pkg.folder,recursive=TRUE,all.files=FALSE) # not hidden files, relative to pkg.folder
      pkg.files <- grep("^data/",all.files,value=TRUE,invert=TRUE) # ignore data directory (recreate later)
      if ( length(all.files)!=length(pkg.files) ) {
        data.files <- setdiff(all.files,pkg.files)
      } else data.files <- character(0)
      # only releevant dot.files are .VEbuildignore and .Rbuildignore
      dot.files <- dir(pkg.folder,pattern="^\\.(VE|R)buildignore$",all.files=TRUE)
      if ( length(dot.files)>0 ) {
        if ( ".Rbuildignore" %in% dot.files ) {
          pkg.files <- c(pkg.files,".Rbuildignore")
        }
        if ( ".VEbuildignore" %in% dot.files ) {
          ignore.files <- ".VEbuildignore"
          # These are patterns to ignore when copying to src/ folder for build
          # Generally a subset of .Rbuildignore (keeping things like the VEModel walkthrough)
        } else {
          ignore.files <- ".Rbuildignore"
          # Do not copy anything that will be ignored during the R build
        }
        read.dot.files <- file.path(pkg.folder,ignore.files)
        ignore.patterns <- readLines(read.dot.files)
        # empty lines in .Rbuildignore would blow away everything
        ignore.patterns <- grep("^[[:space:]]*$",ignore.patterns,invert=TRUE,value=TRUE)
        if ( debug>2 ) {
          cat("Ignoring ",ignore.files," patterns:\n")
          print(ignore.patterns)
        }
        for ( pattern in ignore.patterns ) {
          if ( debug>2 ) {
            cat("Ignoring:",pattern,"; Before:\n")
            print(pkg.files)
          }
          pkg.files <- grep(pattern=pattern,pkg.files,value=TRUE,invert=TRUE)
          if ( debug>2 ) {
            cat("After:\n")
            print(pkg.files)
          }
        }
      } else {
        cat("No .Rbuildignore found in ",pkg.folder,"\n")
        if ( debug>2 ) {
          print(dir(pkg.folder,recursive=TRUE,all.files=FALSE))
          message("dot.files")
          print(dot.files)
          message("pkg.files")
          print(pkg.files)
        }
      }

      # See what is already built and installed
      check.dir <- file.path(pkg.src,paste0(pkg.name,".Rcheck"))
      if ( debug>2 ) cat( pkg.src,"exists:",dir.exists(pkg.src),"\n")
      package.built <- if ( binary.build ) {
        # On Windows, the package is already built if:
        #   a. Binary package is present, and
        #   b. Source package is present, and
        #   c. package source is not newer than ve.src copy of source
        #   d. check.dir exists (previous built test will verify age of check.dir)
        #   e. Binary package is newer than source package
        me <- sc <- de <- ck <- nt <- vr <- as.logical(NA)
        is.built <- (me <- moduleExists(pkg.name, built.path.binary)) &&
        (sc <- moduleExists(pkg.name, built.path.src)) &&
        (de <- ( dir.exists(pkg.src) && ! newerThan(pkg.folder,pkg.src,quiet=(!debug))) ) &&
        (nt <- ! newerThan( quiet=(!debug),
          pkg.folder, # don't use pkg.files here: file lists will be different
          file.path(built.path.binary,
            modulePath(pkg.name,built.path.binary))) ) &&
        (vr <- samePkgVersion(pkg.folder,getPathVersion(pkg.src),debug=debug) )
        if ( ! is.built && debug ) {
          cat("Status of unbuilt",pkg.name,paste0("(",is.built,")"),"\n")
          cat("Module",me)
          # Some of the test results won't exist since && short-circuits
          if ( !is.na(sc) ) cat(" Src",sc)
          if ( !is.na(de) ) cat(" Dir",de)
          if ( !is.na(nt) ) cat(" Newer",nt)
          cat(" Inst",(pkg.name %in% pkgs.installed))
          if ( is.na(vr) ) cat(" Ver",vr)
          cat("\n")
          if ( exists("de") && ( is.na(de) || ! de ) ) {
            cat(pkg.src,ifelse(dir.exists(pkg.src),"Exists","Does not exist"),"\n")
            cat(check.dir,ifelse(dir.exists(check.dir),"Exists","Does not exist"),"\n")
            cat("Newer than on directory (want FALSE):",newerThan(pkg.folder,pkg.src,quiet=FALSE),"\n")
          }
        }
        is.built
      } else {
        # If Source build, the package is "built" if:
        #   a. package source is not newer than ve.src copy of source
        (
          ! is.na(src.module) &&
          dir.exists(pkg.src) &&
          ! newerThan( pkg.name, pkg.files=pkg.files, pkg.src ) &&
          samePkgVersion(pkg.name,getPackageVersion(src.module),debug=(debug>1))
        )
      }
      if ( ! package.built ) {
        cat(pkg.name,"will be built\n")
      } else {
        cat(pkg.name,"is already BUILT\n")
      }

      # Package is installed if it is built and is an available installed package
      package.installed <- (
        package.built &&
        ! is.na( pkgs.installed[pkg.name] ) &&
        samePkgVersion(pkg.folder,pkgs.version[pkg.name],debug=(debug>1))
      )
      if ( ! package.installed ) {
        if ( package.built ) {
          cat(pkg.name,"will be installed\n")
        }
        if ( pkg.name %in% pkgs.installed ) {
          cat("Removing obsolete module package version:",pkgs.version[pkg.name],"\n")
          try( {
            base::unloadNamespace(pkg.name)
            utils::remove.packages(pkg.name,lib=ve.lib)
          }
          ) # ignore any errors
        } else {
          cat(pkg.name,"is NOT INSTALLED\n")
        }
      } else {
        cat(pkg.name,"is INSTALLED\n")
      }

      # Step 3: If package is not built, (re-)copy package source to ve.src
      # On Windows: ve.src copy is used to build source and binary packages and to run tests
      # For Source build: ve.src copy is used to build source package
      if ( ! package.built ) {
        if ( debug>1 ) {
          # Dump list of package source files if debugging
          show.pkg.files <- file.path(pkg.folder,dir(pkg.folder,recursive=TRUE,all.files=TRUE))
          if ( ! any(grepl("Rbuildignore",show.pkg.files)) ) warning("No .Rbuildignore for package ",pkg.name)
          cat(paste("Copying",show.pkg.files,"to",pkg.src,"\n",sep=" "),sep="")
        } else {
          cat("++++++++++ Copying module source",pkg.folder,"to build environment...\n")
        }
        if ( is.null(reset) ) reset <- TRUE
        if ( reset ) {
          if ( dir.exists(pkg.src) || file.exists(pkg.src) )
          unlink(pkg.src,recursive=TRUE) # Get rid of the build directory and start fresh
        }
        pkg.dirs <- c(dirname(pkg.files),"data") # recreate a data directory with nothing in it
        # R build process will remove that data directory if it is still empty at the end of the build
        lapply( grep("^\\.$",invert=TRUE,value=TRUE,unique(file.path(pkg.src,pkg.dirs))),
          FUN=function(x) { dir.create(x, showWarnings=FALSE, recursive=TRUE ) } )
        if ( debug ) {
          cat("Copying package files:\n")
          print(pkg.files)
        }
        invisible(
          file.copy(
            from=file.path(pkg.folder,pkg.files),
            to=file.path(pkg.src,pkg.files),
            overwrite=TRUE, recursive=FALSE
          )
        )

        ###### HACK ALERT
        # Code above prevents the build from looking at the Github 'data' directory, since it is too
        # hard to ensure that such data gets updated when new source data is provided. We will rebuild
        # the data directory in all cases.
        #     HOWEVER:
        # VETravelDemandMM includes pre-estimated data files based on confidential NHTS that have to go
        # into the 'data' directory - they are found in 'data-raw/estimated', so we'll just copy them
        # into place...
        ######
        withr::with_dir(pkg.src,{
          MM.estimated <- dir("data-raw/estimated",full.names=TRUE)
          if ( length(MM.estimated)>0 ) {
            file.copy(MM.estimated,"data")
          }
        })
        ###### END HACK

        if ( ! dir.exists(pkg.src) ) {
          stop("Failed to create build/test environment:",pkg.src)
        }
        # Compare newest dates to see if pkg.src is up to date
        if ( newerThan(pkg.folder,pkg.src,quiet=(!debug)) ) {
          # Not sure if this would ever happen in practice...
          stop("After copying, build/test environment is still older than package.paths")
        }

        # Add Git information to DESCRIPTION if pkg.folder is in a Git repository
        today <- date()
        build.info <- if ( class(try(repo.info <- gert::git_info(pkg.folder))) != "try-error" ) {
          c(
            paste0("Date|",today),                                         # Date and time of build
            paste0("Branch|",repo.info$shorthand),                         # Branch name
            paste0("Commit|",gert::git_commit_id(repo=pkg.folder)),        # Commit ID
            paste0("RemoteURL|",
              gert::git_remote_info(repo.info$remote,repo=pkg.folder)$url, # URL for primary remote
            paste0("UpstreamBranch|",repo.info$upstream),                  # Upstream branch on primary remote
            paste0("LocalRepoPath|",repo.info$path)                        # Local path for repo clone
          )
        } else {
          c(
            paste0("Date|",today),                                         # Date and time of build
            paste0("Branch|Not from Git repository"),                      # Warning message
            paste0("Commit|NA"),                                           # Commit ID
            paste0("RemoteURL|NA",                                         # URL for primary remote
            paste0("UpstreamBranch|NA"),                                   # Upstream branch on primary remote
            paste0("LocalRepoPath|",pkg.folder,                            # Directory path for package source
          )
        }
        desc::desc_set_list("VEBuildID",list_value=build.info,file=file.path(pkg.src,"DESCRIPTION"),normalize=TRUE)
      }

      # Step 4: Run devtools::document() separately to rebuild the /data directory
      # TODO for VE 4.0 - this is where we will load the modules and run their estimation functions
      if ( ! package.built ) {
        cat("++++++++++ Pre-build / Document ",pkg.name,"\n",pkg.src,"\n",sep="")

        # Build collate and namespace
        if ( ve.wantdocs ) { # optionally build docs
          te <- try( withr::with_dir(pkg.src,roxygen2::roxygenise(roclets=c("collate","namespace","rd"))), silent=TRUE )
          if ( class(te)=="try-error" ) {
            stop(paste("Documentation error (full docs):\n",te))
          }# ignore errors
        } else {
          te <- try( withr::with_dir(pkg.src,roxygen2::roxygenise(roclets=c("collate","namespace"))), silent=TRUE)
          if ( class(te)=="try-error" ) {
            stop(paste("Documentation error:\n",te))
          }# ignore errors
        }

        if ( check || ( ! reset && ! dir.exists(check.dir) ) ) {
          # Always run check if reset (building from scratch), otherwise only if there is no trace of a prior check.
          cat("++++++++++ Checking and pre-processing ",pkg.name,"\nin ",pkg.src,"\n",sep="")
          # Run the module check (prior to building anything)
          # Set working directory outside devtools:check, or it gets very confused about where to put generated /data elements.
          # Need to set "check.dir" location explicitly to "check_dir=pkg.src" (otherwise lost in space)
          # Also need to make sure that Suggested packages are also loaded (e.g. VE2001NHTS) (cran=FALSE)
          chk.args <- "--no-tests" # Never do build-time tests
          check.results <- withr::with_dir(  pkg.src,
            devtools::check(
              ".",
              check_dir=pkg.src, # what we call check.dir gets recreated by devtools::check
              document=FALSE,
              args=chk.args,
              cran=FALSE,
              error_on="error"
            )
          )
          cat("++++++++++ Check results\n")
          print(check.results)
        }

        # devtools::document with load_pkgload method leaves the package loaded to a temporary library
        # Therefore we need to explicitly detach it so we can install it properly later on
        if ( (bogus.package <- paste("package:",pkg.name,sep="")) %in% search() ) {
          if (debug) cat("Detaching",bogus.package,"\n")
          detach(bogus.package,character.only=TRUE,unload=TRUE)
          if (debug) print(search())
        }

        # Then get rid of the temporary (and possibly obsolete) source package that is left behind
        # Must build again rather than use that built package, because the results of devtools::check
        #   updates (but does not include) any files in /data
        tmp.build <- file.path(pkg.src,modulePath(pkg.name,pkg.src))
        if ( length(tmp.build)>0 && file.exists(tmp.build) ) unlink(tmp.build)
      }

      # If not built, rebuild the source module from pkg.src (this time, with updated /data)
      # and place the result in built.path.src (the VE package repository we're building)
      if ( ! package.built ) {
        obsolete <- dir(built.path.src,pattern=paste0(pkg.name,"*_"))
        if ( debug && length(obsolete)>0 ) cat("obsolete:",obsolete,"\n")
        unlink( file.path(built.path.src,obsolete) )
        src.module <- devtools::build(pkg.src, path=built.path.src)
      }

      # Step 6: Build the binary package (Windows or Mac) and install the package
      tryCatch(
        {
          # VE_BUILD_PHASE="BUILD" says remove package datasets from R/ space (see visioneval/R/module.R)
          # Tells visioneval::savePackageDataset to remove the dataset object rather than save it again
          # Running devtools::document() will have already saved the dataset for the old-style modules
          # New style modules (e.g. VETravelDemandMM) have pre-built data which gets copied into data/
          # above (see the 'hack' which will eventually become standard procedure). So they don't use
          # visioneval::savePackageDataset and don't need/are immune to this flag.
          # In VE 4.0, the code above will run estimation functions to populate /data
          Sys.setenv(VE_BUILD_PHASE="BUILD")
          if ( binary.build ) {
            # Binary build and install works a little differently from source build/install
            if ( ! package.built ) {
              # Rebuild the binary package from the ve.src folder
              # We do this on Windows (rather than building from the source package) because
              # we want to use devtools::build, but a bug in devtools prior to R 3.5.3 or so
              # prevents devtools:build from correctly building from a source package (it
              # requires an unpacked source directory, which we have in pkg.src)
              if ( debug ) cat("building",pkg.name,"from",pkg.src,"as",build.type,"\n")
              if ( debug ) cat("building into",built.path.binary,"\n")

              obsolete <- dir(built.path.binary,pattern=paste0(pkg.name,"*_"))
              if ( debug && length(obsolete)>0 ) cat("obsolete:",obsolete,"\n")
              unlink( file.path(built.path.binary,obsolete) )
              built.package <- devtools::build(pkg.src,path=built.path.binary,binary=TRUE)
              if ( length(built.package) > 1 ) { # Fix weird bug that showed up in R 3.6.2 devtools::build
                built.package <- grep("zip$",built.package,value=TRUE)
              cat("++++++++++ BUILT","binary package:",pkg.name,"\n")
              }
            } else {
              cat("++++++++++ BUILT","binary package:",pkg.name,ifelse(package.installed,"(Already Installed)",""),"\n")
              built.package <- file.path(built.path.binary, modulePath(pkg.name, built.path.binary))
            }
            if ( ! package.installed ) {
              # On Windows, install from the binary package
              cat("++++++++++ Installing built package:",built.package,"\n")
              utils::install.packages(built.package, repos=NULL, lib=ve.lib, type=build.type) # so they will be available for later modules
              package.installed <- TRUE
            }
              
          } else { # source build
            # Just do installation directly from source package (no binary package created)
            if ( ! package.installed ) {
              cat("++++++++++ Installing source package:",src.module,"\n")
              utils::install.packages(src.module, repos=NULL, lib=ve.lib, type="source")
              package.installed <- TRUE
            }
          }
          cat("++++++++++ DONE",pkg.name,"\n\n")
        }, # we define no handlers: conditions are just passed through to the parent after calling finally
        finally = Sys.unsetenv("VE_BUILD_PHASE")
      )
      return( package.installed ) # errors should be manifest in the console log
    }
  )
}

#### Remainder of file contains helper functions

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
  #   build.type section of the pkg-repository compared to the
  #   required.packages
  
  apb <- utils::available.packages(repos=repos, type=repo.type)
  return( setdiff( required.packages, apb[,"Package"]) )
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

samePkgVersion <- function( pkg.path, version, debug=FALSE ) {
  # Compare version from package path to a target version (already built)
  #
  # Args:
  #   pkg.path: path to root of a package containing DESCRIPTION
  #   version: a Version string from some other package
  #   debug: print a message
  #
  # Returns:
  #   TRUE if the versions are the same, else FALSE

  # The "all" will handle pathological cases where version is a vector longer than 1
  result <- all((old.version<-getPathVersion(pkg.path)) == version)
  if (debug) {
    cat("samePkgVersion checks",pkg.path,old.version,"against",version,":",result,"\n")
  }
  return( result )
}

getPathVersion <- function( path ) {
  # Extract version string from DESCRIPTION on path
  #
  # Args:
  #   path: path to root of a package containing DESCRIPTION
  #         error if no DESCRIPTION on that path
  #
  # Returns:
  #   Version string from DESCRIPTION file
  desc.path <- file.path(path,"DESCRIPTION")
  if ( ! file.exists(desc.path) ) stop("getPathVersion: Did not find package at",desc.path)
  return ( read.dcf(file=desc.path)[1,"Version"] )
}

getPackageVersion <- function( package ) {
  # Extract version string from a built source module (using version encoded in its name)
  #
  # Args:
  #   package: path to a source or binary package (with version encoded)
  #
  # Returns:
  #   Version string from package file name

  # Eliminate package compression formats
  version <- sapply(strsplit(substr(package,1,regexpr(".(\\.tar\\.gz|\\.zip)",package)),"_"),FUN=function(x)x[2],simplify=TRUE)
  return( version )
}
