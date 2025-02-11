#!/bin/env Rscript

# Author: Jeremy Raw

.build.defaults <- c(
  "configure","external","dependencies","modules","install"
)

# Build the "targets", which call functions from the named .build.functions list
# The dots parameter is passed to each target function
ve.build <- function(targets="all",...) {
  # "all" just steps through the .build.functions in order
  on.exit(Sys.unsetenv("VE_BUILD_RUNNING"))
  Sys.setenv(VE_BUILD_RUNNING="Yes")
  if ( targets=="all" ) targets <- .build.defaults
  for ( tgt in targets ) {
    if ( ! tgt %in% names(.build.functions) ) {
      message("No target: ",tgt)
      next
    }
    func <- .build.functions[[tgt]]
    message("Building '",tgt,"'")
    func(...)
  }
}

# TODO: perhaps re-run configure always ahead of the other targets, to update configuration
# before starting any fresh build.

# Objects must be defined before they are added to the list of build functions
.ve.list.targets  <- function(...) {
  message("Available ve.build targets:")
  print(names(.build.functions))
}

.ve.configure     <- function(...) {
  message("Configure function")
  # Always precedes other targets to set build parameters
  # Things to configure (via a YAML ve-config.yml file)
  # - VE build locations (list of Githubs or local directories)
  # - Construct souce packages or just binary
  # - Install or build only (build only will make repositories, build packages, then build installer(s))
  # - Construct end-user installer (online, CRAN-online (VE Local), Full Local; Package type)
  # VEBase will ultimately run the installer (or just boot up from local ve-lib)
}
.ve.external      <- function(...) {
  message("External function")
  # Clones confgured Github repositories and adds their directories to the package build list
}
.ve.dependencies  <- function(...) {
  message("Dependencies function")
  # Loop over package directories, build dependency list, and then retrieve those into dependencies-repo
  # Skip VE dependencies that will get built later
  # Download Bioconductor dependencies into ve-pkg-repo (source and binary) to remove dependency in eventual VEBase
  # Install all the dependencies after download into ve-lib
}
.ve.modules       <- function(...) {
  message("Modules function")
  # Loop over the package directories, identifying those that need to be built and those that are up to date
  # Order the packages based on listed VE dependencies
  #   1. No other VE dependencies
  #   2. VE dependencies already in 1.
  #   3. VE dependencies already in 1 or 2
  #   4. etc. until all package directories are scheduled for building (or an error if still missing depedencies)
  # Iterate over the package directories in dependency order
  # Do the VE pre-processing (Roxygen, estimation, moduleDocs) if package is a VE package
  # Build the packages (binary and/or source as configured)
  # Save built packages (binary and/or source as configured) to ve-pkg-repo
  # Install the package
  # Rebuild the PACKAGES index in the ve-pkg-repo (source and binary)
}

.ve.install  <- function(...) {
  message("Install function")
  # Construct an installer (including zipping it) using ve-config.yml to decide what to put in it
  # See configure
}

# Note that runtime functionality is performed by running VEBase, the runtime bootstrapper
# VEBase identifies VE_INSTALL, VE_HOME and VE_RUNTIME directoreis
# VE_INSTALL says where to look for the local CRAN repositories (default=VE_HOME)
# VE_HOME says where to put ve-lib and related installation
# VE_RUNTIME says where to put "models" and related materials

# a list of callable functions
.build.functions <- list(
   "list"         = .ve.list.targets # list names of .build.functions
  ,"configure"    = .ve.configure    # establish what to build and where to put it
  ,"external"     = .ve.external     # clone/pull Github repositories
  ,"dependencies" = .ve.dependencies # download and install dependencies
  ,"modules"      = .ve.modules      # build packages (including local and external)
  ,"install"      = .ve.install      # build packages (including local and external)
)

.ve.configure <- function() {

  options(install.packages.compile.from.source="never")

  # Set a default repository for non-interactive installation of dependency packages
  # Obsolete in 4.0 as the dependencies will be associated with VEBuild
  #   r <- getOption("repos")
  #   r["CRAN"] <- "https://cloud.r-project.org"
  #   options(repos=r)

  # Use an inner environment for the configuration settings
  ve.builder = as.environment("ve.builder") # from the search path
  assign("ve.config",new.env(parent=ve.builder),envir=ve.builder)

  # Set up the configuration elements
  # Later, attach and detach ve.config during the various build functions
  evalq(
    envir=ve.config,   # should be "ve.config" object in "ve.builder"
    expr={

      # ve.home root - where to do the build work
      # We'll look for VE_BUILD and fall back to VE_HOME if not defined
      ve.build.dir <- Sys.getenv("VE_BUILD",getwd())
      ve.home <- Sys.getenv("VE_HOME",ve.build.dir)
      # TODO 4.0: we do want to create builder startup files in ve.home along the lines of VEBase,
      # but have them launch VEBuild rather than VEBase. Perhaps look for VE_BUILD defined in
      # .Renviron in the standard Setup.R and install/run VEBuild rather than VEBase.
      # If we load VE_BUILD, there should be an easy way to step back to VEBase

      # Identify the platform and supported binary package built types
      ve.platform <- .Platform$OS.type # Used to better identify binary installer type
      ve.platform <- paste(toupper(substring(ve.platform,1,1)),substring(ve.platform,2),sep="")
      ve.mac.build.types <- c("mac.binary","mac.binary.el-capitan") # TODO: update to latest Mac OS binary...
      ve.binary.build.types <- c("win.binary", ve.mac.build.types) 
      ve.build.type <- .Platform$pkgType
      ve.binary.build <- ve.build.type %in% ve.binary.build.types
      if ( ! ve.binary.build ) {
        ve.build.type <- "source"
      }

      # Locate the installer tree (used for boilerplate for visual docs)
      # The following includes a hack to fix a common path problem if you are
      # developing on Windows in a subfolder of "My Documents" (not recommended)
      if ( ve.platform == "Windows" || ve.build.type == "win.binary" ) {
        ve.home <- sub("My Documents", "Documents", ve.home) # does nothing if "My Documents" is not in the home path
        ve.home <- gsub("\\\\", "/", ve.home)
      } else if ( ve.platform == "Unix" && ve.build.type %in% ve.mac.build.types ) {
        ve.platform <- "MacOSX"
      }

      # Presume that .libPaths() is set up (e.g. by loading R_LIBS_USER placed in .Renviron from a
      # previous run of VEBuild, or through VE-Setup.R from VEBase or VEBuild launch)
      
      # TODO 4.0: Technically redundant with package Imports, but done here redundantly in case the
      # dependencies were not installed along with VEBuild.
      if ( ! suppressWarnings(requireNamespace("yaml",quietly=TRUE)) ) {
        install.packages("yaml", lib=dev.lib, repos="https://cloud.r-project.org", dependencies=NA, type=.Platform$pkgType )
        if ( ! suppressWarnings(requireNamespace("git2r",quietly=TRUE)) ) {
          install.packages("git2r", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType )
        }
      } else {
        cat("Attempting to update ALL development packages\n")
        update.packages(lib=dev.lib,repos="https://cloud.r-project.org",type=.Platform$pkgType,ask=FALSE)
      }

      # Specify dependency repositories for known R versions
      # We'll use BiocManager later to get the correct BioConductor repository when the
      # dependencies are downloaded and installed.

      # TODO 4.0: just add dependency on BiocManager; we'll install the required versions of
      # everything as part of the build.
      # rversions <- yaml::yaml.load_file(file.path(ve.installer,"R-versions.yml"))
      #  cat("Building for R version",this.R,"\n")
      #  if ( ! this.R %in% names(rversions) ) {
      #    cat("Supported R versions:\n")
      #    print(names(rversions))
      #    stop("R version ",this.R,"is not supported",call.=FALSE)
      #  }
      #  CRAN.mirror <- rversions[[this.R]]$CRAN
      #  BioC.mirror <- rversions[[this.R]]$BioC

      # Read the configuration file
      # TODO 4.0: what needs to be in the config file is mainly a list of "roots"
      # identifying directories and githubs to refer to.
      # The Githubs should specify optional directory names for extract, or just use a default
      # relative to GithubRoot (default "Get-Repos")
      # Other roots can be specified relative to Sources (which in turn is relative to VE_HOME)
      ve.config.file <- Sys.getenv("VE_CONFIG",file.path(ve.home,"VE-build-config.yml"))
      cat(paste("Loading Configuration File:",ve.config.file,"\n",sep=" "))
      if ( !file.exists(ve.config.file) ) {
        warning("Configuration file",ve.config.file,"not found. Using default.")
        # TODO: what is the default? What do we need to specify
        # In 4.0, main role is to list out source file locations for building, including
        # whether they are Locally managed, or auto-cloned from Github
      }
      ve.cfg <- yaml::yaml.load_file(ve.config.file)

      # TODO 4.0: we're just looking for package in directories
      #  - individual packages
      #  - parent directory for many packages (search recursively for DESCRIPTION)
      #  - github repositories (using git2r to clone/pull - no push option for changes)

      # Directory tree structure for output:

      # VE_BUILD:
      #   ve-pkg-repo
      #   ve.lib ("ve-lib)
      #   ve.dependencies ("dependencies-repo")
      #   ve.pkg ("ve-pkg") # repo/contriburl to bundle in an installer
      #   ve.publish (root ve.pkg.online, "docs") # drat location
      #   ve.src ("ve-src") # intermediate location for post-processed VE packages - installed from here
      #   ve.runtime ("runtime") # default for building runtme
      #   ve.zipout ("installers") # where to put .zip offline installers
      #   ve.docs ("docs") # not currently building this; will do so later
      #   ve.external ("external") # should just be a root for Github clones

      # How to identify this sourrce code
      # Should read this tag from VE-config.yml
      ve.branch <- Sys.getenv("VE_BRANCH","")
      if ( ! nzchar(ve.branch) ) ve.branch <- localBranch(ve.root)
      if ( ! nzchar(ve.branch) ) ve.branch <- "visioneval"

      if ( ! "ve.output" %in% ve.roots || ! exists("ve.output") ) {
        ve.output = normalizePath(file.path(ve.build.dir,"built"),winslash="/",mustWork=FALSE)
      }

      ve.logs <- Sys.getenv("VE_LOGS",file.path(ve.home,"logs",ve.branch,this.R))
      if ( ! dir.exists(ve.logs) ) dir.create( ve.logs, recursive=TRUE, showWarnings=FALSE )

      cat("Building in (and around)",ve.output,"\n")
      cat("Logging into",ve.logs,"\n")

      # Extracting location paths:
      locs.lst <- names(ve.cfg$Locations)
      makepath <- function(x,venv) {
        # Build a location path from root and path and assign it
        # Note that this function is used for its SIDE-EFFECTS, not
        # its return value.
        #
        # Args:
        #   x - the name of a Location (and its veriable)
        #   venv - the environment in which to create the variable (current)
        #
        loc <- ve.cfg$Locations[[x]]
        subdir <- switch(
          loc$augment,
          root    = loc$path,                             # external packages
          branch  = file.path(ve.branch,loc$path),        # package source and documentation
          version = file.path(ve.branch,loc$path,this.R), # version-specific sub-folders (ve-lib, runtime)
          ve.branch
        )
        assign(
          x,
          normalizePath(
            file.path(
              get(loc$root),
              subdir
            ),
            winslash="/",
            mustWork=FALSE
          ),
          pos=venv
        )
        get(x,pos=venv)
      }
      invisible(sapply(locs.lst,FUN=makepath,venv=as.environment("ve.builder")))

      if ( ! exists("ve.lib") ) {
        ve.lib <- file.path(ve.home,"ve-lib")
      }

      # Create the locations
      # Packages and libraries are distinguished by R versions since the
      # R versions are sometimes hidden and we may want to use the same
      # VE-config.yml with different versions of R (e.g. 3.5.1 and 3.5.2)

      for ( loc in locs.lst ) dir.create( get(loc), recursive=TRUE, showWarnings=FALSE )

      # TODO 4.0: there won't be a make environment.
      # We're managing the building all from here, and will always check
      #   and update prerequisite steps (just checking for done-ness)
      # Convey key file locations to the 'make' environment
      # ve.runtime.config <- file.path(ve.logs,"dependencies.RData")
      # make.target <- file.path(
      # ve.installer, # ve.installer is already normalized
      # paste0(paste("ve-output",ve.branch,this.R,sep="-"),".make")
      # )
      # cat("make.target:",make.target,"\n")
      # make.target <- Sys.getenv(
      # "VE_MAKEVARS",
      # unset=make.target
      # )
      # make.variables <- c(
      # VE_R_VERSION      = this.R
      # ,VE_VERSION        = ve.version
      # ,VE_LOGS           = ve.logs
      # ,VE_BUILD          = ve.build.dir
      # ,VE_DEVLIB         = dev.lib
      # ,VE_BRANCH         = ve.branch
      # ,VE_RUNTIME_CONFIG = ve.runtime.config
      # ,VE_PLATFORM       = ve.platform
      # ,VE_INSTALLER      = ve.installer
      # ,VE_OUTPUT         = ve.output
      # ,VE_ZIPOUT         = ve.zipout
      # ,VE_LIB            = ve.lib
      # ,VE_REPOS          = ve.repository
      # ,VE_PKGS           = ve.pkgs
      # ,VE_RUNTIME        = ve.runtime
      # ,VE_SRC            = ve.src
      # ,VE_DOCS           = ve.docs
      # ,VE_DEPS           = ve.dependencies
      # ,VE_HOME           = ve.root
      # )

      # TODO 4.0: generate an .Renviron that will have all the items stored
      #   in it tha we would construct by hand if not present in environment.
      r.environ <- file.path(ve.root,".Renviron")
      r.ve.lib <-gsub(this.R,"%V",ve.lib)
      r.dev.lib <- gsub(this.R,"%V",dev.lib)
      r.libs.user <- c(
        paste0("R_LIBS_USER=",paste(r.ve.lib,r.dev.lib,sep=";")),
        paste0("VE_HOME=",normalizePath(ve.root,winslash="/"))
      )
      if ( file.exists(r.environ) ) {
        # Try to preserve user's changes to .Renviron
        renv.text <- readLines(r.environ)
        renv.text <- grep("R_LIBS_USER|VE_HOME",renv.text,invert=TRUE,value=TRUE) # remove lines we are replacing; keep the rest
        if ( length(renv.text)>0 ) r.libs.user <- c(r.libs.user,renv.text)
      }
      writeLines(r.libs.user,con=r.environ)
      rm( r.environ,r.libs.user,r.dev.lib,r.ve.lib )

#       writeLines( paste( names(make.variables), make.variables, sep="="),make.target)

      # The following are constructed in Locations above, and must be present
      # ve.runtime <- file.path(ve.output, "runtime")
      # ve.lib <- file.path(ve.output, "ve-lib",this.R)
      # ve.repository <- file.path(ve.output, "pkg-repository")
      # ve.dependencies
      # ve.runtime

      # ve.dependencies hosts the external R packages
      # ve.repository hosts the built VE packages
      # ve.publish.url hosts a drat repository that can be committed and pushed
      ve.deps.url <- paste("file:", ve.dependencies, sep="")
      ve.repo.url <- paste("file:", ve.repository, sep="")
      ve.publish.url <- paste("file:", ve.publish, sep="")

      # Load the Components
      # TODO 4.0: just scan the various roots looking for folders that contain DESCRIPTION
      # Read DESCRIPTION for the package name, and report the path as well
      # cat("Loading Components...\n")

      # catn <- function(...) { cat(...); cat("\n") }

      # # ve.components can be set as a location in VE-config.yml
      # if ( ! exists("ve.components") ) ve.components <- file.path( ve.root,"build","config","VE-components.yml" )
      # if ( ! file.exists(ve.components) ) {
        # cat("ve.components = ",ve.components,"\n")
        # stop("Cannot find VE-components.yml in VisionEval build folder",call.=FALSE)
      # }
      # component.file <- c( ve.root = ve.components )
      # includes <- list()
      # excludes <- list()
      # ##### WARNING - we make use of the fact that:
      # #   "ve.root" will always be at component.file[1] !!!!
      # if ( "Components" %in% names(ve.cfg) ) {
        # comps <- ve.cfg$Components
        # components.lst <- names(comps)
        # #   catn("Component list from VE-config.yml:")
        # #   print(components.lst)
        # for ( root in components.lst ) {
          # if ( ! exists(root) ) {
            # stop(paste("Undefined",root,"in Roots: section of",ve.config.file,sep=" "),call.=FALSE)
          # }
          # #     catn("Root:",root,"is",get(root))
          # #     print(names(comps[[root]]))
          # if ( "Root" %in% names(comps[[root]]) ) {
            # comps.root <- comps[[root]]$Root
          # } else {
            # comps.root <- root
          # }
          # component.file[root] <- file.path( get(comps.root),comps[[root]]$Config )
          # if ( "Include" %in% names(comps[[root]]) ) {
            # includes[[root]] <- comps[[root]]$Include
            # #       cat("Includes from",root,"\n")
            # #       print(includes[[root]])
          # } else {
            # includes[[root]] <- character(0)
          # }
          # if ( "Exclude" %in% names(comps[[root]]) ) {
            # excludes[[root]] <- comps[[root]]$Exclude
            # #       catn("Excludes from",root)
            # #       print(comps[[root]]$Exclude)
          # } else {
            # excludes[[root]] <- character(0)
          # }
        # }
      # }

      # # Process component.file like this:
      # #   1. Load components from file into temporary list
      # #   2. Add component from "Include" if not empty
      # #   3. Else skip component if it's in "Exclude"
      # #   4. Put each remaining element of temporary list into final
      # #      component list (by component name, so there is replacement)

      # # cat("Processing component.file\n")

      # build.comps <- list()
      # for ( root in names(component.file) ) {
        # #   catn("Processing components for",root,"from",component.file[root])
        # comps <- ve.cfg <- yaml::yaml.load_file(component.file[root])$Components
        # if ( is.null(comps) ) stop("Failed to find components in",component.file[root],call.=FALSE)
        # for ( cn in names(comps) ) {
          # comp <- comps[[cn]]
          # if ( ( length(excludes[[root]])==0 || ! cn %in% excludes[[root]] ) &&
            # ( length(includes[[root]])==0 || cn %in% includes[[root]] ) ) {
            # comp$Root <- get(root) # retrieves path from variable whose name is in 'root'
            # build.comps[[cn]] <- comp
          # }
        # }
      # }
      # # catn("Build roots:")
      # # print(names(build.comps))
      # # print(build.comps[[names(build.comps)[2]]])

      # # Parse the Components for Dependencies
      # # Do this in a specific order:
      # #   "Type: framework"
      # #   "Type: module"
      # #      Within Module by Test$Group
      # #      Within Group in order from build.comps
      # #   "Type: model"
      # #   "Type: test"
      # #   "Type: script"

      # cat("Parsing dependencies...\n")

      # TODO 4.0: much simpler lists of things to build
      # Really just packages. All docs can stay online.
      # Loop over defined packages and get their dependencies from DESCRIPTION
      # Figure out which are CRAN and which are BioConductor
      # miniCRAN wll visit a list of repositories to download...

      pkgs.db <- data.frame(Type="Type",Package="Package",Target="Target",Root="Root",Path="Path",Group=0,Test="Test")
      save.types <- c("framework","module","model","runtime","script","test","docs","book")
      # iterate over build.comps, creating dependencies
      for ( pkg in names(build.comps) ) {
        it <- build.comps[[pkg]]
        if ( it$Type %in% save.types ) {
          # These are the required elements: Type, Package, Root, and Path
          it.db <- data.frame(Type=it$Type,Package=pkg,Root=it$Root,Path=it$Path)
          if ( "Target" %in% names(it) ) {
            # used for now only in the 'docs' and 'book' types, indicating sub-folder of output 'docs'
            # in which to place elements found at it$Path. Default is 'docs' folder itself for 'docs'
            # type, and it$Package for 'book' type.
            it.db$Target <- it$Target
          } else {
            it.db$Target <- ""
          }
          if ( "Test" %in% names(it) ) {
            tst <- names(it[["Test"]])
            if ( "Group" %in% tst ) {
              it.db$Group <- it$Test$Group
            } else {
              it.db$Group <- 1
            }
            if ( "Script" %in% tst ) {
              scripts <- paste(it$Test$Script,sep=";")
              it.db$Test <- paste(it$Test$Script,collapse=";")
            } else {
              it.db$Test <- ""
            }
          } else {
            it.db$Group <- NA
            it.db$Test <- ""
          }
          pkgs.db <- rbind(pkgs.db,it.db)
          rm(it.db)
          if ( "CRAN" %in% names(it) ) {
            for ( dep in it$CRAN ) {
              dep.db <- data.frame(Type="CRAN",Package=dep,Root=NA,Path=NA,Target=NA,Group=NA,Test=NA)
              pkgs.db <- rbind(pkgs.db,dep.db)
            }
          }
          if ( "BioC" %in% names(it) ) {
            for ( dep in it$BioC ) {
              dep.db <- data.frame(Type="BioC",Package=dep,Root=NA,Path=NA,Target=NA,Group=NA,Test=NA)
              pkgs.db <- rbind(pkgs.db,dep.db)
            }
          }
          if ( "Github" %in% names(it) ) {
            for ( dep in it$Github ) {
              dep.db <- data.frame(Type="Github",Package=basename(dep),Root=NA,Path=dep,Target=NA,Group=NA,Test=NA)
              pkgs.db <- rbind(pkgs.db,dep.db)
            }
          }
          if ( "DevPkg" %in% names(it) ) { # These always come from the current CRAN location for R version
            for ( dep in it$DevPackages ) {
              dep.db <- data.frame(Type="DevPackages",Package=basename(dep),Root=NA,Path=dep,Target=NA,Group=NA,Test=NA)
              pkgs.db <- rbind(pkgs.db,dep.db)
            }
          }
        }
      }
      # print(pkgs.db)
      pkgs.db <- unique(pkgs.db[-1,])           # Remove dummy row
      row.names(pkgs.db) <- NULL                # Remove artificial row.names
      for ( d in names(pkgs.db))                # Convert factors to strings
      if ( is.factor(pkgs.db[,d]) )
      pkgs.db[,d] <- as.character(pkgs.db[,d])
      pkgs.db <- pkgs.db[order(pkgs.db$Type,pkgs.db$Group,pkgs.db$Package),] # Sort by Group (for modules)

      # Save pkgs.db into dependencies.RData
      # Also save row indices of the different types

      pkgs.CRAN      <- which(pkgs.db$Type=="CRAN")
      pkgs.BioC      <- which(pkgs.db$Type=="BioC")
      pkgs.Github    <- which(pkgs.db$Type=="Github")
      pkgs.DevPkg    <- which(pkgs.db$Type=="DevPkg")
      pkgs.framework <- which(pkgs.db$Type=="framework")
      pkgs.module    <- which(pkgs.db$Type=="module")
      pkgs.model     <- which(pkgs.db$Type=="model")
      pkgs.runtime   <- which(pkgs.db$Type=="runtime")
      pkgs.script    <- which(pkgs.db$Type=="script")
      pkgs.test      <- which(pkgs.db$Type=="test")
      pkgs.docs      <- which(pkgs.db$Type=="docs")
      pkgs.book      <- which(pkgs.db$Type=="book")

      # catn("Sorted by Group:")
      # print(pkgs.db[,c("Type","Package","Group")])

#       # Save out the basic setup that is used in later build scripts
# 
#       ve.all.dependencies <- file.path(ve.logs,"all-dependencies.RData")
# 
#       # Non-Standard Coding: Keep this list in an easy maintain format:
# 
#       # Commas precede the item so it can be moved, or deleted, or a new item
#       # added without having to edit more than one line.
# 
#       cat("Saving runtime configuration to:\n",ve.runtime.config,"\n",sep="")
#       ve.env.save <- c(ve.roots,locs.lst
#         , "this.R"
#         , "ve.build.dir"
#         , "dev.lib"
#         , "ve.roots"
#         , "ve.branches"
#         , "ve.output"
#         , "ve.logs"
#         , "ve.installer"
#         , "ve.zipout"
#         , "ve.version"
#         , "ve.platform"
#         , "ve.build.type"
#         , "ve.binary.build"
#         , "ve.all.dependencies"
#         , "CRAN.mirror"
#         , "BioC.mirror"
#         , "ve.deps.url"
#         , "ve.repo.url"
#         , "ve.publish.url"
#         , "pkgs.db"
#         , "pkgs.CRAN"
#         , "pkgs.BioC"
#         , "pkgs.Github"
#         , "pkgs.DevPkg"
#         , "pkgs.framework"
#         , "pkgs.module"
#         , "pkgs.model"
#         , "pkgs.runtime"
#         , "pkgs.script"
#         , "pkgs.test"
#         , "pkgs.docs"
#         , "pkgs.book"
#         , "localBranch"
#         , "checkBranchOnRoots"
#         , "checkVEEnvironment"
#         , "modulePath"
#         , "moduleExists"
#         , "newerThan"
#       )
#       #   print(ve.env.save[grep("function",sapply(ve.env.save,FUN=function(x){ paste(class(get(x)),collapse=",")}))])
#       suppressWarnings(save(file=ve.runtime.config,list=ve.env.save))
    }
  )
}
