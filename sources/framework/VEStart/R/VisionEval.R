# Install, update and launch VisionEval system

# Comprehensively change VEStart to VEStart

# VEStart makes sure that ve-lib is present in VE_HOME. Put it there with ve.build as well (but put
# the other build artifacts into VE_BUILD, defaulting to VE_HOME/built - or VE_HOME/build since the
# build scripts directory is going away). Then VE_RUNTIME defaults to VE_HOME/runtime but can be
# moved anywhere. VEStart will also make sure the runtime is set up with a "models" folder, a
# stub visioneval.cnf with global settings, and the startup scripts (which can also be updated
# in VE_HOME.)

# Can edit file locations (show .Renviron or defaults) then give the user an easy text-based dialog
# to choose different directories for VE_BUILD and VE_RUNTIME. VE_HOME will be the place we're
# running, either getwd() or the pre-set VE_HOME.

# If the user is going to change VE_HOME from VEStart, there needs to be a ve-lib at that location,
# or they need to do a setup (looking for various kinds of things that can be used to populate
# ve-lib - either a snapshot for this version of R, or a source or binary contriburl for the current
# version of R, from which the packages can be installed). The local repository can be either just
# the VE packages or the VE packages plus all CRAN/BioC dependencies. Some basic packages could go
# with the VE packages even in the most basic case - include yaml, BiocManager. Then if we're
# missing dependencies, we can just truck off to CRAN and BiocManager:repositories to find them.
# contriburl parameter for install.packages can be a vector of locations, just like repos, so we can
# just build those out.

# So perhaps the key step is to install VEStart and its dependencies (yaml and BiocManager, perhaps
# git2r) for the current version of R (very simple online packages repository), then browse/download
# various package files based on the current R version.
# https://github.com/VisionEval/VisionEval-Dev/releases/download/VE-3.1.2/VE-3.1-PackageSources-R4.4.1_2024-10-10.zip

# So the assets in the Github release can just be variously sized .zip files containing a small
# config saying what to expect there and where to go for the rest. That can be downloaded manually
# by the user and placed into VE_HOME. VEStart will look for the most recently downloaded .zip file,
# extract the manifest and contents into "install" subdirectory of VE_HOME. If no .zip file, hunt
# around in the current release (corresponding to VEStart version and ) on Github to find the
# assets. Let the user choose the specific release.

# Manifest needs to contain the type of install (win.binary, source, etc.) and the contriburl name.
# Source install requires the presence of RTools.
# How to bootstrap VEStart? Download the 
# Then when it starts, look in the "install" directory if any, otherwise let the user browse the
# Github release assets for this version of VEStart.


# If they're setting up ve.build, they can pick VE_HOME anywhere (again, it will default to
# getwd()) and that will be where the default VE_BUILD will go for intermediate build products
# and where ve-lib will be placed (directly in VE_HOME).

# The VEBuild process needs to start by selecting what directories to build (construct
# ve-build-config.yml).

# VE-Bootstrap.R is intended to run in the root of the Github, and we'll always get that (rather
# than the simpler use of VEStart in VE_RUNTIME): it will load ve.build() and another function
# ve.run() that basically just does the .Rprofile work that VEStart sets up.

# Set up ve-build-config.yml more explicitly if it doesn't exist. So when VEBuild is loaded on
# top of an end-user installation, it checks the ve-build-config.yml if it exists and prompts
# the user to set up the different directories: show the ones can be found, and then prompt
# the user to add another directory (dialog pops up as directory finder, but the main part is
# just a text list of directories to build: enter the number to show the packages that would
# be built, with options to remove the directory from the config or just return; enter a "+" to add
# another directory. Once picked, the packages are shown (or "no packages" in that directory), and
# the user has the option to remove the directory from the config or just go back.

# So don't provide a ve-build-config.yml by default. Instead, if it's not present, go straight
# into the package configuration dialog when the user chooses ve.build(). They can always choose
# different packages by running ve.package.directories() to see what's set up.

# That gives us the option to add new packages into an existing end-user VE_HOME (it will create
# the "built/build" directory.

# So the pre-creation of a default ve-build-config.yml should be factored out into that setup
# function. Bootstrap will just work straight off the Github using what is already in the
# configuration file. Later, the one real package function in VEBuild is to hunt up another
# directory (either interactively or as a function parameter or as getwd()) and add it to the
# build configuration.

#CREATE ENVIRONMENT
# This environment is later copied into VEModel to keep track of ve.runtime, ve.home, etc.
ve.env <- new.env()

#' Called from VEModel to link to VEStart environment
#' Items set in the VEStart environment such as ve.home are propagated to VEModel after everything
#' is installed. This function is used by VEModel.
#' @return Returns the VEStart environment
#' @export
getRuntimeEnvironment <- function() ve.env

#START VISIONEVAL
#================
#' \code{startVisionEval()} is called from .Rprofile to start VE (or it may be called manually to restart VisionEval)
#'
#' The ve.home (VE_HOME) location is loaded from .Renviron or the system or user environment variables, and it defaults
#' to the directory from which R is starting. Ve.home is checked for usability and a directory browse dialog is launched
#' if the directory is not set up or is not "empty enough". Just saying "OK" to that directory will force it to be used
#' anyway, but it is good practice to find (or create) an empty location.
#'
#' The ve.runtime location can be provided as an argument when \code{startVisionEval()} is called, or it can be set in
#' \code{.Renviron} or in the system or user environment variables. If none of those are provided, it will default to
#' the same location as VE_HOME (where startup files will then be created and a "models" subdirectory created when
#' VEModel starts the VisionEval API).
#'
#' Other parameters (repos, ve.lib.name, update and overwrite) control how VisionEval packages are sought, installed
#' and updated. See the parameter descriptions for more information. Usually, these will not need to be changed
#'
#' @param ve.home Location for VE_HOME containing ve-lib and other system files (default getwd())
#' @param ve.runtime Location for VE_RUNTIME where VisionEval will run ("models" directory)
#' @param repos Character vector of additional non-standard repository URLs from which to initialize VE packages
#' @param update If TRUE, update ve-lib from repository locations (default); otherwise use existing ve-lib as-is
#' @param overwrite If TRUE, force rewrite of startup files in VE_RUNTIME, otherwise continue if they exist
#' @param ve.lib.name Character string with name of ve-lib within VE_HOME (default "ve-lib")
#' @param ve.pkg.name Character vector with names of optional local package repositories that may exist in VE_HOME
#' @param ve.repos.list.name Character string with name of file inwhich to seek additional package repository URLs (CRAN-like)
#' @param ve.setup.lib Character string with name of R library for simple bootstrap setup (option to
#'   delete after installation is complete)
#' @return location of VE_RUNTIME, invisibly
#' @import utils tcltk
#' @export
startVisionEval <- function(
  ve.home=NULL,ve.runtime=NULL,
  repos=NULL,update=TRUE,overwrite=FALSE,
  ve.lib.name="ve-lib",
  ve.pkg.name=c("ve-pkg","ve-dependencies"),
  ve.repos.list.name="ve-repos.cnf",
  ve.setup.lib="ve-setup"
) {
  ve.setup.lib.path <- file.path(getwd(),ve.setup.lib)
  ve.pkg.repo <- "pkg-ve-repo" # from VEBuild - local set of packages

  # Identify location for VE_HOME (contains ve-lib, and optionally ve-pkg for local repository installation)
  if ( missing(ve.home) || is.null(ve.home) ) {
    ve.build.dir <- Sys.getenv("VE_BUILD",Sys.getenv("VE_HOME",getwd())) # Just in case we're loading from a build environment
    # VE_BUILD is the target location for VEBuild, the "home" that is constructed by running ve.build
    # TODO: how to transition seamlessly from VEStart (end user installation) to VEBuild (source code installation)?
    # VE_HOME will be set by VEStart. When we later load VEBuild, we need to know where the source code is, and
    # make VE_HOME into VE_BUILD. Need some dialoguing in VEBuild if we have VE_HOME but no VE_BUILD, and if
    # ve-build-config.yml does not exist.
  }
  if ( missing(ve.runtime) ) ve.runtime <- NULL

  # Check if VE_HOME is already set up (contains ve-lib)
  existing.libs <- character(0) # Check below for existence of VE_LIB or VE_SETUP_LIB
  repeat {
    ve.home.contents <- dir(ve.home)
    valid.ve.home <- (
      ( empty.ve.home <- length(ve.home.contents) == 0 ) ||
      any( existing.libs <- c(VE_LIB=ve.lib.name,VE_SETUP_LIB=ve.setup.lib) %in% ve.home.contents )
    )

    # If not set up
    if ( valid.ve.home && ! empty.ve.home ) {
      valid.ve.home <- askYesNo(paste("Install VisionEval in",ve.home,"?"))
      if ( is.na(valid.ve.home) ) {
        message("VisionEval installation was cancelled.")
        message("Please select a suitable VisionEval home directory")
        break # with valid.ve.home set to NA
      }
    }
    if ( ! isTRUE(valid.ve.home) ) {
      # NOTE: ve.home will be offered in the following directory browse dialogs and if the user just re-selects that
      # directory, it will be used anyway, creating ve-lib at that location.
      caption <- "Select directory for VisionEval code installation (VE_HOME)"
      ve.home <- if (exists('utils::choose.dir')) { # Won't exist on non-Windows platforms
        utils::choose.dir(caption = caption)
      } else {
        tcltk::tk_choose.dir(getwd(),caption = caption)
      }
      if ( ! is.na(ve.home) && dir.exists(ve.home) ) { # NA if dialog was cancelled
        message("Setting up VE_HOME as ",ve.home)
        break
      } else {
        message("No valid location selected for VE_HOME:")
        valid.ve.home <- NA
        break # with valid.ve.home set to NA
      }
    } else break
  }
  if ( is.na(valid.ve.home) ) stop("Installation unsuccesful; Re-run startVisionEval()")

  # TODO: Associate VE_RUNTIME setup with ve.seti[

  # Set up VE_RUNTIME
  if ( is.null(ve.runtime) ) {
    ve.runtime <- Sys.getenv("VE_RUNTIME",as.character(NA))
  }
  if ( is.na(ve.runtime) ) {
    home.as.runtime <- askYesNo(paste("Install VisionEval 'models' folder in",ve.home,"?"))
    if ( is.na(home.as.runtime) ) {
      message("Please select a suitable VisionEval runtime directory for models.")
      stop("Installation cancelled.")
    }
  } else home.as.runtime <- ( ve.runtime == ve.home )

  if ( ! home.as.runtime  ) {
    caption <- "Select directory for VisionEval 'models' folder (VE_RUNTIME)"
    ve.runtime <- if (exists('utils::choose.dir')) { # Won't exist on non-Windows platforms
      utils::choose.dir(default=ve.home,caption = caption)
    } else {
      tcltk::tk_choose.dir(default=ve.home,caption = caption)
    }
    if ( is.na(ve.runtime) || ! dir.exists(ve.runtime) ) {
      message("Please select a suitable VisionEval directory for 'models'")
      stop("Installation cancelled.")
    }
  } else ve.runtime <- ve.home

  message("Setting up VE_RUNTIME as ",ve.runtime)

  # Put important parameters into VEStart:::ve.env for use in later functions, and relayed to VEModel
  ve.env$ve.runtime <- ve.runtime
  ve.env$ve.home <- ve.home
  ve.env$ve.repos.list.name = ve.repos.list.name # See VEStart::getRepositories function 
  Sys.setenv(VE_HOME=ve.home,VE_RUNTIME=ve.runtime) # Somewhat redundantly, also save to operating system environment
  # NOTE: ve.setup below will also save VE_HOME and VE_RUNTIME into the .Renviron startup file

  # Clear VEModel if already present
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  base::unloadNamespace("VEModel")

  # Clear visioneval so we can update it too
  if ( "package:visioneval" %in% search() ) detach("package:visioneval")
  unloadNamespace("visioneval")

  # Set up ve-lib (R library location for installed VE packages and dependencies)
  # The same library location will hold sub-directories for the major/minor R version that is
  # running this installation.
  ve.env$ve.pkg.repo <- file.path(ve.home,ve.pkg.repo) # in case of local package installation
  ve.env$this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
  ve.env$ve.lib <- file.path(ve.home,ve.lib.name,tools::file_path_sans_ext(ve.env$this.R))
  if ( ! dir.exists(ve.env$ve.lib) ) dir.create(ve.env$ve.lib,recursive=TRUE)
  .libPaths(ve.env$ve.lib) # will add ve-lib if it's not present; ve.setup.lib may remain there harmlessly

  # Set the installation type based on the OS
  # Not using the default of "both" simplifies online install so we can just supply the relevant contriburl rather than
  # a repository with both source and binary branches.
  ve.env$installType <- if ( .Platform$OS.type == "windows" ) {
    "binary"
  } else {
    # Used for Mac or Linux installations (or any OS for which R and RTools are available)
    # Probably we could compile Mac binaries, but we're currently not building and pushing those
    # We don't currently have enough Mac users to justify doing that.
    "source"
  }

  # Initialize the VE installed package library
  # Installs missing packages or updates them from the provided CRAN-like repositories
  # Note that VEStart won't update until we re-run the standard .Rprofile that ve.setup installs
  ve.init(lib.loc=ve.env$ve.lib,repos=repos,update=update)

  # check and construct startup files in VE_RUNTIME and (optionally) VE_HOME if the latter is different from VE_RUNTIME
  # Configure ve.runtime (.Renviron etc.)
  ve.setup(ve.home,ve.runtime,overwrite=overwrite)

  # Offer to clean up VE-Setup.R and ve.setup.lib
  # This should not happen until after we've bootstrapped
  # Move it to .Rprofile?
  if ( ! ve.setup.lib.path %in% .libPaths() ) {
    seek.setup <- ve.setup.lib.path
    if ( 0 < length( setup.files <- seek.setup[file.exists(seek.setup)] ) ) {
      message("Unnecessary bootstrap setup files are present:")
      print(as.character(setup.files))
      remove.setup <- readline("Remove these setup files? (Y/n)")
      if ( grepl("(^[Yy])|(^$)",remove.setup) ) {
        unlink( setup.files, recursive=TRUE)
      }
    }
  }

  # Make installed library path active (set VE_HOME, with ve.env$ve.lib in .Renviron
  .libPaths(ve.env$ve.lib) # Update .libPaths

  # Return runtime location, invisibly
  invisible(ve.env$ve.runtime)
}

# Install bare bones VisionEval (these are checked to see if installation was complete)
VE.framework <- c("VEStart","VEModel","visioneval")

# Check if framework is installed
checkVE <- function(lib.loc=NULL) {
  return(all( VE.framework %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

# Figure out which repositories to use to install VisionEval
#' Generate a list of CRAN-like repositories to search for VE packages
#' The standard VE package repository (either online or offline) will have the basic model and module packages
#' to support base VERSPM, VE-State and VERPAT. If non-standard repositories are provided, the packages there
#' must have names that start with "VE", and they must be available for the current R version (or as source).
#' @param repos A character vector of additional CRAN-like repository URLs for VE packages
#' @param use.default If TRUE (default), Look for "built-in" VE repositories (including
#'   in pkg-ve-repo for offline installation)
#' @param offline If TRUE, only look for the local VE_HOME/ve-pkg-repo
#' @param ve.home path to VE_HOME; if not provided or NULL, look in VEStart::getRuntimeEnvironment()
#' @return character vector of CRAN-like repositories from which to install or update VE packages
#' @export
getRepositories <- function(repos=NULL, use.default=TRUE, offline=TRUE, ve.home=NULL) {

  if ( missing(ve.home) ) ve.home <- ve.env$ve.home

  # Set up default repositories (local or online)
  browse()
  search.repos <- character(0)
  if ( isTRUE(use.default) ) {
    if ( dir.exists( ve.env$ve.pkg.repo ) ) {
      search.repos <- c(search.repos,paste0("file:",ve.env$ve.pkg.repo))
    }
  }   

  # Add manually defined locations in VE_HOME/ve.repos.list.name (ahead of defaults)
  # Should be a text file with one CRAN-like URL per line
  userfile.name <- Sys.getenv("VE_REPOS",ve.env$ve.repos.list.name) # Override for development
  userfile.repos.file <- file.path(ve.home,userfile.name)
  if ( file.exists(userfile.repos.file) ) {
    userfile.repos <- c(grep("(^\\s*#\\s*)|(^\\s*$)",readLines(userfile.repos.file),invert=TRUE,value=TRUE),search.repos)
    # the contents of ve-repos.cnf is one url per line suitable for use with install.packages or update.packages
    # lines can be commented out if their first non-blank character is a # (hash or pound) symbol
    if ( length(userfile.repos) > 0 ) {
      search.repos <- c( userfile.repos, search.repos )
    }
  } else message("Could not find local repository list: ",userfile.repos.file)

  # Add any repos provided as arguments to this function (ahead of all the others)
  # Repos is a character vector of fully-formed CRAN-like repository URLs from which to install VE package
  if ( is.character(repos) ) {
    search.repos <- c( repos, search.repos )
  }
  # Backstop removal of empty lines
  search.repos <- grep("^\\s*$",search.repos,invert=TRUE,value=TRUE) # Keep only non- blank lines
  if ( length(search.repos) == 0 ) {
    message("No VisionEval repositories available.")
    stop("Minimally need either local VE_HOME/ve-pkg-repo or online https://packages.visioneval.org")
  }

  # Clean up the list - remove duplicates and empty lines
  search.repos <- grep ("^\\s*$",search.repos,invert=TRUE,value=TRUE) # no empty lines
  search.repos <- search.repos[! duplicated(search.repos)]            # only first instance of duplicated repositories

  return (search.repos)
}

# Additional helpers for managing installed VE packages

# Report package names
packageNames <- function(available.matrix) available.matrix[,"Package"] # also works to do row.names(available.matrix)

# lib.loc is the directory that holds the VE files (see ve.lib.name parameter for startVisionEval())
# This function will look for installed.packages called "VE..."
# It returns an avaialble.package matrix
installed.packages.VE <- function(lib.loc) {
  installed <- installed.packages(lib.loc=lib.loc)
  if ( ! is.null(installed) ) {
    inst.names <- grepl("^VE",packageNames(installed))
    installed <- installed[grepl("^VE",packageNames(installed)),]
  }
  installed
}

# Returns an available.packages matrix that filters the packages available at
# repos.list looking for packages that are named "VE..."
# Eventually, could also or instead look for VEModels or VEModules entries in each Package DESCRIPTION
available.packages.VE <- function(repos.list) {
  available.list <- NULL
  for ( repo in repos.list ) {
    avail <- suppressWarnings(
      available.packages(
        repos=repo,
        type=ve.env$installType,
        filters = list(
          add = TRUE,
          function (db) {
            db[grepl("^VE",db[,"Package"]),]
          }
        )
      )
    )
    if ( ! is.null(avail) && nrow(avail) > 0 ) {
      if ( is.null(available.list) ) available.list <- avail else rbind(available.list,avail)
    }
  }
  return(available.list[!duplicated(row.names(available.list)),])
}

# Get the available.packages matrix of out of date packages compared to the repositories in repos.list
old.packages.VE <- function(lib.loc,repos.list) {
  old.list <- NULL
  for ( repo in repos.list ) {
    old <- suppressWarnings( old.packages(
      lib.loc=lib.loc,
      repos=repo,
      type=ve.env$installType
    ) )
    if ( ! is.null(old) && nrow(old) > 0 ) {
      if ( is.null(old.list) ) old.list <- old else rbind(old.list,old)
    }
  }
  return(old.list[!duplicated(row.names(old.list)),])
}

# this function finds any packages in available (package matrix) that are not present in installed (package matrix)
# installed is a matrix of installed package information, available is an available.packages matrix of VE packages
# This function returns a matrix of packages to install
# Do NOT run it on CRAN package lists or you'll get a list of thousands, almost all of which you don't want
uninstalled.packages.VE <- function(installed,available,getNames=FALSE) {
  if ( is.null(installed) || nrow(installed)==0 ) {
    return(available)
  }
  package.names <- packageNames(available)
  uninstalled <- ! ( packageNames(available) %in% packageNames(installed) )
  uninstalled.packages <- if ( any(uninstalled) ) available[uninstalled,,drop=FALSE] else character(0)
  return(uninstalled.packages)
}

#INITIALIZE VISIONEVAL
#=====================
#' Initialize a VisionEval installation by installing or updating VisionEval packages
#'
#' \code{ve.init} will install and update VisionEval R packages and create a runnable VisionEval installation. The
#' standard procedure for installing VisionEval 4.0 is to install the \code{VEStart} package and then run
#' \code{ve.init()}. The \code{startVisionEval()} function simply calls \code{ve.init()}
#'
#' This function installs or (optionally) updates VisionEval packages from a list containing online
#' repository URLs, local folder paths, or paths to .zip files (see \code{ve.install}). Use the VEBuild package to
#' packages from Github source code. The ve-lib parameter says where to put the installed packages (or look for
#' packages to update.
#' 
#' IMPORTANT: lib.loc must be created in the file system before calling this function
#'
#' @param lib.loc character vector, path for ve-lib / first path in R_LIBS_USER (default: \code{libPaths()[1]})
#' @param update logical: Install or update VE packages from repos (packages whose names start in VE) (default: TRUE),
#' @param repos a character vector of repository URLs for non-standard repositories in which to seek VE packages
#' @param namedOnly ignore any of the default or configuration file repositories
#' @return invisibly, a character string containing the normalized path of the selected VisionEval runtime location
#' @export

ve.init <- function(
  lib.loc=.libPaths()[1],  # location of ve-lib / first path in R_LIBS_USER; saved as VE_LIB in .Renviron
  update=TRUE,             # Install or update VE packages from repos (packages whose names start in VE)
  repos=NULL,              # eventually a standard online location e.g. https://visioneval.org/packages
  namedOnly=FALSE          # only look at repositories listed in repos (rather than the defaults or ve-repos.cnf)
) {

  # NOTE: local ve-pkg-repo should have a full repository (not just contriburl) but may have only one source or binary
  #   branch with a single R version (so we can zip up a snapshot)

  # Find VE Repositories using ve.env$ve.home/ve-repos.cnf and built-in standard search locations
  if ( ! namedOnly || ( as.character(repos) && length(repos)>0 ) ) {
    repos.list <- getRepositories(repos)
  } else {
    repos.list <- repos
  }

  # Find installed and available VE packages
  VE.installed.packages <- installed.packages.VE(lib.loc)
  VE.available.packages <- available.packages.VE(repos.list) # VE.available.packages is an available packages matrix

  # Identify and install any VE packages that are available but not installed by comparing the lists
  VE.uninstalled.packages <- uninstalled.packages.VE(VE.installed.packages,VE.available.packages)
  if ( length(VE.uninstalled.packages)>0 && NROW(VE.uninstalled.packages) > 0 ) {
    # We know we'll need yaml as a dependency, so use that to see if existing repos.list has dependencies available
    # If not, add a CRAN mirror for dependencies
    # Since we're still depending on Bioconductor HDF5 implementation, VEBuild is currently putting those dependencies into the
    #  default VE package repository since Bioconductor frowns on direct implementations.
    message("Installing uninstalled packages:")
    print(packageNames(VE.uninstalled.packages))
    install.repos <- if ( ! "yaml" %in% packageNames( suppressWarnings(
        available.packages(type=ve.env$installType,repos=repos.list)
      ) ) ) {
      message("Adding CRAN repository for dependencies as https://cloud.r-project.org")
      c( repos.list ,"https://cloud.r-project.org")
    } else repos.list # if we find yaml, we'll assume all the dependencies have been installed
    print(install.repos)

    # Do the actual installation
    suppressWarnings(
      install.packages(
        packageNames(VE.uninstalled.packages), # install by name
        lib=lib.loc,
        repos=install.repos,
        dependencies=c("Depends", "Imports", "LinkingTo"), # Won't load "Suggests"
        type=ve.env$installType,
        INSTALL_opts="--no-test-load"
      )
    )
    message("Done installing new packages")
  } else message("No VE packages to install.\n")

  # Update any existing packages with new versions
  if ( isTRUE(update) ) {
    # always check online for dependency updates on one of the CRAN mirrors
    message("Checking for VisionEval and dependency updates")
    update.repos <- unique(c(repos.list,"https://cloud.r-project.org"))
    need.update <- old.packages.VE(lib.loc,repos.list=update.repos) # uses installType
    if ( !is.null(need.update) ) {
      message("Updating:")
      print(packageNames(need.update))
      # NOTE: the need.update structure is expected to include the repository location for the newer packages
      suppressWarnings(
        update.packages(need.update,repos=update.repos,type=ve.env$installType,ask=FALSE)
      )
    } else {
      message("VisionEval installation is up to date")
    }
  }
  return(ve.env$ve.runtime)
}

# The next section supports ve.setup which creates the standard runtime startup files,
# notably VisionEval.Rproj and launch.bat
startup.files <- c(
  ".Renviron",
  ".Rprofile",
  "launch.bat",
  "visioneval.cnf.sample",
  "VisionEval.Rproj",
  "r.version"
)

# Look to see if we have good setup files for the current R version
checkSetup <- function(ve.home,ve.runtime,overwrite=FALSE) {

  runtime.files <- file.path(ve.runtime,startup.files)
  names(runtime.files) <- startup.files
  home.files    <- file.path(ve.home,startup.files)

  # Check that R version identified in VE_RUNTIME is the same as the one that is running
  # Won't change anything in VE_RUNTIME unless isTRUE(overwrite)
  good.r.version <- FALSE
  if ( ! "this.R" %in% ls(ve.env) ) ve.env$this.R <- paste(R.version[c("major","minor")],collapse=".")
  r.version = runtime.files["r.version"]
  if ( file.exists(r.version) && ! overwrite ) {
    # As written, this allows multiple "variable:value" pairs in r.version
    # In practice, we're only looking for "that.R"
    ve.vars <- data.frame(
      scan(file=r.version,
        sep=":",
        what=list( var=character(), var=character() ),
        quiet=TRUE
      ), stringsAsFactors=FALSE
    )
    for ( i in 1:nrow(ve.vars) ) {
      # Expect ve.vars[i,1] == "that.R" , i.e. a literal variable name
      # Expect ve.vars[i,2] == R version like what comes from system R.version
      # There could be additional variables, but we're not examining them at this time
      assign(ve.vars[i,1],ve.vars[i,2],envir=ve.env)
    }
    if ( exists("that.R",envir=ve.env) ) {
      if ( ve.env$this.R != ve.env$that.R && ! overwrite ) {
        message("Wrong R version in ",ve.runtime)
        message("Re-run startVisionEval() with 'overwrite=T' to change to R ",ve.env$this.R)
        return(invisible(ve.runtime))
      } else good.r.version <- TRUE
    }
  } else good.r.version <- TRUE # it doesn't exist or we're overwriting it, so we will carry on with this.R

  # Report on R version and startup file existence
  return(
    list(
      RVersion        = good.r.version,
      runtimeComplete = all( file.exists(runtime.files) ),
      homeComplete    = all( file.exists(home.files) )
    )
  )
}

# ve.setup
#' Set up runtime files.
#' 
#' Create or update necessary runtime files, supplying R version-specific parameters
#'   in R_LIBS_USER and launch.bat.
#'
#' @param ve.home the VE_HOME directory where ve-lib is located
#' @param ve.runtime the VE_RUNTIME directory where the "models" directory will be placed
#' @param setupHome a logical indicating whether startup files should also be placed in VE_HOME (default FALSE)
#' @param overwrite a logical indicating whether to overwrite existing startup files (default FALSE)
#' @return the ve.runtime directory, invisibly
#' @export
ve.setup <- function(ve.home,ve.runtime,setupHome=FALSE,overwrite=FALSE) {

  # These should already have been set up by ve.init
  if ( missing(ve.runtime) ) ve.runtime <- ve.env$ve.runtime
  if ( missing(ve.home) )    ve.home    <- ve.env$ve.home

  # Look at what startup files are already present
  setup.status <- checkSetup(ve.home,ve.runtime,overwrite=overwrite)

  # If wrong R version, provide message
  if ( ! isTRUE(setup.status$RVersion) && isFALSE(overwrite) ) {
    stop("Retry with overwrite=TRUE to change R version, or pick a different runtime directory.")
  }

  # Check if startup files exist or we can overwrite them
  write.runtime <- isFALSE(setup.status$runtimeComplete) || overwrite
  # Allowing any "truthy" value for setupHome
  write.home <- isTRUE(setupHome) && ve.home != ve.runtime && ( isFALSE(setup.status$homeComplete) || overwrite )

  # Build startup files as requested or just continue if all are present
  setup.locations <- c(
    as.character( if ( write.runtime ) ve.runtime else NULL ),
    as.character( if ( write.home ) ve.home else NULL )
  )
  if ( length(setup.locations) == 0 ) return(invisible(ve.runtime)) # Not an error - just means setup files are already up to date

  message("Setup locations:")
  print(setup.locations)
  for ( location in setup.locations ) {
    message("Adding startup files to ",location)

    # Create R.version
    this.R <- paste(R.version[c("major","minor")],collapse=".")
    cat("that.R:",this.R,"\n",sep="",file=file.path(location,"r.version"))

    # Create .Renviron (VEBuild will add VE_BUILD to the list of defined locations, defaulting to VE_HOME)
    renv.file      <- file.path(location,".Renviron")
    renv.txt       <- c(
      paste0("R_LIBS_USER=",paste(collapse=";",.libPaths()[-length(.libPaths())])), # ignore base library
      paste0("VE_HOME=",normalizePath(ve.home,winslash="/",mustWork=TRUE)),
      paste0("VE_RUNTIME=",normalizePath(ve.runtime,winslash="/",mustWork=TRUE))
    )
    if ( file.exists(renv.file) ) file.copy(renv.file,file.path(location,"Previous.Renviron"))
    writeLines(renv.txt,renv.file)

    # Write launch.bat, providing default R_HOME
    launch.bat.template <- system.file("startup/launch.bat.template",package="VEStart",mustWork=TRUE)
    launch.bat <- file.path(location,"launch.bat")
    launch.txt <- readLines(launch.bat.template)
    launch.txt <- gsub(pattern = "=R_HOME_DEFAULT", replacement = paste0("=",R.home()), x = launch.txt)
    writeLines(launch.txt, con = launch.bat)

    # Directly copy over .Rprofile and VisionEval.Rproj (no template modifications needed)
    file.copy(system.file("startup/Rprofile.default.R",package="VEStart",mustWork=TRUE),file.path(location,".Rprofile"),overwrite=TRUE)
    file.copy(system.file("startup/VisionEval.Rproj",package="VEStart",mustWork=TRUE),location,overwrite=TRUE)
    file.copy(system.file("startup/visioneval.cnf.sample",package="VEStart",mustWork=TRUE),location,overwrite=TRUE)
    if ( file.exists( has.Rdata <- file.path(location,".Rdata")) ) file.copy(has.Rdata,file.path(location,"previous.Rdata"))
    save(list=character(0),file=file.path(location,".Rdata")) # double click .Rdata to run fully-installed RGUI
  }
    
  invisible(ve.runtime)
}

# FOR REFERENCE: startup functions from VE 3
# Only the walkthrough function is still relevant, and that should be made part of VEModel.
#
# Load tools (helper functions) from their subdirectory in ve.load.dir
# VE 4: This will be a directory in VE_HOME, populated during setup from system
#   files saved inside VE_BASE
# This should be deprecated, as we'll just be packaging these in the future
# In any case, we probably want to move it to VEModel since a design goal for VEStart is not to have
#   any non-system dependencies except VEModel (which is installed/handled internally and not
#   visibly made into an import in the DESCRIPTION file.
#
# load.helpers <- function() {
#   requireNamespace("import",quietly=TRUE)   # to load the tools
#   ve.tools <- file.path(ve.load.dir,"tools")
#   tool.files <- file.path(ve.tools,dir(ve.tools,pattern="\\.R$"),fsep="/")
#   if ( length(tool.files)>0 ) {
#     tools <- character(0)
#     for ( tf in tool.files ) {
#       # Add error checking for tool.contents not present
#       message("Loading tool file: ",tf)
#       try(
#         silent=TRUE,
#         eval(parse(text=paste0("import::here(tool.contents,.from='",tf,"')")))
#       )
#       if ( ! exists("tool.contents") ) next
#       eval(parse(text=paste0("import::into(.into='ve.env',",paste(tool.contents,collapse=","),",.from='",tf,"')")))
#       rm(tool.contents)
#     }
#     rm(tf,tools)
#   }
#   rm(tool.files,ve.tools)
# }
# 
# # create the loadTest function (makes package test functions available)
# # these are in the tools folder - need to clean all that up: no one's using it
# # A better test architecture, coding standard and practice remains to be done
# # Note that this creates an empty environment if no Package is specificed (probably should do nothing)
# loadTest <- function(Package=NULL,files=NULL,clear=FALSE) {
#   test.root <- file.path(ve.load.dir,"tools","tests")
#   if ( !is.character(Package) ) {
#     tests <- dir(test.root,pattern="\\.R$",recursive=TRUE)
#     if ( length(tests) == 0 ) {
#       tests <- "No package tests available"
#     }
#     return(tests)
#   }
# 
#   # Environment to receive test functions/objects
#   test.env <- if ( ! "ve.tests" %in% search() ) {
#     attach(NULL,name="ve.tests")
#   } else {
#     as.environment("ve.tests")
#   }
# 
#   if ( clear ) {
#     to.clear <- ls(test.env,all.names=TRUE)
#     if ( length(to.clear)>0 ) rm(list=to.clear,envir=test.env)
#   }
# 
#   for ( pkg in Package ) {
#     test.dir <- file.path(test.root,Package)
#     if ( dir.exists(test.dir) ) {
#       if ( !is.character(files) ) {
#         tests <- dir(test.dir,pattern="\\.R$",full.names=TRUE)
#       } else tests <- files
#       for ( test in tests ) {
#         message("Loading tests from ",test," for ",Package)
#         sys.source(test,envir=test.env)
#       }
#     } else {
#       message("No tests available for Package ",Package)
#     }
#   }
#   return( objects(test.env) )
# }
# 
# # Function to set up the walkthrough and its runtime
# # This should be set up in VEModel rather than here, since that's what it's walking through
# loadWalkthrough <- function() {
#   # Locate walkthrough directory (sub-directory of ve.runtime)
#   # If not present, load it from system.file
#   env.loc <- if ( ! "ve.env" %in% search() ) attach(NULL,name="ve.env") else as.environment("ve.env")
#   if ( ! dir.exists("walkthrough") ) { # in case we're not in a configured ve.runtime
#     if ( dir.exists(env.loc$ve.runtime) && getwd() != ve.env$ve.runtime ) {
#       setwd(ve.env$ve.runtime)
#     } 
#     if ( ! dir.exists("walkthrough") ) {
#       if ( dir.exists(load.walkthrough <- file.path(env.loc$ve.home,"walkthrough") ) ) {
#         file.copy(load.walkthrough,env.loc$ve.runtime,recursive=TRUE)
#       }
#     }
#     if ( ! dir.exists("walkthrough") ) {
#       stop("Walkthrough is not available in ",env.loc$ve.runtime," or ",ve.env$ve.home)
#     }
#   }
#   setwd("walkthrough") # Go there
# 
#   # Load the setup to create the walkthrough runtime if one is not already present
#   # Will stop in normalizePath if 00-setup.R is not present in getwd()
#   message("Loading walkthrough from ",normalizePath("00-setup.R",winslash="/",mustWork=TRUE))
#   source("00-setup.R") # will stop if we cannot create or change to walkthrough runtime directory
#   walkthroughScripts <- grep("00-setup.R",invert=TRUE,value=TRUE,dir("..",pattern="^[01].*\\.R$",full.names=TRUE))
#   message("Open these script files in order and try out the commands:")
#   print(walkthroughScripts)
# }
# 
