# VisionEval installation script
# Author: Jeremy Raw

# Hunt up .zip files with standard names:
# library_Rx.y (extract contents into install/ve-lib/x.y)
# winbinary_Rx.y (extract contents into install/contrib/x.y and use winbinary oncontriburl on install.packages
# source (extract contents into install/contrib and use source contriburl)

# store an entire repository tree branch starting at "install-temp"
# add that ahead of the online repositories (perhaps from the manifest)
# so we'll drop the manifest at the root of "install-temp"
# Manifest to include:
# - pkgType (with "library" as a non-standard)
# - Manifest (all the stuff generated when a package is built, notably branch, repo, commit ID)
#   * Individual packages have their own Manifests.

# Try not to zip multilayer folders - just the bottom-most directories
# Put the necessary contriburl structure into the manifest (including R version)

# When installing, add online repositories (CRAN, BioC) to search for missing dependencies
# Git manifest is dumped as the installer is built (snapshot of repository situation)

# To hunt up installers, look locally for any already downloaded using name patterns
#   VEInstall-<Type>-Rx.y.zip
# Search for same types on Github for download (see download.R)
# ve-install-config.cnf can list additional VEGithubRepositories:
#   user/repository format (e.g. visioneval/visioneval-dev or jrawbits/visioneval-jr
# Will look for latest release on those alternate repositories 
# Option also to Build - that will clone the repository into VE_HOME and source VE-Bootstrap.R
#   (initially only work for public repositories)
#   (cloning will only work into an empty directory)
#   (cloning is painful due to the historical crud - only allow it for the new rooted repos).

# Options:
#   0. Locally available .zip installer in VE_HOME with conforming name pattern
#   1. Any standard available installer at the Github
#      Standard name pattern filtered by R version (see download.R)
#   2. Alternative Github from ve-install.cnf
#      Just a simple config (readable via "desc" package)
#   3. Alternative to Clone and build from VisionEval-dev Github
#      Use the same repositories that are checked for released assets
#      Use the gert package for Github (easy!)
# Download (if a release) or Clone (for development)
# If Download:
#   Unzip the installer file into "install" directory of VE_HOME
#   Look for type of install in installation manifest
#      (pre-installed, win.binary install, source install)
#   Pre-install
#     Copy to ve-lib (this is the existing approach - big download)
#     win.binary install - install.packages from contriburl
#       If dependencies are not present, load them from online
#     source install - install packages from contriburl
#       Probably requires RTools, especially if getting all
#       dependencies; get dependencies online if not present in
#       installer
#     In every case, do an update from the installer if ve-lib has
#       old packages relative to what was downloaded.
#   Load VEStart
#     Then run startVisionEval()
#     Will prompt user to set startup location for models
#     (VE_RUNTIME)
#     Create startup files in VE_HOME and VE_RUNTIME
#     Then change to that directory and load VEModel
# If Clone
#    Go to selected Github and do gert::git_clone into VE_HOME
#      (reject and prompt for new VE_HOME not empty)
#    Set VE_BUILD, VE_HOME
#    Launch VE-Boostrap.R once complete
#    ve.build() then ve.run()

# Establish target folder for installation
ve.home <- Sys.getenv("VE_HOME",getwd())

# Set up points of reference
ve.bootstrap <- "VE-Bootstrap.R"                                       # identifier for Git repository installation
this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".") # used to find suitable installer
two.digit.R <- tools::file_path_sans_ext(this.R)                       # two digits are the key for ve-lib etc
minimal.ve <- c("VEStart","VEBuild","VEModel","visioneval")            # files suggesting an installation has happened
ve.lib.name <- "ve-lib"                                                # Probably never gets changed
ve.lib <- file.path(ve.home,ve.lib.name,two.digit.R)                   # Location for ve-lib for this R version

# Give user options for installation

# Check for "install" subdirectory - check manifest for version against VE_HOME
#   ve-version.txt
#   Contains Git commit at which repository was built (HEAD for branch running ve.build.installer())
#   Could do package manifest for each package in "install"
#   Compare to what is installed in ve-lib (via date field or commit ID)
# If "install" exists compare DESCRIPTION for each package via VEBuildID field
# We'll also pull out VEBuildID when the model runs - when we parse the ModelScript, for each runModule package, we
# extract its VEBuildID field and add to RunParam_ls for saving / later interrogation - figure out how to extract and
# report efficiently.

# If any difference, offer to update list of different packages.
# Once done (whether or not anything was done), rename "install" to "installed(n)" where n is a
# distinguishing number.

# Put the installation date into a file in the directory.

# Always start by working on the directory.

# Then develop valid installer names and see if any of them are present locally
# Continue looking for installer .zip file locally
#   Standard file patterns for names
#   Construct from possible names plus R version (or source)
# If not, then go looking in an online repository assets section
# Options:
#   0. Locally available .zip installer in VE_HOME
#   1. Any standard available installer at the Github
#      Standard name pattern
#      look for assets in latest release to download
#      Filter based on user's R version
#   2. Alternative Github from ve-install.cnf (YAML format)
#      Look for assets in latest release to download
#      Only look for alternative if yaml is (or can be) installed
#   3. Alternative to Clone and build from VisionEval-dev Github
#      Only look for Githubs if git2r is (or can be) installed
#   4. Alternative Github for CLone from ve-install.cnf (YAML format)
#      Only look for alternative if yaml is (or can be) installed
# Ask the user to pick one

# Once picked, download it and unzip into a new "install" folder
# Then do the installation (copying to ve-lib, install.packages/update.packages)
# Then inspect manifest.txt for how to proceed
#   pre-installed Windows (existing structure - copy to ve-lib)
#   binary contriburl for this platform VE Only (install/update packages)
#   source contriburl for this platform VE Only (install/update packages)
#   pure online installation (VE Packages in online repository too)
# Manifest should also say whether to hunt online for dependencies
# Manifest should be a file format we can handle in Base R

# Change the path when going live
# repository.path <- "https://github.com/visioneval/VisionEval-dev/releases/latest/download"
repository.path <- "C:/Git-Repos/VisionEval-dev-VE40/built/ve-pkg-repo/bin/windows/contrib/4.4"

# Check for presence of VisionEval development environment
start <- character(0)
if ( file.exists(startup.script <- file.path(ve.home,ve.bootstrap)) ) {
  # Developer bootstrap start
  # User will be managing repository externally
  source(startup.script)
  return(invisible(getwd()))
}

# Conduct installation or update as appropriate
start <- if ( dir.exists( ve.lib ) ) {
  # Check if VE minimal packages are present
  inst.pkgs <- utils::installed.packages(lib.loc=ve.lib)[,"Package"]
  if ( all ( minimal.ve %in% inst.pkgs ) ) {
    # If so, just do a regular startup
    "VEStart"
  } else {
    # if missing packages, do an install
    "install"
  }
} else {
  # Make sure ve.lib is present
  dir.create(ve.lib,recursive=TRUE)
  "install"
}
if ( start == "install" ) {

# OBSOLETE: dialog to pick VE_HOME - user now needs to do this externally
#   repeat {
#     ve.home.contents <- dir(ve.home)
#     valid.ve.home <- (
#       ( empty.ve.home <- length(ve.home.contents) == 0 ) ||
#       any( existing.libs <- ve.lib.name %in% ve.home.contents )
#     )
# 
#     # If not set up
#     if ( valid.ve.home && ! empty.ve.home ) {
#       valid.ve.home <- askYesNo(paste("Install VisionEval in",ve.home,"?"))
#     }
#     if ( ! isTRUE(valid.ve.home) ) { # Also handles NA value for valid.ve.home
#       # NOTE: ve.home will be offered in the following directory browse dialogs and if the user just re-selects that
#       # directory, it will be used anyway, creating ve-lib at that location.
#       caption <- "Select directory for VisionEval code installation (VE_HOME)"
#       ve.home <- if (exists('utils::choose.dir')) { # Won't exist on non-Windows platforms
#         utils::choose.dir(caption = caption)
#       } else {
#         tcltk::tk_choose.dir(getwd(),caption = caption)
#       }
#       if ( ! is.na(ve.home) && dir.exists(ve.home) ) { # NA if dialog was cancelled
#         message("Setting up VE_HOME as ",ve.home)
#         break
#       } else {
#         message("No valid location selected for VE_HOME:")
#         valid.ve.home <- NA
#         break # with valid.ve.home set to NA
#       }
#     } else break
#   }
#   if ( is.na(valid.ve.home) ) stop("Installation unsuccesful; Re-install VisionEval")

  if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib),.libPaths())
  ve.release <- file.path(repository.path,vestart.file)
  cat("VE Release:",ve.release,"\n")
  install.packages(ve.release,repos=NULL,lib=ve.lib,type=pkgType)
}
if ( ! require("VEStart",lib.loc=ve.lib,quietly=TRUE) ) {
  message("Installation failed due to missing VisionEval packages.")
  stop("VEStart is not present in ",ve.lib)
}
startVisionEval()
# Creates startup files (.Renviron) in VE_RUNTIME and VE_HOME
# Then loads VEModel

##### Previous work done on online installation; we're working on something simpler.

# Prior Stuff from previous VEStart with integrted installation
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
#' @param ve.home path to VE_HOME; if not provided use "ve.env" version
#' @return character vector of CRAN-like repositories from which to install or update VE packages
#' @export
getRepositories <- function(repos=NULL, use.default=TRUE, offline=TRUE, ve.home=NULL) {

  ve.env <- getRuntimeEnvironmen()
  if ( missing(ve.home) ) ve.home <- ve.env$ve.home

  # Set up default repositories (local or online)
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
  ve.env <- getRuntimeEnvironment()
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
  ve.env <- getRuntimeEnvironment()
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

  ve.env <- getRuntimeEnvironment()

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

