# VisionEval installation script
# Author: Jeremy Raw

# General script overview:

# Organize as a function that then gets called as the last line of the script

# - Load VE_HOME, ve.lib and ve.source (for build installation)

# - Load ve-install-config.yml if it exists
#      If file does not exist, just insert defaults (including environment)
#      See ve-install-config.yml.sample for details
#      Main use is to add to the list of available repositories with released assets

# - Look up ve.distributions and (later) ve.sources
#   Find (latest) release in ve.distributions and look for matching assets
#   Present a dialog showing distribution repository and available assets
#   If ve.sources not specified, go for "VE-4.0" branch in "VisionEval-dev"
#     Initially use VisionEval-JR instead of VisionEval-dev

# - Present dialog for user that lets them choose:
#     * Runtime versus Developer installation
#     * Latest Release Assets (for runtime)
#     * Latest Release "source code (zip)" or configured ve.build branch if available

# - Receive URL for an installer .zip to download
#     * Present confirmation information
#     * Wait for OK

# - Download the installer to VE_HOME/downloads

# - Unzip to suitable location:
#     * WinLibrary goes straight into ve-lib/x.y, overwriting what is there, then running update against
#         CRAN + BioConductor
#       Get the x.y from the MANIFEST pkgType
#     * WinBinary unzips into VE_HOME/install/<contriburl>
#     * SourcePkgs unzips into VE_HOME/install/<contriburl>
#     * Source code (zip) ("zipball" for release) unzips into VE_SOURCE (zipfile interior folder names subfolder)

# - Complete the installation
#     * WinLibrary:
#         Is done just by unzipping
#     * WinBinary
#         Does install/update packages to ve-lib using contriburl plus CRAN/BioConductor
#     * SourcePkgs
#         Checks for and installs RTools if necessary (kick o)
#         Does install/update packages to ve-lib using contriburl plus CRAN/BioConductor
#     * Zipball
#         Checks for and installs RTools if necssary (kick over to manual download/install)
#         Checks for VE-Bootstrap.R in unzipped VE_SOURCE subfolder and sources that script
#         (Effectively starting a build process, using VE_HOME from the installer environment)
#         TODO: change the build script to respect VE_SOURCE

# - Start VisionEval

# Pre-packaged installer name information:
  # Zip file Naming Convention:
  #   VE-Installer_<pkgType>_<Sys.Date()>.zip
  # e.g.
  #   VE-Installer_WinLibrary-R4.3_2025-03-20.zip
  #   VE-Installer_WinBindary-R4.3_2025-03-20.zip
  #   VE-Installer_SourcePkgs_2025-03-20.zip
# Binary naming convention

# Could also allow download of source code zip.
# No manifest; need to pick extraction directory
# Look for existence of VE-Bootstrap.R
# Change working directory then source("VE-Bootstrap.R") to being

# Check for and install RTools if presented with SourcePkgs installer

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

# Search for same types on Github for download (see download.R)
# ve-install-config.cnf can list additional VEGithubRepositories:
#   user/repository format (e.g. visioneval/visioneval-dev or jrawbits/visioneval-jr)
# Will look for latest release on those alternate repositories 
# Option also to Build - that will clone the repository into VE_HOME and source VE-Bootstrap.R
#   (initially only work for public repositories)
#   (cloning will only work into an empty directory)
#   (cloning is painful due to the historical crud - only allow it for the new rooted repos).

# Options:
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

# Set up the working enviroment

require(tcltk,quietly=TRUE)

####### Establish working environment

# Note that this differs in subtle but important ways (notably ve.sources) from the working
#   environment set up in VEBuild::LoadBuildScripts or in VE-Bootstrap.R. It's bare bones
#   and just enough to situate the installer.

ve.env <- if ( ! "ve.env" %in% search() ) {
  attach(NULL,name="ve.env")
} else {
  as.environment("ve.env")
}

ve.env.list <- ls(ve.env)
ve.home    <- if ( ! "ve.home" %in% ve.envlist ) {
  ve.env$ve.home <- Sys.getenv("VE_HOME",getwd()) # Generally won't be set for a new installation
} else {
  ve.env$ve.home
}
ve.env$ve.sources <- file.path(ve.home,"build-source")
# Force ve.sources since it will be receiving downloads
# ve.sources is the directory containing an unzipped Git repository with VE-Bootstrap.R
#   to use for a "Builder" installation
# We'll unzip into a subdirectory of build-source, then go into that looking for VE-Boostrap.R
# If no Bootstrap.R is found, that's a builder error

ve.env$this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
ve.env$two.digit.R <- tools::file_path_sans_ext(this.R)

ve.lib     <- if ( ! "ve.lib" %in% ve.env.list) || basename(ve.env$ve.lib) != ve.env$two.digit.R ) {
  ve.env$ve.lib <- file.path(ve.home,"ve-lib",ve.env$two.digit.R)
} else {
  ve.env$ve.lib
}

####### Load Configuration File

config.file <- file.path(ve.home,"ve-install-config.yml")

# Default repository list for releases
default.ve.repository <- list(
  "jrawbits"=list("visioneval-jr"),   # test repository
  "visioneval"=list("visioneval-40")  # public repository
)
install.config <- if ( file.exists(config.file) ) {
  if ( ! requireNamespace("yaml",lib.loc=ve.lib,quietly=TRUE) ) {
    install.packages("yaml",repos="https://cloud.r-project.org",lib=ve.lib)
    requireNamespace("yaml",lib.loc=ve.lib,quietly=TRUE)
  }
  load.config <- try( silent=TRUE, yaml::yaml.load_file(config.file) ) # will throw error if file is improperly configured
  if ( is.list(load.config) ) {
    # Confirm use of custom destination
    cat("Install Configuration from ve-install-config.yml:\n")
    print(load.config)
    response <- tkmessageBox(
      title = "Use Loaded Configuration?", icon = "question", type = "yesno",
      message = "Do you want to use the displayed configuration?"
    )
    if ( as.character(response) != "yes" ) stop(call.=FALSE,"Installation cancelled: Edit ve-install-config.yml.")
  }
  load.config
} else {
  default.config <- list(
    default.config=TRUE,
    ve.distributions=default.ve.repository
    ve.build=list(user="jrawbits",repository="visioneval=jr",branch="VE-4.0")
    # ve.build=list(user="visioneval",repository="visioneval-40",branch="development")
  )
  cat("Default configuration:\n")
  print(default.config)
  response <- tkmessageBox(
    title = "Install Defaults?", icon = "question", type = "yesno",
    message = paste0(
      "Installing in '",ve.home,"'\n\n",
      "You haven't set up ve-install-config.yml yet.\n\n",
      "Do you want to install VisionEval using these defaults?")
  )
  if ( as.character(response) != "yes" ) stop(call.=FALSE,"Installation cancelled.")
  default.config
}

####### process the installation

installVisionEval <- function(config=install.config) { # no function parameters right now
  installer <- selectInstaller(config)   # pick an available installer
  retrieved <- fetchInstaller(installer) # confirms downloaded location and MANIFEST type
  launch    <- doInstallation(retrieved) # launch selects "end user" or "builder"
}

####### getAllReleases from distributions (plus local built if any)

if ( ! requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE) ) {
  install.packages("rjson",repos="https://cloud.r-project.org",lib=ve.lib)
  requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE)
}

####### Run tcltk dialog to select specific asset to install from releases
# https://stackoverflow.com/questions/3482513/multiple-comboboxes-in-r-using-tcltk

# TODO: the following is working example code to incorporate in release selection
# getAllReleases <- function(user,repository) {
# 
#   # Use the Github API to list releases and their properties
#   # NOTE: all.releases appears to be in descending date-time order, so the first should always be
#   # the latest release.
#   all.releases <- rjson::fromJSON(file=paste0("https://api.github.com/repos/",user,"/",repository,"/releases"))
#   latest <- if ( length(all.releases)>0 ) all.releases[1] else NA # reduce to a list of one
#   if ( ! is.list(latest) ) {
#     return( list() ) # no assets: empty list
#   }
#   release <- latest[[1]] # Get the object from the release list of 1
# 
#   downloads <- lapply(
#     release$assets,
#     function(a) {
#       list(
#         timeout = as.integer(round(a$size/750000,0)),
#         url     = a$browser_download_url,
#         file    = basename(a$browser_download_url)
#        )
#     }
#   )
#   release <- list(timeout=1000,url=release$zipball_url,file=paste0(basename(release$zipball_url),".zip"))
#   downloads <- downloads[[length(downloads)+1]] <- release
#   return(list(downloads=downloads,release=release))
# }

selectInstaller <- function(config) {
  # TODO: loop over the ve.distributions in config
  if ( ! requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE) ) {
    install.packages("rjson",repos="https://cloud.r-project.org",lib=ve.lib)
    requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE)
  }
  # TODO: iterate over all distributions; keep track of the distribution each release belongs to
  all.releases <- rjson::fromJSON(file=paste0("https://api.github.com/repos/",user,"/",repository,"/releases"))

  # TODO: may want to consider VE_BUILD and "install" folder there (see ve.make.installer in
  # VEBuild/inst/build-scripts/01-build.R) so we can do local installations

  # Run the dialog to pick a release
  #   Radio Button for end user (default) / builder
  #   Warn the builder (if they check that) that the download will only let them build VE.
  #     and if they plan to contribute changes back, they should clone the Github externaly
  #     and source VE-Bootstrap.R from the root of their clone.
  #   List box (or choices) for distribution (alternate Githubs or locations)
  #     Only show if more than one distribution is configured - if only one, consider it selected
  #   TODO: do we also want to allow a zipball for a particular repository branch rather than a
  #     Release? How to usefully set up ve.distributions...
  #   Checkbox "See All Releases" (if unchecked - default - , only latest release in each
  #     distribution, and if no releases and builder, then zipball for distribution branch
  #     (default: main)
  #   Selection window showing all releases by Github name
  #   Selection window showing all usable assets in the release
  #     - maybe just one (the zipball) if "builder"
  #     - otherwise, possible WinLibrary or WinBinary for Rx.y , or any R version Source
  #   Everything boils down to selecting a descriptor for a single installer

  # Return value is a list of information for downloading the selected installer (timeout, url, filename, pkgType)
}

####### Download the installer and report what was retrieved (or if it failed)

fetchInstaller <- function(installer) {
  # Dispatch on installer$pkgType
    # WinBinary Rx.y
    # WinLibrary Rx.y
    # Source
    # Zipball
  # Place downloaded file in ve.home/download
  # return installer augmented with local file name
}

####### Perform the installation based on the downloaded installer type and information

doInstallation <- function(retrieved) {
  # Dispatch based on retrieved$localfile and pkgType
    # WinLibrary replaces corresponding ve-lib packages with packages from installer
    # WinBinary always installs ve-lib with binary packages it contains (remove package, then install)
    # Source always installs ve-lib with source packages (remove package first, then install)
    #   For any R repository style install, do "update.packages" from public repositories first for
    #   dependency updates
    # return launch function loading VEStart
  # Zipball
    # Unzip into ve.sources
    # Load VE-Bootstrap.R, with ve.build.sources set to the unzipped zipball
  # Return a function to launch VE (bootstrap or load VEStart)
  # Return a text error message if install failed.
}

####### Run the configured installation

  # launch will do one of two things
  # End User:
  #   require(VEStart)
  #   startVisionEval()
  # Builder:
  #   source(file.path(retrieved,"VE-Bootstrap.R") # retrieved is the folder within ve.sources to launch
launch <- installVisionEval(install.config) # Will run automatically if using defaults (asked user earlier)
if ( is.function(launch) ) launch() else stop(call.=FALSE,"Installation failed:\n",as.character(launch),"\nPlease retry.")

####################
####################
# Remainder of file is an earlier implementation

# # Establish target folder for installation
# ve.home <- Sys.getenv("VE_HOME",getwd())
# 
# # Set up points of reference
# ve.bootstrap <- "VE-Bootstrap.R"                                       # identifier for Git repository installation
# this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".") # used to find suitable installer
# two.digit.R <- tools::file_path_sans_ext(this.R)                       # two digits are the key for ve-lib etc
# minimal.ve <- c("VEStart","VEBuild","VEModel","visioneval")            # files suggesting an installation has happened
# ve.lib.name <- "ve-lib"                                                # Probably never gets changed
# ve.lib.base <- file.path(ve.lib.name,two.digit.R)                      # Prepend ve.home to make ve-lib for this R version
# install.name <- "install"                                              # ve.home folder with extracted installer
# manifest.name <- "MANIFEST"                                            # Manifest file describing installation file
# 
# # Make sure pattern fits with current installer naming convention
# # Need library accounted for as well.
# installer.pattern <- paste0(
# "VEInstaller_.*_", # .* will be the VE Version
# paste0("(Windows_R",two.digit.R,"|Source)"),
# ".zip$")
# 
# # Change the path when going live
# # repository.path <- "https://github.com/visioneval/VisionEval-dev/releases/latest/download"
# CRAN.mirror <- "https://cloud.r-project.org"
# pkgType <- .Platform$pkgType
# git.repository <- c(
#   # user="visioneval",
#   # repository="visioneval-dev"
#   user="jrawbits",
#   repository="visioneval-jr"
# )
# 
# # Check for "install" subdirectory - check manifest for version against VE_HOME
# #   ve-version.txt
# #   Contains Git commit at which repository was built (HEAD for branch running ve.build.installer())
# #   Could do package manifest for each package in "install"
# #   Compare to what is installed in ve-lib (via date field or commit ID)
# # If "install" exists compare DESCRIPTION for each package via VEBuildID field
# # We'll also pull out VEBuildID when the model runs - when we parse the ModelScript, for each runModule package, we
# # extract its VEBuildID field and add to RunParam_ls for saving / later interrogation - figure out how to extract and
# # report efficiently.
# 
# # If any difference, offer to update list of different packages.
# # Once done (whether or not anything was done), rename "install" to "installed(n)" where n is a
# # distinguishing number.
# 
# # Put the installation date into a file in the directory.
# 
# # Always start by working on the directory.
# 
# # Then develop valid installer names and see if any of them are present locally
# # Continue looking for installer .zip file locally
# #   Standard file patterns for names
# #   Construct from possible names plus R version (or source)
# # If not, then go looking in an online repository assets section
# # Options:
# #   0. Locally available .zip installer in VE_HOME
# #   1. Any standard available installer at the Github
# #      Standard name pattern
# #      look for assets in latest release to download
# #      Filter based on user's R version
# #   2. Alternative Github from ve-install.cnf (YAML format)
# #      Look for assets in latest release to download
# #      Only look for alternative if yaml is (or can be) installed
# #   3. Alternative to Clone and build from VisionEval-dev Github
# #      Only look for Githubs if git2r is (or can be) installed
# #   4. Alternative Github for CLone from ve-install.cnf (YAML format)
# #      Only look for alternative if yaml is (or can be) installed
# # Ask the user to pick one
# 
# # Once picked, download it and unzip into a new "install" folder
# # Then do the installation (copying to ve-lib, install.packages/update.packages)
# # Then inspect manifest.txt for how to proceed
# #   pre-installed Windows (existing structure - copy to ve-lib)
# #   binary contriburl for this platform VE Only (install/update packages)
# #   source contriburl for this platform VE Only (install/update packages)
# #   pure online installation (VE Packages in online repository too)
# # Manifest should also say whether to hunt online for dependencies
# # Manifest should be a file format we can handle in Base R
# 
# # Check for presence of VisionEval development environment
# 
# ve.check.bootstrap <- function() {
#   start <- character(0)
#   if ( file.exists(ve.bootstrap)) ) {
#     # Developer bootstrap start
#     # User will be managing repository externally
#     source(ve.bootstrap)
#     return(invisible(getwd()))
#   }
# }
# 
# ve.install.type <- function() {
#   # Determine how to install, if necessary
#   if ( dir.exists( ve.lib.base ) ) { # Check for valid installation in current directory
#     # Check if VE minimal packages are present
#     inst.pkgs <- utils::installed.packages(lib.loc=file.path(getwd(),ve.lib.base))[,"Package"]
#     if ( all ( minimal.ve %in% inst.pkgs ) ) {
#       # If so, just do a regular startup
#       return("VEStart")
#     }
#   }
#   return("install")
# }
# 
# set.ve.home <- function() {
#   # Set ve.home somewhere else if desired
#   # TODO: Use a tk dialog directory chooser to locate VE_HOME
#   ve.home <- getwd()
#   repeat {
#     # See if we can use ve.home
#     ve.home.contents <- dir(ve.home)
#     valid.ve.home <- dir.exists(
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
#         message("Setting up VE_HOME in ",ve.home)
#         break
#       } else {
#         message("No valid location selected for VE_HOME: ",ve.home)
#         valid.ve.home <- NA
#         break # with valid.ve.home set to NA
#       }
#     } else break
#   }
#   if ( is.na(valid.ve.home) ) stop("Invalid VisionEval installation location - please re-install.")
# 
#   # Change to selected ve.home
#   setwd(ve.home)
# 
#   # Make sure ve.lib is present
#   # TODO: need to create current R version subdirectory if it doesn't exist
#   ve.lib <- file.path(ve.home,ve.lib.base)
#   if ( ! dir.exists(ve.lib) ) dir.create(ve.lib,recursive=TRUE)
#   if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib,.libPaths()))
# 
#   return(list(ve.home=ve.home,ve.lib=ve.lib)
# }
# 
# installer.dialog <- function(available,local=FALSE) {
#   # This works if available is either a list or character vector
#   cat("Available Installers:\n")
#   print( available )
#   dl <- 0
#   while ( length(available)>0 && ! is.na(dl) && ( dl<1 || dl > length(available) ) ) {
#     prompt <- if ( ! local ) {
#       "Which release to download (q to exit)? "
#     } else {
#       "Which file to install (d+number to delete, q to exit)? "
#     }
#     raw.dl <- readline(prompt=prompt)
#     dl <- try( suppressWarnings(as.integer(raw.dl)) ) # NA if raw.dl is not an integerable thing
#     if ( is.na(dl) ) {
#       # if local files, process delete option
#       if ( local && grepl("^d",raw.dl) && ! is.na( which <- try( suppressWarnings(as.integer(gsub("^d","",raw.dl))) ) ) ) {
#         confirm <- askYesNo(paste0("Delete local installer: ",dl[[which]],"?"))
#         if ( isTRUE(confirm) ) {
#           unlink(dl[[which]])
#           stop(call.=FALSE,"Deleted local installer - please restart installation")
#           # stop because it's too hard to relay the modified available list out of here
#         }
#       } # wrap around to use the loop test to exit if valid
#     }
#   }
#   return( dl )
# }
# 
# ve.existing.folder <- function(ve.home) {
#   install.dir <- ""
#   if ( install.name %in% dir(ve.home) ) {
#     unzipped <- askYesNo(prompt="Use existing 'install' directory to install or update?")
#     if ( isTRUE(unzipped) ) install.dir <- file.path(ve.home,install.name)
#   }
#   ve.do.install(pkgType="Folder",from=install.dir) # check Manifest.txt and dispatch accordingly
#   return(install.dir)
# }
# 
# ve.online.installer <- function()
# {
#   # Try to obtain an online installer
#   # Gather necessary dependency to retrieve assets
#   if ( ! suppressWarnings(requireNamespace("rjson",quietly=TRUE)) ) {
#     utils::install.packages(
#       "rjson",lib=ve.lib,
#       repos=CRAN.mirror,type=pkgType,
#       verbose=FALSE,quiet=TRUE
#     )
#     suppressWarnings(requireNamespace("rjson",quietly=TRUE))
#   }
# 
#   # Use the Github API to list releases and their properties
#   # NOTE: releases seem to be in descending date-time order, so [1] should be the latest release.
#   downloads <- list()
#   all.releases <- rjson::fromJSON(
#     file=paste0(
#       "https://api.github.com/repos/",
#       git.repository$user,"/",
#       git.repository$repository,"/releases"
#     )
#   )
#   latest <- if ( length(all.releases)>0 ) all.releases[1] else NA # reduce to a list of one
#   if ( is.list(latest) ) {
#     release <- latest[[1]] # Get the object from the release list of 1
#     downloads <- lapply(
#       release$assets,
#       function(a) {
#         list(
#           timeout = as.integer(round(a$size/750000,0)),
#           url     = a$browser_download_url,
#           file    = basename(a$browser_download_url)
#         )
#       }
#     )
#   }
#   if ( length(downloads) > 0 ) {
#     available <- sapply(downloads,function(d)d$file,simplify=TRUE)
#     filtered <- grep(installer.pattern,available)
#     available <- available[filtered]
#     downloads <- downloads[filtered]
#   }
#   asset <- list()
#   if ( length(available)>0 ) {
#     which <- installer.dialog(available)
#     if ( is.integer(which) && which > 0 && which <= length(available) ) {
#       asset <- downloads[[which]]
#     }
#   }
# 
#   # Download the installer
#   if ( is.list(asset) && length(asset)>0 && all(c("timeout","url","file") %in% names(asset)) ) {
#     options(timeout = max(item$timeout, getOption("timeout"))) # ten minute timeout; set dynamically based on reported file size?
#     download.file(item$url,destfile=item$file,method="auto",mode="wb")
#     installer <= item$file
#   } else installer <- ""
# 
#   return(installer)
# }
# 
# ve.local.installer <- function(ve.home) {
#   installer <- ""
#   # See if there are already-downloaded local installers
#   local.installers <- dir(ve.home,pattern=installer.pattern)
#   if ( length(local.installers>0) ) {
#     which <- installer.dialog(local.installers,local=TRUE)
#     if ( is.integer(which) && which > 0 && which <= length(local.installers) ) {
#       installer <- local.installers[[which]]
#     }
#   }
#   return(installer)
# }
# 
# ve.unzip.installer <- function(installer.zip) {
#   # Check the manifest
#   manifest <- read.dcf( unz(installer.zip,filename=manifest.name), n=1 )
#   m.names <- dimnames(manifest)[2]
#   if ( is.null(m.names) || ! "pkgType" %in% m.names  ) {
#     message("Ill-formed installer: ",installer.zip)
#     stop("No valid ",manifest.name," file",call.=FALSE)
#   }
#   pkgType <- manifest[1,"pkgType"]
#   ve.do.install(pkgType,zip=installer.zip)
#   if ( pkgType=="Library" ) {
#     # double check that the zip file has the correct R version
#     # blow away all the files in ve-lib
#     doit <- askYesNo(prompt="Replace your VisionEval installation? (y/n/cancel)")
#     # TODO: Probably should move ve-lib aside rather than delete it outright
#     if ( ! isTRUE(doit) ) stop("Installation cancelled.",call.=FALSE)
#     if ( dir.exists(ve.lib) ) {
#       message("Deleting existing VisionEval library")
#       unlink(ve.lib,recursive=TRUE)
#       message("Old VisionEval library removed; now installing new version")
#     }
#     install.dir <- ve.lib # will re-create when installer.zip is unzipped
#     unzip(installer.zip,exdir=install.dir)
#   } else if ( pkgType %in% c("WinBinary","Source") ) {
#     install.dir <- file.path(ve.home,install.name)
#     if ( dir.exists(install.dir) ) unlink(install.dir,recursive=TRUE)
#     unzip(installer.zip,exdir=install.dir)
#     # Move ve-lib aside
#     # Get BiocManager
#     # Set contriburl for BioC repositories and CRAN consistent with contriburl at
#     # install.dir, and do utils::install.packages.
#   } else {
#     stop("Installation failed: unknown Package Type: ",pkgType,call.=FALSE)
#   }
#   return(install.dir)
# }
#     
# 
#   # 7. Identify installation parameters (R Version, library/source/win.binary)
#   # 8. Put files into appropriate locations
#   ve.do.install(install.dir,ve.lib) # Get the packages from their various places into ve.lib
#   #    - Library copies "assets" to "ve-lib"
#   #    - pkgType constructs contriburl list and does install.packages (online dependencies)
#   #      * do it in 2 phases: install BiocManager from CRAN, then get it to dish repositories
#   #        to install the rest
# 
#   return(install.dir)
# }
# 
# ###############################################
# 
# # Now run the installation
# if ( ve.install.type() == "install" ) { # Otherwise just fall through to load VEStart
# 
#   # Possibly reposition ve.home and ve.lib if installer
#   home.lib <- set.ve.home()
#   ve.home  <- home.lib$ve.home
#   ve.lib   <- home.lib$ve.lib
# 
#   # See if we've already unzipped an installer
#   install.dir <- ve.existing.folder(ve.home)
#     
#   if ( ! dir.exists(install.dir) ) {
#     installer.zip <- ve.local.installer(ve.home)
#     if ( ! file.exists(installer.zip) ) {
#       installer.zip <- ve.online.installer()
#     }
#     if ( file.exists(installer.zip) ) {
#       # Now unzip and install everything
#       install.dir <- ve.unzip.installer(installer.zip)
#     }
#     # install.dir may still not exist
#   }
#   if ( ! dir.exists(install.dir) ) {
#     stop("No installer; please retry",call.=FALSE)
#   }
# }
# 
# # The moment we've all been waiting for: just start it!
# if ( ! require("VEStart",lib.loc=ve.lib,quietly=TRUE) ) {
#   message("Installation failed due to missing VisionEval packages.")
#   stop("VEStart is not present in ",ve.lib)
# }
# startVisionEval()
# # Synchronizes startup files (.Renviron, VE_RUNTIME and VE_HOME) with ve.env
# # Then loads VEModel
