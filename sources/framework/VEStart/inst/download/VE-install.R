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
