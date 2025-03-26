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

# Note that this differs in subtle but important ways from the working environment set up in
#   VEBuild::LoadBuildScripts or in VE-Bootstrap.R. It's bare bones and just enough to situate the
#   installer.

ve.env <- if ( ! "ve.env" %in% search() ) {
  attach(NULL,name="ve.env")
} else {
  as.environment("ve.env")
}

# Allow user to pre-select ve.home rather than go through the dialog below
# If VE_INSTALL is set, we'll go do our work there rather than VE_HOME (for testing the script)
ve.env.list <- ls(ve.env)
ve.home     <- if ( ! "ve.home" %in% ve.env.list ) {
  ve.env$ve.home <- Sys.getenv(
    "VE_INSTALL",Sys.getenv("VE_HOME",getwd()) # VE_INSTALL can be used for testing
  )
} else {
  ve.env$ve.home
}
if ( ! dir.exists(ve.home) ) dir.create(ve.home,recursive=TRUE)
setwd(ve.home)

if ( length(dir(ve.home)) > 0 ) {
  confirm <- tkmessageBox(
    title = "Invalid VE_HOME Directory", icon = "warning", type = "yesno",
    message = "The working directory is not empty. Would you like to find or create a different one?"
    )
  if ( as.character(response) != "yes" ) {
    stop("Please start the VisionEval installation again in an empty folder.",call.=FALSE)
  }
  caption <- "Select installation directory (VE_HOME)"
  ve.home <- if (exists('utils::choose.dir')) { # Won't exist on non-Windows platforms
    utils::choose.dir(default=ve.home,caption = caption)
  } else {
    tcltk::tk_choose.dir(default=ve.home,caption = caption)
  }
  if ( is.na(ve.home) || ! dir.exists(ve.home) ) {
    message("Please select (or create) an empty VE_HOME directory for installation")
    stop("Installation cancelled.",call.=FALSE)
  }
}

ve.env$this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
ve.env$two.digit.R <- tools::file_path_sans_ext(this.R)

# Set ve-lib installation location
ve.env$ve.lib <- file.path(ve.home,"ve-lib",ve.env$two.digit.R)
if ( ! dir.exists(ve.env$ve.lib) ) dir.create(ve.env$ve.lib,recursive=TRUE)
if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib,.libPaths()))

# Create another library for instalation packages (yaml, rjson, BiocManager)
inst.lib <- file.path(ve.home,"ve-inst-lib (remove)",ve.env$two.digit.R)
if ( ! dir.exists(inst.lib) ) dir.create(inst.lib,recursive=TRUE)
if ( ! inst.lib %in% .libPaths() ) .libPaths(c(inst.lib,.libPaths()))

####### Load Configuration File

config.file <- file.path(ve.home,"ve-install-config.yml")

# Default repository list for releases
default.ve.repository <- list(
  list(user="jrawbits",repository=c("visioneval-jr","visioneval-40")),   # test repository
  list(user="visioneval",repository=c("visioneval-dev","visioneval-40"))  # public repository
)

install.config <- if ( file.exists(config.file) ) {
  if ( ! requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE) ) {
    install.packages("yaml",repos="https://cloud.r-project.org",lib=inst.lib)
    requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE)
  }
  load.config <- try( silent=TRUE, yaml::yaml.load_file(config.file) ) # will throw error if file is improperly configured
  if ( is.list(load.config) ) {
    # Confirm use of custom destination
    cat("Install Configuration from ve-install-config.yml:\n")
    cat(yaml::as.yaml(load.config))
    response <- tkmessageBox(
      title = "Use Loaded Configuration?", icon = "question", type = "yesno",
      message = "Do you want to use the displayed configuration?"
    )
    if ( as.character(response) != "yes" ) stop(call.=FALSE,"Installation cancelled: Edit ve-install-config.yml.")
  } else message("Failed to load configuration from:\n",config.file)
  load.config
} else {
  default.config <- list(
    default.config=TRUE,
    ve.distributions=default.ve.repository,
    ve.build=list(user="jrawbits",repository="visioneval=jr",branch="VE-4.0")
    # ve.build=list(user="visioneval",repository="visioneval-40",branch="development")
  )
  cat("Default configuration:\n")
  cat(yaml::as.yaml(load.config))
  response <- tkmessageBox(
    title = "Install Defaults?", icon = "question", type = "yesno",
    message = paste0(
      "Installing VisionEval in '",ve.home,"'\n\n",
      "You haven't set up ve-install-config.yml yet.\n\n",
      "Do you want to install VisionEval using these defaults?")
  )
  if ( as.character(response) != "yes" ) stop(call.=FALSE,"Installation cancelled; please edit ve-install-config.yml")
  default.config
}

####### process the installation

installVisionEval <- function(config=install.config) { # no function parameters right now
  installer <- selectInstaller(config)   # pick an available installer
  retrieved <- fetchInstaller(installer) # confirms downloaded location and MANIFEST type
  launch    <- doInstallation(retrieved) # launch selects "end user" or "builder"
}

####### getAllReleases from distributions (plus local built if any)

# Test file names for installer.pattern
# files <- c(
#   "VE-Installer_Source_2025-03-21.zip",
#   "VE-Installer_Windows-R4.4_2025-03-21.zip",
#   "VE-Installer_WinLibrary-R4.4_2025-03-21.zip"
# )
# Repository ZipBall for building comes as "Build_Source_207439148.zip"
#   where the gaggle of numbers is the Github release ID

installer.pattern <- paste0(
  "VE-Installer_",                                          # .* will be the VE Version
  paste0("((WinLibrary|Windows)-R",two.digit.R,"|Source)"), # The type of installer 
  "_.*\\.zip$"
)

isVEInstaller <- function(filename) all(grepl(installer.pattern,filename))

getReleases <- function(config) {
  # Configured for testing so as not to hit Github API over and over (they are rate limited)
  if ( ! requireNamespace("rjson",lib.loc=inst.lib,quietly=TRUE) ) {
    install.packages("rjson",repos="https://cloud.r-project.org",lib=inst.lib)
    requireNamespace("rjson",lib.loc=inst.lib,quietly=TRUE)
  }
  if ( file.exists(test.file<-"Test-Skip-Download.json") ) {
    # NOTE: uncomment lines below to save JSON locally for testing
    # You don't want to do that generally since it will block release updates.
    all.releases <- rjson::fromJSON(file=test.file,simplify=FALSE)
    message("\n##### USING TEST FILE !!! #####")
  } else {
    all.releases <- list()
    for ( distro in config$ve.distributions) {
      dist.user <- distro$user
      message("Github user: ",dist.user)
      for ( repo in distro$repository ) {
        repo.name <- paste(dist.user,repo,sep="/")
        # message("Processing ",repo.name)
        # Check for repository existence
        repo.addr <- paste0("https://api.github.com/repos/",dist.user,"/",repo)
        check.repo <- base::curlGetHeaders(repo.addr)
        repo.status <- attr(check.repo,'status')
        if ( is.null(repo.status) || repo.status!=200 ) {
          message("Cannot Find Github Repository: ",repo.addr," (",repo.status,")")
          next
        }
        releases <- rjson::fromJSON(file=paste0(repo.addr,"/releases"))
        writeLines(rjson::toJSON(releases,indent=2),con="Raw-release.json")

        release.data <- list()
        for ( r in releases ) {
          r.temp <- list(
            release=r$name,
            date=r$published_at,
            id=r$id
          )
          # The zipball is the complete snapshot of the repository tag
          # that was built into the release. You can use that to set up
          # a VisionEval ve.build() installation without messing with Git.
          zbname <- paste0("Build_Source_",r$id,".zip")
          zipball <- list()
          zipball[[zbname]] <- list(
            timeout = 1200,
            url     = r$zipball_url,
            file    = zbname
          )
          r.temp$zipball <- zipball

          # message("Processing release: ",r$name)
          installers <- list()
          for ( a in r$assets ) {
            a.temp <- list(
              timeout = as.integer(round(a$size/750000,0)),
              url     = a$browser_download_url,
              file    = basename(a$browser_download_url)
            )
            if ( isVEInstaller( a.temp$file ) ) {
              # message(a.temp$file," IS an installer")
              installers[[length(installers)+1]] <- a.temp
            } else {
              # message(a.temp$file," is NOT an installer (",installer.pattern,")")
            }
          }
          if ( length(installers) > 0 ) { # got a valid release
            installer.names <- sapply( installers,function(i) i$file )
            installers <- installers [ # put them in order of desirability
              c(
                grep("Windows",installer.names),
                grep("WinLibrary",installer.names),
                grep("Source",installer.names)
              )
            ]
            names(installers) <- sapply( installers,function(i) i$file )
            r.temp$assets <- installers
            release.data[[length(release.data)+1]] <- r.temp
            # message("Release: ",r.temp$release)
          } else {
            stop("No installers found in release ",r$name,call.=FALSE)
          }
        }
        if ( length(release.data) > 0 ) {
          names(release.data) <- sapply( release.data,function(r) paste0(r$release,":",r$date) )
          all.releases[[repo.name]] <- release.data
        } else {
          message("No releases found with valid installers in ",repo.name)
        }
      }
    }
    # NOTE: Save JSON locally when testing (Github API is rate-limited)
    #   message("Saving test file")
    #   writeLines(rjson::toJSON(all.releases,indent=2),con=test.file)
  }
  return(all.releases)
}

############ Dialog to pick an installer from among those available

# Setup dialog navigates all.releases to select an installer to download and install
setup.dialog <- function(all.releases,max_width=800) {
  # Dialog values to update
  # Keep track of whether we're doing a runtime or build installation
  # And which repository, release and installer we've selected
  runtime <- tclVar("Runtime")                                  # Checkbox selector from "Runtime" or "Build"
  repository <- tclVar(names(all.releases)[1])                  # List box selector from names(all.releases)
  release <- tclVar(names(all.releases[[1]])[1])                # List box selector from names(all.releases[[repository]])
  doit <- tclVar("No")                                          # Change this if user chooses "Install"

  # Quick way to look at different runtime or build installers
  # The build installer downloads the source code from which the release was built
  getAssetType <- function() {
    if ( tclvalue(runtime)=="Runtime" ) "assets" else "zipball"
  }
  installer <- tclVar(names(all.releases[[1]][[1]][[getAssetType()]])[1])  # List box selector for default installer

  # Installation Type Dialog
  # Runtime installs build packages, Build Installation will download a snapshot of the release
  # soruce code, from which VE-Bootstrap.R / VEBuild can be run.
  select_runtime <- function() {
    pick <- tkmessageBox(title = "Install Type", message = "Build Installation?", icon = "question", type = "yesno")
    if (as.character(pick) == "yes") {
      pick <- "Build"
    } else {
      pick <- "Runtime"
    }
    tclvalue(runtime) <- pick
    tclvalue(installer) <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])[1]
  }

  # Our own listbox function to pick from a list
  # Used to select repositories (if more than one configured), releases, and installers
  select_from_list <- function(parent_window, dest_var, items, title="Make a Selection") {
    tt <- tktoplevel(parent = parent_window)
    tkwm.title(tt, title)

    original_var <- tclVar(tclvalue(dest_var))

    Instructions <- tklabel(tt,text=title,justify="center")
    tkgrid(Instructions, row=0, column=0, sticky="ew",pady=5)

    lb.frame <- tkframe(tt,borderwidth=2,relief="solid")
    lb1 <- tklistbox(lb.frame, selectmode = "single", height=0, width=0)

    tkgrid(lb1,row=0,column=0,padx=5,pady=5,sticky="ew")
    tkgrid.columnconfigure(lb.frame,0,weight=1)
    message(length(items)," items to insert.")
    for (item in items) {
      message("inserting ",item)
      tkinsert(lb1, "end", item)
    }
    tkselection.set(lb1,0)
    tkgrid(lb.frame, row = 1, column = 0, sticky = "ew", padx=5, pady=5)

    onOK <- function() {
      selection <- as.integer(tcl(lb1, "curselection")) + 1
      if (length(selection) > 0) {
        tclvalue(dest_var) <- items[selection]
      }
      tkdestroy(tt)
    }

    onCancel <- function() { # leave dest_var unchanged
      tclvalue(dest_var) <- tclvalue(original_var)
      tkdestroy(tt)
    }

    button.frame <- tkframe(tt)
    ok_button <- tkbutton(button.frame, text = "OK", command = onOK)
    cancel_button <- tkbutton(button.frame, text = "Cancel", command = onCancel)

    tkgrid(ok_button, row = 0, column = 0, padx = 5, pady = 5,sticky="e")
    tkgrid(cancel_button, row = 0, column = 1, padx = 5, pady = 5,sticky="w")
    tkgrid(button.frame,row=2,column=0)

    tkgrid.columnconfigure(tt, 0, weight = 1)
    tkgrid.rowconfigure(tt, 0, weight = 1)
  }

  # Here's the GUI driver that shows what has been selected to install and allows
  # the user to pick something different.
  create_dialog <- function(max_width=400) {
    tt <- tktoplevel()
    tkwm.title(tt, "Select Installer")
    tkwm.maxsize(tt, max_width, 10000) # we don't expect to expand vertically

    # Actions to gather information
    runtime_button <- tkbutton(tt, text = "Installation Type", command = select_runtime)
    repos_button <- tkbutton(tt, text = "Repository", command = function() {
      repos.list <- names(all.releases)
      if ( length(repos.list) < 2 ) return() # Button does nothing if not enough items

      # Run the selection dialog, then look up the selected release and choose the
      # default installer from that release.
      select_from_list(tt,repository,repos.list) # will update repository variable
      release_list <- all.releases[[tclvalue(repository)]]
      tclvalue(release) <- names(release_list)[1] # reset to first release
      inst_list <- release_list[[tclvalue(release)]][[getAssetType()]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    release_button <- tkbutton(tt, text = "Release", state="normal", command = function() {
      release_list <- names(all.releases[[tclvalue(repository)]])
      if ( length(release_list) < 2 ) return() # Do nothing if too few items

      # If release changes, change the installer to the default one.
      select_from_list(tt,release,release_list) # will update repository variable
      inst_list <- all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    installer_button <- tkbutton(tt, text = "Installer", state="normal", command = function() {
      installer_list <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])
      if ( length(installer_list) < 2 ) return() # Do nothing if there are too few installers
      select_from_list(tt,installer,installer_list) # will update installer variable
    })

    # Display the buttons
    tkgrid(runtime_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
    tkgrid(repos_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
    tkgrid(release_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
    tkgrid(installer_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)

    # Display the values set by the buttons in label widgets
    # Put the widgets in frames so they resize nicely
    runtime_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    runtime_label <- tklabel(runtime_frame,textvariable=runtime, justify="left")
    tkpack(runtime_label,anchor="w",padx=5,pady=5)
    tkgrid(runtime_frame, column = 1, row = 0, sticky="ew", padx = 5, pady = 5)

    repos_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    repos_label <- tklabel(repos_frame,textvariable=repository, justify="left")
    tkpack(repos_label,anchor="w",padx=5,pady=5)
    tkgrid(repos_frame, column = 1, row = 1, sticky="ew", padx = 5, pady = 5)

    release_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    release_label <- tklabel(release_frame,textvariable=release, justify="left")
    tkpack(release_label,anchor="w",padx=5,pady=5)
    tkgrid(release_frame, column = 1, row = 2, sticky="ew", padx = 5, pady = 5)

    installer_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    installer_label <- tklabel(installer_frame,textvariable=installer, justify="left")
    tkpack(installer_label,anchor="w",padx=5,pady=5)
    tkgrid(installer_frame, column = 1, row = 3, sticky="ew", padx = 5, pady = 5)

    # OK and Cancel buttons
    onOK <- function() {
      tclvalue(doit) <- "Install"
      tkdestroy(tt)
    }

    onCancel <- function() {
      tclvalue(doit) <- "Cancel"
      tkdestroy(tt)
    }

    ok_button <- tkbutton(tt, text = "Install", command = onOK)
    cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)

    tkgrid(ok_button, column = 0, row = 5, pady = 10)
    tkgrid(cancel_button, column = 1, sticky="w", row = 5, pady = 10)
    tkgrid.columnconfigure(tt, 1, weight = 1) #Make the second column expandable.

    tkwait.window(tt) # Run the dialog
    
    return( # Relay tclVar variables back out to calling environment
      list(
        Runtime=tclvalue(runtime),
        Repos=tclvalue(repository),
        Release=tclvalue(release),
        Installer=tclvalue(installer),
        DoIt=tclvalue(doit)
      )
    )
  }
  # Run the GUI
  return( create_dialog(max_width=max_width) )
}

selectInstaller <- function(config) {

  all.releases <- getReleases(config)
  selected <- setup.dialog(all.releases)

  if ( selected$DoIt != "Install" ) {
    stop("Installation cancelled from installer selection dialog",call.=FALSE)
  }

  runtime.installation <- selected$Runtime == "Runtime"
  assetType <- if(runtime.installation) "assets" else "zipball"
  repo <- selected$Repos
  release <- selected$Release
  installer <- all.releases[[repo]][[release]][[assetType]][[selected$Installer]]

  installer$installType <- if ( runtime.installation ) "Runtime" else "Build"
  cat("Installation:",installer$installType,"\n")
  cat("Repository:",repo,"of",length(names(all.releases)),"\n")
  cat("Release:",release,"of",length(names(all.releases[[repo]])),"\n")
  cat("Installer:",installer$file,"of",length(names(all.releases[[repo]][[release]][[assetType]])),"\n")
  return(installer)
}

####### Download the installer and report what was retrieved (or if it failed)

installTypes <- data.frame(
  pattern = c(
    "WinLibrary_R",
    "Windows_R",
    "^Build_Source_",
    "_Source_"
  ),
  type = c(
    "WinLibrary",
    "Windows",
    "BuildSource",
    "Source"
  )
)

installTypeOf <- function(retrieved) { # retrieved is a file name
  for ( p in 1:nrow(installTypes) ) {
    if ( grepl(installTypes$pattern[p],basename(retrieved)) ) return(installTypes$type[p])
  }
  return("Unknown")
}

fetchInstaller <- function(installer) {
  # Place downloaded files in ve.home/download
  # return downloaded file name and type of installation expected
  # File name has attribute distinguishing VE Installer from Github snapshot (Source Code)
  download <- file.path(ve.home,"download")
  if ( ! dir.exists( download ) ) dir.create(download,recursive=TRUE)
  if ( ! installer$file %in% dir(download) ) { # Shorten restart if there was a previous download
    options(timeout = max(installer$timeout, getOption("timeout")))
    message("Timeout: ",getOption("timeout")," seconds")
    destfile <- file.path(download,installer$file)
    download.file(installer$url,destfile=destfile,method="libcurl",mode="wb")
    # use method=libcurl so download.file follows redirect links
  } else {
    message("Installer has already been downloaded.")
    message("Install is ",download)
    message("installer$file is ",installer$file)
    message("For a clean install, remove the downloads directory in VE_HOME")
  }
  retrieved <- dir(download,full.names=TRUE,pattern=installer$file)
  attr(retrieved,"InstallType") <- installTypeOf(retrieved)
  invisible(retrieved) # name of downloaded file with attribute stating "Runtime" or "Build" install type
}

####### Perform the installation based on the downloaded installer type and information

doInstallation <- function(retrieved) {
  # Dispatch based on retrieved$localfile and InstallType attribute
  # VE built installers:
    # WinLibrary replaces corresponding ve-lib packages with packages from installer
    #   - Remove them first based on folder names
    #   - Then copy the entire new package into palce
    # WinBinary always installs ve-lib with binary packages it contains
    #   - Update existing packages from CRAN and BioConductor
    #   - Just install directly (will overwrite any package already there)
    #   - Set up so the repository list can find needed files in CRAN or BioConductor
    # Source always installs ve-lib with source packages it contains
    #   - Update existing packages from CRAN and BioConductor
    #   - Just install directly (will overwite any package already there)
    #   - Set up so the repository list can find needed files in CRAN or BioConductor
    # return launch function loading VEStart or VE-Bootstrap.R as desired
  # Source Code Zipball 
    # Unzip into build-source Location
    # Load VE-Bootstrap.R, with ve.sources set to the unzipped zipball
    # VE_HOME can stay the same, VE_BUILD created within VE_HOME, VE_SOURCE set to
    #   absolute path of "sources" subfolder in unzipped source tree.
    # When VE-Bootstrap.R does ve.build() it saves VE_SOURCE to .Renviron
  # Return a function to launch VE (bootstrap or load VEStart)
  # Return a text error message if install failed.
  installType <- attr(retrieved,"InstallType")
  if ( installType %in% c("WinLibrary","Windows","Source") ) {
    if ( installType == "WinLibrary" ) {
      # This is a pre-installed WinBinary, with all dependencies (like VE installers before 4.0)
      # Unzip directly into ve.lib
      lst <- unzip(retrieved,list=TRUE)
      # Find any packages already in ve.lib and remove those
      # NOTE: some removals may fail for packages loaded while running this script (e.g. rjson or yaml)
      # startVisionEval may try to update those
      replacements <- file.path(ve.lib,sub("/$","",lst[grep("^[^/]+/$",lst$Name),"Name"]))
      replacements <- replacements[dir.exists(replacements)]
      if ( length(replacements) > 0 ) {
        message("Would remove:")
        print(replacements)
        # unlink(replacements,recursive=TRUE)
      }
      # Unzip the replacement packages straight into ve.lib
      message("Would unzip: ",retrieved)
      message("Into       : ",ve.lib)
      # unzip(retrieved,exdir=ve.lib) # simply extract the download back into ve-lib
    } else {
      # Ensure presence of needed packages
      if ( ! requireNamespace("BiocManager",lib.loc=inst.lib,quietly=TRUE) ) {
        install.packages("BiocManager",repos="https://cloud.r-project.org",lib=inst.lib)
        requireNamespace("BiocManager",lib.loc=inst.lib,quietly=TRUE)
      }
      # unzip the single MANIFEST file
      manifest <- read.dcf(unz("VE-Installer_Windows-R4.4_2025-03-21.zip","MANIFEST","r"))
      manifest <- manifest[1,] # turn matrix into named character vector
      pkgType <- manifest["pkgType"]
      destination <- sub("/","",manifest["Destination"])
      ve.repos <- dir.name(retrieved)
      exdir <- file.path(ve.repos,destination)
      message("Would unzip: ",retrieved)
      message("Into       : ",exdir)
      unzip(retrieved,exdir=exdir)
      # Now install those packages plus dependencies online that may be needed
      install.contriburl <-contrib.url(paste0("file:///",ve.repos),type=pkgType),
      all.contriburl <- c(
        install.contrburl,
        contrib.url(BiocManager::repositories(),type=pkgType) # includes https://cran.r-project.org
      )
      Message("install.packages from these locations:")
      print(contriburl)
      available <- available.packages(contriburl=install.contriburl,type=pkgType)
      # install.packages(available,contriburl=all.contriburl,lib=ve.lib,type=pkgType)
    }
    return(
      function() {
        message("Would require VEStart, then startVisionEval")
#         if ( ! require(VEStart,quietly=TRUE) ) stop("Installation failed: could not load VEStart")
#         startVisionEval()
      }
    )
  } else if ( installType == "BuildSource" ) {
    # Unzip to downloads (zip will have an inner top directory)
    message("Would unzip: ",retrieved)
    exdir <- dirname(retrieved)
    message("Into:        ",exdir)
    exname <- sub("/$","",unzip(retrieved,list=TRUE)[1,"Name"])

    # Unzip the build source distribution (may take a while!)
    ve.source.root <- file.path(ve.home,"build-source")
    if ( dir.exists(ve.source.root) ) {
      message("build-source directory already exists.")
      stop("Please remove ",ve.source.root," and try install again")
    }
    # unzip(retrieved,exdir=exdir) # creates exname subdirectory
    file.rename(file.path(exdir,exname),ve.source.root)

    # Point VE-Bootstrap.R to the right stuff
    Sys.setenv(VE_SOURCE=file.path(ve.source.root,"sources"))
    
    return(
      function() {
        message("Would source this file to Bootstrap VE:)
        message(bootstrap <- file.path(ve.source.root,"VE-Bootstrap.R")
#         bootstrap <- file.path(ve.source.root,"VE-Bootstrap.R")
#         if ( ! file.exists(bootstrap) ) stop("Installation failed: could not load VE-Bootstrap.R")
#         source(bootstrap)
      }
    )
  }
  return( function() { message("The installation did not finish properly. Please retry.") } )
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
