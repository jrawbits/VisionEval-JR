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
ve.home <- Sys.getenv("VE_INSTALL",NA)  # VE_INSTALL can be used as a bare VE_HOME for testing
if ( is.na(ve.home) ) {
  ve.home <- if ( ! "ve.home" %in% ve.env.list ) {
    ve.env$ve.home <- Sys.getenv("VE_HOME",getwd())
  } else {
    ve.env$ve.home
  }
} else {
  Sys.setenv(VE_HOME=ve.home) # override VE_HOME with VE_INSTALL just for testing
}

if ( ! dir.exists(ve.home) ) dir.create(ve.home,recursive=TRUE)
setwd(ve.home)

if ( length(dir(ve.home)) > 0 ) {
  confirm <- tkmessageBox(
    title = "Invalid VE_HOME Directory", icon = "warning", type = "yesno",
    message = paste(
      "The VE_HOME directory for installation is not empty:\n",
      ve.home,"\n\n",
      "Would you like to find or create a different one?"
    )
  )
  if ( as.character(confirm) != "yes" ) {
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
if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib)) # will remove extra libraries

# Create another library for instalation packages (yaml, rjson, BiocManager)
inst.lib <- file.path(ve.home,"ve-inst-lib (remove)",ve.env$two.digit.R)
if ( ! dir.exists(inst.lib) ) dir.create(inst.lib,recursive=TRUE)

####### Load Configuration File

config.file <- file.path(ve.home,"ve-install-config.yml")

# Default repository list for releases
default.ve.repository <- list(
  list(user="jrawbits",repository=c("visioneval-jr","visioneval-40")),   # test repository
  list(user="visioneval",repository=c("visioneval-dev","visioneval-40"))  # public repository
)

if ( ! requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE) ) {
install.packages("yaml",repos="https://cloud.r-project.org",lib=inst.lib)
requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE)
}

install.config <- if ( file.exists(config.file) ) {
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
    ve.distributions=default.ve.repository
  )
  config.txt <- yaml::as.yaml(default.config)
  response <- tkmessageBox(
    title = "Install Defaults?", icon = "question", type = "yesno",
    message = paste0(
      "Installing VisionEval in '",ve.home,"'\n\n",
      "You haven't set up ve-install-config.yml yet.\n\n",
      "Do you want to install VisionEval using these defaults?\n\n",
      config.txt
    )
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
#         TODO: Uncomment the above to save release data for later review
#         writeLines(rjson::toJSON(releases,indent=2),con="Raw-release.json")

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
          } # else silently skip releases that do not have VE40 installers
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
#     message("Saving test file")
#     writeLines(rjson::toJSON(all.releases,indent=2),con=test.file)
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
      inst_list <- all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]]
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

    tkbind(tt, "<Return>", function() {
      tkinvoke(ok_button, "command") #invokes the command assigned to the button.
    })

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
    "WinLibrary-R",
    "Windows-R",
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
    message("Install can be found in ",download)
    message("installer$file is ",installer$file)
    message("For a clean install, remove the downloads directory in VE_HOME")
  }
  retrieved <- dir(download,full.names=TRUE,pattern=installer$file)
  attr(retrieved,"InstallType") <- installTypeOf(retrieved)
  message("InstallType is ",attr(retrieved,"InstallType"))
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
  confirm <- tkmessageBox(
    title = "Complete Installation?", icon = "question", type = "yesno",
    message = paste(
      "Ready to install:\n\n",retrieved,
      "\nInstallation Type: ",installType,  
      "\n\nWould you like to proceed?"
    )
  )
  if ( as.character(confirm) != "yes" ) {
    stop("Installation cancelled. Restart to try again.",call.=FALSE)
  }

  if ( installType %in% c("WinLibrary","Windows","Source") ) {
    if ( installType == "WinLibrary" ) {
      # This is a pre-installed WinBinary, with all dependencies (like VE installers before 4.0)
      # Unzip directly into ve.lib
      lst <- unzip(retrieved,list=TRUE)
      # Find any packages already in ve.lib and remove those
      replacements <- file.path(ve.lib,sub("/$","",lst[grep("^[^/]+/$",lst$Name),"Name"]))
      replacements <- replacements[dir.exists(replacements)]
      if ( length(replacements) > 0 ) {
        unlink(replacements,recursive=TRUE)
      }
      # Unzip the replacement packages straight into ve.lib
      message("Unzipping : ",retrieved)
      message("Into      : ",ve.lib)
      unzip(retrieved,exdir=ve.lib) # simply extract the download back into ve-lib
    } else {
      # Ensure presence of needed packages
      if ( ! requireNamespace("BiocManager",lib.loc=inst.lib,quietly=TRUE) ) {
        install.packages("BiocManager",repos="https://cloud.r-project.org",lib=inst.lib)
        requireNamespace("BiocManager",lib.loc=inst.lib,quietly=TRUE)
      }
      # unzip the single MANIFEST file
      mfc<-unz(retrieved,"MANIFEST","r")
      manifest <- read.dcf(mfc)
      close(mfc)
      manifest <- manifest[1,] # turn matrix into named character vector
      pkgType <- manifest["pkgType"]
      destination <- sub("/","",manifest["Destination"])
      ve.repos <- dirname(retrieved)
      exdir <- file.path(ve.repos,destination)
      message("Unzipping : ",retrieved)
      message("Into      : ",exdir)
      unzip(retrieved,exdir=exdir)
      # Now install those packages plus dependencies online that may be needed
      install.repos <-paste0("file:///",ve.repos)
      all.repos <- c(
        install.repos,
        BiocManager::repositories() # includes https://cran.r-project.org
      )
      available <- available.packages(repos=install.repos,type=pkgType)
      packages <- available[,"Package"]
      install.packages(pkgs=packages,repos=all.repos,lib=ve.lib,type=pkgType)
    }
    return(
      # TODO: this appears to be using an earlier VE_HOME setup if that was hanging out
      # in the environment. Need to push our own notion of ve.home back through Sys.setenv
      # so we get the right ve-lib.
      function() {
        if ( ! require(VEStart,quietly=TRUE) ) stop("Installation failed: could not load VEStart")
        startVisionEval(ve.runtime=NA) # ve.runtime=NA says to ignore any VE_RUNTIME set in environment
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
    unzip(retrieved,exdir=exdir) # creates exname subdirectory
    file.rename(file.path(exdir,exname),ve.source.root)

    # Point VE-Bootstrap.R to the right stuff
    Sys.setenv(VE_SOURCE=file.path(ve.source.root,"sources"))
    
    return(
      function() {
        bootstrap <- file.path(ve.source.root,"VE-Bootstrap.R")
        if ( ! file.exists(bootstrap) ) stop("Installation failed: could not load VE-Bootstrap.R")
        source(bootstrap)
      }
    )
  }
  return( function() { message("The installation did not finish properly. Please retry.") } )
}

####### Run the configured installation

launch <- installVisionEval(install.config)
if ( is.function(launch) ) launch() else stop(call.=FALSE,"Installation failed:\n",as.character(launch),"\nPlease retry.")
