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

cache.releases <- FALSE
# remove comment on the following to cache online release information (Warning: won't update)
# cache.releases <- TRUE

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
    ve.env$ve.home <- Sys.getenv("VE_HOME",NA)
    if ( is.na(ve.home) ) {
      ve.home <- getwd()
      home.from <- "getwd()"
    } else home.from <- "VE_HOME"
  } else {
    home.from <- "Existing ve.env"
    ve.env$ve.home
  }
} else {
  home.from <- "VE_INSTALL"
  Sys.setenv(VE_HOME=ve.home) # override VE_HOME with VE_INSTALL for the remainder of testing
  ve.env$ve.home <- ve.home
}

# Helper function for making an info button
question_button_font <- tkfont.create(size = 11)
info_button <- function(parent.frame,popup.text) {
  # Use \u2753 Unicode character for question mark
  tkbutton(
    parent.frame,
    text = "\u2753", font = question_button_font, fg="green",
    command = function() {
      tkmessageBox(message = popup.text, icon = "info")
    }
  )
}

# Confirm installation location

select.ve.home.dialog <- function(ve.home) {
  
  tt <- tktoplevel()
  tkwm.title(tt, "Select installation directory (VE_HOME)")
  tkwm.maxsize(tt, 800, 10000) # we don't expect to expand vertically

  tcl_original_VE_HOME <- tclVar(ve.home)
  tcl_VE_HOME          <- tclVar(ve.home)
  cancel_VE_HOME       <- tclVar(0)
  popup_open           <- tclVar(0)

  select_directory <- function() {
    if (tclvalue(popup_open)==0) {
      tclvalue(popup_open) <- 1
      tkconfigure(directory_button,state="disable")
      dir_path <- tclvalue(tkchooseDirectory(initialdir=ve.home,title="Select VE_HOME"))
      tclvalue(tcl_VE_HOME) <- if (dir_path != "") {
        dir_path
      } else {
        getwd()
      }
      tclvalue(popup_open) <- 0
      tkconfigure(directory_button,state="normal")
    }
  }

  directory_button <- tkbutton(tt, text = "Change VE_HOME", command = select_directory)
  tkgrid(directory_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)

  directory_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
  directory_label <- tklabel(directory_frame,textvariable=tcl_VE_HOME, justify="left")
  tkpack(directory_label,anchor="w",padx=5,pady=5) # pack inside frame

  tkgrid(directory_frame, column = 1, row = 0, sticky="ew", padx = 5, pady = 5)

  tkgrid(
    info_button(tt,
      paste(sep="",
        "VE_HOME is the directory where VisionEval will be installed. ",
        "This home directory should ideally be empty before you continue the installation.\n\n",
        "The default is the directory from which you started R. ",
        "You can use the 'Change VE_HOME' button to pick a different directory (or create a new on) for your installation.\n\n",
        "After the installation is finished, you can set a separate directory to hold your VisionEval models (VE_RUNTIME)."
      )
    ), column=2,row=0,padx=5,pady=5
  )

  # OK and Cancel buttons
  onOK <- function() {
    tkdestroy(tt)
  }
  onCancel <- function() {
    tclvalue(cancel_VE_HOME) <- 1
    tkdestroy(tt)
  }

  button_frame <- tkframe(tt)
  ok_button <- tkbutton(button_frame, text = "Select", command = onOK)
  cancel_button <- tkbutton(button_frame, text="Cancel", command = onCancel)
  tkgrid(ok_button,column=0,row=0,sticky="e",padx=5)
  tkgrid(cancel_button,column=1,row=0,sticky="w",padx=5)
  tkgrid(button_frame, column = 0, row = 1, columnspan=2, sticky="ew", padx = 5, pady = 5)

  tkgrid.columnconfigure(tt, 1, weight = 1) #Make the second column expandable.
  tkgrid.columnconfigure(button_frame,0, weight=1)
  tkgrid.columnconfigure(button_frame,1, weight=1)
  
  tkbind(tt, "<Return>", function() {
    onOK()
  })

  tkwait.window(tt)

  return (
    if ( tclvalue(cancel_VE_HOME) > 0 ) {
      NA
    } else if ( tclvalue(tcl_VE_HOME) != ve.home ) {
      tclvalue(tcl_VE_HOME)
    } else {
      ve.home
    }
  )
}

old.ve.home <- ve.home
ve.home <- select.ve.home.dialog(ve.home)
if ( is.na(ve.home) ) stop(call.=FALSE,"Installation cancelled at user request.")
if ( dir.exists(ve.home) ) {
  ve.env$ve.home <- ve.home
  # Salvage old ve-install-config.yml if it exists
  if ( file.exists( old.install.config.file <- file.path(old.ve.home,"ve-install-config.yml") ) ) {
    file.copy(old.install.config.file, ve.env$ve.home)
  }
  setwd(ve.home)
} else stop(call.=FALSE,"Installation Cancelled. VE_HOME directory does not exist")

ve.env$this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
ve.env$two.digit.R <- tools::file_path_sans_ext(this.R)

# Set ve-lib installation location
ve.env$ve.lib <- file.path(ve.home,"ve-lib",ve.env$two.digit.R)
if ( ! dir.exists(ve.env$ve.lib) ) dir.create(ve.env$ve.lib,recursive=TRUE)
if ( ! ve.lib %in% .libPaths() ) .libPaths(c(ve.lib)) # will remove extra libraries

# Create another library for instalation packages (yaml, rjson, BiocManager)
# That will enable us to install or update instances for the overall VE installation
inst.lib <- file.path(ve.home,"ve-inst-lib (remove)",ve.env$two.digit.R)
if ( ! dir.exists(inst.lib) ) dir.create(inst.lib,recursive=TRUE)

install.config.file <- file.path(ve.env$ve.home,"ve-install-config.yml")

####### Load Configuration File

if ( ! requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE) ) {
  install.packages("yaml",repos="https://cloud.r-project.org",lib=inst.lib)
  requireNamespace("yaml",lib.loc=inst.lib,quietly=TRUE)
}

# Edit installation configuration

default.ve.repository <- list(
  list(user="jrawbits",repository=c("visioneval-jr")),   # test repository
  list(user="visioneval",repository=c("visioneval-dev"))  # public repository
)
default.config <- list(ve.distributions=default.ve.repository)

load.install.config <- function() {

  # Create default configuration structure
  return (
    if ( file.exists(install.config.file) ) {
      load.config <- try( silent=TRUE, yaml::yaml.load_file(install.config.file) ) # will throw error if file is improperly configured
      if ( ! is.list(load.config) ) default.config else load.config
    } else default.config
  )

}

edit.install.config <- function() {
  config <- load.install.config() # re-read the configuration

  # Initialize cfg.data
  parse.config <- function(distributions) {
    cfg.data <- data.frame()
    # distributions is a list of lists, each of which describes repositories for a user
    # The inner list has a "user" name and a vector of "repository" names for that user.
    for ( user in distributions ) {
      username <- user$user
      for ( repository in user$repository ) { # list of repositories for user
        cfg.data <- rbind(cfg.data,data.frame(user=username,repository=repository))
      }
    }
    cfg.data[order(cfg.data$user,cfg.data$repository),]
  }
  cfg.data <- parse.config(config$ve.distributions)

  # Start building the tcltk dialog
  tt <- tktoplevel()
  tkwm.title(tt, "Edit Install Configuration")

  changed <- tclVar("No") # change to "Yes" below when edited config needs to be re-loaded.

  newUser <- tclVar("")
  newRepo <- tclVar("")

  # Make the configuration frame
  config_frame <- tkframe(tt) # create this, expecting to destroy it again the first time we build

  build.config.frame <- function() {
    # Returns a tkframe with a set of rows inside it representing the objects
    # Create a data.frame to hold pairs of tclVar objects for each row

    # message("Building config frame:")
    # print(cfg.data)

    # Map the parsed config into a set of display rows (each of which will have a delete button)
    grid.info <- tcl("grid", "info", config_frame)
    if ( length( grid.info ) > 0 ) {
      print(class(grid.info))
      # message("removing config_frame")
      tcl("grid","remove",config_frame)
      tkdestroy(config_frame)
      config_frame <<- NULL
    }

    config_frame <<- tkframe(tt)
    tkgrid(tklabel(config_frame,text="User/Org"),row=0,column=0,padx=5,pady=5)
    tkgrid(tklabel(config_frame,text="Repository"),row=0,column=1,padx=5,pady=5)
      
    create.trash.button.command <- function(row) {
      as.character(row)
      return(
        function() {
          # message("Removing row ",row," from cfg.data")
          print(cfg.data)
          cfg.data <<- cfg.data[-row,]
          build.config.frame()
        }
      )
    }
    want.trash.button <- nrow(cfg.data) > 1
    tcl.rows <- list()
    for ( row in 1:nrow(cfg.data) ) {
      tcl.row <- list( row=row, user=tclVar(cfg.data$user[row]), repository=tclVar(cfg.data$repository[row]) )
      tcl.rows[[row]] <- tcl.row
      
      user.edit <- tkentry(config_frame,textvariable=tcl.row$user)
      repository.edit <- tkentry(config_frame,textvariable=tcl.row$repository)
      tkconfigure(user.edit,state="readonly")
      tkconfigure(repository.edit,state="readonly")
      tkgrid(user.edit,row=row,column=0,padx=5,pady=5)
      tkgrid(repository.edit,row=row,column=1,padx=5,pady=5)

      if ( want.trash.button ) {
        trash.button <- tkbutton(config_frame,text="X",fg="red",
          command = create.trash.button.command(row))
        tkgrid(trash.button,row=row,column=2,padx=5,pady=5)
      }
    }
    # message("(Re-)displaying config_frame")
    tkgrid(config_frame,row=1,column=0,sticky="ew",padx=5,pady=5)
    tclvalue(newUser) <- "" # clear these for further input
    tclvalue(newRepo) <- ""
  }

  # Create brief instructions row
  instructions <- tklabel(tt,text="Github repositories to search for core VisionEval releases.")
  tkgrid(instructions,row=0,column=0,sticky="w",padx=5,pady=5)

  # make the configuration frame
  build.config.frame()

  # make a row of tkentry items to gather a new user / repository pair, with a "+" button to
  # add them at the end of cfg.data and call build.config.frame

  entry_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
  user_entry  <- tkentry(entry_frame, textvariable = newUser)
  repo_entry  <- tkentry(entry_frame, textvariable = newRepo)
  new_button  <- tkbutton(entry_frame, text = "Add", fg="green", command = function() {
    nu <- tclvalue(newUser)
    nr <- tclvalue(newRepo)
    if ( all( nzchar(c(nu,nr)) ) ) {
      # Add a row to cfg.data
      # message("Before adding:")
      # print(cfg.data)
      cfg.data <<- rbind(cfg.data,data.frame(user=nu,repository=nr))
      # message("After adding:")
      # print(cfg.data)
      # message("Rebuilding config_frame inside add function")
      build.config.frame()
    } else {
      # Can't add unless both variables have something in them
      # message("No change")
      # message("nu = ",nu," and nr = ",nr)
      tclvalue(newUser) = ""
      tclvalue(newRepo) = ""
    }
  })
  tkgrid(user_entry,row=0,column=0,padx=5,pady=5)
  tkgrid(repo_entry,row=0,column=1,padx=5,pady=5)
  tkgrid(new_button,row=0,column=2,padx=5,pady=5)
  tkgrid(entry_frame,row=2,column=0,sticky="ew",padx=5,pady=5)

  # buttons to "Reset", "Save", or "Return" in a row below the entry frames
  # Reset button sets cfg.data to the default repositories
  #  (but does NOT save it; need to also press Save)
  # Save button repacks the user/repository controls into hierarchical structure, saves it,
  #   and returns "changed<-TRUE" leading to retry
  # Return button cancels dialog without changing anything ("changed<-FALSE")
  onReset <- function() {
    # Reset button returns to edit defaults (without changing any saved file)
    # User still needs to save in order to apply this configuration
    cfg.data <- parse.config(default.config)
    build.config.frame(cfg.data)
  }
  onSave <- function() {
    # iterate cfg.data into a hierarchical list by user / repository
    new.distributions <- list()
    for ( row in 1:nrow(cfg.data) ) {
      new.distributions[[row]] <- list(user=cfg.data$user[row],repository=cfg.data$repository[row])
    }
    yaml::write_yaml(list(ve.distributions=new.distributions), install.config.file, indent=2) # ve.env$ve.home/ve-install-config.yml
    tclvalue(changed) <- "Yes"
    tkdestroy(tt)
  }
  onReturn <- function() {
    tclvalue(changed) <- "No"
    tkdestroy(tt)
  }

  # Window action buttons
  button_frame <- tkframe(tt)
  reset_button  <- tkbutton(button_frame, text = "Reset", command = onReset)
  save_button   <- tkbutton(button_frame, text = "Save", command = onSave)
  return_button <- tkbutton(button_frame, text = "Return", command = onReturn)
  tkgrid(reset_button,row=0,column=0,padx=5,pady=5)
  tkgrid(save_button,row=0,column=1,padx=5,pady=5)
  tkgrid(return_button,row=0,column=2,padx=5,pady=5)
  tkgrid(button_frame,row=3,column=0,padx=5,pady=5)

  # Run the dialog
  tkfocus(tt)
  tkwait.window(tt)

  return(tclvalue(changed))
}

####### process the installation

installVisionEval <- function(cache=cache.releases) { # no function parameters right now
  installer <- selectInstaller(cache=cache)   # pick an available installer (config handled internally)
  retrieved <- fetchInstaller(installer) # confirms downloaded location and MANIFEST type
  launch    <- doInstallation(retrieved) # launch selects "end user" or "builder"
}

# TclTk dialog to present a list box with choices
# Used to select repositories (if more than one configured), releases, and installers

select.from.list <- function(parent_window, dest_var, items, title="Make a Selection") {
  tt <- tktoplevel(parent = parent_window)
  tkwm.title(tt, title)

  original_var <- tclVar(tclvalue(dest_var))

  Instructions <- tklabel(tt,text=title,justify="center")
  tkgrid(Instructions, row=0, column=0, sticky="ew",pady=5)

  lb.frame <- tkframe(tt,borderwidth=2,relief="solid")
  lb1 <- tklistbox(lb.frame, selectmode = "single", height=0, width=0)

  tkgrid(lb1,row=0,column=0,padx=5,pady=5,sticky="ew")
  tkgrid.columnconfigure(lb.frame,0,weight=1)
  for (item in items) {
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

  tkwait.window(tt) # Run the dialog
}

# Helper dialog for selecting installer type (for which releases will be offered)

get.buildtype.dialog <- function() {
  # Dialog values to update
  # Keep track of whether we're doing a runtime or build installation
  # And which repository, release and installer we've selected

  tt <- tktoplevel()
  tkwm.title(tt, "Set up Installation")

  build.type <- tclVar("Release")                 # Options: Release, Build Release Snapshot, Build Local Clone

  options = c("Release","Build Release Snapshot", "Build Local Clone")

  # Build Type Selector
  combo <- tklistbox(tt, height = length(options), selectmode = "single", exportselection = FALSE)
  for (option in options) {
    tkinsert(combo, "end", option)
  }

  # Installation Type Dialog

  radio_frame <- tkframe(tt)
  radio1 <- tkradiobutton(radio_frame, text = "Pre-Built Release (recommended)", variable = build.type, value = options[1])
  radio2 <- tkradiobutton(radio_frame, text = "Build from Release Code", variable = build.type,         value = options[2])
  radio3 <- tkradiobutton(radio_frame, text = "Build from Local Clone", variable = build.type,          value = options[3])

  tkgrid(radio1,row=0,column=0,padx=5,pady=2,sticky="w")
  tkgrid(radio2,row=1,column=0,padx=5,pady=2,sticky="w")
  tkgrid(radio3,row=2,column=0,padx=5,pady=2,sticky="w")
  tkgrid(radio_frame,row=0,column=0,padx=5,pady=5,sticky="w")

  instructions <- paste(sep="",
    "Select how you would like to install VisionEval.\n\n",
    "* Pre-Built Release, the recommend choice, will show available VisionEval releases for your version of R.\n\n",
    "* Build from Release Code will let you select a (very large) release zip file and run its build script.\n\n",
    "* Build from Local Clone is the preferred way to build VisionEval from the ground up.",
    "You will need to install Git, clone the repository, and check out the branch you would like to build",
    " before re-running this installation script.\n\n",
    "Building from a Local Clone is recommended",
    " if you are planning to make code changes you would like to save or to contribute back to the VisionEval project.\n\n",
    "NOTE: If there is not a pre-built release for your version of R or your operating system, you can still select 'Pre-Built Release' ",
    " but it will require you to build the VisionEval packages.\n\n",
    "For any of the build options on Windows, or if there is no pre-built release available, you will need to have installed RTools.",
    "See the VisionEval online documentation for information on obtaining RTools. Mac OS and Linux usually already have the required tools."
  )

  onOK <- function() tkdestroy(tt)
  onCancel <- function() {
    tclvalue(build.type) <- "Cancel"
    tkdestroy(tt)
  }

  button_frame <- tkframe(tt)
  ok_button <- tkbutton(button_frame, text = "OK", command = onOK)
  cancel_button <- tkbutton(button_frame, text = "Cancel", command = onCancel)
  tkgrid(ok_button, row = 0, column = 0, padx = 5, pady = 5, sticky="w")
  tkgrid(cancel_button, row = 0, column = 1, padx = 5, pady = 5, sticky="w")
  tkgrid(info_button(button_frame,instructions), row = 0, column = 2, padx = 5, pady = 10, sticky="e")
  tkgrid.columnconfigure(button_frame,2,weight = 1)
  tkgrid(button_frame,row=1,column=0,padx=5,pady=5,sticky="ew")
  tkgrid.columnconfigure(tt, 0, weight = 1)

  tkwait.window(tt) # Run the dialog

  return( tclvalue(build.type) )
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

getReleases <- function(build.type,cache=FALSE) {
  # Option to cache results during testing so as not to hit Github API over and over (they are rate limited)
  if ( ! requireNamespace("rjson",lib.loc=inst.lib,quietly=TRUE) ) {
    install.packages("rjson",repos="https://cloud.r-project.org",lib=inst.lib)
    requireNamespace("rjson",lib.loc=inst.lib,quietly=TRUE)
  }
  if ( file.exists(test.file<-paste0("Test-Skip-Download-",build.type,".json")) ) {
    # NOTE: uncomment lines below to save JSON locally for testing
    # You don't want to do that generally since it will block release updates.
    all.releases <- rjson::fromJSON(file=test.file,simplify=FALSE)
    message("\n##### USING TEST FILE for ",build.type," !!! #####")
  } else {
    # Gather information about Releases
    # 1. Pre-built release (download) "Release"
    # 2. Snapshot of a release (download) "Build Release Snapshot" - skips installer selection and returns zipball
    # 3. Repository clone (already present) "Build Local Clone" - returns empty all.releases
    all.releases <- list()
    if ( build.type == "Build Local Clone" ) {
      all.releases <- ve.env$ve.home
      return(all.releases) # no releases: hunt for local github clone with a VE-Bootstrap.R
    }
    config <- load.install.config() # ve-install-config.yml may be changed by get.buildtype.dialog
    for ( distro in config$ve.distributions ) {
      dist.user <- distro$user
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
        if ( cache ) {
          # Cache results during testing (only saves one; use for inspecting structure)
          writeLines(rjson::toJSON(releases,indent=2),con="Raw-release.json")
        }
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
          if (build.type == "Build Release Snapshot" ) {
            installers <- list()
            zbname <- paste0("Build_Source_",r$id,".zip")
            zipball <- list()
            zipball[[zbname]] <- list(
              timeout = 1200,
              url     = r$zipball_url,
              file    = zbname
            )
            installers[[zbname]] <- zipball
            r.temp$assets <- zipball
            release.data[[length(release.data)+1]] <- r.temp
          } else {
            # Full list of release assets

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
              } # else {
                #   message(a.temp$file," is NOT an installer (",installer.pattern,")")
                # }
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
        }
        if ( length(release.data) > 0 ) {
          names(release.data) <- sapply( release.data,function(r) paste0(r$release,":",r$date) )
          all.releases[[repo.name]] <- release.data
        } else {
          message("No releases found with valid installers in ",repo.name)
        }
      }
    }
    # If we're doing standard releases, create a virtual release for installers we may have built
    # locally. This is intended for testing the install script with updated local intallers.
    if ( build.type == "Release" && ! is.na(ve.build <- Sys.getenv("VE_BUILD",NA) ) ) {
      if ( dir.exists(local.releases <- file.path(ve.build,"install") ) ) { # Locally built installers
        release.data <- list()
        for ( release.name in rev(dir(local.releases,full.names=TRUE)) ) {
          if ( ! dir.exists(release.name) ) next # may be a file not a folder
          local.installers <- rev(dir(release.name,full.names=TRUE))
          local.installers <- local.installers [ # put them in order of desirability
            c(
              grep("Windows",local.installers),
              grep("WinLibrary",local.installers),
              grep("Source",local.installers)
            )
          ]
          local.files <- list()
          for ( file in local.installers ) {
            if( isVEInstaller(file) ) {
              local.files[[basename(file)]] <- list(
                timeout = 0,
                url     = "file",
                file    = file
              )
            }
          }
          if ( length(local.files) > 0 ) {
            release.name <- basename(release.name)
            r.temp <- list( 
              release=release.name,
              date=as.character(Sys.Date()),
              id=0,
              assets=local.files
            )
            release.data[[release.name]] <- r.temp
          }
        }
        if ( length(release.data) > 0 ) {
          all.releases[["Locally Built Installers"]] <- release.data
        } else message("No local installers have been built for ",this.R)
      }
    }
    if ( cache ) {
      # Cache release results for use during testing
      message("Saving test file")
      writeLines(rjson::toJSON(all.releases,indent=2),con=test.file)
      stop(call.=FALSE,"Stop to review all.releases")
    }
  }
    
  return(all.releases)
}

############ Dialog to pick an installer from among those available

# Setup dialog navigates all.releases to select an installer to download and install
setup.dialog <- function(build.type,all.releases,max_width=800) {
  # Dialog values to update
  # different dialog structure depending on build.type
  # "Release" - the classic dialog, presenting repository, release and installer
  # "Build Release Snapshot" - Will work the same, but the installer will the source code zipball
  # "Build Local Clone" - will just run a directory chooser dialog and complain if the directory
  #    does not have a VE-Bootstrap.R script in it.
  # The buttons at the bottom of this dialog should be "Install", "Cancel", and "?" for help
  # The change build type row just presents the selected build type, and picking its button
  #   actually cancels this dialog and returns "TryAgain" in the selected$DoIt element

  # And which repository, release and installer we've selected
  repository <- tclVar("No Repository")  # List box selector from names(all.releases)
  release <- tclVar("No Release")        # List box selector from names(all.releases[[repository]])
  installer <- tclVar("No Installer")    # List box selector for default installer
  doit <- tclVar("No")                   # Change this if user chooses "Install"

  VEValidClone <- function(dir_path) {
    exists <- dir.exists(dir_path) && length ( dir(dir_path,pattern="VE-Bootstrap.R") ) > 0
  }

  # Here's the GUI driver that shows what has been selected to install and allows
  # the user to pick something different.
  tt <- tktoplevel()
  tkwm.title(tt, "Select Installer")
  tkwm.maxsize(tt, max_width, 10000) # we don't expect to expand vertically

  # Actions to gather information
  runtime_button <- tkbutton(tt, text = "Change Installation Type", command = function() {
    tclvalue(doit) <- "TryAgain"
    tkdestroy(tt)
  })
  tkgrid(runtime_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
  runtime_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
  runtime_label <- tklabel(runtime_frame,text=build.type, justify="left")
  tkpack(runtime_label,anchor="w",padx=5,pady=5)
  tkgrid(runtime_frame, column = 1, row = 0, sticky="ew", padx = 5, pady = 5)

  if ( build.type != "Build Local Clone" ) {
    # Classic Dialog
    tclvalue(repository) <- names(all.releases)[1]
    tclvalue(release) <- names(all.releases[[1]])[1]
    tclvalue(installer) <- names(all.releases[[1]][[1]][["assets"]])[1]

    # Button management
    disable_buttons <- function() {
      tkconfigure(repos_button,state="disabled")
      tkconfigure(release_button,state="disabled")
      tkconfigure(installer_button,state="disabled")
      tkconfigure(repos_config,state="disabled")
    }
    enable_buttons <- function() {
      tkconfigure(repos_button,state="normal")
      tkconfigure(release_button,state="normal")
      tkconfigure(installer_button,state="normal")
      tkconfigure(repos_config,state="normal")
    }

    # Button definitions
    repos_button <- tkbutton(tt, text = "Repository", command = function() {
      disable_buttons()
      repos.list <- names(all.releases)
      if ( length(repos.list) < 2 ) return() # Button does nothing if not enough items

      # Run the selection dialog, then look up the selected release and choose the
      # default installer from that release.
      select.from.list(tt,repository,repos.list) # will update repository variable
      release_list <- all.releases[[tclvalue(repository)]]
      tclvalue(release) <- names(release_list)[1] # reset to first release
      inst_list <- release_list[[tclvalue(release)]][["assets"]]
      tclvalue(installer) <- names(inst_list)[1]
      enable_buttons()
    })

    release_button <- tkbutton(tt, text = "Release", state="normal", command = function() {
      disable_buttons()
      release_list <- names(all.releases[[tclvalue(repository)]])
      if ( length(release_list) < 2 ) return() # Do nothing if too few items

      # If release changes, change the installer to the default one.
      select.from.list(tt,release,release_list) # will update repository variable
      inst_list <- all.releases[[tclvalue(repository)]][[tclvalue(release)]][["assets"]]
      tclvalue(installer) <- names(inst_list)[1]
      enable_buttons()
    })

    installer_button <- tkbutton(tt, text = "Installer", state="normal", command = function() {
      disable_buttons()
      installer_list <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][["assets"]])
      if ( length(installer_list) < 2 ) return() # Do nothing if there are too few installers
      select.from.list(tt,installer,installer_list) # will update installer variable
      enable_buttons()
    })

    # Display the buttons-
    tkgrid(repos_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
    tkgrid(release_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
    tkgrid(installer_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)
    button_row = 4

    # Display the values set by the buttons in label widgets
    # Put the widgets in frames so they resize nicely
    repos_outer_frame <- tkframe(tt)
    repos_frame <- tkframe(repos_outer_frame, borderwidth = 2, relief = "groove")
    repos_label <- tklabel(repos_frame,textvariable=repository, justify="left")
    repos_config <- tkbutton(repos_outer_frame,text="Add Repositories",state="normal",command = function() {
      disable_buttons()
      changed <- edit.install.config()
      if ( changed == "Yes" ) {
        tclvalue(doit) <- "TryAgain"
        tkdestroy(tt)
      }
      enable_buttons()
    })
    tkgrid(repos_frame,row=0,column=0,sticky="ew",padx=5,pady=5)
    tkgrid(repos_label,row=0,column=0,sticky="w",padx=5,pady=5)
    tkgrid(repos_config,row=0,column=1,padx=5,pady=5) # edit install config button
    tkgrid.columnconfigure(repos_outer_frame,0,weight = 1)
    tkgrid(repos_outer_frame, column = 1, row = 1, sticky="ew", padx = 5, pady = 5)

    release_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    release_label <- tklabel(release_frame,textvariable=release, justify="left")
    tkpack(release_label,anchor="w",padx=5,pady=5)
    tkgrid(release_frame, column = 1, row = 2, sticky="ew", padx = 5, pady = 5)

    installer_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    installer_label <- tklabel(installer_frame,textvariable=installer, justify="left")
    tkpack(installer_label,anchor="w",padx=5,pady=5)
    tkgrid(installer_frame, column = 1, row = 3, sticky="ew", padx = 5, pady = 5)

    instructions <- if ( build.type == "Release" ) {
      "Release instructions"
    } else {
      "Build Release Snapshot instructions"
    }
  } else { # Build Local Clone
    tclvalue(repository) <- all.releases[[1]] # set in getReleases to ve.home
    repos_button <- tkbutton(tt, text = "Repository Clone Directory", command = function() {
      tkconfigure(repos_button,state="disabled")
      dir_path <- tclvalue(tkchooseDirectory())
      tclvalue(repository) <- if ( ! VEValidClone(dir_path) ) paste(dir_path,"(Not a clone)") else dir_path
      tkconfigure(repos_button,state="normal")
    })
    tkgrid(repos_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)

    repos_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    repos_label <- tklabel(repos_frame,textvariable=repository, justify="left")
    tkpack(repos_label,anchor="w",padx=5,pady=5)
    tkgrid(repos_frame, column = 1, row = 1, sticky="ew", padx = 5, pady = 5)

    button_row <- 2
    instructions <- "Local clone instructions"
  }

  # OK and Cancel buttons
  onOK <- function() {
    if ( build.type == "Build Local Clone" && ! VEValidClone(tclvalue(repository)) ) {
      tclvalue(doit) <- "TryAgain"
    } else {
      tclvalue(doit) <- "Install"
    }
    tkdestroy(tt)
  }

  onCancel <- function() {
    tclvalue(doit) <- "Cancel"
    tkdestroy(tt)
  }

  button_frame <- tkframe(tt)
  ok_button <- tkbutton(button_frame, text = "Install", command = onOK)
  cancel_button <- tkbutton(button_frame, text = "Cancel", command = onCancel)

  tkgrid(ok_button, row = 0, column = 0, padx = 5, pady = 5, sticky="w")
  tkgrid(cancel_button, row = 0, column = 1, padx = 5, pady = 5, sticky="w")
  tkgrid(info_button(button_frame,instructions), row = 0, column = 2, padx = 5, pady = 10, sticky="e")
  tkgrid.columnconfigure(button_frame,2,weight = 1)
  tkgrid(button_frame,row=button_row,column=0,padx=5,pady=5,sticky="ew")

  tkgrid.columnconfigure(tt, 1, weight = 1) #Make the second column in the main frame expandable.

  # These will only work if the focus is on the TK dialog (which doesn't seem to be able to happen
  # without clicking into the dialog window).
  tkbind(tt, "<Return>", function() {
    onOK()
  })
  tkbind(tt, "i", function() {
    onOK()
  })

  tkfocus(tt)
  tkwait.window(tt) # Run the dialog

  return( # Relay tclVar variables back out to calling environment
    list(
      Runtime=build.type,
      Repos=tclvalue(repository),
      Release=tclvalue(release),      # Ignored for Build Local Clone
      Installer=tclvalue(installer),  # Ignored for Build Local Clone
      DoIt=tclvalue(doit)
    )
  )
}

selectInstaller <- function(cache=FALSE) {

  repeat {
    build.type <- get.buildtype.dialog()
    if ( is.na(build.type) || build.type=="Cancel" ) stop(call.=FALSE,"Installation cancelled from build type selection dialog.")
    all.releases <- getReleases(build.type,cache=cache)
    selected <- setup.dialog(build.type,all.releases) # different dialog versions depending on release type
    if ( ! selected$DoIt == "TryAgain") {
      if ( selected$DoIt != "Install" ) stop(call.=FALSE,"Installation cancelled from installer selection dialog")
      break
    }
  }

  if ( selected$Runtime != "Build Local Clone" ) {
    repo <- selected$Repos
    release <- selected$Release
    installer <- all.releases[[repo]][[release]][["assets"]][[selected$Installer]]
  } else {
    installer <- list(
      Repos=selected$Repos
    )
  }
  installer$installType <- selected$Runtime
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
  if ( installer$installType == "Build Local Clone" ) {
    retrieved <- installer$Repos
    attr(retrieved,"InstallType") <- "LocalClone"
  } else {
    download <- file.path(ve.home,"download")
    if ( ! dir.exists( download ) ) dir.create(download,recursive=TRUE)
    if ( ! basename(installer$file) %in% dir(download) ) { # Shorten restart if there was a previous download
      if ( installer$url == "file" ) {
        file.copy(installer$file, download) # Copy local installer
      } else { # need to download it
        options(timeout = max(installer$timeout, getOption("timeout")))
        message("Timeout: ",getOption("timeout")," seconds")
        destfile <- file.path(download,installer$file)
        download.file(installer$url,destfile=destfile,method="libcurl",mode="wb")
        # use method=libcurl so download.file follows redirect links
      }
    } else {
      message("Installer has already been downloaded.")
      message("Install can be found in ",download)
      message("installer$file is ",installer$file)
      message("For a clean install, remove the downloads directory in VE_HOME")
    }
    retrieved <- dir(download,full.names=TRUE,pattern=basename(installer$file))
    attr(retrieved,"InstallType") <- installTypeOf(retrieved)
  }
  message("InstallType is ",attr(retrieved,"InstallType"))
  invisible(retrieved) # name of downloaded file with attribute stating "Runtime" or "Build" install type
}

####### Perform the installation based on the downloaded installer type and information

doInstallation <- function(retrieved) {
  # Dispatch based on retrieved$localfile and InstallType attribute
  # LocalClone just launches VE-Bootstrap.R from the local clone directory
  # VE built installers:
  # WinLibrary replaces corresponding ve-lib packages with packages from installer
  #   - Remove them first based on folder names
  #   - Then copy the entire new package into place
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

  if ( installType == "LocalClone" ) {
    # LocalClone is just looking at a directory containing VE-Bootstrap.R
    # Point VE_SOURCE at it, then run its VE_Bootstrap.R
    Sys.setenv(
      VE_SOURCE=file.path(retrieved)
    ) # 
    return(
      function() {
        bootstrap <- file.path(retrieved,"VE-Bootstrap.R")
        if ( ! file.exists(bootstrap) ) stop("Installation failed: could not load ", bootstrap)
        source(bootstrap)
      }
    )
  } else {
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
          if ( ! file.exists(bootstrap) ) stop("Installation failed: could not load ",bootstrap)
          source(bootstrap)
        }
      )
    }
  }
  return( function() { message("The installation did not finish properly. Please retry.") } )
}

####### Run the configured installation

launch <- installVisionEval(cache.releases)
if ( is.function(launch) ) launch() else stop(call.=FALSE,"Installation failed:\n",as.character(launch),"\nPlease retry.")
