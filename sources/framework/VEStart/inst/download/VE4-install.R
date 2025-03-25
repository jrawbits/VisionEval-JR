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
ve.home    <- if ( ! "ve.home" %in% ve.env.list ) {
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

ve.lib     <- if ( ! "ve.lib" %in% ve.env.list || basename(ve.env$ve.lib) != ve.env$two.digit.R ) {
  ve.env$ve.lib <- file.path(ve.home,"ve-lib",ve.env$two.digit.R)
} else {
  ve.env$ve.lib
}

####### Load Configuration File

config.file <- file.path(ve.home,"ve-install-config.yml")

# Default repository list for releases
default.ve.repository <- list(
  list(user="jrawbits",repository=c("visioneval-jr","visioneval-40")),   # test repository
  list(user="visioneval",repository=c("visioneval-dev","visioneval-40"))  # public repository
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
  print(default.config)
  response <- tkmessageBox(
    title = "Install Defaults?", icon = "question", type = "yesno",
    message = paste0(
      "Installing VisionEval in '",ve.home,"'\n\n",
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

# Test file names for installer.pattern
# files <- c(
#   "VE-Installer_Source_2025-03-21.zip",
#   "VE-Installer_Windows-R4.4_2025-03-21.zip",
#   "VE-Installer_WinLibrary-R4.4_2025-03-21.zip"
# )

installer.pattern <- paste0(
  "VE-Installer_",                                          # .* will be the VE Version
  paste0("((WinLibrary|Windows)-R",two.digit.R,"|Source)"), # The type of installer 
  "_.*\\.zip$"
)

isVEInstaller <- function(filename) all(grepl(installer.pattern,filename))

getReleases <- function(config) {
  # Configured for testing so as not to hit Github API over and over (they are rate limited)
  if ( ! requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE) ) {
    install.packages("rjson",repos="https://cloud.r-project.org",lib=ve.lib)
    requireNamespace("rjson",lib.loc=ve.lib,quietly=TRUE)
  }
  if ( file.exists(test.file<-"Test-Skip-Download.json") ) {
    all.releases <- rjson::fromJSON(file=test.file,simplify=FALSE)
    message("\n##### USING TEST FILE !!! #####")
  } else {
    all.releases <- list()
    for ( distro in config$ve.distributions) {
      dist.user <- distro$user
      message("Github user: ",dist.user)
      for ( repo in distro$repository ) {
        repo.name <- paste(dist.user,repo,sep="/")
        message("Processing ",repo.name)
        # Check for repository existence
        repo.addr <- paste0("https://api.github.com/repos/",dist.user,"/",repo)
        check.repo <- base::curlGetHeaders(repo.addr)
        repo.status <- attr(check.repo,'status')
        if ( is.null(repo.status) || repo.status!=200 ) {
          message("Cannot Find Github Repository: ",repo.addr," (",repo.status,")")
          next
        }
        releases <- rjson::fromJSON(file=paste0(repo.addr,"/releases"))
        # writeLines(rjson::toJSON(releases,indent=2),con="Raw-release.json")

        release.data <- list()
        for ( r in releases ) {
          r.temp <- list(
            release=r$name,
            date=r$published_at,
            id=r$id
          )
          zbname <- paste0("Source_code_",r$id,".zip")
          zipball <- list()
          zipball[[zbname]] <- list(
            timeout = 1200,
            url     = r$zipball_url,
            file    = zbname
          )
          r.temp$zipball <- zipball

          message("Processing release: ",r$name)
          installers <- list()
          for ( a in r$assets ) {
            a.temp <- list(
              timeout = as.integer(round(a$size/750000,0)),
              url     = a$browser_download_url,
              file    = basename(a$browser_download_url)
            )
            if ( isVEInstaller( a.temp$file ) ) {
              message(a.temp$file," IS an installer")
              installers[[length(installers)+1]] <- a.temp
            } else {
              message(a.temp$file," is NOT an installer (",installer.pattern,")")
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
            message("Release: ",r.temp$release)
          } else {
            message("No installers found in ",r$name)
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
    message("Saving test file")
    writeLines(rjson::toJSON(all.releases,indent=2),con=test.file)
  }
  return(all.releases)
}

############ Dialog to pick an installer from among available

# TODO: may want to consider VE_BUILD and "install" folder there (see ve.make.installer in
# VEBuild/inst/build-scripts/01-build.R) so we can do local installations

# Load the tcltk package
library(tcltk)

# Setup dialog navigates all.releases to select an installer to download and install
setup.dialog <- function(all.releases,max_width=800) {
  # Dialog values to update
  runtime <- tclVar("Runtime")                                  # Checkbox selector from "Runtime" or "Build"
  repository <- tclVar(names(all.releases)[1])                  # List box selector from names(all.releases)
  release <- tclVar(names(all.releases[[1]])[1])                # List box selector from names(all.releases[[repository]])
  installer <- tclVar(names(all.releases[[1]][[1]][["assets"]])[1])  # List box selector from names(all.releases[[repository]][[release]]$assets)
  doit <- tclVar("No")                                          # Change this if user chooses "Install"

  getAssetType <- function() {
    if ( tclvalue(runtime)=="Runtime" ) "assets" else "zipball"
  }

  # Installation Type
  select_runtime <- function() {
    pick <- tkmessageBox(title = "Install Type", message = "Build Installation?", icon = "question", type = "yesno")
    if (as.character(pick) == "yes") {
      pick <- "Build"
    } else {
      pick <- "Runtime"
    }
    tclvalue(runtime) <- pick
    tclvalue(installer) <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])[1]
    print(names(all.releases[[tclvalue(repository)]][[tclvalue(release)]]))
    print(all.releases[[tclvalue(repository)]][[tclvalue(release)]])
    message("Asset Type: ",getAssetType())
    message("New installer: ",tclvalue(installer))
  }

  # Our own listbox function to pick from a list
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

  # Example 6: A simple GUI with buttons to trigger the dialogs
  create_dialog <- function(max_width=400) {
    tt <- tktoplevel()
    tkwm.title(tt, "Select Installer")
    tkwm.maxsize(tt, max_width, 10000) # we don't expect to expand vertically

    # Actions to gather information
    runtime_button <- tkbutton(tt, text = "Installation Type", command = select_runtime)
    repos_button <- tkbutton(tt, text = "Repository", command = function() {
      repos.list <- names(all.releases)
      if ( length(repos.list) < 2 ) return()
      select_from_list(tt,repository,repos.list) # will update repository variable
      release_list <- all.releases[[tclvalue(repository)]]
      tclvalue(release) <- names(release_list)[1] # reset to first release
      inst_list <- release_list[[tclvalue(release)]][[getAssetType()]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    release_button <- tkbutton(tt, text = "Release", state="normal", command = function() {
      release_list <- names(all.releases[[tclvalue(repository)]])
      if ( length(release_list) < 2 ) return()
      select_from_list(tt,release,release_list) # will update repository variable
      inst_list <- all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    installer_button <- tkbutton(tt, text = "Installer", state="normal", command = function() {
      installer_list <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])
      if ( length(installer_list) < 2 ) return()
      select_from_list(tt,installer,installer_list) # will update repository variable
    })

    # Display the buttons
    tkgrid(runtime_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
    tkgrid(repos_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
    tkgrid(release_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
    tkgrid(installer_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)

    # Display the values set by the buttons in label widgets  
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

    tkwait.window(tt)
    return(
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

  if (FALSE ) {
    # Select runtime installation by default
    runtime.installation <- TRUE

    # Filter available releases for R version and 4.x+ installer
    repos.list <- names(all.releases)   # List of repositories with releases

    # Select first repository
    repo <- repos.list[1] # default repository user/name

    # Select first (latest) release
    releases <- all.releases[[repo]] # List of releases in selected repository
    release <- releases[[1]]         # list of release properties (release, date, assets)

    # Put assets into priority order and select the first one
    assets <- release$assets
    installer <- assets[[1]] # list of default asset properties (file,size,url)
  } else {
    print(selected)
    if ( selected$DoIt == "Install" ) {
      runtime.installation <- selected$Runtime == "Runtime"
      repo <- selected$Repos
      release <- all.releases[[selected$Repos]][[selected$Release]]
      installer <- all.releases[[selected$Repos]][[selected$Release]]$assets[[selected$Installer]]
    } else stop("Installation cancelled from installer selection dialog",call.=FALSE)
  }

  # Ask if user wants Runtime or Build installation
  # Selection dialog for Runtime vs Build
  
  # If repos.list has more than one entry, make a selection dialog available

  # Select first release in repository
  # If select repository has more than one release, make a selection dialog available

  # List assets from selected release
  # if "Build" show zipball (no choice)
  # If "Runtime" select WinBinary, then WinLibary, then Source depending on available
  # If release has more than one installer, make a selection dialog available

  # Buttons at the bottom of the dialog are "install" or cancel

  installer$installType <- if(runtime.installation) "Runtime" else "Build"
  cat("Installation:",installer$installType,"\n")
  cat("Repository:",repo,"of",length(names(all.releases)),"\n")
  cat("Release:",release$release,paste0("(",release$date,")"),"of",length(names(all.releases[[repo]])),"\n")
  cat("Installer:",installer$file,"of",length(names(release$assets)),"\n")
  print(installer)
  return(installer)
}

####### Download the installer and report what was retrieved (or if it failed)

fetchInstaller <- function(installer) {
  # Dispatch on installer$pkgType
    # WinBinary Rx.y
    # WinLibrary Rx.y
    # Source
    # Zipball
  # Place downloaded file in ve.home/download
  # return downloaded file name
  download <- file.path(ve.home,"download")
  if ( ! dir.exists( download ) ) dir.create(download,recursive=TRUE)
  if ( ! installer$file %in% dir(download) ) {
    options(timeout = max(installer$timeout, getOption("timeout"))) # ten minute timeout; set dynamically based on reported file size?
    message("Timeout: ",getOption("timeout")," seconds")
    destfile <- file.path(download,installer$file)
    download.file(installer$url,destfile=destfile,method="libcurl",mode="wb") # use method=libcurl so it follows redirect links
  } else {
    message("Install is ",download)
    message("installer$file is ",installer$file)
    message("Download already present:")
    print(dir(download,pattern=installer$file))
  }
  retrieved <- dir(download,full.names=TRUE,pattern=installer$file)
  attr(retrieved,"InstallType") <- installer$installType 
  invisible(retrieved)
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
  message("Would unzip: ",retrieved)
  # TODO: sort out the unzip strategy; read the MANIFEST, etc
  message("Then process as ",attr(retrieved,"InstallType"))
  return( function() { message("This would be the launch function to start VE after installing") } )
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
