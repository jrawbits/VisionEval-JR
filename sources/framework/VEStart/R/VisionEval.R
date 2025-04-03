# Install, update and launch VisionEval system

# Plan for installation / VEStart running

# https://github.com/VisionEval/VisionEval-Dev/releases/download/VE-3.1.2/VE-3.1-PackageSources-R4.4.1_2024-10-10.zip

# Installation strategy

# 1. One-liner VE-get.R that user can copy from the website
# 2. Sources VE-install.R

# The VE installation process entails selecting an installer
# We could have the user source an online script to find and download the installer they need
# - SourceInstaller (requires RTools, plus online connection for (source) dependencies)
# - WindowsInstaller_Rx.y (requires compatible R version, plus online connection for dependencies)
# - WindowsInstaller_Rx.y_Offline (requires compatible R version, downloads VE + all dependencies)
# We can encode the R version based on what is running the install script. Show what is available
# for their version of R, forcing source install at a minimum - but that requires RTools.

# The script should insist on an empty directory and could send up the directory chooser dialog the
# way that VEStart does now (also with option to override desire for empty). Then it pulls down the
# installer and unzips it. Then it points to ve-lib in VE_HOME and will install.packages or
# update.packages using the manifest packageType, and finally it will load VEStart allowing
# startVisionEval().

# VEStart can presume that everything is installed and just move ahead to setting up ve.env
# and hosting startVisionEval().

# Move the dialogs to the installation script.

# So perhaps we need a ve.install() function that does the stuff above, then loads VEStart and runs
# startVisionEval()

# The startup file setup should create launch.bat, Startup.Rdata, .Rprofile, VisionEval.Rproj and
# .Renviron.

# We are not any longer doing installation from here: just making sure the startup files are in the
# right place. VE-Bootstrap.R (development startup) and VE-Install.R (online end user script) will
# create the ve.env environment and populate with default values. User should be instructed to
# edit .Renviron if they want to update (or perhaps have a VEStart function) to run dialogs to
# gather the file locations and set them up).
# Note that we won't let the user reset VE_HOME : to run any of the packages other than VEBuild,
# it needs to have ve-lib populated. So part of the install script should be picking VE_HOME and
# then setting up "install" and later "ve-lib" folders.

# VEStart makes sure that ve-lib is present in VE_HOME. Put it there with ve.build as well (but put
# the other build artifacts into VE_BUILD, defaulting to VE_HOME/built). Then VE_RUNTIME defaults to
# VE_HOME/runtime but can be moved anywhere. VEStart will also make sure the runtime is set up with
# a "models" folder, a stub visioneval.cnf with global settings, and the startup scripts (which can
# also be updated in VE_HOME.)

# If VE_HOME gets changed, we need to return to VE-install.R if "ve-lib" is not already present.

#### Now back to our regularly scheduled programming...

#CREATE ENVIRONMENT
# This environment is later copied into VEModel to keep track of ve.runtime, ve.home, etc.
# ve.env <- new.env()

#' Create the runtime environment and return it
#' @return the "ve.env" environment from the search path
getRuntimeEnvironment <- function() {
  if ( ! "ve.env" %in% search() ) {
    attach(NULL,name="ve.env")
  } else {
    as.environment("ve.env")
  }
}

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
#' @param overwrite If TRUE, force rewrite of startup files in VE_RUNTIME, otherwise continue if they exist
#' @param ve.lib.name Character string with name of ve-lib within VE_HOME (default "ve-lib")
#' @return location of VE_RUNTIME, invisibly
#' @import utils tcltk
#' @export
startVisionEval <- function(
  ve.home=NULL,ve.runtime=NULL,
  overwrite=FALSE,
  ve.lib.name="ve-lib",
  debug=FALSE
) {
  # Attach the runtime environment for important configuration parameters (see below)
  ve.env <- getRuntimeEnvironment()

  # Identify location for VE_HOME (contains ve-lib)
  if ( missing(ve.home) || is.null(ve.home) ) {
    if ( exists("ve.home",ve.env,inherits=FALSE) ) {
      ve.home <- ve.env$ve.home
    } else {
      ve.home <- Sys.getenv("VE_HOME",getwd())
    }
  }
  message("launch ve.home: ",ve.home)
  # set up VE_SOURCE (only used when building, but we want to preserve it in .Renviron)
  if ( exists("ve.sources",ve.env,inherits=FALSE) ) {
    ve.sources <- ve.env$ve.sources
  } else {
    ve.sources <- Sys.getenv("VE_SOURCE",as.character(NA))
  }

  # ve.runtime can be made non-missing by providing an existing directory or setting it to NA
  # (it defaults when missing to NULL)
  message("Launched runtime: ",ve.runtime)
  if ( ! is.character(ve.runtime) ) {
    home.as.runtime <- askYesNo(paste("Install VisionEval 'models' folder in",ve.home,"?"))
    if ( is.na(home.as.runtime) ) {
      message("Please select a suitable VisionEval runtime directory for models.")
      stop("Installation cancelled.")
    }
  } else home.as.runtime <- ( ve.runtime == ve.home )

  if ( ! home.as.runtime  ) {
    caption <- "Select directory for VisionEval 'models' folder (VE_RUNTIME)"
    ve.runtime <- if (exists('utils::choose.dir')) { # Won't exist on non-Windows platforms
      utils::choose.dir(default=ve.runtime,caption = caption)
    } else {
      tcltk::tk_choose.dir(default=ve.runtime,caption = caption)
    }
    if ( is.na(ve.runtime) || ! dir.exists(ve.runtime) ) {
      message("Please select a suitable VisionEval VE_RUNTIME directory for 'models'")
      stop("Installation cancelled.")
    }
  } else ve.runtime <- ve.home
  message("Selected runtime: ",ve.runtime)

  message("Setting up VE_RUNTIME as ",ve.runtime)
  message("You will want to start VisionEval from that folder.")

  # Save the important parameters
  ve.env$ve.runtime <- ve.runtime
  ve.env$ve.home <- ve.home
  Sys.setenv(VE_HOME=ve.home,VE_RUNTIME=ve.runtime) # Somewhat redundantly, also save to operating system environment
  # NOTE: ve.setup below will also save VE_HOME and VE_RUNTIME into the .Renviron startup file

  # Clear VEModel if already present
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  base::unloadNamespace("VEModel")

  # Clear visioneval so we can update it too
  if ( "package:visioneval" %in% search() ) detach("package:visioneval")
  unloadNamespace("visioneval")

  # TODO: ensure that ve-lib is set up for the current R and has VisionEval in it
  # If not, kick back to the installation script.

  # Set up ve-lib (R library location for installed VE packages and dependencies)
  # The same library location will hold sub-directories for the major/minor R version that is
  # running this installation.
  ve.env$this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
  ve.env$ve.lib <- file.path(ve.home,ve.lib.name,tools::file_path_sans_ext(ve.env$this.R))
  if ( ! dir.exists(ve.env$ve.lib) ) dir.create(ve.env$ve.lib,recursive=TRUE)
  .libPaths(ve.env$ve.lib)

  # The following is key for setting up the basic operation
  # NOTE: if VE_HOME is a Github clone, we don't want to mess with the .Rprofile
  # Check and construct startup files in VE_RUNTIME and (optionally) VE_HOME if the latter is different from VE_RUNTIME
  # Configure ve.runtime (.Renviron etc.)
  # TODO: this setup isn't working on a VE4-install.
  ve.setup(ve.home,ve.runtime,overwrite=overwrite)

  # Make installed library path active (set VE_HOME, with ve.env$ve.lib in .Renviron
  if ( ! ve.env$ve.lib %in% .libPaths() ) {
    # This will crap out for some reason if we try to add ve.lib and it's already there
    # That's a bug in R circa 4.4.x
    .libPaths(ve.env$ve.lib) # Update .libPaths # Also gets rid of other non-base paths
  }

  # Load VEModel
  if ( ! require("VEModel",quietly=TRUE) ) {
    message("Could not load VEModel.")
    message("Please check VE_HOME location and re-install there if needed.")
    stop(call.=FALSE,"Failed to start VisionEval")
  } else {
    VEModel::initVisionEval() # should setwd to ve.env$ve.runtime
  }

  # Return runtime location, invisibly
  invisible(ve.env$ve.runtime)
}

# ve.setup creates the standard runtime startup files, notably VisionEval.Rproj and launch.bat
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
  ve.env <- getRuntimeEnvironment()
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

  # Key locations should already have been set up
  ve.env <- getRuntimeEnvironment()
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

#   message("Setup locations:")
#   print(setup.locations)
  for ( location in setup.locations ) {
    message("Adding startup files to ",location)

    # Create R.version
    this.R <- paste(R.version[c("major","minor")],collapse=".")
    cat("that.R:",this.R,"\n",sep="",file=file.path(location,"r.version"))

    # Create .Renviron (VEBuild will add VE_BUILD to the list of defined locations, defaulting to VE_HOME)
    # TODO: overwrite line items in .Renviron rather than all-or-nothing
    # TODO: 
    renv.file      <- file.path(location,".Renviron")
    renv.txt       <- c(
      paste0("R_LIBS_USER=",paste(collapse=";",.libPaths()[-length(.libPaths())])), # ignore base library
      paste0("VE_HOME=",normalizePath(ve.home,winslash="/",mustWork=TRUE)),
      paste0("VE_RUNTIME=",normalizePath(ve.runtime,winslash="/",mustWork=TRUE))
    )
    if ( file.exists(renv.file) ) file.copy(renv.file,file.path(location,"Previous.Renviron"))
    writeLines(renv.txt,renv.file)

    # Write launch_Rx.y.bat, providing default R_HOME and encoding the R version in the batch name
    this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
    launch.bat.template <- system.file("startup/launch.bat.template",package="VEStart",mustWork=TRUE)
    launch.bat <- file.path(location,paste0("launch_R",this.R,".bat"))
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
