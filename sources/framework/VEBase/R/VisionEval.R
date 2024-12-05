# ve.init
# Install, update and launch VisionEval system

#SET UP VISIONEVAL AND START
#===========================
#' Initialize a VisionEval installation
#'
#' \code{ve.init} will install and update VisionEval R packages and create a runnable VisionEval
#' installation. The standard procedure for installing VisionEval 4.0 is to install the
#' \code{VEBase} package and then run \code{ve.init()}.

#' This function does the following:
#' \enumerate{
#' \item sets the VisionEval home (VE_HOME) and runtime (VE_RUNTIME) locations
#' \item sets (optionally creating) the writable R library directory where VisionEval R packages
#' \will be installed (ve-lib within VE_HOME)
#' \item installs or (optionally) updates VisionEval packages from a list containing online
#' repository URLs, local folder paths, paths to .zip files (see \code{ve.install}), or Github
#' repositories containing source code for single VisionEval packages.
#' \item (optional) creates or updates launch files in the runtime location (see details below)
#' }
#' 
#' The startup files that can be created in the runtime location include an R Studio project file, the \code{.Rprofile}
#' that will load the VEBase startup package, a Windows batch file that will run VisionEval in the standard RGUI, and a
#' .Renviron file that will record the VE_HOME and VE_RUNTIME locations and the VisionEval package library location
#' (ve-lib).
#'
#' @param home character string, location of VE_HOME (default: \code{getwd()})
#' @param runtime character string, location of VE_RUNTIME (default: \code{home})
#' @param lib.loc character vector, location of ve-lib / first path in R_LIBS_USER (default: \code{libPaths()[1]})
#' @param setup logical: create input files in VE_RUNTIME (default: TRUE),
#' @param update logical: Install or update VE packages from repos (packages whose names start in VE) (default: TRUE),
#' @param repos eventually a standard online location e.g. \code{https://visioneval.org/packages}
#' (default: \code{file.path(getwd(),"ve-packages")}),
#' @param dialog start an interactive dialog to collect parameters (otherwise use command line arguments only)
#' (default: \code{interactive()})
#' @return invisibly, a character string containing the normalized path of the selected VisionEval runtime location
#' @export

ve.init <- function(
  home=getwd(),                           # location of VE_HOME
  runtime=NULL,                           # location of VE_RUNTIME; if NULL then VE_RUNTIME <- VE_HOME
  lib.loc=.libPaths()[1],                 # location of ve-lib / first path in R_LIBS_USER; saved as VE_LIB in .Renviron
  setup=TRUE,                             # Create or update input files in VE_RUNTIME
  update=TRUE,                            # Install or update VE packages from repos (packages whose names start in VE)
  repos=file.path(getwd(),"ve-packages"), # eventually a standard online location e.g. https://visioneval.org/packages
  dialog=interactive()                    # start an interactive dialog to collect parameters (otherwise use command line arguments only)
) {
  startParams <- ve.env$startupParams     # ve.env and startupParams are created during .onAttach for VEBase
  setFromStart <- function(param,startList,default) {
    return( if ( param in names(startList) ) startList[[param]] else default )
  }
  setupParams <- list(
    runtime = setFromStart("runtime",startParams,runtime),
    lib.loc = setFromStart("lib.loc",startParams,lib.loc),
    setup   = setFromStart("setup",startParams,setup),
    update  = setFromStart("update",startParams,update)
    repos   = repos
  )
  
  if ( dialog ) {
    setupParams <- ve.setupDialog(setupParams)
  } # otherwise use command parameters or their defaults

  # Set up VisionEval package library
  # Note that CRAN will always be used for dependencies
  ve.update(lib.loc=setupParams$lib.loc,repos=setupParams$repos,update=setupParams$update)

  # Set up runtime
  if ( setupParams$setup ) {
    Sys.setenv( VE_RUNTIME=ve.setup(runtime=setupParams$runtime,copy=setupParams$copy) )
  }
  venv$ve.runtime <- .getenv("VE_RUNTIME",getwd())
  return( loadVisionEval() )
}

#' Start a VisionEval session
#' @return invisibly, a character string containing the normalized path of the selected VisionEval runtime location
#' @export
ve.start <- function()
  # Attempt to reload VEModel and fail if it can't be loaded
  if ( !suppressWarnings(require(VEModel,quietly=TRUE)) ) {
    error("VEModel is still missing; re-run ve.init()")
    invisible( getwd() )
  }

  # Complete VEModel setup
  if ( "package:VEModel" %in% search() ) {
    VEModel::runtimeEnvironment(ve.env) # point VEModel to the VEBase environment
    VEModel::getSetup(reload=TRUE)      # reload global RunParam_ls; also will align with ve.env$ve.runtime
  } else {
    message("Uh-oh! VEModel should be on the search path but isn't")
    print(search())
  }

  # Return runtime location, invisibly
  invisible(venv$ve.runtime)
}

# ve.update
# Get the VE packages from an online or local repository

VE.framework <- c("VEBase","VEModel","visioneval")
VE.core <- c(
  "VESnapshot",
  "VESimHouseholds",
  "VESimLandUseData",
  "VESyntheticFirms",
  "VETransportSupply",
  "VETransportSupplyUse",
  "VEHouseholdTravel",
  "VEHouseholdVehicles",
  "VELandUse",
  "VEPowertrainsAndFuels",
  "VESimLandUse",
  "VETravelPerformance",
  "VESimTransportSupply",
  "VETravelDemandMM",
  "VEState",
  "VERPAT"
)

#' @param lib.loc character vector, location of ve-lib / first path in R_LIBS_USER (default: \code{libPaths()[1]})
#' @param repos eventually a standard online location e.g. \code{https://visioneval.org/packages}
#' (default: \code{file.path(getwd(),"ve-packages")}),
#' @return No value is returned
#' @export
ve.update <- function(lib.loc,repos,update) {
  if ( missing(lib.loc) ) lib.loc <- .libPaths()[1]
  if ( missing(repos) ) repos <- ve.env$VErepos
  # Clear VEModel so we can update it too
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  unloadNamespace("VEModel")

  # TODO: if packages are already present, update
  # Install the framework packages
  install.packages(VE.framework,repos=c(repos,"https://cloud.r-project.org"),dependencies=TRUE)
  # Install the core module packages
  install.packages(VE.core,repos=c(repos,"https://cloud.r-project.org"),dependencies=TRUE)
  # TODO:
  #   Let the installed model drive that
  #   Protocol is to install a package with a desired model variant, then install/update packages named in its ModelScript
}

startupFiles <- c(
  "VisionEval.Rproj",
  "launch.bat",
  ".Rprofile",
  ".Renviron"
)

# ve.setup
# set up runtime files
# Create or update necessary runtime files, supplying R version-specific parameters
#   in R_LIBS_USER and launch.bat.
ve.setup <- function() {
  # Copy the system files one by one
  # Replace templates for VE_HOME, VE_RUNTIME, VE_LIB, R_HOME in each file
}
