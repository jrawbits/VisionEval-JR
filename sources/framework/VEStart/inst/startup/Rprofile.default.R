# Set up and run VisionEval

# Change the bootstrap.lib name if you like (don't use "ve-lib"!)
bootstrap.lib <- normalizePath("ve-setup-lib",winslash="/",mustWork=FALSE)

# Install VEStart from likely locations if it is not available in .libPaths() (which could include
# R_LIBS_USER)
if ( ! require(VEStart,,quietly=TRUE ) {
  # Find usable repositories
  # Default repository list
  local.repos <- Sys.getenv("VE_REPOS","ve-repos.cnf") # Override for development
  message("Local repository file: ",local.repos)

  # TODO: A full bootstrap installation comes with folders set up with VE and dependencies
  #   and those are referred to in a default VE_REPOS so this file just works

  if ( ! file.exists(local.repos) ) {
    warning("VE_REPOS is defined but file ",local.repos," does not exist.")
    local.repos <- "ve-repos.cnf" # Fall back to default repository locations
    message("Checking standard repository file: ",local.repos)
  }
  if ( ! file.exists(local.repos) ) {
    message("Using default online repositories")
    ve.repos <- c("https://packages.visioneval.org","https://cloud.r-project.org")
  } else {
    ve.repos <- c(grep("(^\\s*#\\s*)|(^\\s*$)",readLines(local.repos),invert=TRUE,value=TRUE),ve.repos)
  } # the contents of ve-repos.cnf is one url per line suitable for use with install.packages or update.packages
  # The first URL in ve-repos.cnf will be checked first, then the rest
  # in order, followed finally by the two default URLs above
  message("Searching VisionEval Repositories for bootstrap package:")
  print(ve.repos)

  # Load the installation driver (bare) into whatever user-writable R library exists
  if ( ! dir.exists(bootstrap.lib) ) {
    dir.create(bootstrap.lib,recursive=TRUE)
  } else {
    # TODO: remove previous contents of bootstrap.lib if it is not empty
    message("Resetting bootstrap library.")
    bootstrap.clear <- dir(bootstrap.lib,full.names=TRUE)
    unlink(boostrap.clear,recursive=TRUE)
  }

  # Install VEStart
  installType <- if ( .Platform$OS.type == "windows" ) "binary" else "source"
  utils::install.packages("VEStart",lib.loc=bootstrap.lib,repos=ve.repos,type=installType)
  if ( ! require(VEStart,lib.loc=bootstrap.lib,quietly=TRUE) ) {
    stop("VEStart is not available; check access to VisionEval repositories.")
  }
  
  # Get here with VEStart loaded
  .VEStart::ve.init())           # Force pre-defined VE_HOME or interact to set VE_HOME
                                 # If user is manually setting VE_HOME, do selection of VE_RUNTIME here
                                 # Add installed VE library to .libPaths and R_LIBS_USER
                                 # Respect VE_BUILD and VE_BRANCH to locate libraries (don't reinstall)
  VEStart::ve.setup()            # Force pre-defined VE_RUNTIME or use VE_HOME
                                 # Returns with working directory set to VE_RUNTIME

  # Clean up bootstrap installation of VEStart
  unloadNamespace(VEStart)
  if ( dir.exists(bootstrap.lib) ) unlink(bootstrap.lib,recursive=TRUE)
  require(VEStart,quietly=TRUE)  # Reload from VE_HOME/ve-lib
}
require(VEModel,quietly=TRUE)
VEStart::ve.init()               # Reload VE_HOME and VE_RUNTIME and set .libPaths()
VEModel::initVisionEval()        # Set up VEModel
