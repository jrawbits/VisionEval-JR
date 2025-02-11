# These functions will load the build scripts from VEBuild systemdata build-scripts folder

# We don't just make the build functions as elements of the package namespace, because
# rebuilding the VEBuild package itself may require detaching it so it can be rebuilt and
# reinstalled into the VE library.

# Dependencies in the loaded files (e.g. yaml, miniCRAN) are identified for the package
# DESCRIPTION file.

# Note: may still want to use the import package so that private objects can be hidden from
# the environment.

.onAttach <- function(libname, pkgname) {

  # Create an environment on the search path to hold build functions
  env.build <- if ( ! "ve.builder" %in% search() ) {
    attach(NULL,name="ve.builder")
  } else {
    as.environment("ve.builder")
  }
  running <- Sys.getenv("VE_BUILD_RUNNING",NA) # Don't reload scripts if one of them might be rebuilding VEBuild
  if ( is.na(running) ) {
    packageStartupMessage("Bootstrapping VisionEval...")
    build.scripts <- system.file("build-scripts",package="VEBuild")
    script.files <- file.path(build.scripts,dir(build.scripts,pattern="\\.R$"),fsep="/")
    for ( sf in script.files ) {
      packageStartupMessage("Loading script file: ",sf)
      sys.source(sf,envir=env.build)
    }
  }
}
