#!/bin/env Rscript

# Author: Jeremy Raw

script.contents <- c(
  "build.instructions.installer",
  "ve.make.installer"
) # for "import" package to construct ve.build.env pseudo-package

# Legal installer types
legalTypes <- tolower(c("win.binary","win.library","source"))

#Build instructions
build.instructions.installer <- function() {
  if ( "VEBuild" %in% utils::installed.packages(lib.loc=ve.env$ve.lib)[,"Package"] ) {
    paste( collapse="\n", c(
      "ve.make.installer(<build.type>) to build an installer",
      paste0("  where <build.type> is one of (",paste(c(legalTypes,"all"),collapse=", "),")"),
      paste0("  with default of ",legalTypes[1])
    ) )
  } else NULL
}

ve.make.installer <- function(pkgType=.Platform$pkgType,debug=FALSE) {

  ve.env <- try( silent=TRUE, as.environment("ve.env") )
  if ( ! is.environment(ve.env) ) {
    stop("VisionEval environment is unavailable. Use VE-Bootstrap.R or VEBuild to begin.", call. = FALSE)
  }

  # WARNING: currently does not allow override of ve-build-config.yml name
  bld.env <- getBuildEnvironment()
  if ( ! "this.R" %in% ls(bld.env) ) ve.build.config(debug=debug,quiet=TRUE)

  # Helper function for consistent error messages
  failure <- function(msg,pkgType) {
    message(msg)
    stop("Invalid pkgType: ",pkgType,call.=FALSE)
  }

  # error check pkgType
  if ( ! is.character(pkgType) ) failure("pkgType must be a character string",pkgType)
  pkgType <- tolower(pkgType)
  if ( pkgType == "all" ) {
    pkgType <- legalTypes
  } else if ( ! pkgType %in% legalTypes ) {
    failure(paste0("pkgType must be one of ",legalTypes,"or the shortcut 'all'\nYou selected '",pkgType,"'"))
  }

  for ( pt in pkgType ) buildOneInstaller(pt,bld.env,debug)
}

buildOneInstaller <- function(pkgType,bld.env,debug) {

  # dated install directory will hold the zipped results
  ve.install <- file.path(ve.env$ve.build.dir,"install")
  if ( ! dir.exists(ve.install) ) dir.create(ve.install)
  installer.date <- as.character(Sys.Date())
  release.name <- paste0("Release_",installer.date)
  ve.install <- file.path(ve.install,release.name)
  if ( ! dir.exists(ve.install) ) dir.create(ve.install)
  cat("Making",pkgType,"Installer in",ve.install,"\n")

  # Zip file name
  zipName <- function(zipname,folder=".") {
    elements <- c(
      "VE-Installer_",
      zipname,
      "_",
      installer.date,
      ".zip"
    )
    file.path(folder,paste(elements,collapse=""))
  }

  # Basis for MANIFEST file (Git information)
  build.info <- c(list(pkgType=pkgType),makeGitInfo(ve.env$ve.home))

  #
  owd <- getwd()
  if ( pkgType == "win.library" ) {
    # Zip up all of ve-lib, including all dependencies
    # This will be very large (circa 500Mb)
    # Destination says where to unzip
    build.info[["Destination"]] <- "ve-lib"
    zipfile <- zipName(paste0("WinLibrary-R",bld.env$two.digit.R),ve.install)
    try( setwd(bld.env$ve.lib) )
    if ( getwd() != bld.env$ve.lib ) failure(paste0("Could not change to library ",bld.env$ve.lib))
    manifest <- saveGitInfo(build.info,".",filename="MANIFEST") # just put it in ve-lib
    # saveGitInfo is defined in 01-build.R
  } else {
    # get suitable repository contriburl for one of c("source","win.binary")
    # zip the contriburl contents
    installType <- if ( pkgType=="win.binary" ) paste0("Windows-R",bld.env$two.digit.R) else "Source"
    contriburl <- utils::contrib.url(bld.env$ve.repository,pkgType) # source directory
    contrib.dest <- sub(bld.env$ve.repository,"",contriburl)
    build.info[["Destination"]] <- contrib.dest
    zipfile <- zipName(installType,ve.install)
    try( setwd(contriburl) )
    if ( getwd() != contriburl ) failure(paste0("Could not change to contriburl ",contriburl))
    manifest <- saveGitInfo(build.info,".",filename="MANIFEST") # just put it in contriburl
  }
  zip.flags <- if (debug) "-r9X" else "-r9Xq" # quiet if not debugging, otherwise a full list of zipped files (warning: long!)
  utils::zip(zipfile,c("."),flags=zip.flags)
  setwd(owd)
  cat("Zip Installer Created:\n")
  cat(zipfile,"\n")
  invisible(zipfile)
}
