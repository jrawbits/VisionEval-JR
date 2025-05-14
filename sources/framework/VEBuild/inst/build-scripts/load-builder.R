#!/bin/env Rscript

# Author: Jeremy Raw

# NOTE: keep this up to date with VEBuild/R/LoadBuildScripts.R
#' \code{load.builder()} will import the build scripts into a pre-created attached environment called "ve.builder".
#' This function uses the import package to load build functions into an attached environment ve.builder.
#' The script calling load.build should have created and attached the ve.builder environment.
#' See \code{VE-Bootstrap.R} at the root of the source tree, or \code{VEBuild::.onAttach()}
#' @param ve.scripts is the directory in which to seek the builder scripts (usually "inst/build-scripts" within VEBuild)
#' @value NULL
#' @name load.builder
load.builder <- function(ve.scripts) {

  # install and load import package
  # .libPaths()[1] should be ve.lib
  ve.env <- try( silent=TRUE, as.environment("ve.env" ) )
  if ( ! is.environment(ve.env) ) {
    ve.lib <- .libPaths()[1]
    CRAN.mirror <- Sys.getenv("VE_CRAN_MIRROR","https://cloud.r-project.org")
  } else {
    ve.lib <- get0("ve.lib",envir=ve.env,ifnotfound=.libPaths()[1])
    # TODO: make sure ve.lib exists so we can install into it below
    CRAN.mirror <- get0("CRAN.mirror",envir=ve.env,ifnotfound="https://cloud.r-project.org")
  }

  if ( ! suppressWarnings(requireNamespace("import",quietly=TRUE,lib.loc=ve.lib)) ) {
    utils::install.packages("import", lib=ve.lib, repos=CRAN.mirror, type=.Platform$pkgType )
    requireNamespace("import",quietly=TRUE,lib.loc=ve.lib)
  }

  script.files <- file.path(ve.scripts,dir(ve.scripts,pattern="\\.R$"),fsep="/")
  for ( sf in script.files ) {
    # script.contents amounts to an export namespace for the script file
    # those imported functions can access other objects defined in each script
    try(
      silent=TRUE,
      eval(parse(text=paste0("import::here(script.contents,.from='",sf,"')")))
    )
    if ( ! exists("script.contents") ) next

    eval(parse(text=paste0("import::into(.into='ve.builder',",paste(script.contents,collapse=","),",.from='",sf,"')")))
    if ( length( instructions <- ls("ve.builder",pattern="^build\\.instructions") ) > 0 ) {
      eval(parse(text=paste("message(",instructions,"()",")")))
    }
    rm(script.contents)
  }
  unloadNamespace("import") # so we can load it again as part of ve.build
  rm(sf,script.files)
}
