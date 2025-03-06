#!/bin/env Rscript

# Author: Jeremy Raw

# Use import package to load build functions into an attached environment ve.builder
# The script calling load.build should have created and attached the ve.builder environment.
#' @param ve.scripts List of package directory patterns to search for packages to build
#' @param CRAN.mirror URL of CRAN repository containing "import" package
load.builder <- function(ve.scripts,CRAN.mirror="https://cloud.r-project.org") {

  # install and load import package
  # .libPaths()[1] shoule be ve.lib
  if ( ! suppressWarnings(requireNamespace("import",quietly=TRUE)) ) {
    utils::install.packages("import", lib=.libPaths()[1], repos=CRAN.mirror, type=.Platform$pkgType )
  }

  

  script.files <- file.path(ve.scripts,dir(ve.scripts,pattern="\\.R$"),fsep="/")
  for ( sf in script.files ) {
    # Add error checking for script.contents not present
    # script.contents amounts to an export namespace for the script file
    # those imported functions can access other objects defined in each script
    try(
      silent=TRUE,
      eval(parse(text=paste0("import::here(script.contents,.from='",sf,"')")))
    )
    if ( ! exists("script.contents") ) next
    eval(parse(text=paste0("import::into(.into='ve.builder',",paste(script.contents,collapse=","),",.from='",sf,"')")))
    rm(script.contents)
  }
  unloadNamespace("import") # so we can load it again as part of ve.build
  rm(sf,script.files)
}
