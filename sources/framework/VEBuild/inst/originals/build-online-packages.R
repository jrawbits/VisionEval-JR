#!/bin/env Rscript

# Author: Jeremy Raw

# This script will push the ve.repository to a drat online location specified in the config root ve.pkg.online

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# Load required libraries
if ( ! suppressWarnings(require("drat",quietly=TRUE)) ) {
  install.packages("drat", lib=dev.lib, repos=CRAN.mirror, dependencies=NA, type=.Platform$pkgType)
}

message("========== PUBLISH VE PACKAGES (VisionEval-Packages) ==========")

# mirror ve.repository into ve.publish using brute force plus write_PACKAGES

for ( repo.type in unique(c(ve.build.type,"source")) ) {
  ve.repo.dir <- contrib.url(ve.repository,type=repo.type)
  files.available <- grep("PACKAGES",dir(ve.repo.dir,no..=TRUE,full.names=TRUE),invert=TRUE,value=TRUE)
  cat("Available files of type",repo.type,"\n")
  print(files.available)
  ve.publish.dir <- contrib.url(ve.publish,type=repo.type)
  cat("\nFiles previous published in",ve.publish.dir,":\n")
  files.published <- grep("PACKAGES",dir(ve.publish.dir,no..=TRUE,full.names=TRUE),invert=TRUE,value=TRUE)
  print(files.published)
  unlink(files.published)
  file.copy(files.available,ve.publish.dir)
  cat("\nFiles now published:\n")
  files.published <- grep("PACKAGES",dir(ve.publish.dir,no..=TRUE,full.names=TRUE),invert=TRUE,value=TRUE)
  print(files.published)
  cat("\nwriting PACKAGES\n")
  tools::write_PACKAGES(ve.publish.dir, type=repo.type)
  cat("Done with",repo.type,"\n")
}
