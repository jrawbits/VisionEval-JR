#!/bin/env Rscript

# Author: Jeremy Raw

# VE 4.0 build installer:

# build-installer will assemble and then .zip the repos/contriburl for packages along with the
# runtime. VEBase will be pre-installed (if Windows) and this.R vs that.R will be checked The
# installer can be set to package type "binary" (Windows) or "source" (and forced manually to
# "source" for testing). Packages to include can be just the bare R startups (finding VEBase online or
# elseshere), VEBase preinstalled (online install), VisionEval (local install of VE; dependences from
# outside), Full (local install of VE and dependencies). Use environment variables to control which
# of those happens.

# VE_INSTALLER_SOURCE = "Bare", "Online","VE-local","VE-full"
# VE_INSTALLER_TYPE = default (also if undefined), binary (Windows only), source (Linux or Mac)

# Builds .zip files for installers

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

message("========== BUILD FULL SOURCE INSTALLER (.zip files) ==========")

ve.build.type = "source"

# Set up .zip file names
# Need the ".zip" extension?
build.date <- Sys.Date()

installer.base   <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Runtime-R",this.R,"_",build.date)),".zip")
if ( ! file.exists(installer.base) ) {
  stop("Must run installer-base build first for base elements",call.=FALSE)
}
installer.pkg  <- paste0(file.path(ve.zipout,paste0("VE-",ve.version,"-Installer-Full-R",this.R,"_",build.date)),".zip")

owd <- getwd()

pkg.build.type = "source"
pkgs.contrib.url <- contrib.url(basename(ve.pkgs), type=pkg.build.type)
cat("pkgs located here:",pkgs.contrib.url,"\n")
# Add ve-pkgs (
cat("Building cross-platform Full Installer...")
unlink( installer.pkg )
setwd(file.path(ve.pkgs,".."))
zip(installer.base,pkgs.contrib.url,flags=c("-r9Xq",paste0("--output-file=",installer.pkg)))
cat("Done\n")

setwd(owd)