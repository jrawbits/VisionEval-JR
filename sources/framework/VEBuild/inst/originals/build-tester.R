#!/bin/env Rscript

# Author: Jeremy Raw

# Try out some stuff

# Load runtime configuration
if ( ! exists("ve.installer" ) ) ve.installer <- getwd()
source(file.path(ve.installer,"scripts","get-runtime-config.R"))

# Load packages used here
# https://desc.r-lib.org/reference/description.html
require(desc,quietly=TRUE)
if ( ! suppressWarnings(requireNamespace("desc",quietly=TRUE)) )
  install.packages("desc", lib=dev.lib, repos=CRAN.mirror, type=.Platform$pkgType )

if ( ! suppressWarnings(require("miniCRAN",quietly=TRUE)) )
  install.packages("miniCRAN", lib=dev.lib, repos=CRAN.mirror, type=.Platform$pkgType)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager",repos=CRAN.mirror, type=.Platform$pkgType)

message("========== MODULE DEPENDENCIES ==========")

# In VEBuild, these will come from searching the user-supplied source directories
# for the VE packages (search directory tree for folders containing DESCRIPTION)
# Could do this:
# grep("\\.Rcheck",dirname(normalizePath(dir(ve.src,recursive=TRUE,pattern="^DESCRIPTION$",full.names=TRUE),winslash="/")),value=TRUE,invert=TRUE)
package.names <- list.dirs(ve.src,full.names=TRUE,recursive=FALSE)
print(package.names)

# This loop will parse the DESCRIPTION Files looking for dependencies and VE characteristics
# Probably want to create a list for each package with VEModules, VEModels, and Dependencies
deps.df <- data.frame()
for ( src.module in package.names ) {

  cat("Getting description for ",src.module,"\n")
  DESC <- desc::description$new(file.path(src.module,"DESCRIPTION"))
  mod.deps <- DESC$get_deps()
  # print(DESC$fields())
  cat("VEModules:\n")
  print( unlist(strsplit(DESC$get_field("VEModules",default=as.character(NA)),split="\\s")) )
  cat("VEModels:\n")
  print( unlist(strsplit(DESC$get_field("VEModels",default=as.character(NA)),split="\\s")) )
  mod.deps$Module <- basename(src.module) # recycle
  mod.deps$NotR <- mod.deps$package != "R"
  if ( nrow(deps.df) == 0 ) {
    deps.df <- mod.deps[mod.deps$NotR,]
  } else {
    deps.df <- rbind(deps.df,mod.deps[mod.deps$NotR,])
  }
}

# The following steps cull through the dependencies, looking for CRAN and BioConductor packages
deps.df <- deps.df[order(deps.df$package,deps.df$type),c("package","Module","type")]
cat("All root dependencies:\n")
print(root.deps <- unique(deps.df$package))

pkg.deps <- grep("^VE|^visioneval",root.deps,value=TRUE,invert=TRUE)
cat("Non-VE dependencies:\n")
print(pkg.deps)

base.lib <- dirname(find.package("MASS")) # looking for recommended packages; picking one that is required
pkgs.BaseR <- as.vector(installed.packages(lib.loc=base.lib, priority=c("base", "recommended"))[,"Package"])
cat("Base R packages:\n")
print(pkgs.BaseR)

cat("True Dependencies (ignore Base R which are always present):\n")
true.deps <- setdiff(pkg.deps,pkgs.BaseR)
print(true.deps)

cat("CRAN Dependencies (packages present in CRAN):\n")
pkgs.CRAN <- miniCRAN::pkgAvail( repos=CRAN.mirror )
CRAN.deps <- intersect(true.deps,pkgs.CRAN)
print(CRAN.deps)

cat("Other Dependencies (try BioConductor):\n")
Other.deps <- true.deps[ ! true.deps %in% CRAN.deps ]
print(Other.deps)

cat("Expanded CRAN dependencies (recursive dependencies):\n")
all.CRAN.deps <- miniCRAN::pkgDep( CRAN.deps, repos=CRAN.mirror, suggests=FALSE)
print(sort(all.CRAN.deps))

cat("Expanded BioConductor dependencies:\n")
options(BiocManager.check_repositories = FALSE)
Bioc.repos <- BiocManager::repositories()
Bioc.available <- BiocManager::available()
Bioc.available <- Bioc.available[ Bioc.available %in% Other.deps  ]
all.Bioc.deps <- miniCRAN::pkgDep( Bioc.available, repos=Bioc.repos, suggests=FALSE )
print(sort(all.Bioc.deps))

cat("Unsatisfied dependencies:\n")
missing.deps <- Other.deps[ !Other.deps %in% Bioc.available ]
print(deps.df[ deps.df$package %in% missing.deps,c("package","type") ])
