# test install.packages with contriburl and dependencies

ve.home <- Sys.getenv("VE_HOME",getwd())
ve.built <- file.path(ve.home, "built")
ve.lib <- file.path(ve.built, "test-ve-lib")
ve.pkgs <- file.path(ve.built, "ve-pkg-repo")
ve.pkgs.url <- paste0("file:///",ve.pkgs)
ve.install <- file.path(ve.built,"test-install")
if ( dir.exists(ve.lib) ) unlink(ve.lib,recursive=TRUE)
dir.create(ve.lib)
if ( dir.exists(ve.install) ) unlink(ve.install,recursive=TRUE)
dir.create(ve.install)

oldwd <- setwd(ve.pkgs)
print(getwd())
pkg.url <- contrib.url(".",type=.Platform$pkgType)
target.url <- contrib.url(ve.install,type=.Platform$pkgType)
if ( ! dir.exists(target.url) ) dir.create(target.url,recursive=TRUE)
message("pkgs via contrib.url")
print(getwd())
print(ve.pkgs)
print(pkg.url)
file.copy(dir(pkg.url,full.names=TRUE),target.url)
print(target.url)
print(dir(target.url))
setwd(owd)

# message("ve.pkgs.url")
# print(ve.pkgs.url)
# 
# install.packages(
#   c("VEStart","VEBuild","VEModel","visioneval"),
#   lib=ve.lib,
#   repos=c(ve.pkgs.url,"https://cloud.r-project.org"),
#   type="win.binary"
# )
# 


