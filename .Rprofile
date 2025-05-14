# Set up and run VisionEval

# Find possible source code bootstrap startup file
# Change VE_HOME or VE_SOURCE by editing .Renviron or run ve.setup() once VE is running
ve.home <- Sys.getenv("VE_HOME",getwd())
ve.source <- Sys.getenv("VE_SOURCE",file.path(ve.home,"build-source"))
this.R <- paste(c(R.version["major"],R.version["minor"]),collapse=".")
ve.lib <- file.path(ve.home,"ve-lib",tools::file_path_sans_ext(this.R))
.libPaths(ve.lib)
bootstrap.files <- unique(
  file.path(
    c(
      ve.home,
      ve.source
    ),
    "VE-Bootstrap.R"
  )
)
bootstrap.files <- bootstrap.files[file.exists(bootstrap.files)]
if ( length(bootstrap.files) > 0 ) {
  # Start from a source code bootstrap
  source(bootstrap.files[1])
} else {
  # Otherwise, do a runtime start (VEStart must be somewhere in .libPaths())
  require(VEStart,quietly=TRUE )
  startVisionEval()
}
