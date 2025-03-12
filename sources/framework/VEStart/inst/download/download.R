# Sample code to list out latest release assets at Github and select one for download.
# The filter pattern will look for standard Installer URLs

# TODO: integrate into install.R script that is downloaded as a one-liner from website for
# installation.
if ( ! suppressWarnings(requireNamespace("rjson",quietly=TRUE)) ) {
  # Used to read configuration files - always get from online source
  utils::install.packages("rjson", lib=ve.env$ve.lib, repos=ve.env$CRAN.mirror, type=build.type, quiet=!debug )
  suppressWarnings(requireNamespace("rjson",quietly=TRUE))
}

# Format for API release call:
# https://api.github.com/repos/{user}/{repo}/releases/latest

# user is Github user or organization
# repository is the repository name (case-insensitive) minus any .git suffix
ve.get.release <- function(user="visioneval",repository="visioneval-dev") {

  # Use the Github API to list releases and their properties
  # NOTE: all.releases appears to be in descending date-time order, so the first should always be
  # the latest release.
  all.releases <- rjson::fromJSON(file=paste0("https://api.github.com/repos/",user,"/",repository,"/releases"))
  latest <- if ( length(all.releases)>0 ) all.releases[1] else NA # reduce to a list of one
  if ( ! is.list(latest) ) {
    return( list() ) # no assets: empty list
  }
  release <- latest[[1]] # Get the object from the release list of 1

  downloads <- lapply(
    release$assets,
    function(a) {
      list(
        timeout = as.integer(round(a$size/750000,0)),
        url     = a$browser_download_url,
        file    = basename(a$browser_download_url)
      )
    }
  )
  return(downloads)
}

ve.select.installer <- function(downloads=list()) {
  # cat("Raw downloads:\n")
  # print(sapply(downloads,function(d)d$)
  # cat("All Download files:\n")
  # print(available)
  this.R <- sub("\\.[^.]+$","",paste(R.version[c("major","minor")],collapse="."))
  pattern <- paste0("R",this.R)
  # cat("Pattern: '",pattern,"'\n",sep="")
  available <- sapply(downloads,function(d)d$file,simplify=TRUE)
  # TODO: filter only .zip files, and only those that contain "Installer", and if they contain
  # "Rx.y" than x.y must match this.R
  # We want a Source installer or installers for the current R version
  filter.this.R <- grepl(paste0("Installer.*_R",this.R,"_.*\\.zip$"),available)
  filter.source <- grepl("Installer_Source_.*\\.zip$",available)
  filter <- filter.this.R | filter.source

  # print(filter)
  downloads <- downloads[filter]
  available <- available[filter]
  cat("Available Installers:\n")
  print( available )
  dl <- 0
  while ( length(downloads)>0 && ! is.na(dl) && ( dl<1 || dl > length(downloads) ) ) {
    dl <- readline(prompt="Which to download (q to exit)? ")
    dl <- try( suppressWarnings(as.integer(dl)) ) # NA if dl is not an integerable thing
  }
  if ( ! is.na(dl) && dl > 0 ) {
    return( downloads[[dl]] )
  } else return(NA)
}  

ve.fetch.installer <- function(item) {
  options(timeout = max(item$timeout, getOption("timeout"))) # ten minute timeout; set dynamically based on reported file size?
  message("Timeout: ",getOption("timeout")," seconds")
  download.file(item$url,destfile=item$file,method="auto",mode="wb")
  invisible(item$file)
}

downloads <- ve.get.release()
installer <- ve.select.installer(downloads)
message("Retrieving installer: ",installer)
retrieved <- ve.fetch.installer(installer)
message("Retrieved: ", retrieved)

# Next step is to unzip the download
# Need to create the proper directory structure for a repository/contriburl
# But could do that in the .zip file itself
# Keep it simple. We do want a manifest, but we won't look at it.
