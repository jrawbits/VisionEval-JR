# Author: Jeremy Raw

# VEModel Package Code

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

requireNamespace("jsonlite")
requireNamespace("R6")
requireNamespace("visioneval")
requireNamespace("futile.logger")
requireNamespace("tryCatchLog")

# Function: ve.model.path
# Determine if parameter is a list of locations of run_model.R riles
# First parameter is character vector of directories that may contain
# run_model.R
# Check first whether modelPath contains directories or full paths
# to run_model.R (if the latter, replace with dirnames)
# If modelPath contains directories, each one must have a run_model.R
# If only one directory, and it does not contain run_model.R, look
# for a staged model scenario - so find all the subdirectories and
# verify that each has a run_model.R.

ve.model.path <- function(modelPath=NULL) {
  # Check how modelPath specifies a run_model.R file
  # Does modelPath make sense (absolute or relative to getwd())?
  if ( is.null(modelPath) ) stop("Must provide model path locator.\n")
  if ( ! all( file.exists(modelPath) ) ) {
    modelPath = file.path(ve.runtime,"models",modelPath)
    if ( ! all( dir.exists(modelPath) ) ) {
      stop("Could not locate model directory for [",paste(modelPath,dir.exists(modelPath),sep=":",collapse=","),"]")
    }
  }
  # Figure out if we can use modelPath to find "run_model.R"
  # script(s)
  if ( all ( file.exists(modelPath) & toupper(basename(modelPath))=="RUN_MODEL.R" ) ) {
    # Provided full path to run_model.R (possibly more than one)
    modelPath <- dirname(modelPath)
  } else if ( ! all( file.exists( file.path(modelPath,"run_model.R") ) ) ) {
    if ( length(modelPath)==1 ) { # no run_model.R in modelPath
      # Check for staged model (must be single root directory)
      subs <- dir(modelPath,full.names=TRUE)
      modelPath <- subs[dir.exists(subs) & file.exists(file.path(subs,"run_model.R"))]
      if ( length(modelPath)==0 ) {
        stop("No run_model.R in [",paste(modelPath,collapse=","),"]")
      }
    } else {
      stop("Could not locate run_model.R for [",paste(modelPath,collapse=","),"]")
    }
  }
  return(normalizePath(modelPath,winslash="/",mustWork=TRUE))
}

load.model.state <- function(path) {
  ms.env <- new.env()
  if ( ! grepl("ModelState\\.Rda$",path) ) path <- file.path(path,"ModelState.Rda")
  if ( file.exists(path) ) {
    model.state <- visioneval::readModelState(FileName=path, envir=ms.env)
  } else {
    model.state <- list()
  }
  rm(ms.env)
  return(model.state)
}

# TODO: conduct the standard model search within the "models" folder of the package
# Split input+defs into "skeleton" (empty files) and "sample"
# Push ahead on the framework "path search", so a model will search its parent
# for inputs/defs/local.
ve.init.model <- function(modelPath=NULL,modelName=NULL) {
  self$modelPath <- ve.model.path(modelPath)
  names(self$modelPath) <- basename(self$modelPath)
  if ( is.null(modelName) ) {
    self$modelName <- if ( length(self$modelPath)>1 ) {
       # default modelName for multi-stage model is basename of
       # the directory containing the first stage subdirectory
      basename( dirname(self$modelPath[1]) )
    } else { # For a one-stage model it is the basename of the first path itself
      names(self$modelPath)[1]
    }
  } else {
    self$modelName <- modelName
  }
  # Gather defs/run_parameters.json
  if ( file.exists(rpfile <- file.path(self$modelPath[1],"defs","run_parameters.json")) ) {
    self$runParams <- jsonlite::fromJSON(rpfile)
  } else {
    stop("Cannot construct model; missing: ",rpfile)
  }
  self$stageCount <- length(self$modelPath)
  self$modelState <- lapply(
    self$modelPath,
    load.model.state
  )
  if ( length(self$modelState)>0 && any(unlist(lapply(self$modelState,length))>0) ) {
    private$index()
  }

  self$runStatus <- sapply(
    simplify=TRUE,
    self$modelState,
    function(ms) {
      if ( length(ms) > 0 ) {
        ifelse(
          "runStatus" %in% names(ms),
          ms$runStatus,
          "Prior Run"
        )
      } else {
        "Not Run"
      }
    }
  )
  self$status <- self$runStatus[length(self$runStatus)]
}

log.level <- function(level) {
  legal.levels <- list(
    "DEBUG"=futile.logger::DEBUG,
    "ERROR"=futile.logger::ERROR,
    "FATAL"=futile.logger::FATAL,
    "INFO"=futile.logger::INFO,
    "TRACE"=futile.logger::TRACE,
    "WARN"=futile.logger::WARN
  )
  if ( level %in% names(legal.levels) ) {
    return(legal.levels[level])
  } else {
    return(legal.levels["ERROR"])
  }
}

ve.model.run <- function(verbose=TRUE,path=NULL,stage=NULL,log="ERROR") {
  # Unlike .dir the path/stage says where to start - the run will
  # then continue by running that stage then each following stage
  if ( missing(path) ) path <- stage    # Still might be NULL; allow alias
  pathLength <- length(self$modelPath)
  stageStart <- if ( ! is.null(path) ) path else 1
  for ( ms in stageStart:self$stageCount ) {
    stage <- self$modelPath[ms]
    if ( verbose ) {
      message("Running model stage:")
      message(stage)
    }
    owd <- setwd(stage)
    if ( ! "ve.model" %in% search() ) {
      envir <- attach(NULL,name="ve.model")
    } else {
      envir <- as.environment("ve.model")
    }
    self$status <- ""
    futile.logger::flog.threshold(log.level(log))
    tryCatchLog::tryCatchLog(
      {
        self$status <- "Running"
        sys.source("run_model.R",envir=envir)
        if (verbose) message("Model stage ",stage," complete")
        self$status <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        message("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$status <- msg
        self$runStatus[ms] <- "Failed"
      },
      finally =
      {
        if ( self$status == "Running" ) {
          self$status <- "Failed"
        }
        if ( self$status == "" ) {
          self$status <- "Stopped"
        }
        if (verbose) {
          cat("Model Stage:",gsub(ve.runtime,"",stage),"\n")
          if ( self$status != "Complete" ) cat("Error:",self$status,"\n")
        }
        model.state.path <- file.path(self$modelPath[ms],"ModelState.Rda")
        if ( file.exists(model.state.path) ) {
          visioneval::setModelState(
            list(runStatus=self$runStatus[ms]),
            FileName=model.state.path
          )
        }
        setwd(owd)
      }
    )
    if (verbose) {
      cat("Status:",self$status,"\n")
    }
    if ( self$status != "Complete" ) break;
  }
  self$modelState <- lapply(
    self$modelPath,
    load.model.state
  )
  if ( length(self$modelState)>0 && all(unlist(lapply(self$modelState,length))>0) ) {
    private$index()
  }

  return(invisible(self$status))
}

ve.model.dir <- function(pattern=NULL,recursive=FALSE,shorten="",path=NULL,stage=NULL) {
  # path/stage can be a vector of discrete stages (e.g. c(1,3); only
  # those will be inspected.
  if ( missing(path) ) path <- stage
  if ( is.null(path) ) path<-c(1:self$stageCount)
  files <- dir(self$modelPath[path],pattern=pattern,recursive=recursive,full.names=TRUE)
  if ( nzchar(shorten) ) files <- gsub(shorten,"",files)
  return(files)
}

confirm <- function(msg) {
  conf <- askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE
  return(conf)
}

# outputOnly will just report the extraction results
# (not the model run)
ve.artifacts <- function(path=NULL,stage=NULL,outputOnly=FALSE) {
  if ( missing(path) ) path <- stage
  if ( ! outputOnly ) {
    mstates <- self$dir(pattern=".*(Previous)*ModelState\\.Rda",path=path)
    mstates <- mstates[!dir.exists(mstates)]
    dstores <- self$dir(pattern="Datastore_*",path=path)
    dstores <- dstores[dir.exists(dstores)]
    logs    <- self$dir(pattern="Log_*.*\\.txt",path=path)
    logs    <- logs[!dir.exists(logs)]
    artifacts <- c(mstates,dstores,logs)
  } else artifacts <- character(0)
  outputs <- self$dir(pattern="output",path=path)
  return(c(artifacts,outputs))
}

ve.model.clear <- function(force=FALSE,outputOnly=NULL,path=NULL,stage=NULL) {
  if ( missing(path) ) path <- stage
  if ( missing( outputOnly ) ) {
    # If "output" exists in any stage, only offer to clear outputs
    # unless the user manually overrides. Makes it harder to
    # accidentally delete a model run.
    outputOnly = any( dir.exists( file.path(self$modelPath,"output") ) )
  }
  to.delete <- private$artifacts(path=path,outputOnly=outputOnly)
  if ( length(to.delete)>0 ) {
    to.delete <- gsub(paste0("^",getwd(),"/"),"",to.delete)
    print(to.delete)
    if ( interactive() ) {
      choices <- to.delete
      preselect <- if (force || outputOnly ) to.delete else character(0)
      to.delete <- select.list(choices=choices,preselect=preselect,multiple=TRUE,title="Delete Select Outputs")
      force <- length(to.delete)>0
    } else {
      force <- ( force || length(to.delete)>0 )
    }
    if ( force) {
      print(dir(to.delete),recursive=TRUE)
      unlink(to.delete,recursive=TRUE)
      self$modelState <- lapply(
        self$modelPath,
        function(x) list()
      )
      cat("Model",(if(outputOnly) "outputs" else "results"),"cleared.\n")
    } else {
      cat("Model results NOT cleared.\n")
    }
  } else {
    cat("No prior results to clear.\n")
    force = FALSE
  }
  return(invisible(force))
}

ve.path.prefix <- function(x) {
    x   <-sort(x)                          # sort the vector
    d_x <-strsplit(x[c(1,length(x))],"")   # split the first and last element by character (list of two vectors of single characters)
    pfx <-match(FALSE,do.call("==",d_x))-1 # match the first not common element and back up to last matching one
    if(is.na(pfx)) {                       # all vector elements are the same
      return(x[1])
    } else if (pfx==0) {                   # if there is no matching element, return an empty vector, else return the common part
      return(character(0))
    } else {
      return(substr(x[1],1,pfx))
    }
}

ve.model.copy <- function(newName=NULL,newPath=NULL) {
  if ( is.null(newPath) ) {
    if ( self$stageCount>1 ) {
      newPath <- dirname(self$modelPath)
      newPath <- ve.path.prefix(newPath)
      if ( ! dir.exists(newPath) ) newPath <- dirname(newPath) # match might extend into basename
      if ( ! nzchar(newPath) ) {
        newPath <- dirname(self$modelPath[1])
      } else {   # assume there's an embracing directory
        newPath <- dirname(newPath)
      }
    } else {
      newPath <- dirname(self$modelPath[1])
    }
  } else {
    if ( ! dir.exists(newPath) ) newPath <- dirname(newPath)
  }
  newPath <- normalizePath(newPath,winslash="/",mustWork=TRUE)
  if ( is.null(newName) ) newName <- paste0(self$modelName,"-Copy")
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  get.destination <- if ( self$stageCount == 1 ) {
    function(modelPath,...) modelPath
  } else {
    function(modelPath,basenameStage) file.path(modelPath,basenameStage)
  }
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- get.destination(newModelPath,basename(self$modelPath[p]))
    print(copy.to)
    dir.create(copy.to,showWarnings=FALSE)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }
    
  return( openModel(newModelPath,newName) )
}

# Check if a specified attribute belongs to the Datastore row
attributeExist <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(TRUE)
    }
  }
  return(FALSE)
}

# Get a specified attribute for a Datastore row
attributeGet <- function(variable, attr_name){
  if(is.list(variable)){
    if(!is.na(variable[[1]])){
      attr_value <- variable[[attr_name]]
      if(!is.null(attr_value)) return(attr_value)
    }
  }
  return(NA)
}

ve.model.status <- function(status) {
  if ( missing(status) ) return(private$runError)
  private$runError <- status
}

ve.model.print <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

selectName <- c(fields="Name",tables="Table",groups="Group")
selectOrder <- c("Name","Table","Group","Stage")
prepSelect <- function(self,what,details=FALSE) {
  # self is a VEModel object
  # what is the character name of the thing we're selecting (groups,tables,fields)
  # details if TRUE pastes all the fields, otherwise
  df <- self[[what]]
  name <- selectName[what]
  if ( details ) {
    show <- selectOrder[ selectOrder %in% names(df) ]
    detail.fields <- show[-grep(name,show)]
    if ( what=="fields" ) {
      list.fields <- self[["list"]](details=TRUE,selected=FALSE)
      df$Description <- substr(list.fields$Description,1,40)
      detail.fields <- c(detail.fields,"Description")
      show <- c(show,"Description")
      }
    detail.function <- function(x) {
      paste(x[name],paste(paste(detail.fields,x[detail.fields],sep=": "),collapse=", "),sep=" | ")
    }
  } else {
    show <- name
    detail.function <- function(x) x
  }
  choices <- apply(df[,show,drop=FALSE], 1, detail.function)
  selected <- choices[which(df$Selected=="Yes")]
  names <- df[,name,drop=TRUE]
  return( list(choices=choices,selected=selected,names=names) )
}

# Create an output object from the model output
ve.model.output <- function() {
  if ( ! private$outputObject ) {
    output <- VEOutput$new() # parameters TBD
    if ( output.isValid() ) {
      private$outputObject <- output
    } else {
      private$outputObject <- NULL
    }
  }
  return(private$outputObject)
}

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,
    modelPath=NULL,
    stageCount=NULL,
    runParams=NULL,
    runStatus=NULL,

    # Methods
    initialize=ve.init.model,
    run=ve.model.run,
    print=ve.model.print,                    # provides generic print functionality
    dir=ve.model.dir,
    clear=ve.model.clear,
    copy=ve.model.copy,
    output=ve.model.output                   # Create a VEOutput object (if model is run)
    status=ve.model.status
  ),
  private = list(
    # Private Members
    runError=NULL,
    artifacts = ve.artifacts,               # Function may interrogate an existing Output
    outputObject=NULL                       # VEOutput object for this model
  )
)

#' Install and Use a VisionEval Model
#'
#' @description
#' `openModel` opens a VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' @section Model Path and Name
#' The `modelPath` parameter locates a model object. When a model is opened, a
#'   a relative modelPath will be sought in the user's runtime `models` directory.
#'   An absolute path will be sought only in the user's file system.
#'
#' The `modelName` parameters will specify the name of the model directory within
#'   modelPath. That can be used to install a standard model again (in a different
#'   subdirectory), or to look for a model in a different directory than the visioneval
#'   runtime `models` directory (or a substitute provided through `options(VEModelPath=...)`.
#'
#' You can set an alternate location for the "models" subdirectory by providing an
#'   a path using, for example, `options(VEModelPath='mymodels')`. Relative paths
#'   will be sought below the VisionEval runtime directory. Absolute paths will
#'   be sought in the user's file system.
#'
#' An error will be raised if a model cannot be found or created with the indicated
#'   modelPath and modelName.
#'
#' @section Standard Models
#' If a model is indicated by a name with no path (e.g. `openModel('VERSPM')`) and it is
#'   not found in the `models` directory, then a VisionEval standard model will be sought
#'   by that name in the VEModel package and copied to the `models` directory. To skip that search,
#'   open the model with `installModel=FALSE`. If the parameter `installData=TRUE` (default),
#'   the full sample data will be copied and the sample model can be run. If `installData=FALSE`
#'   only skeleton `inputs` and `defs` will be copied (no actual data will ge supplied and the
#'   model won't run until data is provided).
#'
#' @section Available Models
#' You can see the available models by providing an empty string for the `modelPath`.
#'   A VEModelList (q.v.) object will be printed as a side-effect, and also returned invisibly.
#'   You can open an installed model from the VEModelList using
#'   square brackets to index the list by position (`VEModelList[1]`) or by name
#'   (`VEModelList['VERSPM']`). To open an un-installed standard model, just provide
#'   the standard model name as modelPath.
#'
#' @param modelPath Directory containing a VisionEval model; if an empty character string is
#'     provided, prints a list of available standard models (see details)
#' @param modelName Name displayed for this model (also used as basename for model copy);
#'   defaults to `basename(modelPath)`
#' @param installModel if TRUE (default), if the model path and name are not found,
#'   look instead for `modelPath` as the name of a standard model and install
#'   (copy) it to the `models` directory as modelName.
#' @param installData if TRUE (default), install full sample data if modelPath installs a standard model (see details)
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'   provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath, modelName = NULL, installModel=TRUE, installData = TRUE) {
  return( VEModel$new(modelPath = modelPath, modelName = modelName) )
}
