# Author: Jeremy Raw

# VEModel Package Code

# R6 Class documentation example:
# https://github.com/r-lib/R6/issues/3#issuecomment-173855054
# https://github.com/r-lib/processx/blob/bc7483237b0fbe723390cbb74951221968fdb963/R/process.R#L2
# https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation
# https://roxygen2.r-lib.org/articles/rd.html#r6

self=private=NULL

# Function: ve.model.path
# Use the modelPath parameter to find run_model.R files
# modelPath is a character vector of directories that may contain run_model.R
#
# 1. Is run_model.R mentioned explicitly in each element of the vector of paths?
#    If so, reduce modelPath to just the elements that contain run_model.R
#        Then replace the vector with the dirnames and return those
#    If not, proceed to step 2
# 2. Does modelPath contain existing directories?
#    Are modelPath absolute (check first one)? If so examine each for run_model.R and if found, return modelPath
#        If not found, examine first-level subdirectories of each listed path, and include the absolute path of all
#          of those that contain run_model.R
#        If run_model.R is not found, throw an error "no model found at modelPath"
#    If modelPath are relative (check first one), try finding run_model.R by normalizing as follows:
#      A. If ve.runtime exists:
#         Look relative to ve.runtime/models if ve.runtime exists
#         Look relative to ve.runtime (directly)
#         If run-model not found directly, also consider first-level sub-directories as with absolute paths
#      B. If ve.runtime does not exist, or run_model.R not found, repeat A. using getwd() instead of ve.runtime
# 3. If directories are still not found and dirname(modelPath) is an empty string, conduct standard model search
#    If dirname(modelPath) is not empty, throw an error "no model found at modelPath"
#    If modelPath contains more than one element, throw an error "no model found on modelPath"
#    Search for models within extdata directory (system.file); but also search in package tree in case we're running
#      the source interactively for testing.
#    If no matching model exists, throw an error "no model found at modelPath"
#    If a matching model exists, if "confirm" is TRUE, conduct a dialog asking if the user wants to install the
#      standard model named in modelPath. Otherwise, use the "skeleton" parameter (TRUE/FALSE) to install either
#      (TRUE) the standard model skeleton files (bare inputs/defs) or (FALSE) the full sample model
#    Then attempt to install the standard model

## Helper
confirmDialog <- function(msg) {
  conf <- utils::askYesNo(msg,prompts="y/n/c")
  if ( is.na(conf) ) conf<-FALSE # Cancel is the same as No
  return(conf)
}

## Helper
#  return TRUE if modelPath looks like an absolute file path
isAbsolutePath <- function(modelPath) {
  # TODO: may need a more robust regular expression
  any(grepl("^([[:alpha:]]:|/)",modelPath))
}

## Helper
#  Model roots: ve.runtime/models, getwd()/models, ve.runtime, getwd()
getModelRoots <- function(RunParam_ls,get.root=0) {
  roots <- c( getwd() )
  if ( exists("ve.runtime") ) {
    ve.runtime <- get("ve.runtime")
    if ( ve.runtime != getwd() ) roots <- c( ve.runtime, roots )
  }
  # Develop the list of places to look for modelPaths if modelPaths is
  #   not an absolute path.
  # Hierarchy of roots:
  #    visioneval::getRunParameter("VEModelRoot")
  #    ve.runtime/models (if exists)
  #    getwd()/models (if exists)
  #    ve.runtime
  #    getwd()
  modelPath <- visioneval::getRunParameter("VEModelRoot",RunParam_ls,character(0)) # Default is NA
  if ( length(modelPath)>0 ) {
    modelPath <- modelPath[1]
    if ( ! isAbsolutePath(modelPath) ) {
      test.paths <- normalizePath(file.path(roots,modelPath))
      modelPath <- test.paths[dir.exists(test.paths)]
      if ( length(modelPath)==0 || ! nzchar(modelPath[1]) ) {
        modelPath <- NULL
      } else {
        modelPath <- modelPath[1]
      }
    }
  }
  # VEModelDir is specified as a subdirectory of key roots (default "models")
  roots <- c( modelPath, file.path(roots,visioneval::getRunParameter("VEModelDir",RunParam_ls,"models")), roots)
  if ( get.root > length(roots) ) get.root <- 1
  if ( get.root>0 ) return(roots[get.root]) else return(roots)
}

## Helper
#  Get unique file name based on newName in folder newPath
#  NewPath is the directory it should go in, newName is the name to disambiguate
getUniqueName <- function(newPath,newName) {
  newModelPath <- file.path(newPath,newName)
  tryName <- newName; try <- 1
  while ( dir.exists(newModelPath) ) {
    tryName <- paste0(newName,"(",try,")")
    newModelPath <- file.path(newPath,tryName)
    try <- try+1
  }
  return (newModelPath)
}

## Helper function
#  Look for run_model.R (any case) in root or subdirectories and return full paths
modelInRoot <- function(modelPath,runModelName) {
  modelSpec <- paste0(runModelName,"$")
  paths <- grep(modelSpec,ignore.case=TRUE,dir(modelPath,full.names=TRUE,recursive=TRUE),value=TRUE)
  # All runModelName files with path relative to modelPath
  paths <- normalizePath(paths,winslash="/")
  if ( any(paths==modelPath) ) {
    paths <- paths[ paths==modelPath ] <- "."
  } else {
    paths <- dirname(sub(paste0(modelPath,"/"),"",paths)) # remove modelPath so the names are relative.
  }
  return(paths)
}

## Helper function

## Helper function
# Examine modelPath and return (sub-)directories containing run_model.R
# RunParam_ls provides a list of configuration values 
# Returns a list with the following elements:
#   isValid : a boolean, TRUE if we found model(s), FALSE if we did not
#   runModelName : a character vector (length 1) containing the name of
#      the run_model.R file (default, "run_model.R"), which we just attempt to
#      "get" with inheritance (it will be loaded in the ve.runtime config file, if
#      defined, otherwise default as always to run_model.R
#   modelPath : character vector (length 1); the normalized version of parameter
#   stagePaths : character vector of absolute paths below modelPath that were found
#      to contain files named <runModelName> (per configuration). First part of
#      each path will exactly match modelPath.
findModel <- function( modelPath, RunParam_ls ) {

  find_ls <- list()
  find_ls$isValid <- FALSE # we'll change our mind later...

  if ( missing(modelPath) || ! is.character(modelPath) ) {
    message("Must provide modelPath locator.")
    return( find_ls )
  }

  # Establish the local name for run_model.R
  # VisionEval.R startup will set ve.env$runModelName from site configuration file
  #   located in runtime root (first of .visioneval, VisionEval.ini, VisionEval.cnf)
  #   with same default as here ("run_model.R")
  find_ls$runModelName <- visioneval::getRunParameter("runModelName",RunParam_ls,Default="run_model.R")
  
  # check if modelPath contains runModelName
  if ( grepl(modelPath,paste0(find_ls$runModelName,"$"),ignore.case=TRUE) ) {
    modelPath <- modelPath[1]
    find_ls$modelPath <- normalizePath(dirname(modelPath),winslash="/")
    find_ls$stagePaths <- "." # modelPath root is the only stagePath
    find_ls$isValid <- TRUE
    return(find_ls)
  }

  # if modelPath is not an absolute path, search for it amongst the "roots"
  if ( ! isAbsolutePath(modelPath) ) {
    roots<-getModelRoots(RunParam_ls)
    possiblePaths <- file.path(roots,modelPath)
    existing <- dir.exists(possiblePaths)
    if ( ! any(existing) ) {
      message("Failed to find any model in:\n",paste(roots,collapse="\n"))
      return(find_ls)
    } else {
      # Use first of the existing paths
      find_ls$modelPath <- normalizePath(possiblePaths[existing][1],winslash="/")
    }
  } else if ( ! dir.exists(modelPath) ) {
    message("Model directory ",modelPath," does not exist")
    return(find_ls)
  } else {
    find_ls$modelPath <- modelPath
  }

  # Attempt to locate runModelName in directory and sub-directories
  # The resulting vector are the stage paths
  find_ls$stagePaths <- modelInRoot(find_ls$modelPath,find_ls$runModelName)
  if ( length(find_ls$stagePaths)<=0 ) {
    message("No ",find_ls$runModelName," found in ",find_ls$modelPath)
  } else {
    find_ls$isValid <- TRUE
  }
  
  return( find_ls )
}

## Dir
#  Look up a standard model
#  model is bare name of standard model
findStandardModel <- function( model ) {
  standardModels <- system.file("models",package="VEModel")
  if ( is.null(model) || ! nzchar(model) ) return( dir(standardModels) )

  model <- model[1]
  if ( ! nzchar(standardModels) || ! model %in% dir(standardModels) ) {
    standardModels <- getOption("VEStandardModels",default=normalizePath("inst/models"))
  }
  model <- file.path(standardModels,model)
  if ( ! dir.exists(model) ) {
    stop(
      "No standard model called ",model,"\n",
      "installModel() to list available models"
      )
  }
  return(model)
}

## install a standard model either as a template (skeleton==TRUE) or
## a sample (skeleton==FALSE)
#  We're still expecting to distribute with standard models pre-installed
#  Called automatically from findModel, where modelPath must be a bare model name
#  Can install from other locations by calling this function with a more elaborate modelPath

SampleModelDataFormat <- c( templ="Template",samp="Sample" )

installStandardModel <- function( modelName, modelPath, RunParam_ls, confirm, skeleton ) {
  # Locate and install standard modelName into modelPath
  #   If modelPath is NULL or empty string, create conflict-resolved modelName in first available standard root
  #   If modelPath is an existing directory, put modelName into it (conflict-resolved name)
  #   If modelPath does not exist, but dirname(modelPath) exists, create new directory and put the model there
  #   If dirname(modelPath) also does not exist, tell user dirname(modelPath) does not exist and they have to try again
  model <- findStandardModel( modelName )
  if ( is.null(modelName) || ! nzchar(modelName) ) return(model) # Vector of available standard model names

  # Set up destination modelPath
  root <- getModelRoots(RunParam_ls,1)
  if ( missing(modelPath) || is.null(modelPath) ) modelPath <- modelName
  if ( ! isAbsolutePath(modelPath) ) {
    installPath <- normalizePath(file.path(root,modelPath),winslash="/",mustWork=FALSE)
  }
  if ( dir.exists(installPath) ) {
    installPath <- getUniqueName( dirname(installPath), basename(modelPath) )
  }

  # Confirm installation if requested
  install <- TRUE
  skeleton <- if ( skeleton ) "templ" else "samp"
  if ( confirm && interactive() ) {
    msg <- paste0("Install standard model '",modelName,"' (",SampleModelDataFormat[skeleton],") in ",installPath,"?\n")
    install <- confirmDialog(msg)
  }

  if ( ! install ) stop("Model ",modelName," not installed.",call.=FALSE)

  # Now do the installation
  message("Installing ",modelName," from ",model," as ",SampleModelDataFormat[skeleton])
  dir.create(installPath)

  # Locate the model and data source files
  model.path <- file.path(model,"model")
  model.files <- dir(model.path,full.names=TRUE)

  data.path <- file.path(model,skeleton)
  if ( ! dir.exists(data.path) ) stop("No ",SampleModelDataFormat[skeleton]," available for ",modelName)
  data.files <- dir(data.path,full.names=TRUE)

  file.copy(model.files,installPath,recursive=TRUE) # Copy standard model into modelPath
  file.copy(data.files,installPath,recursive=TRUE) # Copy skeleton data into modelPath
  message("Installed ",modelName," in ",installPath)

  return( list(modelName=modelName,modelPath=installPath) )
}

ve.model.copy <- function(newName=NULL,newPath=NULL) {
  if ( is.null(newPath) ) {
    if ( self$stageCount>1 ) {
      newPath <- dirname(self$stagePaths)
      newPath <- stripPathPrefix(newPath)
      if ( ! dir.exists(newPath) ) newPath <- dirname(newPath) # match might extend into basename
      if ( ! nzchar(newPath) ) {
        newPath <- dirname(self$stagePaths[1])
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
  newModelPath <- getUniqueName(newPath,newName)

  get.destination <- 
  newModelPath <- normalizePath(newModelPath,winslash="/",mustWork=FALSE)
  dir.create(newModelPath,showWarnings=FALSE)
  for ( p in 1:self$stageCount ) {
    copy.from <- setdiff(self$dir(path=p),private$artifacts(path=p))
    copy.to <- file.path(newModelPath,self$stagePaths[p])
    dir.create(copy.to,showWarnings=FALSE)
    file.copy(copy.from,copy.to,recursive=TRUE)
  }
    
  return( openModel(newModelPath) )
}

loadModelState <- function(path) {
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

# Initialize a VEModel from modelPath
ve.model.init <- function(modelPath=NULL,verbose=TRUE) {
  # Load system and user model configuration
  # TODO: figure out where to put RunParam_ls from those guys
  # Probably need to keep it within the VEModel structure
  self$RunParam_ls <- visioneval::loadConfiguration(ModelRun=FALSE)

  # Identify the run_model.R root location
  modelPaths <- findModel(modelPath,self$RunParam_ls)
  if ( ! modelPaths$isValid ) {
    stop("No VisionEval model (",modelPaths$runModelName,") found at ",modelPath)
  }

  # Set up the model name, folder tree and script files
  self$modelPath <- modelPaths$modelPath
  self$modelName <- basename(self$modelPath)
  if ( verbose ) print(self$modelPath)
  self$stagePaths <- modelPaths$stagePaths; # named vector of absolute paths to "stages" (folder with run_model.R)

  self$RunParam_ls$runModelName <- modelPaths$runModelName; # should be identical retrieving parameters

  # TODO: make this work with the new initializeModel(RunMode="Load") option.
  # We will actually load the ModelState for each stage, and build an in-memory
  #  one if the results folder is not yet created (no "run" attempted yet), or
  #  if there is no subfolder of results for a modelStage, or if there is no ModelState.rda
  #  in that subfolder. If we find a ModelState.rda, load it.
  # TODO: initializing the run_model.R script should allow for a single script that
  #  creates multiple stages, so we will rebuild the stages based on what we find
  #  when we initialize/parse the run_model.R scripts here.

  self$stageCount <- length(self$stagePaths)
  private$ModelState <- lapply(
    self$stagePaths,
    loadModelState
  )

  # TODO: rethink whether we're still using the VEOutput object...
  if ( length(private$ModelState)>0 && any(unlist(lapply(private$ModelState,length))>0) ) {
    private$outputObject <- VEOutput$new(private$ModelState,self)
  }

  self$runStatus <- sapply(
    simplify=TRUE,
    private$ModelState,
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
  legal.levels <- c(
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

# Run the modelPath (through its stages)
# Find the runModel script
# Work in the Results directory - need to relay locations from here to initializeModel
# Need to set Inputs directory
ve.run.model <- function(verbose=TRUE,path=NULL,stage=NULL,lastStage=NULL,log="ERROR") {

  if ( missing(path) ) path <- stage    # Still might be NULL; allow alias

  resultsDir <- file.path(self$modelPath,visioneval::getRunParameter("ResultsDir",self$RunParam_ls,"."))
  modelStateName <- visioneval::getRunParameter("modelStateFileName",self$RunParam_ls,"ModelState.Rda")

  pathLength <- length(self$stagePaths)
  stageStart <- if ( ! is.null(path) ) path else 1

  if ( is.null(lastStage) || lastStage < stageStart ) {
    lastStage <- stageStart
  } else if ( lastStage==0 ) {
    lastStage <- self$stageCount
  } # else it is what it is (hopefully a number > stageStart)

  if ( is.null(lastStage) ) lastStage <- self$stageCount
  for ( ms in stageStart:lastStage ) {
    stage <- self$stagePaths[ms]
    if ( verbose ) {
      if ( stage != "." ) {
        message("Running model stage:")
        message(stage)
      } else {
        message("Running model ",self$modelName)
      }
    }
    owd <- setwd(file.path(self$modelPath,stage))
    if ( ! "ve.model" %in% search() ) {
      envir <- attach(NULL,name="ve.model")
    } else {
      envir <- as.environment("ve.model")
    }
    envir$RunParam_ls <- self$RunParam_ls; # put it where initializeModel can see it
    self$status <- ""
    futile.logger::flog.threshold(log.level(log))
    tryCatchLog::tryCatchLog(
      {
        self$status <- "Running"
        # TODO: evaluate the parsed model script from ModelState_ls
        # TODO: which requires loading the model state (or pre-building) when we open the model
        # Currently runs model script for stage (opened relative to self$modelPath)
        sys.source(file.path(self$modelPath,stage,self$runModelName),envir=envir)
        if (verbose) visioneval::writeLog(paste0("Model stage ",stage," complete"),Level="info")
        self$status <- self$runStatus[ms] <- "Complete"
      },
      error = function(e) {
        remark <- paste0("Model stage ",stage," failed")
        msg <- as.character(e)
        if ( ! nzchar(msg) ) msg <- "Stopped."
        self$status <- "Error"
        visioneval::writeLog(c(remark,msg),Level="error")
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
          visioneval::writeLog(
            c(
              paste("Model Stage:",stage),
              self$status
            )
          )
        }
        modelStatePath <- file.path(resultsDir,self$stagePaths[ms],modelStateName)
        visioneval::setModelState(
          list(
            runStatus=self$runStatus[ms]
          ),
          FileName=modelStatePath,
          Save=file.exists(modelStatePath)
        )
        setwd(owd)
      }
    )
    if (verbose) {
      cat("Status:",self$status,"\n")
    }
    if ( self$status != "Complete" ) {
      break;
    }
  }
  private$ModelState <- lapply(
    file.path(resultsDir,self$stagePaths),
    FUN=loadModelState,
    modelStateName=modelStateName
  )
  if ( length(private$ModelState)>0 && all(unlist(lapply(private$ModelState,length))>0) ) {
    # 
    output <- self$output
    if ( ! is.null(output) ) {
      cat("Indexing output\n")
      output$index()
    }
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
      to.delete <- utils::select.list(choices=choices,preselect=preselect,multiple=TRUE,title="Delete Select Outputs")
      force <- length(to.delete)>0
    } else {
      force <- ( force || length(to.delete)>0 )
    }
    if ( force) {
      print(dir(to.delete),recursive=TRUE)
      unlink(to.delete,recursive=TRUE)
      private$ModelState <- lapply(
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

stripPathPrefix <- function(x) {
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

ve.model.print <- function() {
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParam_ls$DatastoreType,"\n")
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
ve.model.output <- function(rhs) {
  if ( ! missing(rhs) ) stop("Cannot assign to model output",call.=FALSE)
  if ( is.null(private$outputObject) ) {
    output <- VEOutput$new(private$ModelState,self) # parameters TBD
    if ( output$valid() ) {
      private$outputObject <- output
    } else {
      private$outputObject <- NULL
    }
  }
  if ( is.null(private$outputObject) ) {
    invisible(private$outputObject)
  } else {
    return(private$outputObject)
  }
}

# Here is the VEModel R6 class
# One of these objects is returned by "openModel"

VEModel <- R6::R6Class(
  "VEModel",
  public = list(
    # Public Data
    modelName=NULL,
    modelPath=NULL,
    stagePaths=NULL,
    stageCount=NULL,
    RunParam_ls=NULL,
    runStatus=NULL,
    status="Uninitialized",                 # used to be a function

    # Methods
    initialize=ve.model.init,
    run=ve.run.model,
    print=ve.model.print,                   # provides generic print functionality
    dir=ve.model.dir,
    clear=ve.model.clear,
    copy=ve.model.copy
  ),
  active = list(
    output=ve.model.output                  # Create a VEOutput object (if model is run)
  ),
  private = list(
    # Private Members
    runError=NULL,
    artifacts = ve.artifacts,               # Function may interrogate an existing Output
    outputObject=NULL,                      # VEOutput object for this model
    ModelState=NULL                         # ModelState placeholder
  )
)

#' Open a VisionEval Model
#'
#' @description
#' \code{openModel} opens a VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' @section Model Path and Name:
#'
#' The `modelPath` parameter locates a model object. When a model is opened, a
#'   a relative modelPath will be sought in the user's runtime `models` directory.
#'   An absolute path will be sought only in the user's file system.
#'
#' You can set an alternate location for the "models" subdirectory by providing an
#'   a path using, for example, `options(VEModelPath='mymodels')`. Relative paths
#'   will be sought below the VisionEval runtime directory. Absolute paths will
#'   be sought in the user's file system.
#'
#' An error will be raised if a model cannot be found with the indicated
#'   modelPath and modelName.
#'
#' @section Available Models:
#'
#' You can see the available models by providing an empty string for the `modelPath`.
#'   A VEModelList (q.v.) object will be printed as a side-effect, and also returned invisibly.
#'   You can open an installed model from the VEModelList using
#'   square brackets to index the list by position (`VEModelList[1]`) or by name
#'   (`VEModelList['VERSPM']`).
#'
#' @param modelPath Directory containing a VisionEval model; if an empty character string is
#'     provided, prints a list of available models (see details)
#' @return A VEModel object or a VEModelList of available models if no modelPath or modelName is
#'   provided; see details and `vignette("VEModel")`
#' @export
openModel <- function(modelPath="") {
  if ( missing(modelPath) || !nzchar(modelPath) ) {
    runtime <- get0("ve.runtime",ifnotfound=getwd())
    Param_ls <- visioneval::loadConfiguration(ModelRun=FALSE) # system/user profiles only
    return(dir(file.path(runtime,visioneval::getRunParameter("ModelDir",Param_ls=Param_ls,Default="models"))))
  } else {
    return( VEModel$new(modelPath = modelPath) )
  }
}

#' Install a Standard VisionEval Model
#'
#' @description
#' `installModel` installs a local copy of a standard VisionEval model and returns a VEModel object (q.v.) through
#'    which it can be manipulated (run or queried)
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#
#' An error will be raised if a model cannot be found or created with the indicated
#'   modelPath and modelName.
#'
#' You can see the available built-in (standard) models by providing an empty string for the `modelName`.
#'
#' @param modelName Name of a standard model to install; if empty or NULL (default), list
#'   available standard models.
#' @param modelPath Location to place the copy of modelName standard model. Created relative to
#'   ve.runtime/models. If directory does not exist, create it and copy the modelName into it.
#'   If directory does exist, create a unique variant of modelName adjacent to it. If it is NULL
#'   create a unique variant of modelName in ve.runtime/models.
#' @param skeleton if TRUE (default), install just skeleton files for the model; otherwise
#'   install the sample model.
#' @param confirm if TRUE (default) and running interactively, prompt user to confirm, otherwise
#'   just do it.
#' @return A VEModel object of the model that was just installed
#' @export
installModel <- function(modelName=NULL, modelPath=NULL, skeleton=FALSE, confirm=!skeleton) {
  model <- installStandardModel(modelName, modelPath, confirm, skeleton)
  if ( is.list(model) ) {
    return( VEModel$new( modelPath=model$modelPath ) )
  } else {
    return( model ) # should be a character vector of available standard models
  }
}
