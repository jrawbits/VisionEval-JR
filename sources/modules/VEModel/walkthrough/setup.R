# Locate ve-lib
checkVE <- function(lib.loc=NULL) {
  return(all( c("visioneval","VEModel") %in% installed.packages(lib.loc=lib.loc)[,"Package"]))
}

vePresent <- function() {
  if ( checkVE() ) return(TRUE)
  for ( wd in c(getwd(),dirname(getwd())) ) {
    lib.loc <- file.path(wd,"ve-lib")
    if ( checkVE(lib.loc) ) {
      .libPaths( c( lib.loc, .libPaths() ) )
      return(TRUE)
    }
  }
  return(FALSE)
}

if ( ! vePresent() ) stop("Cannot locate ve.lib - are you starting from your VE runtime?")

# Set up a runtime directory for a walkthrough
local( {
  ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
  if ( ! is.na(ve.runtime ) ) {
    if ( ! dir.exists(ve.runtime) ) {
      ve.runtime <- NA
    }
  }
  walkthrough.action <- "Using"
  if ( is.na(ve.runtime) ) {
    # Create a walkthrough directory within the current working directory
    message("Setting up walkthrough in ",getwd())
    ve.runtime <- grep("runtime.*",list.dirs(),value=TRUE)[1]
    if ( ! dir.exists(ve.runtime) ) {
      ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="."),winslash="/",mustWork=FALSE)
      dir.create(ve.runtime)
      walkthrough.action <- "Creating"
    }
    ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  }
  message(walkthrough.action," walkthrough runtime directory:")
  message(ve.runtime)

  Sys.setenv(VE_RUNTIME=ve.runtime)
  setwd(ve.runtime)
} )

# Load VEModel package (in effect, the visioneval environment)
require(VEModel)

# Set up sample models
message("Creating model environment")
if ( ! dir.exists("models") ) {
  dir.create("models")
} else {
  # clean up the mini-model if it's still there
  if ( dir.exists("model/BARE") ) unlink("model/BARE",recursive=TRUE)
}
message("Pre-running model (Base VERSPM)")
model.path <- "VERSPM-run"
if ( ! dir.exists(file.path("models",model.path)) ) {
  message("Installing VERSPM ('base' variant) as ",model.path)
  vr <- installModel("VERSPM",modelPath=model.path,variant="base",confirm=FALSE)
} else {
  message("Using existing ",model.path)
  vr <- openModel(model.path)
}
message("Making sure model '",model.path,"' has been run...")
vr$run() # default "continue" will not re-run model if already "Run Complete"
print(vr)

# delete any other models in "models" directory
otherModels <- grep("VERSPM-run",dir("models",full.names=TRUE),invert=TRUE,value=TRUE)
if ( length(otherModels) > 0 ) unlink(otherModels,recursive=TRUE)

# create helper function to switch logLevel
# ("info" gives a lot more details, "trace" is overwhelming)
logLevel <- function(log="warn") visioneval::initLog(Save=FALSE,Threshold=log)

# Define a function to make a mini-model (adapted from tests/test.R)
makeMiniModel <- function(baseModel,log="warn" ) {

  logLevel(log)

  if ( missing(baseModel) || ! "VEModel" %in% class(baseModel) ) {
    stop("Must provide baseModel (opened VEModel)")
  }

  message("Cleaning up previous mini models.")
  models.dir <- file.path(getwd(),"models")
  obsolete <- dir(models.dir,pattern="^bare",ignore.case=TRUE,full.names=TRUE)
  for ( oo in obsolete ) {
    if ( dir.exists(oo) ) unlink(oo,recursive=TRUE)
  }

  bare.dir <- file.path(models.dir,"BARE")
  message("Making mini model in ",bare.dir)
  bare.script <- file.path(bare.dir,visioneval::getRunParameter("ScriptsDir"))
  bare.inputs <- file.path(bare.dir,visioneval::getRunParameter("InputDir"))
  bare.defs   <- file.path(bare.dir,visioneval::getRunParameter("ParamDir"))
  dir.create(bare.dir)
  dir.create(bare.script)
  dir.create(bare.inputs)
  dir.create(bare.defs)
  print( dir(bare.dir,full.names=TRUE) )

  message("Create the model configuration")
  runConfig_ls <-  list(
      Model       = "Mini Model Test",
      Scenario    = "MiniModel",
      Description = "Minimal model constructed programmatically",
      Region      = "RVMPO",
      State       = "OR",
      BaseYear    = "2010",
      Years       = c("2010")
    )
  viewSetup(Param_ls=runConfig_ls)

  message("Save the model configuration")
  configFile <- file.path(bare.dir,"visioneval.cnf")
  cat(configFile,"\n")
  yaml::write_yaml(runConfig_ls,configFile)

  message("Make the ModelScript")
  runModelFile <- file.path(bare.script,"run_model.R")
  runModel_vc <- c(
    '', # Don't ask why, but without this line the script gets written wrong...
    'for(Year in getYears()) {',
    'runModule("CreateHouseholds","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictWorkers","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    '}'
  )
  cat(runModelFile,paste(runModel_vc,collapse="\n"),sep="\n")
  writeLines(runModel_vc,con=runModelFile)

  message("Create model geography (copying from base VERSPM)")
  base.defs <- baseModel$setting("ParamPath",shorten=FALSE)
  from <- file.path(base.defs,c("units.csv","deflators.csv","geo.csv"))
  file.copy(from=from,to=bare.defs)
  print(bare.defs)
  print(dir(bare.defs,full.names=TRUE))

  message("Now open the bootstrapped mini model")
  bare <- openModel(basename(bare.dir))
  print(bare)

  message("Copy over input files from baseModel (including model_parameters.json)")
  # Note this strategy won't work if the baseModel has a complex InputPath
  #   (files in stage folders or otherwise distributed)
  # In that case, you probably want to use "insider information" to copy the files using
  #   File Explorer or an equivalent tool
  base.inputs <- unique(baseModel$dir(inputs=TRUE,shorten=FALSE))
  inputs <- bare$list(inputs=TRUE,details=c("FILE"))
  required.files <- unique(file.path(base.inputs,c(inputs[,"FILE"],"model_parameters.json")))
  required.files <- required.files[which(file.exists(required.files))]
  file.copy(from=required.files,to=bare.inputs) # or copy to bare.defs...

  message("Re-open BARE model and review input files")
  bare$configure()
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  required.files <- file.path(ifelse(is.na(inputs$INPUTDIR),"",inputs$INPUTDIR),inputs$FILE)
  required.exists <- file.exists(required.files)
  required.files <- sub( runtimeEnvironment()$ve.runtime, "", required.files )
  required.files <- data.frame(EXIST=ifelse(is.na(inputs$INPUTDIR),FALSE,required.exists),FILE=required.files)
  message("Required Files (all should EXIST):\n")
  print(unique(required.files))

  message("Making demo for LoadModel as 'BARE-load'")

  loadPath <- file.path("models","BARE-load")
  if ( dir.exists(loadPath) ) unlink(loadPath,recursive=TRUE)
  loadModel <- bare$copy("BARE-load",copyResults=FALSE)
  print(loadModel)
  message("Set up load script...")
  loadModelPath <- loadModel$setting("ModelDir",shorten=FALSE)
  runModelFile <- file.path(
    loadModelPath,
    loadModel$setting("ModelScript")
  )
  runModel_vc <- c(
    '',
    'for(Year in getYears()) {',
    'runModule("AssignLifeCycle","VESimHouseholds",RunFor = "AllYears",RunYear = Year)',
    'runModule("PredictIncome", "VESimHouseholds", RunFor = "AllYears", RunYear = Year)',
    'runModule("PredictHousing", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("LocateEmployment", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    'runModule("AssignLocTypes", "VELandUse", RunFor = "AllYears", RunYear = Year)',
    '}'
  )
  writeLines(runModel_vc,con=runModelFile)

  message("Configure LoadModel")
  # We'll just do it from scratch rather than reading/modifying
  # Region, BaseYear and Years don't change
  runConfig_ls <-  list(
    Model       = "LOAD Model Test",
    Scenario    = "Test LoadModel / LoadDatastore",
    Description = "Add a step onto a different previous model",
    Region      = bare$setting("Region"),
    BaseYear    = bare$setting("BaseYear"),
    Years       = loadModel$setting("Years"),
    LoadModel   = bare$modelPath,
    LoadStage   = names(bare$modelStages)[1] # would default there anyway
  )
  configFile <- file.path(loadModelPath,"visioneval.cnf")
  yaml::write_yaml(runConfig_ls,configFile)

  message("Reload model with LoadDatastore")
  browser()
  loadModel <- openModel(basename(loadModel$modelPath),log=log)

  message("Copy additional inputs")
  base.inputs <- file.path(
    baseModel$setting("InputPath",shorten=FALSE),
    baseModel$setting("InputDir")
  )
  cat("Base Inputs",base.inputs,"\n")
  inputs <- loadModel$list(inputs=TRUE,details=c("FILE"))
  required.files <- unique(file.path(base.inputs,inputs[,"FILE"]))
  required.files <- required.files[which(file.exists(required.files))]
  cat("Required Files:\n")
  print(basename(required.files))

  message("Copying additional input files")
  load.inputs <- file.path(
    loadModel$setting("InputPath",shorten=FALSE),
    loadModel$setting("InputDir")
  )
  message("Remove base model inputs - will just have new ones")
  unlink(load.inputs,recursive=TRUE)
  dir.create(load.inputs)
  from <- required.files
  file.copy(from=from, to=load.inputs )

  message("Copy model parameters to 'inputs' - could also be in 'defs')")
  from <- file.path(base.inputs,"model_parameters.json")
  file.copy(from=from,to=load.inputs) # or copy to bare.defs...
  print(dir(load.inputs))

  return(bare)

