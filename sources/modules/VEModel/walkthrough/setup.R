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
    ve.runtime <- grep("runtime.nohup make *",list.dirs(),value=TRUE)[1]
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
}
message("Pre-running model (Staged VERSPM)")
model.path <- "VERSPM-run"
if ( ! dir.exists(file.path("models",model.path)) ) {
  message("Installing VERSPM ('pop' variant) as ",model.path)
  vr <- installModel("VERSPM",modelPath=model.path,variant="pop",confirm=FALSE)
} else {
  message("Using existing ",model.path)
  vr <- openModel(model.path)
}
message("Making sure model '",model.path,"' has been run...")
vr$run() # default "continue" will not re-run model if already "Run Complete"
print(vr)

logLevel <- function(log="warn") visioneval::initLog(Save=FALSE,Threshold=log)

# Define a function to make a mini-model (adapted from tests/test.R)
makeMiniModel <- function(baseModel,log="warn" ) {

  logLevel(log)

  message("Cleaning up previous mini models.")
  models.dir <- file.path(getwd(),"models")
  obsolete <- dir(models.dir,pattern="^bare",ignore.case=TRUE,full.names=TRUE)
  for ( oo in obsolete ) {
    if ( dir.exists(oo) ) unlink(oo,recursive=TRUE)
  }

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
  yaml::write_yaml(runConfig_ls,configFile)
  cat(configFile,paste(readLines(configFile),collapse="\n"),sep="\n")

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
  base.inputs <- unique(baseModel$dir(inputs=TRUE,shorten=FALSE))
  inputs <- bare$list(inputs=TRUE,details=c("FILE"))
  required.files <- unique(file.path(base.inputs,c(inputs[,"FILE"],"model_parameters.json")))
  required.files <- required.files[which(file.exists(required.files))]
  file.copy(from=required.files,to=bare.inputs) # or copy to bare.defs...

  message("Re-open BARE model and review input files")
  bare$configure()
  inputs <- bare$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
  required.files <- file.path(ifelse(is.na(inputs$INPUTDIR),"",inputs$INPUTDIR),inputs$FILE)
  required.files <- data.frame(EXISTS=ifelse(is.na(inputs$INPUTDIR),FALSE,file.exists(required.files)),FILE=required.files)
  cat("Required Files (all should EXIST):\n")
  print(unique(required.files))

  return(bare)
}
