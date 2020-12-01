#============
#visioneval.R
#============

#This script defines the main functions that implement the VisionEval framework
#and are intended to be exported.

utils::globalVariables(c("initDatastore","Year"))

#INITIALIZE MODEL
#================
#' Initialize model.
#'
#' \code{initializeModel} a visioneval framework model user function
#' that initializes a VisionEval model, loading all parameters and inputs, and
#' making checks to ensure that model can run successfully.
#'
#' This function does several things to initialize the model environment and
#' datastore including:
#' 1) Initializing a file that is used to keep track of the state of key model
#' run variables and the datastore;
#' 2) Initializes a log to which messages are written;
#' 3) Creates the datastore and initializes its structure, reads in and checks
#' the geographic specifications and initializes the geography in the datastore,
#' or loads an existing datastore if one has been identified;
#' 4) Parses the model run script to identify the modules in their order of
#' execution and checks whether all the identified packages are installed and
#' the modules exist in the packages;
#' 5) Checks that all data requested from the datastore will be available when
#' it is requested and that the request specifications match the datastore
#' specifications;
#' 6) Checks all of the model input files to determine whether they they are
#' complete and comply with specifications.
#'
#' @param ModelScriptFile A string identifying the path to the file that contains
#'   the steps in the model to run (i.e. the calls to \code{runModule})
#' @param ParamDir A string identifying the relative or absolute path to the
#'   directory where the parameter and geography definition files are located.
#'   The default value is "defs".
#' @param RunParamFile A string identifying the name of a JSON-formatted text
#' file that contains parameters needed to identify and manage the model run.
#' The default value is "run_parameters.json".
#' @param GeoFile A string identifying the name of a text file in
#' comma-separated values format that contains the geographic specifications
#' for the model. The default value is "geo.csv".
#' @param ModelParamFile A string identifying the name of a JSON-formatted text
#' file that contains global model parameters that are important to a model and
#' may be shared by several modules.
#' @param LoadDatastore A logical identifying whether an existing datastore
#' should be loaded.
#' @param DatastoreName A string identifying the full path name of a datastore
#' to load or NULL if an existing datastore in the working directory is to be
#' loaded.
#' @param SaveDatastore A logical identifying whether if an existing datastore
#' in the working directory should be renamed rather than removed.
#' @param SimulateRun A logical identifying whether the model run should be
#' simulated: i.e. each step is checked to confirm that data of proper type
#' will be present when the module is called.
#' @return None. The function prints to the log file messages which identify
#' whether or not there are errors in initialization. It also prints a success
#' message if initialization has been successful.
#' @export
initializeModel <-
function(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = FALSE,
  DatastoreName = NULL,
  SaveDatastore = TRUE,
  SimulateRun = FALSE,
  RunMode = NULL
) {
  # NOTE: This function expects to have getwd() be the same directory that the
  # ModelState and Datastore will be created in (the model run/scenario). The
  # actual run_model.R script can be elsewhere, as can the inputs, run_parameters,
  # model_parameters, defs, etc.

  # Determine which initialization steps to run (supports VEModel end user
  # API). Default is to run all three, reproducing original initializeModel
  # functionality. Set ve.model$runSteps to select runSteps (or modify
  # the initializeModel call).
  # runSteps will also affect any runModule/runScript/Stage calls - if they are
  # executed outside a "Run" step, they will do nothing and return.
  # instructions - they won't run unless
  runSteps <- getInitializeMode(RunMode)

  # The runSteps do the following:
  #
  #   Empty and replace the ve.model environment.
  #
  #   "Init" will create a new ModelState. Any existing ModelState will be moved aside. An
  #   existing Datastore will be moved aside if SaveDatastore is TRUE and it is not being
  #   reloaded. A new ModelState will be created with basic model run type settings in place (Name,
  #   Log, etc.). The ve.model environment will be reinitialized. Then "Load" will always be
  #   performed. 
  #
  #   "Load" will load an existing ModelState and check that the model has everything it needs to
  #   run, updating ModelState as needed. "Load" will perform "Init" first if no ModelState file
  #   exists, but it will build the ModelState_ls without saving it. We'll only save it when "Run"
  #   is processed. It will clear the ve.model environment, load the ModelState, parse the
  #   run_model.R script, check presence of files, check specifications, etc. ModelState_ls
  #   remains in the ve.model environment. "Load" will be performed if ModelState is not present
  #   in ve.model when we try to "Run". "Load" will also be performed automatically when a VEModel
  #   object is created.
  #
  #   "Run" will initialize the Datastore by loading an existing the "Datastore" (if LoadDatastore
  #   is TRUE), or by creating a new blank Datastore. It will presume that ModelState in ve.model
  #   is current. Once the "Run" initialization step is complete, the model can be run by
  #   executing runModule / runScript / Stage steps.

  #===================================
  #DETERMINE RUN STEPS TO PERFORM
  #===================================

  # Clear the current ve.model environment (will create if missing)
  envir <- getModelEnvironment()
  rm( list=ls(envir=envir, all=TRUE),envir = envir )

  previousModelState <- NULL
  previousModelStateName <- ""
  savedPreviousModelState <- FALSE
  Timestamp <- gsub(" ", "_",Sys.time()) # Suitable for use in file name

  if ( "Run" %in% runSteps ) {
    initLog(Timestamp) # start/reset the model run logger
  }

  currentModelStateName <- getModelStateFile()
  saveModelState <- any(c("Init","Run") %in% runSteps)
  # Subsequent setModelState calls in "Run" should include "Save=saveModelState"

  if (
    saveModelState || # always make a new one if we're doing Init or Run
    ! attr(currentModelStateName,"exists") ) # make a new one, but don't save if just Loading
  {
    # Make a new ModelState_ls and perhaps save it to a file
    writeLog("Initializing Model. This may take a while",Level="info")

    # Set up a new ModelState with parameters for the model
    # If a ModelState.Rda file exists and this one is to be save, rename it
    if (attr(currentModelStateName,"exists") && saveModelState) {
      # Read the model state to see if there's a run time
      previousModelState <- readModelState(FileName=currentModelStateName,envir=new.env())

      # Get timestamp from previous ModelState
      if ( "LastChanged" %in% previousModelState ) {
        previousTimestamp <- gsub(" ","_",previousModelState$LastChanged)
      } else {
        previousTimestamp <- Timestamp
      }

      # Move the previous model state out of the way
      previousModelStateName <- paste0("ModelState_",previousTimestamp,".Rda")
      savedPreviousModelState <- file.rename(currentModelStateName, previousModelStateName)
    }

    # TODO: rationalize the initializeModel arguments versus .VisionEval environment
    # versus RunParamFile. Should verify existence of the run_model.R file at this
    # point - we can invoke initialize without 

    initModelStateFile(Dir = ParamDir, ParamFile = RunParamFile, Save=saveModelState)
    readGeography(Dir = ParamDir, GeoFile = GeoFile, Save=saveModelState)

  } else { # ModelState file exists and not re-initializing
    # open the existing model state
    loadModelState(currentModelStateName)
  }

  # Assign the correct datastore interaction functions
  # Those go into ve.model environment to support the ModelState
  assignDatastoreFunctions(readModelState()$DatastoreType)

  #=======================================
  #CHECK CONFLICTS WITH EXISTING DATASTORE
  #=======================================
  # Do this step in all run modes (requires having a ModelState in ve.model)
  # We'll act on what we find out about the Datastore in the "Run" mode
  # e.g. by renaming the old one, loading a previous one, etc.
  DstoreConflicts <- local(
    {
      # Prepare to accumulate errors and warnings
      ErrMsg <- character(0)
      InfoMsg <- character(0)

      # Get the model state
      G <- getModelState()

      #Normalized path name of the datastore from the ModelState
      RunDstoreName <-
      normalizePath(G$DatastoreName, winslash = "/", mustWork = FALSE)
      RunDstoreDir <- dirname(RunDstoreName)

      #Normalized path name of the datastore to be loaded, or empty string
      if (is.null(DatastoreName) ) {
        LoadDstoreName <- ""
        LoadDstoreDir <- ""
      } else {
        LoadDstoreName <- normalizePath(DatastoreName, winslash = "/", mustWork = FALSE)
        LoadDstoreDir <- dirname(LoadDstoreName)
      }

      # Force LoadDatastore to TRUE if a DatastoreName was provided
      LoadDatastore <- LoadDatastore && !is.null(DatastoreName)

      #Error if LoadDatastore and the DatastoreName does not exist
      if ( LoadDatastore && !file.exists(LoadDstoreName) ) {
        ErrMsg <- c(ErrMsg,
          paste(
            "Failed to find Datastore",DatastoreName,". ",
            "Perhaps the full path name was not specified."
          )
        )
      }

      # Flag if we're trying to (re-)load the existing Datastore
      loadExistingDatastore <- (RunDstoreName == LoadDstoreName) && (RunDstoreDir == LoadDstoreDir)

      #Error if existing datastore overwritten and not loaded or saved
      if ( file.exists(RunDstoreName) ) {
        if ( !(LoadDatastore || SaveDatastore) ) {
          ErrMsg <- c(ErrMsg,
            paste(
              "The existing datastore, ",RunDstoreName," would be overwritten.\n",
              "To move it aside, set SaveDatastore=TRUE.",
              "To add to it, set DatastoreName='",RunDstoreName,"' and set LoadDatastore=TRUE.\n"
            )
          )
        } else if (
          LoadDatastore &&
          ! SaveDatastore &&
          ! loadExistingDatastore ) {
          ErrMsg <- c(ErrMsg,
            paste(
              "Loading DatastoreName='",LoadDstoreName,"'.",
              "Existing datastore ",RunDstoreName," would be overwritten.",
              "Set SaveDatastore=TRUE to move the existing datastore aside.\n",
              "Set DatastoreName='",RunDstoreName,"' to add to it.\n"
            )
          )
        }
      } else if ( SaveDatastore ) {
        InfoMsg <- c(InfoMsg,
          paste(
            "Run parameter SaveDatastore=TRUE, but there is ",
            "no existing Datastore at ",runDstoreName,".\n",
            "SaveDatastore will be ignored."
          )
        )
      }

      #If errors configuring Datastore, print error message to log, and stop
      if (length(ErrMsg) != 0) {
        writeLog(ErrMsg)
        return( list( Err = ErrMsg) )
      }

      #---------------------------------------------------------
      #If LoadDatastore from somewhere else, check compatibility
      #---------------------------------------------------------
      if (LoadDatastore) {
        if ( ! loadExistingDatastore ) { # Loading a different datastore (other model/stage)
          #Run datastore type from current ModelState
          RunDstoreType <- (G$DatastoreType)
          #Load the model state associated with the previous Datastore and compare to current
          loadModelStateName <- if ( nzchar(previousModelStateName) ) {
            previousModelStateName
          } else {
            file.path(LoadDstoreDir,basename(currentModelStateName))
          }
          LoadEnv <- new.env()
          load(loadModelStateName, envir = LoadEnv)
          LoadDstoreType <- (LoadEnv$ModelState_ls$DatastoreType)
          #Save the VERequiredPackages from that model state

          #Check that run and load datastores are of same type
          if ( ! loadExistingDatastore && RunDstoreType != LoadDstoreType) {
            ErrMsg <- c(ErrMsg, paste(
              "Incompatible datastore types.\n",
              "This Model Run has type: ", RunDstoreType, ".\n",
              "Load Datastore has type: ", LoadDstoreType, "."
            ))
          }
          #Check that geography, units, deflators and base year are consistent
          BadGeography <-
          !all.equal(ModelState_ls$Geo_df, LoadEnv$ModelState_ls$Geo_df)
          BadUnits <-
          !all.equal(ModelState_ls$Units, LoadEnv$ModelState_ls$Units)
          BadDeflators <-
          !all.equal(ModelState_ls$Deflators, LoadEnv$ModelState_ls$Deflators)
          BadBaseYear <- ! (ModelState_ls$BaseYear == LoadEnv$ModelState_ls$BaseYear)

          if ( BadGeography || BadUnits || BadDeflators || BadBaseYear) {
            elements <- paste(
              "Geography"[BadGeography],
              "Units"[BadUnits],
              "Deflators"[BadDeflators],
              "Base Year"[BadBaseYear],
              sep="_"
            )
            elements <- gsub("_",", ",gsub("^_+|_+$","",elements))
            ErrMsg <- c(ErrMsg, paste(
              "Inconsistent ",elements," between Model Run and Load Datastore."
            ))
          }
          #If errors, restore model state, print error message to log, and stop
          if (length(ErrMsg) != 0) {
            writeLog(ErrMsg)
            return(list(Err = ErrMsg))
          }
        }

        #Write information message to log if any
        if (length(InfoMsg) != 0) {
          writeLog(InfoMsg)
          return(list(Err=character(0)))
        }
      }
    }
  )

  if (length(DstoreConflicts$Err) != 0) {
    # Restore previous model state if we had moved it out of the way
    if ( nzchar(previousModelStateName) && savedPreviousModelState ) {
      file.remove(currentModelStateName)
      if (file.exists(previousModelStateName)) {
        file.rename(previousModelStateName, currentModelStateName)
      }
    }
    stop("Datastore configuration error: See log messages."))
}
  rm(DstoreConflicts)

  if ( any(c("Load","Run") %in% (runSteps)) )  {
    # We should already have ve.model$ModelState_ls in place

    # TODO: Parse out the run_model.R script, all the model inputs and outputs, create runnable structures.
    # Finish ginning up the ModelState for the indicated run_model.R script
    # Process the model specifications and load up things like ModulesByPackage_df
    # "Load" gives us a way to do that without actually starting a "Run"
    #===========================================================================
    #PARSE SCRIPT TO MAKE TABLE OF ALL THE MODULE CALLS, CHECK AND COMBINE SPECS
    #===========================================================================
    # TODO: Put this into the "Load" step

    #Parse script and make data frame of modules that are called directly
    # JRaw: changed parseModelScript so it always returns the elements, rather
    #       than secretly stuffing them into the ModelState
    parsedScript <- parseModelScript(ModelScriptFile)

    # JRaw: Create required package list
    RequiredPkg_ <- parsedScript$RequiredVEPackages
    # JRaw: Handle requirements from previously loaded model state
    if ( LoadDatastore ) {
      G <- getModelState()
      if ( "RequiredVEPackages" %in% names(G) ) {
        RequiredPkg_ <- c(RequiredPkg_, G$RequiredVEPackages)
      }
      rm(G)
    }
    setModelState(
      list(
        ModuleCalls_df=parsedScript$ModuleCalls_df,
        RequiredVEPackages=RequiredPkg_),
      Save=saveModelState
    )

    # JRaw: not an error to have already saved ModuleCalls_df: ModelState hosts module calls with duplicates
    ModuleCalls_df <- unique(parsedScript$ModuleCalls_df)

    # Report any required packages that are not also in module calls
    umc <- unique(ModuleCalls_df$PackageName)
    explicitRequired_ <- unique(RequiredPkg_)
    if ( any( not.in.umc <- ! (explicitRequired_ %in% umc) ) ) {
      for ( p in explicitRequired_[not.in.umc] ) message(paste("Package",p,"is required"))
    }
    RequiredPkg_ <- c(umc,explicitRequired_)

    #Get list of installed packages
    #Check that all module packages are in list of installed packages
    InstalledPkgs_ <- rownames(installed.packages())
    MissingPkg_ <- RequiredPkg_[!(RequiredPkg_ %in% InstalledPkgs_)]
    if (length(MissingPkg_ != 0)) {
      Msg <-
      paste0("One or more required packages need to be installed in order ",
        "to run the model. Following are the missing package(s): ",
        paste(MissingPkg_, collapse = ", "), ".")
      stop(Msg)
    }
    #Check for 'Initialize' module in each package if so add to ModuleCalls_df
    Add_ls <- list()
    for (Pkg in unique(ModuleCalls_df$PackageName)) {
      PkgData <- data(package = Pkg)$results[,"Item"]
      if ("InitializeSpecifications" %in% PkgData) {
        Add_df <-
        data.frame(
          ModuleName = "Initialize",
          PackageName = Pkg,
          RunFor = "AllYears",
          Year = "Year"
        )
        Add_ls[[Pkg]] <- Add_df
      }
    }
    #Insert Initialize module entries into ModuleCalls_df
    Pkg_ <- names(Add_ls)
    for (Pkg in Pkg_) {
      Idx <- head(grep(Pkg, ModuleCalls_df$PackageName), 1)
      End <- nrow(ModuleCalls_df)
      ModuleCalls_df <- rbind(
        ModuleCalls_df[1:(Idx - 1),],
        Add_ls[[Pkg]],
        ModuleCalls_df[Idx:End,]
      )
      rm(Idx, End)
    }
    rm(Pkg, Pkg_, Add_ls)
    #Identify all modules and datasets in required packages
    Datasets_df <-
    data.frame(
      do.call(
        rbind,
        lapply(RequiredPkg_, function(x) {
          data(package = x)$results[,c("Package", "Item")]
        })
      ), stringsAsFactors = FALSE
    )
    WhichAreModules_ <- grep("Specifications", Datasets_df$Item)
    ModulesByPackage_df <- Datasets_df[WhichAreModules_,]
    ModulesByPackage_df$Module <-
    gsub("Specifications", "", ModulesByPackage_df$Item)
    ModulesByPackage_df$Item <- NULL
    DatasetsByPackage_df <- Datasets_df[-WhichAreModules_,]
    names(DatasetsByPackage_df) <- c("Package", "Dataset")
    #Save the modules and datasets lists in the model state
    setModelState(
      list(
        ModulesByPackage_df = ModulesByPackage_df,
        DatasetsByPackage_df = DatasetsByPackage_df),
      Save=saveModelStatex
    )
    rm(Datasets_df, WhichAreModules_)
    #Iterate through each module call and check availability and specifications
    #create combined list of all specifications
    #JRaw: AllSpecs_ls is optionally used to simulate the model run, then discarded
    #      However, parsing the specs as we build AllSpecs_ls does critical error checking
    # ModulesByPackage_df lists all modules available in the packages
    # ModuleCalls_df lists only modules that appear in runModule commands
    Errors_ <- character(0)
    AllSpecs_ls <- list()
    for (i in 1:nrow(ModuleCalls_df)) {
      AllSpecs_ls[[i]] <- list()
      ModuleName <- ModuleCalls_df$ModuleName[i]
      AllSpecs_ls[[i]]$ModuleName <- ModuleName
      PackageName <- ModuleCalls_df$PackageName[i]
      AllSpecs_ls[[i]]$PackageName <- PackageName
      AllSpecs_ls[[i]]$RunFor <- ModuleCalls_df$RunFor[i]
      #Check module availability
      Err <- checkModuleExists(ModuleName, PackageName, InstalledPkgs_)
      if (length(Err) > 0) {
        Errors_ <- c(Errors_, Err)
        next()
      }
      #Load and check the module specifications
      Specs_ls <-
      processModuleSpecs(getModuleSpecs(ModuleName, PackageName))
      Err <- checkModuleSpecs(Specs_ls, ModuleName)
      if (length(Err) > 0) {
        Errors_ <- c(Errors_, Err)
        next()
      } else {
        AllSpecs_ls[[i]]$Specs <- Specs_ls
      }
      #If the 'Call' spec is not null and is a list, check the called module
      if (!is.null(Specs_ls$Call) && is.list(Specs_ls$Call)) {
        #Iterate through module calls
        for (j in 1:length(Specs_ls$Call)) {
          Call_ <- unlist(strsplit(Specs_ls$Call[[j]], "::"))
          #Check module availability
          if (length(Call_) == 2) { # package explicitly specified
            Err <-
            checkModuleExists(
              Call_[2],
              Call_[1],
              InstalledPkgs_,
              c(Module = ModuleName, Package = PackageName))
          } else  {
            if (length(Call_) == 1) { # only module name is provided
              if (! any(Call_ %in% ModulesByPackage_df$Module) ) {
                Err <- c(
                  paste0("Error in runModule call for module ", Call_,"."),
                  "It is not present in any package already identified in the model run script.",
                  "Please add requirePackage(<package-with-module>) to the script."
                )
              } else {
                callPkgs_ <- ModuleCalls_df$PackageName[Call_ %in% ModuleCalls_df$ModuleName]
                if ( length(callPkgs_)==1 ) { # use existing explicit call to module
                  Call_ <- c( callPkgs_, Call_ )
                } else  { # callPkgs_ is probably length 0, but could also have more than 1
                  Pkg <- ModulesByPackage_df$Package[ModulesByPackage_df$Module == Call_]
                  Call_ <- c(unique(Pkg), Call_)
                  if (length(Call_) > 2 ) { # More than one package contains the module
                    callPkgs_ <- Call_[-length(Call_)]
                    callModule_ <- Call_[length(Call_)]
                    testPkgs_ <- callPkgs_[callPkgs_ %in% explicitRequired_]
                    if ( length(testPkgs_) == 0 ) testPkgs_ <- callPkgs_  # No explicit required package
                    if ( length(testPkgs_) > 1 ) { # Could not resolve by explicit required package
                      Msg_ <- paste("Providing module",callModule_,"from package",testPkgs_[1])
                      Warn_ <- c(
                        Msg_,
                        paste("Also present in: ", paste(testPkgs_[2:length(testPkgs_)],collapse=", ")),
                        "Use requirePackage() to force selection."
                      )
                      message(Msg_)
                      writeLog(Warn_)
                    } else { # Resolved to exactly one package with the module
                      writeLog(paste("Provided module",callModule_,"from Package",testPkgs_[1]))
                    }
                    Call_ <- c(testPkgs_[1],callModule_) # Use the first package found, unless explicit
                  }
                }
              }
            } else {
              Err <- paste("Cannot fathom Call specification:",Specs_ls$Call[[j]])
            }
          }
          if (length(Err) > 0) {
            Errors_ <- c(Errors_, Err)
            next()
          }
          # Load and check the module specifications and add Get specs if
          # there are no specification errors
          for (i in 1:(length(Call_)-1)) { # Code above forces length(Call_) always to be 2 or fail
            CallSpecs_ls <-
            processModuleSpecs(getModuleSpecs(Call_[length(Call_)], Call_[i]))
            Err <- checkModuleSpecs(CallSpecs_ls, Call_[length(Call_)])
            if (length(Err) > 0) {
              Errors_ <- c(Errors_, Err)
              next()
            } else {
              AllSpecs_ls[[i]]$Specs$Get <-
              c(AllSpecs_ls[[i]]$Specs$Get <- Specs_ls$Get)
            }            
          }
        }
      }
    }
    #If any errors, print to log and stop execution
    if (length(Errors_) > 0) {
      Msg <-
      paste0("There are one or more errors in the module calls: ",
        "package not installed, or module not present in package, ",
        "or errors in module specifications. ",
        "Check the log for details.")
      writeLog(Errors_)
      stop(Msg)
    }

    #==================
    #SIMULATE MODEL RUN
    #==================
    # TODO: Simply goes thorugh the specs to make sure everything is present when it needs to be
    if (SimulateRun) {
      simDataTransactions(AllSpecs_ls)
    }
  }

  if ( "Run" %in% runSteps ) {
    initLog() # Create an auxiliary logger for run model errors (in addition to console)

    #==============================================================
    # PREPARE THE DATASTORE (SAVE PREVIOUS THEN LOAD OR INITIALIZE)
    #==============================================================
    # TODO: some of the modelstate setup should happen in "Load" step
    #----------------------------
    #Set up objects and functions
    #----------------------------
    #Get the model state
    G <- getModelState()
    #Define function to load model state file to assigned name
    assignLoadModelState <- function(FileName) {
      TempEnv <- new.env()
      load(FileName, envir = TempEnv)
      TempEnv$ModelState_ls
    }
    #Normalized path name of the datastore used in the model run
    RunDstoreName <- normalizePath(G$DatastoreName, winslash = "/", mustWork = FALSE)
    RunDstoreDir <- dirname(RunDstoreName)
    RunDstoreFile <- basename(RunDstoreName)
    #--------------------------------------------------------
    #Save previous datastore and model state if SaveDatastore
    #--------------------------------------------------------
    if (SaveDatastore & file.exists(RunDstoreName)) {
      #Create a directory in which to save the datastore
      TimeString <- gsub(" ", "_", as.character(Sys.time()))
      TimeString <- gsub(":", "-", TimeString)
      ArchiveDstoreName <- paste(RunDstoreName, TimeString, sep = "_")
      dir.create(ArchiveDstoreName)
      #Copy the datastore into the directory
      file.copy(RunDstoreName, ArchiveDstoreName, recursive = TRUE)
      #Copy the previous model state file into the directory
      file.copy("PreviousModelState.Rda",
        file.path(ArchiveDstoreName, getModelStateFile()))
    }
    # TODO: Arrive here needing a fresh Datastore
    #----------------------------------------
    #Load datastore if specified (at runtime)
    #----------------------------------------
    # JRaw::LoadDatastore
    #       Should already have trapped missing Datastore to load
    # TODO: Rationalize this for the two use cases:
    #   (1) Loading (continuing) the existing Datastore
    #   (2) Loading a Datastore from a previous stage (or another run)
    #       Need to clear existing Datastore, if there is one
    if (LoadDatastore) {
      #Normalized path name of the datastore to be loaded
      LoadDstoreName <- normalizePath(DatastoreName, winslash = "/", mustWork = FALSE)
      LoadDstoreDir <- dirname(LoadDstoreName)
      LoadDstoreFile <- basename(LoadDstoreName)

      #Identify where loaded datastore is relative to run datastore
      SameName <- (LoadDstoreName == RunDstoreName)
      SameDir <- (LoadDstoreDir == RunDstoreDir)

      # Copy and load the model state file for the load datastore
      if (SameDir) { # JRaw: intended (but broken) to restart the current model
        # assumes we already renamed the existing model state
        file.rename("PreviousModelState.Rda", "LoadModelState.Rda")
      } else {
        LoadModelStateFileName <- file.path(RunDstoreDir, "LoadModelState.Rda")
        file.copy(file.path(LoadDstoreDir, getModelStateFile()), LoadModelStateFileName)
      }

      # JRaw: This will fail with undefined variable if SameDir is TRUE
      LoadModelState_ls <- assignLoadModelState(LoadModelStateFileName)
      file.remove(LoadModelStateFileName)

      # Copy load datastore if not same as run datastore
      # JRaw: Should remove existing datastore first since there may be more in
      #       it than what is present in the LoadDstore...
      if (LoadDstoreDir != RunDstoreDir) {
        file.copy(LoadDstoreName, RunDstoreDir, recursive = TRUE)
      }
      # Renames the datastore to be the name specified for the model run
      # JRaw: which fails if RunDstoreFile is already out there...
      if (LoadDstoreFile != RunDstoreFile) {
        file.rename(
          file.path(RunDstoreDir, LoadDstoreFile),
          RunDstoreName
        )
      }

      # Copy the datastore inventory to the ModelState_ls
      ModelState_ls$Datastore <- LoadModelState_ls$Datastore
      ModelState_ls$RequiredVEPackages <- if ( "RequiredVEPackages" %in% names(LoadModelState_ls) ) {
        unique(c(LoadModelState_ls$RequiredVEPackages,LoadModelState_ls$ModuleCalls_df$PackageName))
      } else {
        unique(LoadModelState_ls$ModuleCalls_df$PackageName)
      }
      setModelState(ModelState_ls,Save=TRUE)
      #Initialize geography for years not present in datastore
      RunYears_ <- ModelState_ls$Years
      LoadYears_ <- LoadModelState_ls$Years
      if (!all(RunYears_ == LoadYears_)) {
        NewYears_ <- RunYears_[!(RunYears_ %in% LoadYears_)]
        initDatastore(AppendGroups = NewYears_)
        initDatastoreGeography(GroupNames = NewYears_)
      }
    }

    #-------------------------------------------
    #Initialize datastore if no datastore loaded
    #-------------------------------------------
    if (!LoadDatastore) {
      initDatastore()
      #           # JRaw: Suspect the following is redundant - initializing ModelState_ls reads the geography
      #           readGeography(Dir = ParamDir, GeoFile = GeoFile) # TODO: adds model geography to model state, part of "Init"
      initDatastoreGeography()
      loadModelParameters(ModelParamFile = ModelParamFile) # Puts them in the Datastore
    }
  }
}

#===============
#REQUIRE PACKAGE
#===============
#' Require package.
#'
#' \code{requireModule} a visioneval control function that
#' introduces a package dependency.
#'
#' This function simply returns TRUE. It is used to state a module
#' dependency explicitly to support internal Module calls that do not
#' explicitly name a package.
#'
#' @param Module During parsing, module is added to the list of
#'   packages to be searched for modules. Otherwise ignored.
#' @return TRUE. The function returns TRUE.
#' @export
requirePackage <- function(Module) TRUE

#==========
#RUN MODULE
#==========
#' Run module.
#'
#' \code{runModule} a visioneval framework model user function that
#' runs a module.
#'
#' This function runs a module for a specified year.
#'
#' @param ModuleName A string identifying the name of a module object.
#' @param PackageName A string identifying the name of the package the module is
#'   a part of.
#' @param RunFor A string identifying whether to run the module for all years
#' "AllYears", only the base year "BaseYear", or for all years except the base
#' year "NotBaseYear".
#' @param RunYear A string identifying the run year.
#' @return list returned from module function, with Errors and Warnings as
#'   attributes.
#' @export
runModule <- function(ModuleName, PackageName, RunFor, RunYear, StopOnErr = TRUE) {
  #Check whether the module should be run for the current run year
  #---------------------------------------------------------------
  BaseYear <- getModelState()$BaseYear
  if (RunYear == BaseYear & RunFor == "NotBaseYear") {
    return()
  }
  if (RunYear != BaseYear & RunFor == "BaseYear") {
    return()
  }
  #Log and print starting message
  #------------------------------
  Msg <-
    paste0(Sys.time(), " -- Starting module script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)
  #Load the package and module
  #---------------------------
  Function <- paste0(PackageName, "::", ModuleName)
  Specs <- paste0(PackageName, "::", ModuleName, "Specifications")
  M <- list()
  M$Func <- eval(parse(text = Function))
  M$Specs <- processModuleSpecs(eval(parse(text = Specs)))
  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      Function <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(Function, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == Function) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }
  #Initialize vectors to store module errors and warnings
  #------------------------------------------------------
  Errors_ <- character(0)
  Warnings_ <- character(0)
  #Run module
  #----------
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
      }
    }
    #Run module
    if (exists("Call")) {
      R <- M$Func(L, Call$Func)
    } else {
      R <- M$Func(L)
    }
    #Save results in datastore if no errors from module
    if (is.null(R$Errors) ) {
      setInDatastore(R, M$Specs, ModuleName, Year = RunYear, Geo = NULL)
    }
    #Add module errors and warnings if any
    Errors_ <- c(Errors_, R$Errors)
    Warnings_ <- c(Errors_, R$Warnings)
    #Handle errors
    if (!is.null(R$Errors)) {
      writeLog(Errors_)
      Msg <-
        paste0("Module Script ", ModuleName, " has reported one or more errors. ",
               "Check log for details.")
      stop(Msg)
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      writeLog(Warnings_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more warnings. ",
               "Check log for details.")
      warning(Msg)
      rm(Msg)
    }
  } else {
    #Identify the units of geography to iterate over
    GeoCategory <- M$Specs$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
      }
    }
    #Run module for each geographic area
    Geo_ <- readFromTable(GeoCategory, GeoCategory, RunYear)
    for (Geo in Geo_) {
      #Get data from datastore for geographic area
      L <-
        getFromDatastore(M$Specs, RunYear, Geo, GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
        }
      }
      #Run model for geographic area
      if (exists("Call")) {
        R <- M$Func(L, Call$Func)
      } else {
        R <- M$Func(L)
      }
      #Save results in datastore if no errors from module
      if (is.null(R$Errors)) {
        setInDatastore(R, M$Specs, ModuleName, RunYear, Geo, GeoIndex_ls)
      }
      #Add module errors and warnings if any
      Errors_ <- c(Errors_, R$Errors)
      Warnings_ <- c(Errors_, R$Warnings)
      #Handle errors
      if (!is.null(R$Errors)) {
        writeLog(Errors_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
                 "Check log for details.")
        stop(Msg)
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        writeLog(Warnings_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
    }
  }
  #Log and print ending message
  #----------------------------
  Msg <-
    paste0(Sys.time(), " -- Finish module '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)
  #Return error and warning messages if not StopOnErr
  #--------------------------------------------------
  if (!StopOnErr) {
    list(
      Errors = Errors_,
      Warnings = Warnings_
    )
  }
}

#RUN SCRIPT
#=========================================================
#' Run an R function or script file as if it were a module
#'
#' \code{runScript} a visioneval framework module developer function that runs a function in
#' run_model.R as if it were a packaged module.
#'
#' This function runs a function based module for a specified year. The module function can be
#' passed as an R function, the name of a script file, or as an (exported) module name from a
#' package - the last will reproduce runModule functionality, except that you can provide a
#' revised specification list. If a script file is passed, it is expected to have the same
#' components (at least module function and module specifications) as a package-based module. The
#' script will be run during the "initializeModel" process, so if it has estimation code included,
#' that will be run before the rest of the model.
#'
#' This function does NOT write to the Datastore by default. run_model.R can capture the returned
#' output structure. The function will run "standalone" provided a compatible ModelState and
#' Datastore have been loaded (exist in ve.model).
#'
#' @param Module An R function or character string function name (or string identifying a
#'   file to source) containing module code
#' @param Specification An R specification list or NULL (default), in which case a list
#'   will be sought using the name 
#' @param RunYear A string identifying the run year.
#' @param writeDatastore A logical indicating whether or not to write the results into
#'   the current Datastore, or just to return them
#' @return (invisible) The list of results returned by the module
#' @export
runScript <- function(Module, Specification=NULL, RunYear, writeDatastore = FALSE) {
  #Locate the Module function and Specification
  #  Substitute/Deparse to get the object/name passed as "Module"
  ModuleSource <- substitute(Module)
  ModuleFunc <- (Module)

  #Set up failure message
  #----------------------
  badModule <- function(ModuleName,Msg) {
    if ( missing(Msg) ) {
      Msg <- paste0(ModuleName, ": Cound not find module")
    }
    writeLog(Msg)
    stop(Msg)
  }

  if ( is.function(ModuleFunc) ) {
    #  If module is a function and specification is a list, just use those
    ModuleName <- deparse(ModuleSource)
  } else if ( is.character(ModuleFunc) ) {
    # see if "ModuleFunc" is really "ModuleName"
    if ( exists(ModuleFunc,parent.frame()) ) {
      ModuleFunc <- get(ModuleFunc,parent.frame())
      if ( ! is.function(ModuleFunc) ) badModule("Not a module function.")
    } else {
      if ( grepl("\\.R$",ModuleFunc) ) {
        ModuleFile <- ModuleFunc
        ModuleName <- basename(sub("\\.R$","",ModuleFile))
        ModuleEnv <- new.env(parent.frame())
        sys.source(ModuleFile,envir=ModuleEnv)
        ModuleFunc <- get0(ModuleName,envir=ModuleEnv)
        if ( ! is.function(ModuleFunc) ) badModule(ModuleName)
        environment(ModuleFunc) <- ModuleEnv
      } else badModule(ModuleName)
    }
  } else badModule(ModuleName)

  # Find the specifications
  if ( is.list(Specification) ) {
    ModuleSpecs <- Specification
  } else if ( is.character(Specification) ) {
    SpecName <- paste0(ModuleName, "Specifications")
    if ( exists(SpecName,environment(ModuleFunc)) ) { # also searches parent.frame
      ModuleSpecs <- get0(SpecName,environment(ModuleFunc))
    } else badModule(ModuleName,"Cannot find module specifications")
  }
  if ( ! is.list(ModuleSpecs) ) badModule(ModuleName,"Specifications not in valid format (list)")

  #Log and print starting message
  #------------------------------
  Msg <-
    paste0(Sys.time(), " -- Starting script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)

  #---------------------------------------------------------------
  #Set up the module function and specifications
  #---------------------------------------------------------------
  M <- list()
  M$Func <- ModuleFunc
  M$Specs <- processModuleSpecs(ModuleSpecs)

  # TODO: Factor the following out to support both runModule and runScript

  #Load any modules identified by 'Call' spec if any
  if (is.list(M$Specs$Call)) {
    Call <- list(
      Func = list(),
      Specs = list()
    )
    for (Alias in names(M$Specs$Call)) {
      #Called module function when specified as package::module
      Function <- M$Specs$Call[[Alias]]
      #Called module function when only module is specified
      if (length(unlist(strsplit(Function, "::"))) == 1) {
        Pkg_df <- getModelState()$ModuleCalls_df
        if (sum (Pkg_df$Module == Function) != 0  ) {
          Pkg_df <- getModelState()$ModuleCalls_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)          
        } else {
          Pkg_df <- getModelState()$ModulesByPackage_df
          Function <-
            paste(Pkg_df$Package[Pkg_df$Module == Function], Function, sep = "::")
          
          rm(Pkg_df)
        }
      }
      #Called module specifications
      Specs <- paste0(Function, "Specifications")
      #Assign the function and specifications of called module to alias
      Call$Func[[Alias]] <- eval(parse(text = Function))
      Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
      Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
    }
  }
  #Initialize vectors to store module errors and warnings
  #------------------------------------------------------
  Errors_ <- character(0)
  Warnings_ <- character(0)
  #Run module
  #----------
  R <- list()
  if (M$Specs$RunBy == "Region") {
    #Get data from datastore
    L <- getFromDatastore(M$Specs, RunYear = Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        L[[Alias]] <-
          getFromDatastore(Call$Specs[[Alias]], RunYear = Year)
      }
    }
    #Run module
    if (exists("Call")) {
      R <- M$Func(L, Call$Func)
    } else {
      R <- M$Func(L)
    }
    #Save results in datastore if no errors from module
    if (writeDatastore && is.null(R$Errors)) {
      setInDatastore(R, M$Specs, ModuleName, Year = RunYear, Geo = NULL)
    }
    #Add module errors and warnings if any
    Errors_ <- c(Errors_, R$Errors)
    Warnings_ <- c(Errors_, R$Warnings)
    #Handle errors
    if (!is.null(R$Errors)) {
      writeLog(Errors_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more errors. ",
               "Check log for details.")
      warning(Msg)
      rm(Msg)
    }
    #Handle warnings
    if (!is.null(R$Warnings)) {
      writeLog(Warnings_)
      Msg <-
        paste0("Module ", ModuleName, " has reported one or more warnings. ",
               "Check log for details.")
      warning(Msg)
      rm(Msg)
    }
  } else {
    #Identify the units of geography to iterate over
    GeoCategory <- M$Specs$RunBy
    #Create the geographic index list
    GeoIndex_ls <- createGeoIndexList(c(M$Specs$Get, M$Specs$Set), GeoCategory, Year)
    if (exists("Call")) {
      for (Alias in names(Call$Specs)) {
        GeoIndex_ls[[Alias]] <-
          createGeoIndexList(Call$Specs[[Alias]]$Get, GeoCategory, Year)
      }
    }
    #Run module for each geographic area
    Geo_ <- readFromTable(GeoCategory, GeoCategory, RunYear)
    for (Geo in Geo_) {
      #Get data from datastore for geographic area
      L <-
        getFromDatastore(M$Specs, RunYear, Geo, GeoIndex_ls)
      if (exists("Call")) {
        for (Alias in names(Call$Specs)) {
          L[[Alias]] <-
            getFromDatastore(Call$Specs[[Alias]], RunYear = Year, Geo, GeoIndex_ls = GeoIndex_ls[[Alias]])
        }
      }
      #Run model for geographic area
      if (exists("Call")) {
        R <- M$Func(L, Call$Func)
      } else {
        R <- M$Func(L)
      }
      #Save results in datastore if no errors from module
      if (writeDatastore && is.null(R$Errors)) {
        setInDatastore(R, M$Specs, ModuleName, RunYear, Geo, GeoIndex_ls)
      }
      #Add module errors and warnings if any
      Errors_ <- c(Errors_, R$Errors)
      Warnings_ <- c(Errors_, R$Warnings)
      #Handle errors
      if (!is.null(R$Errors)) {
        writeLog(Errors_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more errors. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
      #Handle warnings
      if (!is.null(R$Warnings)) {
        writeLog(Warnings_)
        Msg <-
          paste0("Module ", ModuleName, " has reported one or more warnings. ",
                 "Check log for details.")
        warning(Msg)
        rm(Msg)
      }
    }
  }

  #Log and print ending message
  #----------------------------
  Msg <-
    paste0(Sys.time(), " -- Finish module script '", ModuleName,
           "' for year '", RunYear, "'.")
  writeLog(Msg)
  print(Msg)

  #Return error and warning messages
  #--------------------------------------------------
  attr(R,"Errors") <- Errors_
  attr(R,"Warnings") <- Warnings_
  return(R)
}
