# Results.R
#' @include environment.R
NULL

# Documentation for VEResultsList
#' VEResultsList class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEResultsList
NULL

# Documentation for VEResults
#' VEResults class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEResults
NULL

# Documentation for VESelection
#' VESelection class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VESelection
NULL

self=private=NULL # To avoid global variable warnings

##############################
#
###### Implement VEResultsList
#
##############################

# Initialize a VEResultsList from a list of model stages
ve.resultslist.init <- function(stages,model) {

  self$Model <- model

  # Build results
  self$Results <- lapply(
    stages,
    function(stg) VEResults$new(stg$RunPath,ResultsName=stg$Name,ModelStage=stg)
  )
  names(self$Results) <- names(stages)
  # Build results index (S/G/T/N plus other metadata) for use in selecting
  self$resultsIndex <- do.call("rbind", lapply( self$Results,function(r) r$list() )

  valid <- sapply( self$Results, function(r) r$valid() )

  # Validation warning (stages missing results)
  if ( any( ! valid ) ) {
    writeLog(
      paste("No results yet for stage(s): ",names(self$Results)[!valid],collapse=", "),
      Level="warn"
    )
    writeLog("Have you run the model?",Level="warn")
  }
}

# export results using the data.frame exporter by default
ve.resultslist.extract <-  function(exporter="VEExporter.Dataframe",...) {
  return(invisible(self$export(exporter=exporter,...)$data())
}

# Export results from each set in the results list to an exporter
# If selection is not defined, use the selection embedded in the results list (default=everything)
# exporter=NULL uses the system default exporter (if undefined in visioneval.cnf, use "csv")
# Otherwise it can be a character string (default="csv") or a pre-constructed VEExporter
# Connection (string) and Partition parameters are passed to exporter factory (see export.R for
# docs).
# wantMetadata TRUE will generate a top-level tabel from the selection parameter list
# convertUnits TRUE will use an available display_units.csv conversion table (FALSE use raw Datastore units)
ve.resultslist.export <- function(
  selection=NULL,     # what we get from selecting a subset using VEResultsList$select() and VESelection operations
  exporter="VEExporter.CSV",connection=NULL,partition=NULL, # see export.R for docs on exporter, connection, partition
  wantMetadata=TRUE,
  convertUnits=TRUE
) {

  # Set up the exporter (defaults to CSV - use $extrct to default to list of data.frames)
  if ( ! inherits(exporter,"VEExporter") ) {
    exporter <- newExporter(exporter,connection,partition)
  } else if ( ! missing(partition) && !is.null(partition) ) {
    exporter$partition(partition)
  }

  # Apply a selection if provided, otherwise use entire list of outputs
  # Reduces the modelIndex to a subset then figures out S/G/T/N from whatever is left
  selection <- if ( ! is.null(selection) ) {
    self$select(selection)$list()
  } else self$list() # different from self$select, self$list does not deep-copy the selection

  # TODO: decide whether a selection is a set of selected indexes or the results of applying that
  # to the index table. If the latter, do the index table reduction here; lines below presume
  # we have the actual table.

  # partition the selection into stages/scenarios and iterate across just those results
  selected.stages <- unique(selection$Scenario) # names of scenario

  for ( stage in selected.stages ) {
    result.selection <- selection[selection$Scenario==stage,c("Group","Table","Name","Units")]
    # Just the elements selected for this stage
    data <- result$extract(selection=result.selection,convertUnits=convertUnits) # Generates a list of data.frames
    # data is a named list of Group, each being a named list of Tables
    for ( group in names(data) ) {
      for ( table in names(data[[group]]) ) {
        # Get the table name this way since result$extract may spawn new tables (VERPAT hack)
        # group[[table]] is a data.frame
        exporter$writeTable( data[[group]][[table]], Scenario=stage, Group=group, Table=table ) # exporter will partition...
        # NOTE: exporter will create, write or re-write tables (re-write happens if the data we're
        # writing includes fields that were not in the table as originally written). Rewrite is
        # unlikely to occur unless we're merging different stage and groups into a single table
        # and the stages have significantly different ModelScripts.
      }
    }
  }
      
  if ( wantMetadata ) {
    # TODO: Use self$Model to access model parameters (via VEModel::getSetup)
    # TODO: pick a metadata name - ideally it's a configuration parameter
    # TODO: develop an "Exporter" block for visioneval.cnf that will be part of the model's
    #       RunParam_ls (with suitable defaults). That means if the ResultsList is associated with a
    #       model, we should have the model around so we can ask about its settings.
    #       The Exporter block can set up default connection information...     
   
    # write the selection table to the exporter connection
    metadataName <- "Metadata"
    exporter$writeTable( selection, Table=metadataName ) # Unspecified Scenario/Group: place in connection "root"
  }

  # Printing the returned exporter will show the list of tables created and connection summary
  # doing exporter$data will enable pulling out a list of tables into data.frames
  return(invisible(exporter))
}

# Produce a bare named list of VEResults objects
# Probably don't ever need this except for the idiom below for getting a subset of results from
# this list. But scenarios can also be selected using the standard selection facility.
ve.resultslist.results <- function(stages=NULL) {
  if ( missing(stages) ) {
    return(self$Results)
  } else {
    return(self$Results[stages])
  }
}

# IDIOM to grab a subset of stages (if not done using model$results):
#    resultsSubset <-
#    VEResultsList$new(results$results()[vector.of.stages]) # vector.of.stages are names or indices

# Print summary of results in the list
ve.resultslist.print <- function(...) {
  for ( nm in names(self$Results)) {
    print(self$Results[[nm]],name=nm,...) # Call VEResults$print
  }
}

ve.resultslist.list <- function(pattern="", selected=TRUE, details=FALSE, ...) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # details = FALSE returns just the "Name" vector from self$modelIndex

  filter <- if ( missing(selected) || selected ) {
    which( 1:nrow(self$resultsIndex) %in% self$selection$selection )
  } else {
    rep(TRUE,nrow(self$resultsIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$resultsIndex$Name,ignore.case=TRUE )
  }

  if ( missing(details) || ! details ) {
    ret.value <- with( self$resultsIndex[ filter, ], paste(Scenario,Group,Table,Name,sep="/") ) # generates a character vector
  } else {
    ret.value <- self$resultsIndex[ filter, ] # Generates a data.frame with all metadata columns
  }
  return(unique(ret.value))
}

# TODO: is this function ever used, or is it still relevant?
# TODO: written to work on an individual VEResults - now should list all selected
# We won't necessarily know the input file until after the model is run (otherwise, this function should have been a
#  member of VEModel)
ve.resultslist.inputs <- function( fields=FALSE, module="", filename="" ) {
  # fields=TRUE, show all Fields/Datasets that originated as file inputs (lists all columns within input files)
  # fields=FALSE, just show the module, file, input directory (lists the input files)
  # This is a convenience function for processing the results index. Need to ensure File and
  # InputDir are included in that table.
  if ( ! self$valid() ) stop("Model has not been run yet.")

  if ( ! missing(fields) && fields ) {
    ret.fields <- c("Module","Group","Table","Name","File","InputDir","Units","Description")
  } else {
    ret.fields <- c("Module","Name","File","InputDir")
  }

  filter <- nzchar(self$modelIndex$File)
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,self$modelIndex$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,self$modelIndex$File)
  }

  ret.value <- unique(self$modelIndex[ filter, ret.fields ])
  return( ret.value[order(ret.value$InputDir,ret.value$File),] )
}

ve.resultslist.select <- function(selection) {
  return.self <- missing(selection)
  if ( return.self ) {
    self$selection <- VESelection$new(self,self$list())
  } else {
    if ( ! inherits("VESelection", selection) || self$Model$modelPath != selection$modelPath ) {
      # see if we can make a selection (stop inside $new if selection can't be interpreted)
      selection <- VESelection$new(self,selection)
    } else self$selection <- selection
  }
  invisible( self$selection )
}

# Find fields (as objects) within the current results list
# Note: "Scenarios" correspond to model stages in the model
ve.resultslist.find <- function(pattern=NULL,Scenario=NULL,Group=NULL,Table=NULL,Name=NULL,select=FALSE) {
  selection <- self$select() # generate a base selection from what is already selected.
  found <-selection$find(pattern=pattern,Scenario=Scenario,Group=Group,Table=Table,Name=Name,as.object=TRUE)
  # without "select=TRUE", found is an independent selection (not bound to results)
  if ( select ) found <- self$select(found) # create a selection from this set of results
  return( found )
}

#' @export
VEResultsList <- R6::R6Class(
  "VEResultsList",
  public = list(
    # public data
    Model        = NULL,
    Results      = NULL,
    modelPath    = NULL,
    selection    = NULL,
    resultsIndex = NULL, # consolidated datastore index for all stages

    # methods
    initialize=ve.resultslist.init,
    results=ve.resultslist.results   # manipulate the inner list
    print=ve.resultslist.print,      # summary of model results (index)
    export=ve.resultslist.export,    # move the results to an external data storage format
    extract=ve.resultslist.extract,  # generate nested list of data.frames from model results (for further processing in R)
    list=ve.resultslist.list,        # show the consolidated resultsIndex (used by export to develop metadata table)
    select=ve.resultslist.select,    # return the list's selection definition
    find=ve.resultslist.find         # constructs but does not embed the selection definition
  )
)

##########################
#
###### Implement VEResults
#
##########################

# VEResults handles the low-level interactions with the ModelState_ls and Datastore for a
# single stage. It automatically does virtual "flattening" by walking back up the StartFrom
# tree. It will generate an index list of all the Group/Table/Name for the specific scenarios
# that will be later used to generate table specifications for export, and also for selecting
# subsets of the state results.

# Create VEResults object (manipulates Datastore/ModelState)
# Extracts tabular data
ve.results.init <- function(ResultsPath,ResultsName=NULL,ModelStage=NULL) {
  # ResultsPath is the normalized path to a directory containing the model results
  #  typically from a Reportable model stage. Expect to find a ModelState.Rda file
  #  and a Datastore in that folder.
  self$resultsPath <- ResultsPath
  self$Name <- if ( !is.character(ResultsName) ) basename(ResultsPath) else ResultsName
  self$index() # Will find model state
  self$RunParam_ls <- self$ModelState()$RunParam_ls
  self$modelStage <- ModelStage # may be NULL; only used to get stage elements for category scenarios via the R visualizer
  return(self$valid())
}

# Get tables of data from the Datastore for a specific stage/scenario
# Return a list of groups containing data.frames for each table/name set within the group
ve.results.extract <- function(
  selection,             # data.frame of Group/Table/Name/Units elements for this stage
  convertUnits=TRUE      # will convert if display units are present; FALSE not to attempt any conversion (use Units from selection)
) {
  if ( ! self$valid() ) {
    bad.results <- if ( ! is.null(self$modelStage) ) self$modelStage$Name else self$resultsPath )
    stop("Model Stage contains no results: ",bad.results)
  }
  scenarioName <- if ( is.null(self$modelStage) basename(self$resultsPath) else self$modelStage$Name )

  if ( convertUnits) {
    selection <- addDisplayUnits(selection,Param_ls=self$RunParam_ls)
  } else {
    selection$DisplayUnits <- selection$Units
  }
  extract <- selection[ , c("Scenario","Name","Table","Group","Units", "DisplayUnits") ]

  extractTables <- unique(extract[,c("Group","Table")])
  extractGroups <- unique(extractTables$Group)

  QueryPrep_ls <- self$queryprep()
  results <- list()

  for ( group in extractGroups ) {
    # Build Tables_ls for readDatastoreTables
    Tables_ls <- list()
    tables <- extractTables$Table[ extractTables$Group == group ]
    if ( length(tables)==0 ) next # should not happen given how we built extract
    for ( table in tables ) {
      meta <- extract[ extract$Group==group & extract$Table==table, ]
      fields <- meta[ , c("Name","DisplayUnits") ]
      dispUnits <- fields$DisplayUnits # Will just be fields$Units if not converting
      names(dispUnits) <- fields$Name
      Tables_ls[[table]] <- dispUnits
    }

    # Get a list of data.frames, one for each Table configured in Tables_ls
    Data_ls <- visioneval::readDatastoreTables(Tables_ls, group, QueryPrep_ls)

    # Report Missing Tables from readDatastoreTables
    HasMissing_ <- unlist(lapply(Data_ls$Missing, length)) != 0
    if (any(HasMissing_)) {
      WhichMissing_ <- which(HasMissing_)
      Missing_ <- character(0)
      for (i in WhichMissing_) {
        Missing_ <- c(
          Missing_,
          paste0(
            names(Data_ls$Missing)[i], " (",
            paste(Data_ls$Missing[[i]], collapse = ", "), ")"
          )
        )
      }
      msg <- paste("Missing Tables (Datasets):",paste(Missing_, collapse = "\n"),sep="\n")
      stop( writeLog( msg, Level="error" ) )
    }

    # This code exists to handle a bad part of the VERPAT design, where base and future
    # vehicles are loaded into the same table. Fields associated with base and future may
    # have different lengths (each field is only associated with one). This code will
    # be used to split out the base and future into different tables based on matching
    # the lengths of each field column.

    # Handle tables with different lengths of data elements ("multi-tables")
    # readDatastoreTables will have returned a ragged list rather than a data.frame

    if ( ! all(is.df <- sapply(Data_ls$Data,is.data.frame)) ) {
      # Unpack "multi-tables"
      MultiTables <- Data_ls$Data[which(! is.df)] # usually, there's just one of these...
      if ( length(MultiTables) > 0 ) {
        for ( multi in names(MultiTables) ) {
          # multi is a list of datasets not made into a data.frame by readDatastoreTables
          multi.data <- MultiTables[[multi]]
          lens <- sapply(multi.data,length) # vector of lengths of datastores
          multi.len <- unique(lens)
          for ( dfnum in 1:length(multi.len) ) { # work through unique dataset lengths
            dfname <- paste(multi,multi.len[dfnum],sep="_") # Encode the length onto the table name
            fordf <- which(lens==multi.len[dfnum])
            try.df <- try( data.frame(multi.data[fordf]) )
            if ( ! is.data.frame(try.df) ) {
              msg <- paste("Could not make data.frame from Datastore Table",multi)
              stop( writeLog( msg, Level="error" ) )
            }
            Data_ls$Data[[dfname]] <- try.df
          }
        }
        # Remove original tables (they will now have names like "Vehicles.13747" and "Vehicles.15444")
        # That way we can later merge equivalent tables across multiple scenarios depending on the
        # conosolidation strategy in VEResultsList$export.Okay
        Data_ls$Data <- Data_ls$Data[-which(names(Data_ls$Data) %in% names(MultiTables))]
      }
    }
    # Now visit each of the resulting data.frames and prepend the Scenario column
    # TODO: Or do we want to elevate this to VEResultsList$export?
    Data_ls$Data <- lapply(Data_ls$Data,function(df) cbind(Scenario=scenarioName,df))
    # TODO: stop around here and look at the columns of Data_ls$Data

    # Process the table data.frames into results
    results[[ group ]] <- Data_ls$Data
  }
  invisible(results) # results will be a named list of groups from the stage results, with each group list
                     # contains the tables extracted for that group.
}

# Check results validity (all files present)
ve.results.valid <- function() {
  valid <- ! is.null(self$RunParam_ls) &&
           dir.exists(self$resultsPath) &&
           !is.null(self$modelIndex) && length(self$modelIndex)>0
  modelStatePath <- file.path(self$resultsPath,visioneval::getRunParameter("ModelStateFile",Param_ls=self$RunParam_ls))
  valid <- valid && all(file.exists(modelStatePath))
  return(valid)
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

ve.results.modelstate <- function(ModelState_ls=NULL) {
  if ( ! is.null(ModelState_ls) ) {
    if ( is.null(private$modelStateEnv) ) {
      private$modelStateEnv <- new.env()
    }
    private$modelStateEnv$ModelState_ls <- ModelState_ls
  }
  return (private$modelStateEnv$ModelState_ls)
}

ve.results.list <- function(pattern="", details=FALSE, selected=TRUE, ...) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from self$modelIndex
  
  if ( ! self$valid() ) stop("Model has not been run yet.")

  filter <- if ( missing(selected) || selected ) {
    which( 1:nrow(self$modelIndex) %in% self$selection$selection )
  } else {
    rep(TRUE,nrow(self$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name,ignore.case=TRUE )
  }

  if ( missing(details) || ! details ) {
    ret.value <- with( self$modelIndex[ filter, ], paste(Group,Table,Name,sep="/") ) # generates a character vector
  } else {
    ret.value <- self$modelIndex[ filter, ] # Generates a data.frame with all columns
  }
  return(unique(ret.value))
}

ve.results.index <- function() {
  # Load model state from self$resultsPath
  # Note that if the model is using a non-standard ModelState name,
  # we might not find it here. ModelState name should be set globally.
  FileName=normalizePath( file.path(
    self$resultsPath, # Should already include ResultsDir
    visioneval::getModelStateFileName()
  ), winslash="/", mustWork=FALSE)
  if ( file.exists(FileName) ) {
    ms <- try(visioneval::readModelState(FileName=FileName,envir=new.env()))
  } else {
    ms <- NULL
  }
  if ( ! is.list(ms) ) {
    self$ModelState(list())
    writeLog(Level="info",paste("No ModelState:",FileName))
    return(list())
  }
  self$ModelState(ms) # save ModelState
  if ( is.null(self$RunParam_ls) && is.list( ms ) ) {
    self$RunParam_ls <- ms$RunParam_ls
  }

  msList <- rev(visioneval::getModelStatePaths(dropFirst=FALSE,envir=private$modelStateEnv))
  combinedDatastore <- list()
  if ( length(msList) > 0 ) {
    msFirst <- TRUE
    for ( ms in msList ) {
      dsListing <- ms$ModelState_ls$Datastore
      if ( msFirst ) {
        combinedDatastore <- dsListing
        msFirst <- FALSE
      } else {
        combinedDatastore <- visioneval::mergeDatastoreListings(combinedDatastore,dsListing)
      }
    }
  }

  Index <- data.frame()
  Inputs <- data.frame()

  ds <- combinedDatastore

  Description <- sapply(ds$attributes, attributeGet, "DESCRIPTION",simplify=TRUE) # should yield a character vector
  Module <- sapply(ds$attributes, attributeGet, "MODULE",simplify=TRUE) # should yield a character vector
  Units <- sapply(ds$attributes, attributeGet, "UNITS",simplify=TRUE) # should yield a character vector
  InputDir <- sapply(ds$attributes, attributeGet, "INPUTDIR",simplify=TRUE) # should yield a character vector
  InputDir[ is.na(InputDir) ] <- ""
  File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
  File[ is.na(File) ] <- ""
  scenario <- rep(visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=self$RunParam_ls),length(Description))

  splitGroupTableName <- strsplit(ds$groupname, "/")
  if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")

  maxLength <- max(unlist(lapply(splitGroupTableName, length)))
  if ( maxLength != 3 ) {
    writeLog(Level="warn",paste0("Model state at ",self$resultsPath," is incomplete (",maxLength,")"))
    return(list())
  }
  splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))
  # splitGroupTableName is a list of character vectors with Group, Table, Name components

  fieldGTN <- do.call(rbind.data.frame,splitGroupTableName)
  names(fieldGTN) <- c("Group","Table","Name")

  # Build parallel data.frame for Inputs (including File parameter)
  # message("Input data frame...")
  Index <- data.frame(
    Group       = fieldGTN$Group,
    Table       = fieldGTN$Table,
    Name        = fieldGTN$Name, # Should be identical to ds$name
    Description = Description,
    Units       = Units,
    # TODO: May need some other specification fields in order to identify variable type for SQL or other export
    Module      = Module,
    Scenario    = scenario,
    File        = File,          # "" if not an Input
    InputDir    = InputDir       # "" if not an Input
  )

  # GroupTableName is now a data.frame with nine columns
  # complete.cases blows away the rows that have any NA values
  # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each column)
  # Reduces the raw Datastore index to just the Fields ("Name"s) in the Datastore
  ccases <- stats::complete.cases(Index[,c("Group","Table","Name")])
  Index <- Index[ccases,]
  self$modelIndex <- Index
  invisible(self$modelIndex)
}

# Helper function to attach DisplayUnits to a list of Group/Table/Name rows in a data.frame
# Need to do this in VEResults since we need access to the model state...
# TODO: Move this to VEResultsList (using Param_ls from Model or first VEResults)
addDisplayUnits <- function(GTN_df,Param_ls) {
  # GTN_df is a data.frame with "Group","Table","Name" rows for each Name/field for which display
  #  units are sought. Always re-open the DisplayUnits file, as it may have changed since the last
  #  run.
  ParamPath <- visioneval::getRunParameter("ParamPath",Param_ls=Param_ls) # location of structural files

  DisplayUnitsFile <- visioneval::getRunParameter("DisplayUnitsFile",Param_ls=Param_ls)
  # Where to look for DisplayUnitsFile...
  # By its name, in ParamPath for model (preferred) or runtime directory (global values)
  DisplayUnitsFile <- c(file.path(c(ParamPath,getRuntimeDirectory()),DisplayUnitsFile))

  existing <- file.exists(DisplayUnitsFile)
  if ( ! any(existing) ) {
    writeLog( Level="info",
      c("Specified DisplayUnits file does not exist (using default units):",paste(DisplayUnitsFile,collapse="; "))
    )
    return( cbind(GTN_df,DisplayUnits=NA,DisplayUnitsFile="None") )
  } else {
    DisplayUnitsFile <- DisplayUnitsFile[existing][1]
  }
#   cat("DisplayUnitsFile:\n")
#   print(DisplayUnitsFile)
  displayUnits <- try(utils::read.csv(DisplayUnitsFile),silent=TRUE)   # May fail for various reasons
  if ( ! "data.frame" %in% class(displayUnits) ) {
    writeLog( Level="warn",
      c(
        "Error reading DisplayUnits file:",
        DisplayUnitsFile,
        paste("Error:",conditionMessage(attr(displayUnits,"condition")))
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  displayColumns <- c("Group","Table","Name","DisplayUnits")
  if ( ! all( displayColumns %in% names(displayUnits) ) ) {
    writeLog( Level="warn",
      c("Specified DisplayUnits file does not have correct columns:",DisplayUnitsFile,
        paste("Columns:",names(displayUnits),collapse=", ")
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  # Remove existing DisplayUnits, if present, prior to merging
  if ( "DisplayUnits" %in% names(GTN_df) ) GTN_df <- GTN_df[,! grepl("DisplayUnits",names(GTN_df),fixed=TRUE)]
  # Only look at relevant columns in displayUnits when merging
  displayUnits <- try( merge(GTN_df,displayUnits[,displayColumns],by=c("Group","Table","Name"),all.x=TRUE), silent=TRUE )
  if (
    ! "data.frame" %in% class(displayUnits) ||
    ! all( c("Group","Table","Name","DisplayUnits") %in% names(displayUnits) ) # it can have other fields, e.g. original Units
  ) {
    if ( "data.frame" %in% class(displayUnits) ) {
      displayUnits <- paste("Bad Fields - ",names(displayUnits),collapse=", ")
    } else {
      displayUnits <- conditionMessage(attr(displayUnits,"condition"))
    }
    writeLog( Level="warn",
      c(
        "Error reading DisplayUnits file:",
        DisplayUnitsFile,
        paste("Error:",displayUnits)
      )
    )
    return( cbind(GTN_df,DisplayUnits=NA, DisplayUnitsFile="None") )
  }
  # Add displayUnitsFile
  displayUnits$DisplayUnitsFile <- DisplayUnitsFile
  # get here with displayUnits being GTN_df, augmented by matching DisplayUnits
  return(displayUnits) # Minimally includes Group, Table, Name, DisplayUnits, DisplayUnitsFile
}

# Return a named list of ScenarioElements and Levels for this set of
# results (from the ModelStage) - for use with category scenarios.
ve.results.elements <- function() {
  # Get scenario element names plus level values for associated model stage
  # Model stage must have scenario elements to use Visualizer
  if ( is.null(self$modelStage) ) return(list()) # need the modelStage to get elements
  elements <- self$modelStage$ScenarioElements
  if ( !is.character(elements) ) {
    return(list())
  }
  return(as.list(elements)) # converted named vector to named list
}

# List out the units applied to results for this scenario (see addDisplayUnits above)
# TODO: Should have a function in VEResultsList that will aggregate these results...
# TODO: Also, the selection is managed in VEResultsList now, so we should always just do this for
#   the entire VEResults set, and let the VEResultsList pare that down to what we are interested in
ve.results.units <- function(selected=TRUE,display=NULL) {
  # if display==TRUE, show DisplayUnits plus Datastore Units
  # if display==FALSE, show only Datastore units
  # if display is NULL (default) merge Display and Datastore and show source
  # selected == FALSE shows units for ALL fields, not just selected
  # Transiently attaches DisplayUnits to field list (transient because user
  #   may be editing the file in this session)
  # Displays a data.frame for the selected (TRUE) or all (FALSE) fields with
  #   Group, Table, Name, DisplayUnits, UnitsSource ("Datastore" or DisplayUnitsFilePath)
  selected <- if ( selected ) self$selection$selection else 1:nrow(self$modelIndex)
  Units_df <- self$modelIndex[ selected, c("Group","Table","Name","Units") ]
  Units_df$Source <- "Datastore"
  returnFields <- c("Group","Table","Name","Units","Source")
  if ( ! is.logical(display) || display ) {
    # Add Display Units if requested
    Units_df <- addDisplayUnits(Units_df,Param_ls=self$RunParam_ls)
    displayUnits <- !is.na(Units_df$DisplayUnits) # find elements where DisplayUnits are available
    Units_df$Source[ displayUnits ] <- basename(Units_df$DisplayUnitsFile[ displayUnits ])
    if ( is.null(display) ) {
      # merge into single Units Column
      Units_df$Units[ displayUnits ] <- Units_df$DisplayUnits[ displayUnits ]
    } else {
      returnFields <- c(returnFields,"DisplayUnits")
    }
  }
  return( Units_df[,returnFields] )
}

# Wrapper for visioneval::copyDatastore
# TODO: add a wrapper in VEResultsList that will copy all the model results to another
#  ToDir - VEResultsList will need to manage the directories...
ve.results.copy <- function(ToDir, Flatten=TRUE, DatastoreType=NULL, overwrite=FALSE) {
  if ( missing(ToDir) ) {
    stop(writeLog("Must provide target directory path.",Level="error"))
  }
  if ( ( existing <- dir.exists(ToDir) ) && ! overwrite ) {
    stop(writeLog("ToDir exists: provide another ToDir or set overwrite=TRUE",Level="error"))
  } else if ( existing && overwrite ) {
    unlink(ToDir,recursive=TRUE)
    existing <- FALSE
  }
  if ( ! existing ) dir.create(ToDir)

  owd <- setwd(self$resultsPath) # copyDatastore must work in that directory
  on.exit(setwd(owd))
  success <- visioneval::copyDatastore(
    ToDir=ToDir,
    Flatten=Flatten,
    DatastoreType=DatastoreType,
    envir=self$ModelStateEnv
  )
  if ( ! success ) writeLog("Attempt to copyDatastore Failed!",Level="error")
  invisible(success)
}

# Use this to prepare the VEResults for processing by VEQuery
ve.results.queryprep <- function() {
  visioneval::prepareForDatastoreQuery(
    DstoreLocs_ = file.path(self$resultsPath,self$ModelState()$DatastoreName),
    DstoreType  = self$ModelState()$DatastoreType
  )
}

# Print the VEResults summary (called recurively when printing VEResultsList)
ve.results.print <- function(name="",details=FALSE) {
  # Update for output
  cat("VEResults object for",if(nzchar(name)) name else self$Name,":\n")
  print(self$resultsPath)
  cat("Output is valid:",self$valid(),"\n")
  if ( self$valid() ) {
    if ( ! details ) {
      sel <- length(self$selection$selection)
      all <- nrow(self$modelIndex)
      if ( sel < all ) {
        cat("Selected",sel,"out of",all,"fields.\n")
        print(self$selection) # Just the field names
      } else cat("Selected all fields.\n")
    } else {
      print(self$selection,details=TRUE)
    }
  }
}

# Definition of VEResults R6 class
# Constructed within VEResultsList (above)

#' @export
VEResults <- R6::R6Class(
  "VEResults",
  public = list(
    # public data
    Name        =NULL,
    modelStage  =NULL,
    resultsPath =NULL,
    modelIndex  =NULL,

    # methods
    initialize=ve.results.init,
    index=ve.results.index,          # Index Datastore from ModelState (part of init)
    copy=ve.results.copy,            # Apply visioneval::copyDatastore
    valid=ve.results.valid,          # has the model been run, etc.
    extract=ve.results.extract,      # generate data.frames from model results
    list=ve.results.list,            # show the modelIndex - is this used other than to initialize VEResultsList?
    queryprep=ve.results.queryprep,  # For query or other external access
    print=ve.results.print,          # summary of model results (index)
    units=ve.results.units,          # Set units on field list (modifies self$modelIndex) TODO: Move/wrap in VEResultsList
    elements=ve.results.elements,    # Get scenario elements (named list of scenario levels) for this scenario
    ModelState=ve.results.modelstate # Set/Get the model state for these results
  ),
  private = list(
    RunParam_ls=NULL,
    modelStateEnv=NULL
  )
)

############################
# VESelection Implementation
############################

# VESelection is managed within VEResultsList, and will specify a subset of available
# Scenarios (Reportable Stages), Groups (Year/Global), Tables, and Names (fields) (S/G/T/N).
# The mechanism uses integer indexes into the table of fields found in the Results
# based on ModelState_ls/Datastore list for each Result set. But character vectors formatted
# as "S/G/T/N" will also be understood, and those are produced for human readability.
# The integer index is easier to use when it comes time actually to select stuff.

# results is a VEResultsList
# selection is pretty much anything that can be recognized as a selection
# (vector of integers, vector of S/G/T/N names, another selection on the same or a different
# set of model results - if same, treat like vector of integers; if different, first generate
# a character vector of S/G/T/N (and only select the ones that are present in this
# selection/resultset).
ve.select.initialize <- function( results, select=integer(0) ) {
  # default select (integer(0)) selects everything
  # self$selection is just a list of integers pointing to rows
  #  in self$results$modelIndex
  self$results <- results
  if ( self$results$valid() ) {
    rows <- self$parse(select)
    if ( is.null(rows) || any(is.na(rows)) ) {
      self$selection <- as.integer(NA) # no rows selected
    } else if (
      ! is.numeric(rows) ||
      length(rows)==0 ||
      ! min(rows)>0 ||
      max(rows)>nrow(self$results$modelIndex) ) {
      self$selection <- 1:nrow(self$results$modelIndex)
    } else {
      self$selection <- rows
    }
  } else {
    self$selection <- integer(0)
  }
}

ve.select.copy <- function(select) VESelection$new(self$results,self$selection)

ve.select.print <- function(details=FALSE) {
  # print the selected fields
  if ( ! self$valid() ) cat("No results: has model been run?\n") else {
    if ( ! details ) {            # just the field names (see below)
      print( self$fields() )
    } else {                      # full data frame of selected model results
      print( self$show() )
    }
  }
}

ve.select.show <- function() { # show the subset of results$modelIndex for this selection
  return( self$results$modelIndex[ self$selection, ] )
}

ve.select.valid <- function() { return(self$results$valid()) }

ve.select.scenarios <- function() {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    # TODO: what exactly is this test trying to save us from?
    # I think it's the caase where we previously suggested stuff and nothing is selected
    message("No scenarios selected")
    return(character(0))
  }
  idxScenarios <- unique(self$results$modelIndex[self$selection,c("Scenario"),drop=FALSE])
  return(idxScenarios[order(idxScenarios$Scenario),]) # Scenarios
}


ve.select.groups <- function() {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No groups selected")
    return(character(0))
  }
  idxGroups <- unique(self$results$modelIndex[self$selection,c("Scenario","Group"),drop=FALSE])
  return(idxGroups[order(idxGroups$Group),]) # Group
}

ve.select.tables <- function(nameOnly=FALSE) {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No tables selected")
    return(character(0))
  }
  idxTables <- unique(self$results$modelIndex[self$selection,c("Scenario","Group","Table")])
  tables <- if ( nameOnly ) {
    unique(idxTables$Table) # pure name
  } else {
    sort(paste(idxTables$Group,idxTables$Table,sep="/")) # Group/Table
  }
  return( tables )
}

ve.select.fields <- function() {
  # extract fields from the index where groups and tables are selected
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No fields selected")
    return(character(0))
  }
  idxFields <- self$results$modelIndex[self$selection,c("Scenario","Group","Table","Name")]
  return(sort(paste(idxFields$Group,idxFields$Table,idxFields$Name,sep="/"))) # Group/Table/Name
}

# TODO: Here's how to select if we keep the selection as Scenario/Group/Table/Name:
# 
#   Here is a generic solution for this type of problem which is very efficient:
# 
# data.1.ID <- paste(data.1[,1],data.1[,2],data.1[,3])
# keep.these.ID <- paste(keep.these[,1],keep.these[,2],keep.these[,3])
# desired.result <- data.1[data.1.ID %in% keep.these.ID,]

# Internal helper function to make a selection vector out of various other types of objects
# TODO: Note that parsing gets simpler if we're just keeping the list of selected Scenario/Group/Table/Name

ve.select.parse <- function(select) {
  # Though select can be a vector of field names, they need to be the full Scenario/Group/Table/Name field names,
  #  so you should get them from ve.select.find, rather than manually construct them.
  #  Anything mentioned by name in select will be ignored if that combination of S/G/T/N is
  #  not present in the associated results.
  # if select is NA, return NA
  # select can be another VESelection
  #   if it is the same model, just copy its selection
  #   if not the same model, get other selection's VEResults object and parse that
  if ( "VESelection" %in% class(select) ) {
    if ( select$results$resultsPath != self$results$resultsPath ) {
      select <- select$fields()
      # fall through to parse the character strings
    } else {
      return( select$selection )
    }
  }
  # select can be another VEResults object
  #   if the other VEResults is not from the same model, use its $fields set of names
  #   then parse as a character vector
  #   if it is the same model, just copy its selection
  if ( "VEResults" %in% class(select) ) {
    if ( select$resultsPath != self$results$resultsPath ) {
      select <- select$selection$fields()
    } else {
      return( select$selection$selection )
    }
  }
  # select can be a character vector
  #   split the vector into group/table/name, providing defaults
  # locate the rows with matching group/table/name in results$modelIndex
  #   That vector of row indices becomes the selection to act on
  if ( is.character(select) ) {
    build <- integer(0)
    for ( s in select ) {
      t <- unlist(strsplit(s,"/"))
      name <- c( rep(NA,3-length(t)), t )
      if ( is.na(name[3]) || ! nzchar(name[3]) ) next  else field=name[3]
      if ( is.na(name[2]) || ! nzchar(name[2]) ) table <- NULL else table=name[2]
      if ( is.na(name[1]) || ! nzchar(name[1]) ) group <- NULL else group=name[1]
      fld <- self$find(Name=field,Group=group,Table=table,as.object=FALSE)
      build <- union( build, fld )
    }
    select <- build # should be a vector of integers
  }
  
  # if select is a numeric vector, validate its range and return it
  if ( is.numeric(select) ) {
    if ( length(select)>0 ) {
      if ( any(is.na(select)) ) return( as.integer(NA) )
      if ( ! ( min(select)>0 && max(select)<=nrow(self$results$modelIndex) ) ) {
        message("Field selection out of range")
        return( as.integer(NA) )
      }
    }
    return( select )
  }
  message("Invalid field selection specification")
  message(deparse(select))
  return( as.integer(NA) )
}

# Select something new (optional) and then return the (possibly updated) self
ve.select.select <- function(select) {
  if ( ! missing(select) ) self$selection <- self$parse(select)
  invisible(self)
}

# NOTE: Strictly speaking, the Datastore table key fields should be recoverable from the module
# specifications, but I haven't found a way to do that comprehensively yet. That's a deficiency
# in the table specification that may come back to bite us when exporting to SQL.
allTheKeys = c(
  "Marea","Azone","Bzone",
  "HhId","VehId","WkrId"
)

ve.select.addkeys <- function(Scenario=NULL,Group=NULL,Table=NULL,Keys=NULL) {
  # Helper to move key fields across into selection
  # In general, it may be too complex to specify S/G/T - this function should
  #    just select the key fields in any currently selected S/G/T
  # "Scenario" if not specified will be currently selected scenarios
  # "Group" if not specified will be currently selected groups
  # "Table" if not specified will be currently selected tables
  # "Keys" if not specified will be all of them; if provided here,
  # will drop any that are not in the Keys list (so if you give it
  # something that is not a "key", it just ignores it).
  if ( missing(Group) ) Group <- self$groups()
  if ( missing(Table) ) Table <- self$tables(nameOnly=TRUE) # returns just the Table name(s)
  theKeys <- allTheKeys
  if ( is.character(Keys) ) {
    theKeys <- setdiff(allTheKeys,Keys) # Only include the named ones
  }
  # add the Key fields for selected Group/Table if they're not
  # already there
  theKeys <- self$find(Group=Group,Table=Table,Name=theKeys)
  self$or( theKeys )
  invisible( self )
}

# Find does NOT alter the object it is called on unless 'select=TRUE'
# It either produces a new VESelection from the matching elements of self$selection (as.object==TRUE)
# OR it produces a vector of matching element indices (as.object==FALSE)
ve.select.find <- function(pattern=NULL,Group=NULL,Table=NULL,Name=NULL,as.object=TRUE,select=FALSE) {
  # if pattern (regexp) given, find names matching pattern (only within the "fields"/Name part)
  # if group or table not specified, look in any group or table
  # return vector of indices for matching rows or (as.object==TRUE) a new VESelection object
  searchGroup <- Group
  searchTable <- Table
  searchName  <- Name
  newSelection <- self$selection
  newSelection <- with( self$results$modelIndex, {
    if ( !is.null(pattern ) ) {
      fld <- grepl(pattern,Name,ignore.case=TRUE)     # RegEx search for name
    } else if ( !is.null(searchName) ) {
      fld <- Name %in% searchName                     # Exact name match
    } else {
      fld <- rep(TRUE,nrow(self$results$modelIndex))  # Start with all selected
    }
    if ( !is.null(searchGroup) ) {
      if ( any(searchGroup %in% c("Year","Years","AllYears")) ) {  # shorthand for non-Global group
        group <- Group != "Global"
      } else {
        group <- (Group %in% searchGroup)
      }
      fld <- fld & group
    }
    if ( !is.null(searchTable) ) {
      fld <- fld & (Table %in% searchTable)
    }
    which(fld)
  })
  if ( length(newSelection) == 0 ) newSelection <- as.integer(NA)
  if ( as.object ) {
    if ( select ) {
      self$select(newSelection)
      found <- self
    } else {
      found <- VESelection$new(self$results, select=newSelection)
    }
  } else {
    if ( select ) {
      self$selection <- newSelection
      found <- self$selection
    } else {
      found <- newSelection
    }
  }
  return( if ( select ) invisible(found) else found )
}

# Add another selection to self (add + assign)
# Matching indices will be included
ve.select.add <- function(select) {
  select <- self$parse(select)
  self$selection <- union(self$selection,select)
  invisible(self)
}

# Remove contents of another selection from self (remote + assign)
# Matching indices in select will be removed
ve.select.remove <- function(select) {
  select <- self$parse(select)
  self$selection <- setdiff(self$selection,select)
  invisible(self)
}

# Keep only fields that are in both self and select (logical "and")
# Indices in both will be kept, and those present in only one will be removed
ve.select.and <- function(select) {
  select <- self$parse(select)
  self$selection <- intersect(self$selection,select)
  invisible(self)
}

# 
ve.select.all <- function() {
  self$selection <- 1:nrow(self$results$modelIndex)
  invisible(self)
}

ve.select.none <- function() {
  self$selection <- integer(NA)
  invisible(self)
}

#' Conversion method to turn a VESelection into a vector of selection indices
#'
#' Mostly used internally.
#'
#' @param x a VESelection object (or something that can be coerced to one)
#' @param ... Additional arguments to support generic signature
#' @return an integer vector of selected fields
#' @export
as.integer.VESelection <- function(x,...) x$selection

# The VESelection R6 class
# This interoperates with VEResultsList to keep track of what subsets of results data

#' @export
VESelection <- R6::R6Class(
  "VESelection",
  public = list(
    # public data
    selection = integer(0),
    results = NULL,  # a VEResultsList object

    # methods
    initialize=ve.select.initialize, # Initial selection for a results list
    copy=ve.select.copy,             # Create a new selection object with the same results and indices
    print=ve.select.print,           # Print list of fields or (details=TRUE) the subset of results$modelIndex
    show=ve.select.show,             # retrieve the selected subset of results$modelIndex (data.frame)
    valid=ve.select.valid,           # is the selection valid against results$modelIndex
    find=ve.select.find,             # general search facility for selecting group/table/name
    parse=ve.select.parse,           # interpret different ways of specifying a selection (number, field descriptor)
    select=ve.select.select,         # assign - set self to other selection value
    add=ve.select.add,               # "union" - indices are included from either selection
    addkeys=ve.select.addkeys,       # add keys (e.g. HhID, BZone) for already SELECTED Tables (uses "or")
    remove=ve.select.remove,         # "setdiff" - keep indices that are not in the other selection
    and=ve.select.and,               # "intersection" - keep indices in both selections
    or=ve.select.add,                # alias for "add" (just expressed as a logical operation)
    all=ve.select.all,               # select all indices (resets the selection)
    none=ve.select.none,             # select no indices (empty selection) - usually as the basis for adding in certain ones

    # Field lists (read-only)
    groups=ve.select.groups,
    tables=ve.select.tables,
    fields=ve.select.fields
  )
)

# TODO: the following function should not be for a single result set, but rather a
# VEResultsList. It should interpret the path as possibly a root for multiple scenarios
# and also attempt to find and open the associated model by looking up one level. So
# we should be able, in effect, to open a Model or bare results as long as we can find
# the key pieces...

#' Open VisionEval results from a directory
#'
#' @description
#' `openResults` opens a directory containing VisionEval model run results and
#'    returns a VEObject instance that can be used to extract the results or
#'    to perform queries. Limited probing occurs to attempt to identify the model
#'    that might contain these results.
#'
#' @details
#' See `vignette(package='VEModel')` for available help and reference materials.
#'   The basic use of `openModel` is also described in the VisionEval Getting-Started
#'   document on the VisionEval website (also in the VisionEval installer).
#'
#' The path provided as a parameter needs to contain ModelState.Rda and Datastore, using the names
#'   for those elements in the VisionEval run parameters ModelStateFile and DatastoreName
#'   respectively. Generally, it is most reliable to open an output using the model object returned
#'   by VEModel::openModel, since that will ensure that the same run environment is used to find the
#'   result files as when those results were created. The openResults file does not load any
#'   configurations.
#'
#' @param path A relative or absolute path to a directory (default is the working directory)
#'   in which VisionEval results can be found for a single model run, stage or scenario
#'   combination.
#' @return A VEResultsList object giving access to the VisionEval results in (or below) `path`
#' @export
openResults <- function(path=NULL) {
  if ( ! dir.exists(path) ) path <- getwd()

  # Start by seeing if path is model or a child or grandchild of a model.
  # 
  # If not valid, look for parent directory and try to open a model there and get its results list
  # If no VEModel "overhead", see if there are sub-directories and open each of those as a separate
  #   VEResults object in the VEResultsList

  # TODO: If we start in a subfolder of a model, generate a results list attached to that model, but
  #   only including the subdirectory stage. If we start in a 'results' directory (no ModelState,
  #   but with subdirectories containing Datastores, open the model from the first subdirectory and
  #   if the model is in the path, attach it to the Results list (which will contain all the
  #   subdirectoreis).
  return(VEResultsList$new(...))
}
