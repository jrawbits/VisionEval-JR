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
ve.resultslist.init <- function(stages,modelPath) {
  self$modelPath <- modelPath

  # Build results
  self$Results <- lapply(
    stages,
    function(stg) VEResults$new(stg$RunPath,ResultsName=stg$Name,ModelStage=stg)
  )
  names(self$Results) <- names(stages)
  # Build results index (S/G/T/N plus other metadata)
  self$resultsIndex <- do.call("rbind", lapply(self$Results,function(r) r$list)

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

# Extract from the list - works for everything
# selections are problematic since they are tied to specific result sets
ve.resultslist.extract <- function(stage=character(0),...) {
  # TODO: beef this up to handle an output format/destination
  # Create output tables, append results to them so a single set of tables
  #   can accumulate results from all reportable stages.
  if ( length(stage)==0 ) stage<-names(results)
  for ( stg in stage ) {
    self$Results[[stg]]$extract(...) # Change mechanism to have the list manage output data.frames and tables
  }
  return(invisible(stage))
}

# Produce a bare named list of VEResults objects
ve.resultslist.results <- function() return(self$Results)

# Print summary of results in the list
ve.resultslist.print <- function(...) {
  for ( nm in names(self$Results)) {
    print(self$Results[[nm]],name=nm,...) # Call VEResults$print
  }
}

ve.resultslist.export <- function() {
  # export result tables to external data storage format (or internal - wrap "extract")
}

ve.resultslist.extract <- function() {
  # create a nested list of data.frames of all the result tables
}

ve.resultslist.list <- function() {
  # TODO: Build a function to consolidate the metadata for each requested stage
  # TODO: Metadata should always include Scenario (Stage Name) and Year(s)
}

ve.resultslist.select <- function() {
  # TODO: move selection to be a list of S/G/T/N (G implies Year)
  # Use to filter fields during extract/export
}

ve.resultslist.find <- function() {
  # TODO: move selection to be a list of S/G/T/N (G implies Year)
  # Use to filter fields during extract/export
}

#' @export
VEResultsList <- R6::R6Class(
  "VEResultsList",
  public = list(
    # public data
    Results      = NULL,
    modelPath    = NULL,
    selection    = NULL,
    resultsIndex = NULL, # consolidated datastore index for all stages

    # methods
    initialize=ve.resultslist.init,
    print=ve.results.print,          # summary of model results (index)
    export=ve.resultslist.export,    # move the results to an external data storage format
    extract=ve.resultslist.extract,  # generate nested list of data.frames from model results (for further processing in R)
    list=ve.resultslist.list,        # show the consolidated modelIndex (used by export to develop metadata table)
    select=ve.resultslist.select,    # return the list's selection definition
    find=ve.resultslist.find         # constructs but does not embed the selection definition
  )
)

##########################
#
###### Implement VEResults
#
##########################

# Create VEResults object (manipulates Datastore/ModelState)
ve.results.init <- function(OutputPath,ResultsName=NULL,ModelStage=NULL) {
  # OutputPath is the normalized path to a directory containing the model results
  #  typically from the last model stage. Expect to find a ModelState.Rda file
  #  and a Datastore in that folder.
  self$resultsPath <- OutputPath
  self$Name <- if ( !is.character(ResultsName) ) basename(OutputPath) else ResultsName
  self$index()
  private$RunParam_ls <- self$ModelState()$RunParam_ls
  self$modelStage <- ModelStage # may be NULL
  self$selection <- VESelection$new(self)
  return(self$valid())
}

# Check results validity (all files present)
ve.results.valid <- function() {
  valid <- ! is.null(private$RunParam_ls) &&
           dir.exists(self$resultsPath) &&
           !is.null(self$modelIndex) && length(self$modelIndex)>0
  modelStatePath <- file.path(self$resultsPath,visioneval::getRunParameter("ModelStateFile",Param_ls=private$RunParam_ls))
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
  if ( is.null(private$RunParam_ls) && is.list( ms ) ) {
    private$RunParam_ls <- ms$RunParam_ls
  }

  # TODO: mark fields as being in the first (writable) model state path versus earlier ones
  # so we can pull out just fields explicitly in this stage; not a priority
  # Could add a column to dsListing, but need to change mergeDatastoreListings to respect it
  # ThisStage (or similar name) would get set to TRUE for first set, then FALSE for later ones
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
  scenario <- rep(visioneval::getRunParameter("Scenario",Default="Unknown Scenario",Param_ls=private$RunParam_ls),length(Description))

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
  row.names(Index) <- 1:nrow(Index) # Row names used for selection by index; may not need with new scheme
  self$modelIndex <- Index
  invisible(self$modelIndex)
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

# Helper function to attach DisplayUnits to a list of Group/Table/Name rows in a data.frame
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

# TODO: is this function ever used, or is it still relevant
# If kept, move to resultslist
ve.results.inputs <- function( fields=FALSE, module="", filename="" ) {
  # fields=TRUE, show all Datasets that originated as file inputs (lists all columns within input files)
  # fields=FALSE, just show the module, file, input directory (lists the input files)
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

# Return a named list of ScenarioElements and Levels for this set of
# results (from the ModelStage)
ve.results.elements <- function() {
  # Get scenario element names plus level values for associated model stage
  # Model stage must have scenario elements to use Visualizer
  elements <- self$modelStage$ScenarioElements
  if ( !is.character(elements) ) {
    return(list())
  }
  return(as.list(elements)) # converted named vector to named list
}

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
    Units_df <- addDisplayUnits(Units_df,Param_ls=private$RunParam_ls)
    displayUnits <- !is.na(Units_df$DisplayUnits)
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

# Update this selection, or just return what is already selected
# TODO: make this part of VEResultsList

# The selection will just be a list of G/T/N applied to the master
# index list, and then applied during extraction to the s
ve.results.select <- function(select=integer(0)) {  # integer(0) says select all by default. Use NA or NULL to select none
  # if is.null(select) do not change the current results selection
  # integer(0) says reset and select all
  # the is.environment test picks of an R6 VESelection object
  if ( missing(select) || is.environment(select) || ( ! is.null(select) && ! is.na(select) ) ) {
    self$selection <- VESelection$new(self,select=select)
  }
  invisible(self$selection)
}

# Find fields (as objects) within the current selection
ve.results.find <- function(pattern=NULL,Group=NULL,Table=NULL,Name=NULL,select=FALSE) {
  selection <- self$select()
  found <-selection$find(pattern=pattern,Group=Group,Table=Table,Name=Name,as.object=TRUE)
  # without "select=TRUE", found is an independent selection (not bound to results)
  if ( select ) found <- self$select(found) # bind selection to results
  return( found )
}

# Wrapper for visioneval::copyDatastore
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

ve.results.queryprep <- function() {
  visioneval::prepareForDatastoreQuery(
    DstoreLocs_ = file.path(self$resultsPath,self$ModelState()$DatastoreName),
    DstoreType  = self$ModelState()$DatastoreType
  )
}

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
    selection   =NULL,

    # methods
    initialize=ve.results.init,
    index=ve.results.index,          # Index Datastore from ModelState (part of init)
    copy=ve.results.copy,            # Apply visioneval::copyDatastore
    valid=ve.results.valid,          # has the model been run, etc.
    select=ve.results.select,        # return the object's selection object
    find=ve.results.find,            # does select() then VESelection$find
    extract=ve.results.extract,      # generate data.frames from model results
    export=ve.results.export,        # alias for 'extract' except that saving to a file is the default
    list=ve.results.list,            # show the modelIndex
    queryprep=ve.results.queryprep,  # For query or other external access
    print=ve.results.print,          # summary of model results (index)
    units=ve.results.units,          # Set units on field list (modifies self$modelIndex)
    elements=ve.results.elements,    # Get scenario elements (named list of scenario levels) for this scenario
    ModelState=ve.results.modelstate # Set/Get the model state for these results
  ),
  private = list(
    outputPath=NULL,                # root for extract
    RunParam_ls=NULL,
    modelStateEnv=NULL
  )
)

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

ve.select.groups <- function() {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No groups selected")
    return(character(0))
  }
  idxGroups <- unique(self$results$modelIndex[self$selection,c("Group"),drop=FALSE])
  return(idxGroups[order(idxGroups$Group),]) # Group
}

ve.select.tables <- function(nameOnly=FALSE) {
  if ( ! self$results$valid() ) stop("Model has not been run yet.")
  if ( any(is.na(self$selection)) ) {
    message("No tables selected")
    return(character(0))
  }
  idxTables <- unique(self$results$modelIndex[self$selection,c("Group","Table")])
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
  idxFields <- self$results$modelIndex[self$selection,c("Group","Table","Name")]
  return(sort(paste(idxFields$Group,idxFields$Table,idxFields$Name,sep="/"))) # Group/Table/Name
}

# Internal helper function to make a selection vector out of various other types of objects
ve.select.parse <- function(select) {
  # Though select can be a vector of field names, they need to be the full Group/Table/Name field names,
  #  so you should get them from ve.select.find, rather than manually construct them.
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

# Return a reference to this selection, changing its indices if an argument is provided
ve.select.select <- function(select) {
  if ( ! missing(select) ) self$selection <- self$parse(select)
  invisible(self)
}

# Strictly speaking, the Datastore table key fields should be
# recoverable from the module specifications, but I haven't found a
# way to do that comprehensively yet.
allTheKeys = c(
  "Marea","Azone","Bzone",
  "HhId","VehId","WkrId"
)

ve.select.addkeys <- function(Group=NULL,Table=NULL,Keys=NULL) {
  # Helper to move key fields across
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
# OR it products a vector of matching element indices (as.object==FALSE)
ve.select.find <- function(pattern=NULL,Group=NULL,Table=NULL,Name=NULL,as.object=TRUE,select=FALSE) {
  # if pattern (regexp) given, find names matching pattern (only within the "fields" part)
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

# Build data.frames based on selected groups, tables and dataset names
ve.select.extract <- function(
  saveResults=FALSE,
  saveTo=visioneval::getRunParameter("OutputDir",Param_ls=private$RunParam_ls),
  prefix="",
  convertUnits=TRUE,
  data=NULL
) {
  # Delegates to the result object, setting its selection in the process
  invisible( self$results$extract(saveResults=saveResults,saveTo=saveTo,prefix=prefix,select=self,convertUnits=convertUnits,data=data) )
}

#' Conversion method to turn a VESelection into a vector of selection indices
#'
#' @param x a VESelection object (or something that can be coerced to one)
#' @param ... Additional arguments to support generic signature
#' @return an integer vector of selected fields
#' @export
as.integer.VESelection <- function(x,...) x$selection

# The VESelection R6 class
# This interoperates with VEResult to keep track of what to print

#' @export
VESelection <- R6::R6Class(
  "VESelection",
  public = list(
    # public data
    selection = integer(0),
    results = NULL,

    # methods
    initialize=ve.select.initialize,
    copy=ve.select.copy,          # Create a new selection object with the same results and indices
    print=ve.select.print,        # Print list of fields or (details=TRUE) the subset of results$modelIndex
    show=ve.select.show,          # retrieve the selected subset of results$modelIndex (data.frame)
    valid=ve.select.valid,        # is the selection valid against results$modelIndex
    extract=ve.select.extract,    # extract the selection from associated results
    export=ve.select.extract,     # export the extracted selection (eventually split out "saving a table" from "getting a table")
    find=ve.select.find,          # general search facility for selecting group/table/name
    parse=ve.select.parse,        # interpret different ways of specifying a selection (number, field descriptor)
    select=ve.select.select,      # assign - set self to other selection value
    add=ve.select.add,            # "union" - indices are included from either selection
    addkeys=ve.select.addkeys,    # add keys (e.g. HhID, BZone) for already SELECTED Tables (uses "or")
    remove=ve.select.remove,      # "setdiff" - keep indices that are not in the other selection
    and=ve.select.and,            # "intersection" - keep indices in both selections
    or=ve.select.add,             # alias for "add" (just expressed as a logical operation)
    all=ve.select.all,            # select all indices (resets the selection)
    none=ve.select.none,          # select no indices (empty selection) - usually as the basis for adding in certain ones

    # Field lists (read-only)
    groups=ve.select.groups,
    tables=ve.select.tables,
    fields=ve.select.fields
  )
)

#' Open VisionEval results from a directory
#'
#' @description
#' `openResults` opens a directory containing VisionEval model run results and
#'    returns a VEObject instance that can be used to extract the results or
#'    to perform queries.
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
#' @return A VEResults object giving access to the VisionEval results in `path`
#' @export
openResults <- function(path=NULL) {
  if ( ! dir.exists(path) ) path <- getwd()
  return(VEResults$new(path))
}
