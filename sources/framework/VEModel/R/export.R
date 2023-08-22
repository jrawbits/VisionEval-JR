# export.R#' @include environment.R
NULL

# Documentation for VEExporter
#' VEExporter class for exporting tabular data from model or query results
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEEXporter
NULL

# Documentation for VEPartition
#' VEPartition class for partitioning tabular data from model or query results
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEPartition
NULL

# Documentation for VEConnection
#' VEConnectionxs class for connecting to an external table database
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @aliases VEConnection.Dataframe VEConnection.CSV VEConnection.DBI
#' @name  VEConnection
NULL

self=private=NULL # To avoid global variable warnings

############################
#
###### Implement VEPartition
#
############################

# VEPartition will take a partition description (named character vector of partition actions)
# Basic format is c(Field="action",Field2="action",...)
# Actions are the following:
#   "none" or "merge"  : ignore the fields - just there as documentation...
#   "path" or "folder" : partition data on this element and add its value to the list of "paths"
#   "name"             : partition data on this element and add its value to the list of "names
# Constructor copies the vector (or installs a default), dropping the none/merge elements
#
# Other main function is $partition with this interface:
# $partition(data,Table,Scenario=NULL,Group=NULL,Metadata=NULL)
#    data is a table with 0 or more of the partition fields in it
#    Table is the base name for the table
#    Optional Scenario and Group entries get pushed as columns into the Metadata element of the TableLoc structure
#
# Partition returns a list of TableLoc structures that define one or more partitions for the data
#    Range : a numeric vector of rows in this partition
#    Partition : a structure describing the Table:
#       Path : a character vector of path elements describing the Table in the connection (prepended to Table name
#              perhaps as folders
#       Name : a character vector of path elements describing the Table in the connection (prepended to Table name)
#       Table : root table name
#    Metadata : a data.frame of all the Names in the Table (TableLoc, separate element, is
#               implicitly a member
#               Includes Scenario, Group, Table, Name, plus any columns (by Name) in the furnished
#               Metadata (often Units and Description, but possibly others)
#    TableString : a string built from Path, Name and Table describing this entry

#    THe VEExporter will keep a list of all generated TableLocs using The TableLoc name for lookup.
#    Presence of the TableLoc in that list will lead to the table being opened (using TableLoc) for
#    appending.
#    Metadata is accumulated for each table created (not written) in a single Metadata table for the
#    VEExporter
#    We'll use the TableLoc in the VEExporter$write function

ve.partition.init <- function(partition=character(0)) {
  # "partition" is a character vector of partitioning vectors for possible fields
  # The default partitioning scheme is this:
  if ( missing(partition) || length(partition)==0 || ! is.character(partition) ) {
    self$Partition <- character(0) # Default is not to partition
  } else {
    # remove "none" or "merge" elements and keep the rest
    self$Partition <- partition[ grep("(merge)|(none)",partition,invert=TRUE) ]
    # collapse alternate names to standard names
    self$Partition <- gsub("path","folder",self$Partition)
    # get rid of (and warn about) any partition actions that are not defined
    badPartitions <- ! self$Partition %in% c("name","folder") # eventually allow "database"
    if ( any(badPartitions) ) {
      writeLog(paste("Ignoring bad partitions:",paste(self$Partitions[badPartitions],collapse=",")),Level="warn")
      self$Partition <- self$Partition[ ! badPartitions]
    }
  }
}

ve.partition.partition <- function(theData,Table) {
  # theData is a data.frame possible with partitionable columns
  # Table is the root name of the table for its locator
  # returns a list of table locators (lists) for each of the partitions
  if ( missing(theData) || missing(Table) || is.null(theData) || is.null(Table) ) {
    traceback(1)
    stop("Invalid parameters for VEPartition$partition",call.=FALSE)
  }

  # reduce self$Partition to fields present in theData (can't partition what is not there)
  use.partition <- self$Partition[ which( names(self$Partition) %in% names(theData) ) ]
  # reduce partition to ignore any field which only contains NA values
  naFields <- sapply(theData[,names(use.partition)],function(d) all(is.na(d)) )
  if ( any(naFields) ) use.partition <- use.partition[ ! naFields ]

  # now do the same with fields having some NA values, but warn about it
  invalidFields <- sapply(theData[,names(use.partition)],function(d) any(is.na(d)))
  if ( any(invalidFields) ) {
    warning("Export Data to be partitioned has fields with some but not all NA values.")
    warning(paste(names(use.partition)[invalidFields],collapse=",")," will not be partitioned.")
    use.partition <- use.partition[ ! invalidFields ]
  }

  # identify unique values in each of the partition columns
  partition.values <- lapply( names(use.partition), function(p) unique(theData[[p]]) )
  names(partition.values) <- names(use.partition)
  locations <- expand.grid(partition.values,stringsAsFactors=FALSE)
  # yields a data.frame where columns are the partition fields and rows are the values
  
  # Build the partitions based on the locations...
  TableLocs <- list()
  locNames <- names(locations)
  pathNames <- names(use.partition)[use.partition=="folder"]
  nameNames <- names(use.partition)[use.partition=="name"]

  for ( loc in 1:nrow(locations) ) {
    thisLocation <- locations[loc,]
    Paths <- thisLocation[pathNames]
    if ( length(Paths) > 0 ) { # at least one value in the Paths partition
      names(Paths)<-pathNames;
      Paths[is.na(Paths)]<-"NA" # Turn NA into a valid value; shouldn't have any of these
      Paths <- as.character(Paths)
      names(Paths) <- pathNames
    } else Paths <- character(0)
    Names <- thisLocation[nameNames]
    if ( length(Names) > 0 ) { # at least one value in the Names partition
      names(Names)<-nameNames;
      Names[is.na(Names)] <- "NA" # Turn NA into a valid value; shouldn't have any of these
      Names <- as.character(Names)
      names(Names) <- nameNames
    } else Names <- character(0)
    # Arrive here with Names and Paths both named character vectors, possibly empty
    Partition <- list(
      Paths = Paths,
      Names = Names,
      Table = Table
    )
    
    locFields <- locations[loc,,drop=FALSE]
    locSelection <- rep(TRUE,nrow(theData))
    for ( n in locNames ) {
      nValue <- locFields[[n]]
      locSelection <- locSelection & (theData[[n]] == nValue) # knock off rows not matching this value
    }
    Range <- which(locSelection) # vector of data rows to include in this partition location

    tableLoc <- list(
      Range = Range,
      Partition = Partition
    )
    # makeTableString defined below (used mostly by VEConnection)
    TableString <- makeTableString( tableLoc$Partition$Paths, tableLoc$Partition$Names, Table )
    # message("Table String: ",TableString)
    TableLocs[[TableString]] <- structure( tableLoc , class="VETableLocator")
  }
  return(TableLocs)
}

ve.partition.location <- function(theData,Table) {
  cat("Table =",Table,"\n")
  tableLoc <- list(
    Range = 1:nrow(theData),
    Partition = list(
      Paths = character(0),
      Names = character(0),
      Table = Table
    )
  )
  TableString <- makeTableString( tableLoc$Partition$Path, tableLoc$Partition$Name, Table )
  locList <- list()
  locList[[TableString]] <- structure( tableLoc, class="VETableLocator" )
  return( locList)
}

ve.partition.print <- function(...) {
  if ( length(self$Partition) == 0 || ! nzchar(self$Partition[1]) ) {
    cat("No partitioning")
  } else {
    for ( p in 1:length(self$Partition) ) {
      cat(names(self$Partition)[p],":",self$Partition[p],"\n")
    }
  }
}

print.VETableLocator <- function(x,...) {
  cat("Range: ",paste(x$Range,collapse=" "),"\n",sep="")
  cat("Partition:\n")
  cat("  Table: ",x$Partition$Table,"\n",sep="")
  cat("  Paths: ",paste(x$Partition$Paths,collapse=","),"\n",sep="")
  cat("  Names: ",paste(x$Partition$Names,collapse=","),"\n",sep="")
}
    
# Save and load are used by VEExporter$save to make an easy-to-parse YAML named list
#   (YAML has trouble distinguishing a named vector from a named list)
ve.partition.load <- function(partitions) {
  self$Partitions <- sapply(partitions,as.character) # preserves names of partitions
}
ve.partition.save <- function() {
  return( lapply(self$Partitions,function(s) s ) ) # named list of partition elements
}

VEPartition <- R6::R6Class(
  "VEPartition",
  public = list(
    # public data
    Partition = character(0),           # Describes fields to be partitioned into different output tables

    # methods
    initialize=ve.partition.init,     # initialize internal partition
    print=ve.partition.print,         # print the partition
    partition=ve.partition.partition, # partition a data.frame into output tables
    location=ve.partition.location,   # generate a TableLocator from data + Table name
    save=function() self$Partition,   # generate a list of self$Partition for saving
    load=ve.partition.load            # load a named list of strings into self$Partition
  )
)

###########################
#
###### Implement VEExporter
#
###########################

# TODO: how to implement the exporter:
# Interface:
#    initialize  ## Individual Exporter classes will receive connection, partition, and
#                ## optional tag and optional VEModel object
#                ## connection is a string or a named list, depending on the requirements
#                ##  of the specific exporter
#                ## partition vector with name of variable and a strategy from among
#                ##  c("none","folder","name")
#                ##  exporter may not support "folder" (e.g. DBI/SQL in which case we
#                ##  should warn and fall back to "name")
#                ## tag parameter is a character string that will be used to look up
#                ##  default connection and partition data in the "Exporter" configuration block
#                ##  in visioneval.cnf
#                ## Model parameter can add default places to search for connection
#                ##  parameters, which will be looked up by the connection tags
#                ## Can load from a file containing connection descriptor, partitions and table locations
#                ##   which is useful for manipulating generated results with dplyr
#    config      ## Report the partition string and connecton configuration list used to build connection
#    load        ## Read an .VEexport (.Rdata) file with the Exporter elements
#                ##   (called from $initialize for re-opening connection to additional items)
#    save        ## Write a .VEexport (.Rdata) file with the Exporter elements
#    connection  ## Change/set the exporter connection (can't call if anything has been written)
#    partition   ## Change the exporter partitioning strategy (can't call if anything has
#                ##   already been written) e.g. c(Group=name,Scenario=name,Table=name)
#                ##   CSV Default is folder for Scenario, name for Table, and merge for Group/Scenario
#                ##   SQL Default is merge for Scenario, name for Table, and merge for Group
#                ## Doing folder for SQL might one day allow us to write to a different DBI
#                ##   connection
#    write       ## a data.frame plus partition flags (perhaps passed as ...)
#                ## default for unsupplied parameters is to ignore them (treating them as 'merge"
#                ## and not checking if they are really there)
#    metadata    ## a function to turn self$TableList into a writable data.frame
#    writeMetadata ## helper to write self$metadata() into a table on self$Connection
#    list        ## log of tables that have been built during export
#    print       ## just cat the names from the list
#    data        ## convert table locators (parameters, default=all) to flat list of data.frames
#                ## can also produce other types (e.g. tbl for dbplyr from DBI, data.tables, tibbles)

ve.exporter.init <- function(Model,load=NULL,tag=NULL,connection=NULL,partition=NULL) {
  # We may be able to live without subclassed Exporters
  # connection is a VEConnection object; if NULL, create a default one
  # partition is a VEPartition object; if NULL, use the default one for tag
  #   To force no partition, pass c() or character(0) - Table name will just get written/appended as is
  self$Model <- Model
  if ( is.character(load) ) {
    # Presumes load is an existing file
    # Usually we'll get here via VEModel$exporter
    if ( ! file.exists(load) ) stop("Could not find saved exporter to load: ",load,call.=FALSE)
    self$load(Model,load)
    return()
  }
  if ( ! missing(tag) ) {
    # if tag is already a VEExporter:
    if ( inherits(tag,"VEExporter") ) {
      self$Configuration <- tag$config()
    } else if ( is.character(tag) ) {
      defaultPartition <- !is.character(partition)
      # Default exporters
      defaultConfigs <- defaultExporters()
      self$Configuration <- if ( tag %in% names(defaultConfigs) ) defaultConfigs[[tag]] else list(
        Partition = character(0),
        Connection = list()
        )
      # Model definitions
      globalConfigs <- getSetup(paramNames="Exporters",fromFile=TRUE)
      if ( tag %in% names(globalConfigs) ) {
        globalConfig <- globalConfigs[[tag]]
        if ( !is.null(globalConfig$Connection) && length(globalConfig$Connection)>0 ) {
          self$Configuration$Connection[ names(globalConfig$Connection) ] <- globalConfig$Connection
        }
        if ( defaultPartition && is.character(globalConfig$Partiton) ) self$Configuration$Partition <- globalConfig$Partition
      }
      modelConfigs <- getSetup(paramNames="Exporters",fromFile=TRUE) # will have inherited from global
      if ( tag %in% names(modelConfigs) ) {
        modelConfig <- modelConfigs[[tag]]
        if ( !is.null(modelConfig$Connection) && length(modelConfig$Connection)>0 ) {
          self$Configuration$Connection[ names(modelConfig$Connection) ] <- modelConfig$Connection
        }
        if ( defaultPartition && is.character(modelConfig$Partiton) ) self$Configuration$Partition <- modelConfig$Partition
      }
      if ( is.list(connection) ) {
        self$Configuration$Connection[ names(connection) ] <- connection
      }

      # Pull TablePrefix and TableSuffix from connection, if present
      if ( "TablePrefix" %in% names(self$Configuration$Connection) ) {
        self$TablePrefix <- self$Configuration$Connection$TablePrefix
        self$TablePrefix <- as.character(self$TablePrefix)[[1]]
      } else self$TablePrefix <- character(0)
      self$TablePrefix <- as.character(self$TablePrefix)[[1]]

      if ( "TableSuffix" %in% names(self$Configuration$Connection) ) {
        self$TableSuffix <- self$Configuration$Connection$TableSuffix
        self$TableSuffix <- as.character(self$TableSuffix)[[1]]
      } else self$TableSuffix <- character(0)
    }
    # set up the Partition
    if ( ! defaultPartition ) { # partition was provided as a parameter
      self$Configuration$Partition = partition
    }
  } else {
    # No tag; let's hope connection and partition have what we need
    self$Configuration <- list(
      Connection = connection,
      Partition = partition
    )
  }
  # either of the following may stop if the Configuration is inadequate
  self$Connection <- makeVEConnection(Model,self$Configuration$Connection) # Returns a VEConnection subclass
  self$Partition <- VEPartition$new(self$Configuration$Partition)
}

# subclasses will do more with Connection and Partition prior to saving them
# May rewrite "folder" to "name" e.g.
# Folders will get created as data is written

ve.exporter.write <- function(Data, Table, Metadata=NULL, Scenario=NULL, Group=NULL, overwrite=FALSE ) {
  # Table must be provided as the root name for the Table being written. Table is used to
  #   select the recipient for the partitioned data. The actual Table name written will be
  #   composed of the results of partitioning data into path elements, name elements, the
  #   Table name and built by the connection object (adding any TablePrefix or TableSuffix).
  Table <- paste0(self$TablePrefix,Table,self$TableSuffix)
  message("DEBUG: exporting Table ",Table)

  locations <- if ( ! ( is.null(Scenario) || is.null(Group) ) ) { # no partitioning
    self$Partition$partition(Data,Table)
  } else self$Partition$locate(Data,Table) # creates a TableLocation with the entire range

  # process each location into low-level calls to Connection$writeTable
  for ( loc in locations ) {
    TableName <- self$Connection$nameTable(loc)
    TableFields <- self$Connection$writeTable(Data[loc$Range,],TableName)
    # Save metadata if provided
    if ( ! is.null(Metadata) ) {
      message("DEBUG: Metadata Construction for ",TableName)
      selector <- Metadata$Name %in% TableFields
      if ( ! is.null(Scenario) ) selector <- selector & Metadata$Scenario == Scenario
      if ( ! is.null(Group) ) selector <- selector & Metadata$Group == Group
      self$TableList[[TableName]] <- Metadata[selector,]
      self$TableList[[TableName]] <- cbind(self$TableList[[TableName]],DBTable=TableName)
    } else {
      if ( is.null(Scenario) ) Scenario <- NA
      if ( is.null(Group) )    Group <- NA
      self$TableList[[TableName]] <- data.frame(Name=TableFields,Scenario=Scenario,Group=Group,DBTable=TableName)
    }
  }
}

ve.exporter.list <- function(names=NULL, namesOnly=TRUE) {
  # namesOnly is intended for interactive use to list out the tables in the export (as written)
  # if this function is used internally, set namesOnly to False, or if its really deeply
  #   internal, just access self$TableList (the "metadata")
  if ( is.null(self$TableList) ) { # Nothing has been exported yet
    return("Nothing exported yet")
  }
  allNames <- ( is.null(names) || !is.character(names) || length(names)==0 || ! all(nzchar(names) ) )
  if ( allNames ) {
    names <- names(self$TableList)
    if ( isTRUE(namesOnly) ) return(names)
  }
  # Return a subset of the TableList
  return (
    if ( isTRUE(namesOnly) ) {
      names[names %in% names(self$TableList)] # vector of table locations string
    } else {
      self$TableList[names] # a list of field name vectors.
    }
  )
}

ve.exporter.print <- function(names=NULL,...) {
  cat("Exporter for Model: ",self$Model$modelName,"\n")
  cat("Connection:\n")
  print(self$Connection)
  cat("Partition:\n")
  print(self$Partition)
  tables <- self$list(names=names,...) # could do namesOnly=FALSE in ...
  if ( ! is.null(tables) ) {
    cat("Exported Tables:\n")
    print(tables)
  } else cat("No Tables Exported Yet.\n")
}

ve.exporter.formatter <- function(Data,format="data.frame") {
  if ( ! is.data.frame(Data) ) stop("Can't format Data (expecting data.frame):",class(Data))
  if ( format == "data.frame" ) {
    return( as.data.frame(Data) ) # probably already is...
  } else if ( format == "data.table" ) {
    return( data.table::as.data.table(Data) )
  } else if ( format == "tibble" ) {
    return( tibble::as.tibble(Data) )
  } else if ( format == "tbl" ) {
    return( dplyr::tbl(Data) )
  } else {
    message("Unrecognized format for VEExporter:$data: ",format,"; returning Data verbatim")
    return(Data)
  }
}

ve.exporter.data <- function(tables=NULL,format="data.frame") { # tables: a list of table strings
  # Generate a list of requested table data in the requested format (which will be
  # interpreted by self$Connection as it reads out the data).
  tables <- self$list(tables,namesOnly=TRUE)
  exportedData <- if ( missing(format) || format=="data.frame" ) {
    lapply( tables, function(t) self$Connection$readTable(t) )
  } else {
    lapply( tables, function(t) self$formatter(self$Connection$readTable(t),format=format) )
  }
  if ( length(exportedData)>0 ) names(exportedData) <- tables
  return(exportedData)
}

ve.exporter.metadata <- function() {
# Compile accumulated metadata plus DB TableName (one row per N/TableName), and include
#  the Units actually written plus the N description. DBTableName is the locator encoded
#  form (makeTableString with default parameters). Metadata is just intended to understand
#  what is in TableName.
  metadatalist <- lapply( names(self$TableList), function(t) {
    metadata <- self$TableList[[t]]
    medadata$DBTableName <- self$Connection$findName(t) # standard name => database corresponding name
  } )
  return( unique(do.call(rbind,metadatalist)) )
  # will fail if not all metadata tables have the same columns. In practice, shouldn't be
  # a problem: Un-partitioned tables do not end up in the metadatalist, and the are best
  # all written from the same VEResultsList.
}

ve.exporter.writeMetadata <- function(data, Table="Metadata") {
  self$write( self$metadata(), Table=Table) # No partitioning
}

# Save Connection, Partition, TableList, and Metadaa.
ve.exporter.load <- function(filename) { # .VEexport file (.Rdata)
  # Filename should already be normalized to the model's output location
  # This function will usually be called via VEModel$exporter(file=...), which will build a good path
  otherExporter <- new.env()
  if ( ! file.exists(filename) ) {
    checkFilename <- file.path(self$Model$exportPath,filename)
    if ( ! file.exists(checkFilename) ) stop("Could not find file: ",filename,call.=FALSE)
    filename <- checkFilename
  }
  load(filename,envir=otherExporter)
  self$Configuration <- otherExporter$Configuration
  self$TableList     <- otherExporter$TableList

  if ( ! is.null(self$Connection) ) self$Connection$close() # varies by class

  self$Connection <- makeVEConnection(self$Model,self$Configuration$Connection)
  self$Partition  <- VEPartition$new(self$Configuration$Partition)
}

ve.exporter.save <- function(filename) { # .VEexport file (.Rdata)
  filename      <- file.path(self$Model$exportPath,basename(filename))
  Configuration <- self$Configuration
  TableList     <- self$TableList
  save(Configuration,TableList,file=filename)
  message("Saved Exporter to ",filename)
}

VEExporter <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEExporter",
  public = list(
    # public data
    Model      = NULL,      # The Model associated with this exporter (for load/save path, connection building)
    Partition  = NULL,      # default is c(Scenario="merge",Group="merge",Table="name") # probably never want to change Table partition
    Connection = NULL,      # may be named list or character vector or string depending on derived class needs
    Configuration = list(), # Parameters to save for this connection (to rebuild it)
    TableList  = list(),    # Names in this list are table IDs of created tables, elements of the list are lists of the
                            # fields that exist in the tables (names). Need to maintain fields so we can
                            # make existing table and new written data be conformant during $write (typically will be)
    Metadata   = NULL,      # data.frame of S/G/T/N/Metadata passed into $write (TableList prior to partitioning)
                            # Any N row in the Metadata for an S/G/T is a candidate for partitioning

    # methods
    initialize=ve.exporter.init,       # call from subclasses to establish internal connection and partition
    load=ve.exporter.load,             # called from init if load=savedExporter is provided (a file.path)
    save=ve.exporter.save,             # called e.g. by VEResultsList$export when the export is complete

    write=ve.exporter.write,           # write rows onto a table (creating if necessary)
    list=ve.exporter.list,             # list tables that have been created within this exporter
    print=ve.exporter.print,           # print list of table identifiers
    data=ve.exporter.data,             # return list of data.frames corresponding to list of table names (all or some of self$list)
                                       # If format is given, use VEExporter$formatter to convert
    formatter=ve.exporter.formatter,

    # Use Prefix/Suffix to identify different exports
    TablePrefix = NULL,                         # if set, send to Table during $writeTable or $readTable
    TableSuffix = NULL,                         # if set, send to connection$writeTable or readTable

    # implementation methods
    # for the following, "table" is a table navigator (folder, folder, name) as stored in TableList
    createTable = function(data,table) NULL,  # Create or re-create a Table from scratch (includes append)
    writeTable  = function(data,table) NULL,  # Append data to existing table (restructure as needed)
    readTable   = function(table)      NULL   # Read named table into (always) a data.frame; use VEExporter$formatter for something else
  ),
  private = list(
    saveFileName = NULL,               # File name if exporter has been loaded/saved
                                       # from $initialize(load=filename) - need not exist
                                       # Can construct a default file name when initializing, for later use
    readOnly = FALSE # TRUE for loaded exporter if we don't specify the "append=TRUE" flag on $initialize
    # A re-opened exporter can be written to (append mode)
    # Don't include the TimeStamp in the saved exporter name, but should track the connection "tag" and Model
    # plus perhaps the "Database" (CSV/Parquet root folder, SQLite database - for other DBI, the connection
    # is internal, and the "discriminator" is optional and arbitrary (e.g. "MySQL" or "Access"). Presumably the
    # discriminator is the sub-directory/root file name in the same directory as the .VEexport file.
    #   Need to differentiate between writing new data versus skipping provided data already written
    #   That's a Metadata operation based on original S/G/T/N (and perhaps just initial S(cenario))
    #   It would be quick and easy to check loaded Metadata for Scenario and refuse to write it if
    #     it's already present in the Exporter output.
    #   Aiming to run new scenarios on a model and add them to the same export connection
  )
)

#############################
#
###### Implement VEConnection
#
#############################

# The VEConnection is initialized from a "tag" and named list of connection parameters that are
# interpreted as needed by each driver. Default settings for all of them will yield a valid
# exporter for:
#    data.frames (option to use data.tables, tibbles or something else) default is hierarchical names lists of data.frames
#    csv (option perahsp to use parquet or alternte csv driver) default is csv in subdirectory of OutputDir named after model
#    dbi (options to pick a specific driver) default is SQLite into a file in OutputDir named after model

ve.connection.missing <- function(dataFields,Table) {
  browser()
  tableFields <- private$tableFields[[Table]]
  missingTableNames <- ! dataFields %in% tableFields # add these fields as NA to existing table
  missingDataNames  <- ! tableFields %in% dataFields # add these fields as NA to incoming data source
  missing <- list(Data=character(0),Table=character(0))
  if ( any(missingDataNames) ) {
    missing$Data <- dataFields[missingDataNames]
  }
  if ( any(missingTableNames) ) {
    message("Missing TableNames:")
    print(tableFields[missingTableNames])
    missing$Tables <- tableFields[missingTableNames]
  }
  return(missing)
}

# ve.connection.init        <- function(config) {} # initialize the connection from parameters
# ve.connection.raw         <- function() { message("Base class does not have raw data"); return(NULL) } # override in specific subclasses
# ve.connection.close       <- function() {} # Base class doesn't need to do anything
# ve.connection.nameTable   <- function(loc) {} # turn table location into a string
# ve.connection.createTable <- function(Data,Table) {}
# ve.connection.writeTable  <- function(Data,Table,addFields=character(0)) {}
# ve.connection.readTable   <- function(Table) {} # Table can be a "loc" or compatible nameTable

VEConnection <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEConnection",
  public = list(
    # public data

    # methods (each connecton type will implement its own version of these)
    initialize  = function(config) {},          # call from subclasses to establish internal connection and partition
    summary     = function() { "Base VEConnection" }, # simplified text representation of the connection details
                                                # format and content depends on the connection class
    print       = function(...) cat(self$summary(),"\n"), # Whatever makes sense for the derived class
    raw         = function() {},                # returns CSV folder or list of data.frames or DBI connection
                                                # specific value is class-specific and is enough to re-open and
                                                # review what is in the connection (e.g. to list actually written
                                                # tables)
    close       = function() {},                # release internal connection (whatever is returned by "raw")

    nameTable   = function(TableLoc) NULL,      # Turn the TableLoc into an actual table name suitable for creating/writing/reading
                                                # Exporter will add this name to the TableLoc Metadata
    createTable = function(data,table) NULL,    # Create or re-create a Table from scratch (includes append)
    writeTable  = function(data,table) NULL,    # Append data to existing table (restructure as needed)
    readTable   = function(table) NULL,         # Read named table into a data.frame
                                                # Throughout, 'table' may be a TableLoc or a TableLocator string
    missingFields = ve.connection.missing       # Internal helper to find differences between saved table and new data
  ),
  private = list(
    tableFields = list()
  )
)

# VEConnection implementations

#####################
# Helpers
#####################

# makeTableString is used in the connection types to convert a TableLoc to
#   whatever is needed to make a table name on the connection. Default format
#   is generic representation used by VEPartition to name the TableLocs it generates
#
# e.g. CSV:
#   loc <- list( Paths='MyScenario', Names='2019, Table='Household' )
#   csv.table.file <- makeTableString( loc$Partition$Paths, loc$Partition$Names, Loc$Partition$Table, self$connectTime, tableSep="/", timeSep="_" )
#   # MyScenario/2019_Household_20230816084102
#
# e.g. SQL/DBI:
#   sql.table <- makeTableString( loc$Partition$Paths, loc$Partition$Names, Loc$Partition$Table, pathSep="_", tableSep="_" )
#   # MyScenario_2019_Household
#
# e.g. data.frame:
#   df.name <- makeTableString( Paths=c(), Names=loc$Partition$Names, Table=loc$Partition$Table )
#   # 2019_Household - Paths (MyScenario)  become nested lists of data.frames
#
# In reality, both of those connections use the Paths=TableLoc shortcut:
#   csv.table.file <- makeTableString( tableLocator, TimeStamp=self$connectTime, timeSep="_" )
#   # note in this case that TimeStamp must be a named parameter (to leave Names, Table empty)
#
makeTableString <- function(Paths, Names, Table, TimeStamp=NULL, locString=NULL,
  tablePrefix=character(0), tableSuffix=character(0),
  pathSep="/", nameSep="_", tableSep=":", timeSep="#" ) { # default separators let us reconstruct a TableLocator
  # TimeStamp = NULL or FALSE: do not include
  # TimeStamp = TRUE: include system time at moment of function call
  # TimeStamp = POSIXc like Sys.time(), attach that
  if ( inherits(Paths,"VETableLocator") ) {
    Paths <- Paths$Partition$Paths
    Names <- Paths$Partition$Names
    Table <- Paths$Partition$Table   # Table already had prefix/suffix attached
  } else if ( ! is.null(locString) ) {
    locator <- parseTableString(locString)[[1]] # Default parameters
    Paths <- locator$Partition$Paths
    Names <- locator$Partition$Names
    Table <- locator$Partition$Table # Table already had prefix/suffix attached
  } else{ # Prefix/Suffix should include separator if needed (suggest "+" or "!" or "@", depending on connection)
    Table <- paste0( tablePrefix,Table,tableSuffix )
  }
  pathString <- if ( length(Paths) > 0 ) paste(Paths,collapse=pathSep) else character(0)
  tableString <- if ( length(Names) > 0 ) paste(Table,paste(Names,collapse=nameSep),sep=nameSep) else Table
  
  tableString <- paste(pathString,tableString,sep=tableSep)
  if ( ! is.null(TimeStamp) && ! any(is.na(TimeStamp)) ) {
    TimeStamp <- TimeStamp[1] # vector not allowed
    if ( isTRUE(TimeStamp) ) TimeStamp <- Sys.time() # system default string time format
    else if ( inherits(TimeStamp,"POSIXct") ) TimeStamp <- format(TimeStamp,"%Y%m%d%H%M")
    else TimeStamp <- as.character(TimeStamp) # may get "sanitized" by VEConnection for use as table name
    tableString <- paste(tableString,TimeStamp,sep=timeSep)
  }
  return(tableString)
}

# Reconstruct TableLocators from articulated TableStrings
# pathSep, nameSep, tableSep and timeSep must all be distinct
#   and may not include regular expression special characters
parseTableString <- function(tableString,pathSep="/",nameSep="_",tableSep=":",timeSep="#") {
  # expects tableString to be a character vector of articulated TableStrings
  locators <- lapply( strsplit(tableString, timeSep), function(tableTime) {
    if ( length(tableTime) > 1 ) {
      TimeStamp <- tableTime[2]
    } else {
      TimeStamp <- character(0)
    }
    pathNames <- strsplit(tableTime[1],tableSep)[[1]]
    if ( length(pathNames) > 1 ) {
      Paths <- strsplit(pathNames[1],pathSep)
      namesTable <- pathNames[2]
    } else {
      Paths <- character(0)
      namesTable <- pathNames[1]
    }
    namesTable <- strsplit(namesTable,nameSep)[[1]]
    if ( length(namesTable) > 1 ) {
      Names <- namesTable[-length(namesTable)]
      Table <- namesTable[length(namesTable)]
    } else {
      Names <- character(0)
      Table <- namesTable
    }
    return ( structure( list(
      Paths = Paths,
      Names = Names,
      Table = Table,
      TimeStamp = TimeStamp
    ), class="VETableLocator" ) )
  } )
  return( if ( length(locators)==1 ) locators[[1]] else locators )
}

#######################################
#
###### Implement VEConnection.Dataframe
#
#######################################

ve.connection.df.createTable <- function(Data,Table) {
  if ( ! is.character(Table) ) stop("createTable: Table must come from nameTable")
  private$tableFields[[Table]] <- names(Data)
  private$exportedData[[Table]] <- Data
  }

ve.connection.df.writeTable <- function(Data,Table) {
  if ( ! is.character(Table) ) Table <- self$nameTable(Table) # TODO: check explicitly with VETableLocator...
  if ( ! Table %in% names(private$exportedData) ) {
    self$createTable(Data,Table) }
  else {
    # Conform the columns (ideally, we never end up using this code...)
    missingFields <- self$missingFields(names(Data),Table)
    # How to conform them will depend on the underlying table storate mechanism
    if ( length(missingFields$Data) > 0 ) {
      naValue <- as.list(rep(NA,length(missingFields$Data)))
      names(naValue) <- missingFields$Data
      Data <- cbind(Data,data.frame(naValue))
    }
    if ( length(missingFields$Table) > 0 ) {
      # External data sources will need to read back the table, add fields, recreate (drop/add) table
      naValue <- as.list(rep(NA,length(missingFields$Table)))
      names(naValue) <- missingFields$Table
      private$exportedData[[Table]] <- cbind(private$exportedData[[Table]],data.frame(naValue))
    }
  }
  # Append the rows
  private$exportedData[[Table]] <- rbind(private$exportedData[[Table]],Table)
  return( names(private$exportedData[[Table]]) ) # required return for writeTable API: names in DB table
}

ve.connection.df.readTable <- function(Table) {
  return( private$exportedData[[Table]] )
}

VEConnection.Dataframe <- R6::R6Class(
  # Accumulates data.frames in memory
  # Connection may create a different storage class (e.g. data table / tibble / data.frame or even arrow)
  #   Later, when $data is called, it will produce the desired type automatically
  "VEConnection.Dataframe",
  inherit = VEConnection,
  public = list(
    # methods
    initialize  = function(config) {},  # No initialization needed
    summary     = function() {
      c("Tables:")
      if ( length(self$exportedData)>0 ) names(self$exportedData) else "None written yet"
    },
    # print = {} # base class implementation
    raw         = function() { invisible(self$exportedData) },
    # implementing methods
    nameTable   = function(loc) { makeTableString( Paths=loc$Partition$Paths, Names=loc$Partition$Names, Table=loc$Partition$Table ) },
    createTable = ve.connection.df.createTable,
    writeTable  = ve.connection.df.writeTable,
    readTable   = ve.connection.df.readTable
  ),
  private = list(
    exportedData = list(),              # named list of data.frames (tage is from nameTable)
    Tags = c("data.frame","raw","data") # valid tags used in initialize; first one is default tag
  )
)

#################################
#
###### Implement VEConnection.CSV
#
#################################

ve.connection.csv.init      <- function(config) {}
ve.connection.csv.raw       <- function() {} # override in specific subclasses
ve.connection.csv.createTable <- function() {}
ve.connection.csv.writeTable <- function() {}
ve.connection.csv.readTable <- function() {}

VEConnection.CSV <- R6::R6Class(
  # CSV implementation writes out CSV files using the Partition strategy
  # Depending on how much overlap, could also use this to implement parquet format
  "VEConnection.CSV",
  public = list(
    # methods
    initialize  = ve.connection.csv.init,
    # implementing methods
    nameTable = function(loc) {
      makeTableString(loc$Partition$Paths, loc$Partition$Names, loc$Partition$Table, self$connectTime,
                      tableSep="/", timeSep="_")
    },
    createTable = ve.connection.csv.createTable,
    writeTable  = ve.connection.csv.writeTable,
    readTable   = ve.connection.csv.readTable

    # methods
  ),
  private = list(
    Directory = NULL,    # Default to "OutputDir/Export_<Timestamp>" in initializer
    Tags = c("csv") # later perhaps also parquet
  )
)

#################################
#
###### Implement VEConnection.DBI
#
#################################

ve.connection.dbi.init      <- function(config) {}
ve.connection.dbi.raw       <- function() {} # override in specific subclasses
ve.connection.dbi.createTable <- function() {}
ve.connection.dbi.writeTable <- function() {}
ve.connection.dbi.readTable <- function() {}

VEConnection.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection
  # "folder" partitions are mapped to "name"
  "VEConnection.DBI",
  public = list( 
    # methods
    initialize  = ve.connection.dbi.init,
    # implementing methods
    nameTable = function(loc) {
      makeTableString(loc$Partition$Paths, loc$Partition$Names, loc$Partition$Table, pathSep="_", tableSep="_")
    },
    createTable = ve.connection.dbi.createTable,
    writeTable  = ve.connection.dbi.writeTable,
    readTable   = ve.connection.dbi.readTable
  ),
  private = list(
    Tags = c("sql","sqlite","excel","dbi") # sql and sqlite are probably the same; "dbi" unpacks a more complex connection string
  )
)

##################
# Exporter factory
##################

defaultExporters <- function() {
  exporters <- list(
    # bare minimum default exporters
    # augment with actual connection details in global or model visioneval.cnf
    csv = list(
      Connection = list(
        driver = "csv"
      ),
      Partition = c(Global="name")
    ),
    sql = list(
      Connection = list(
        driver = "dbi"
      ),
      Partition = c(Global="name") # break out Global, otherwise
    ),
    data.frame = list(
      Connection = list(
        driver = "data.frame"
      ),
      Partition = c(Scenario="folder",Global="folder",Year="folder")
    )
  )
  exporters[["sqlite"]] <- exporters[["sql"]]
  exporters[["dbi"]] <- exporters[["sql"]]
  return(exporters)
}

# This is really the connection list...
connectionList <- list(
  csv=VEConnection.CSV,
  dbi=VEConnection.DBI,
  data.frame=VEConnection.Dataframe,
  sqlite=VEConnection.DBI # shortcut - implies default connection
  # parquet=VEConnection.Parquet  # Apache arrow implementation for parquet
  # excel=VEConnection.Excel      # Native Excel implementation rather than DBI
)
# Instantiate an Explorer
# Still to create: parquet format, others...

###########################################################
#
# Functions to create Exporters (open existing, create new)
#
###########################################################

#' Instantiate a VisionEval Exporter using a lookup name to find the Connection
#'
#' @description
#' `makeExporter` creates a VEExporter object to receive data exported from model results or
#' query results.
#'
#' @details
#' With no parameters or unknown `tag` or `driver`, will return a list of available Exporters.
#'
#' Connection: This string is used to initialize the exporter and will be a base directory (for CSV
#' or SQLite or other file-based output format) or a DBI connection string (identifying the
#' database and credentials). See the specific VEExporter documentation for details on the
#' connection string and any other optional parameters.
#' 
#' Partition Scheme: The partitioning can specify"merge" (just leave the field in the table),
#' "folder" where the table in that scenario or group is placed into a hierarchical table (folders
#' are always constructed first and layered as Scenario/Group/Table), or "name" in which case the
#' Scenario, Group or Table is pasted into the table name. If Table is "folder" and no name is
#' specified on an partition level, the actual table will be created by its name in a subfolder also
#' named after the Table. That's mostly useful if Group or Scenario is also identified by name. In
#' connections that do not support folders, "folder" will silently be replaced by "name". Not every
#' exporter format supports "folder" partitions (e.g. SQL, where flat tables are the order of the
#' day).
#' 
#' The partition scheme `c(Scenario="folder",Group="name",Table="folder)` will create a Scenario
#' folder, Table folders within that, and the name the tables within after the Group plus Table
#' (Table will always be included in the Table name, even if "name" is not the partitioning scheme).
#' If "merge" is specified for everything, it is the same as saying
#' `c("Scenario"="merge",Group="merge",Table="name")` - that is, the Table name will always be
#' forced into the name.
#'
#' NOTE: In keeping with usual DBI protocol, the "sql"/"dbi" connections will NOT create a database.
#'  So if you're hoping to export to an Access, MySQL, or PostgreSQL database, you should create the
#'  database first outside of VisionEval and include database location information in the
#'  `connection` parameter per the syntax of the various drivers. Exceptions are if you are creating
#'  Excel or SQLite output, in which case the interface does support creating output files (but
#'  that's because DBI allows it for those). They will be named after the Model associated with the
#'  VEResultsList or the VEQuery plus a Timestamp.
#'
#' NOTE: A typical call would look like `exporter <- newExporter("csv",Model=myModel)`. In practice,
#'   unless you are playing around with alternative connections to some new database, you won't call
#'   this function directly. Instead, you'll just pass the connection identifier (e.g. "csv") plus
#'   any connection or partition information to the VEResultsList or VEQuery $export function. If
#'   you need to make standard changes for production, add an Exporter block to your global or
#'   model-specific visioneval.cnf and those will be used by default.
#'
#' @param config a named list of parameters used to build a VEConnection
#' @return A VEConnection (or derived) object giving access to the VisionEval results in `path`
#' @export
makeVEConnection <- function(Model,config) {
  # Find driver class from config (default is "csv")
  message("DEBUG: driver config")
  print(config)
  driver <- if ( ! "driver" %in% names(config) ) "csv" else config$driver
  driverClass <- connectionList[[driver]]
  if (
    ! inherits( driverClass, "R6ClassGenerator" )
  ) {
    message("Available Drivers:")
    print(names(connectionList))
    stop("Driver type '",driver,"' does not have a VEConnection associated with it",call.=FALSE)
  } else {
    print(connectionList)
    message("Creating Driver for ",driverClass$classname)
  }
  # Create new driver object using "config"
  return ( driverClass$new(config) )
}

# if ( interactive() ) {
# 
#   writeLog <- function(msg,...) stop(msg,call.=FALSE)
# 
#   makeData <- function() {
#     # prepare a data set
#     df <- CO2 # standard R dataset
#     for ( i in 1:length(df) ) {
#       if ( is.factor(df[[i]]) ) df[[i]] <- as.character(df[[i]])
#     }
#     df$Scenario <- sample(paste("Scenario",1:2,sep="_"),nrow(df),replace=TRUE)
# 
#     dfg <- df
#     dfg$Global <- "Global"
#     dfg$Group <- dfg$Global
# 
#     dfy <- dfg
#     dfy$Year <- "2023"
#     dfy$Group <- dfy$Year
#     dfy[["Global"]] <- NULL
# 
#     dfy2 <- dfy
#     dfy2$Year <- "2044"
#     dfy2$Group <- dfy2$Year
#     dfy2[["Global"]] <- NULL
#     
#     return(list(
#       Global=df,
#       Yr2023=dfy,
#       Yr2044=dfy2
#     ) )
#   }
# 
#   testPartition <- function() {
#     # Try several partitions and connections
#     message("Testing VEPartition")
#     dataSets <- makeData() # pseudo-data with various fields for reviewing output
#     part <- VEPartition$new(c(Global="folder",Year="folder",Type="path",Treatment="name"))
#     print(part)
#     message("Testing $location function")
#     loc <- part$location(dataSets$Global,"MyTable") # view location structure
#     print(loc)
#     message("Testing $partition function")
#     message("Fields in Global:")
#     print(names(dataSets$Global))
#     message("\nTesting Partitions")
#     message("Global\n")
#     partGlobal <- part$partition(dataSets$Global,"Global")
#     print(partGlobal)
#     message("Year 2023\n")
#     part2023 <- part$partition(dataSets$Yr2023,"CO2-data")
#     print(part2023)
#     message("Year 2044\n")
#     part2044 <- part$partition(dataSets$Yr2044,"CO2-data")
#     print(part2044)
#     message("Change partition to make Treatment a folder\n")
#     part <- VEPartition$new(c(Global="folder",Year="folder",Type="name",Treatment="folder"))
#     print(part$partition(dataSets$Yr2044,"RetryTreatment"))
#     message("Try with NA data in a partition field\n")
#     dftry <- dataSets$Yr2044$Global <- NA
#     print(part$partition(dataSets$Yr2044,"NAGlobal"))
#   }
# 
# }
