# export.R
#' @include environment.R
NULL

# Documentation for VEExporter
#' VEExporter class for exporting tabular data from model or query results
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEEXporter
NULL

self=private=NULL # To avoid global variable warnings

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
#    writeMetadata ## dump accumulated metadata to a specific output table
#    list        ## log of tables that have been built during export
#    print       ## just cat the list
#    data        ## convert selected table names into the corresponding data.frame

###########################
#
###### Implement VEExporter
#
###########################

ve.exporter.connection <- function(connection) {
  self$Connection <- connection
}

ve.exporter.partition <- function(partition) {
  self$Partition <- partition
}

# subclasses will do more with Connection and Partition prior to saving them
# May rewrite "folder" to "name" e.g.
# Folders will get created as data is written

ve.exporter.write <- function(data,Scenario=NULL,Group=NULL,Table=NULL) {
  # Start by using S/G/T and the partition to create the table identifer
  # Then see if the table is in the list of created tables (self$Tables, which is a hierarchical
  #   named list with "folders" containing "folders" containing "tables")
  # If it is not in the list, create it and add the table structure to the list
  # If it is in the list, check conforming columns (the Table list also establishes the order,
  #   for formats like CSV where order matters)
  # If columns conform, do a writeTable (append rows of data to the table)
  # If columns do not conform, do a readTable on the existing data, add the missing columns,
  #   append the "data" sent to this function, then (re)createTable with the joined data
  # Return the table name written
}

ve.exporter.list <- function(namesOnly=TRUE) {
  if ( isTRUE(namesOnly) ) names(self$TableList) else self$TableList
}

ve.exporter.print <- function() print(self$list(namesOnly=TRUE))

ve.exporter.data <- function(tables=NULL) { # tables: a nested named list of table identifiers to retrieve
  # Could look at the partitioning scheme to interpret "tables"
  # Generally won't pass anything, but we could as long as tables is a proper subset of self$TableList
  # Iterate down through tables and build a parallel list of nested data.frames
  # Each data frame is returned via readTable which reaches through the partitions (what does a
  # "folder" mean in terms of locating the table? - the individual exporters will need to handle that).
}

#' @export
VEExporter <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEExporter",
  public = list(
    # public data
    Locked       = FALSE, # Locked is true if the exporter is loaded from a file and the connection has data in it
                          # We can override that behavior by setting "reset=TRUE" in the 
    Partition    = NULL,  # default is c(Scenario="merge",Group="merge",Table="name") # probably never want to change Table partition
    Connection   = NULL,  # may be named list or character vector or string depending on derived class needs
    TableList    = list() # Names in this list are table IDs of created tables, elements of the list are lists of the
                          # fields that exist in the tables (name and type). Should we keep this list in "partition
                          # format"? Would make it easier to generate $data as a hierarchy.

    # methods
    # No "initialize" function needed for super-class
    connection=ve.exporter.connection, # stash connection scheme (subclasses will do other things with it)
    partition=ve.exporter.partition,   # change partition scheme (subclasses will do other things with it)

    write=ve.exporter.write,           # write rows onto a table (creating if necessary)
    list=ve.exporter.list,             # list tables that have been created within this exporter
    print=ve.exporter.print,           # print list of table identifiers
    data=ve.exporter.data,             # return list of data.frames corresponding to list of table names (all or some of self$list)
                                       # generates a partitioned (nested) list of data.frames (each level is a "folder"

    # implementation methods
    # for the following, "table" is a table navigator (folder, folder, name) as stored in TableList
    createTable = function(data,table) NULL, # Create or re-create a Table from scratch (includes append)
    writeTable  = function(data,table) NULL, # Append data to existing table (restructure as needed)
    readTable   = function(table) NULL       # Read named table into a data.frame
  )
)

#####################################
#
###### Implement VEExporter.Dataframe
#
#####################################

#' @export
VEExporter.Dataframe <- R6::R6Class(
  # Accumulates data.frames in memory
  # Connection may create a different storage class (e.g. data table / tibble / data.frame or even arrow)
  #   Later, when $data is called, it will produce the desired type automatically
  "VEExporter.Dataframe",
  inherit = VEExporter,
  public = list(
    # methods
    # implementing methods
    createTable = ve.exporter.csv.createTable,
    writeTable  = ve.exporter.csv.writeTable,
    readTable   = ve.exporter.csv.readTable,

    # override functions...
    initialize=ve.exporter.dataframe.init,     # initialize exporter
    list=ve.exporter.dataframe.list,           # list tables existing within this exporter
    print=ve.exporter.dataframe.print,         # print list of tables
    data=ve.exporter.dataframe.data            # return list of data.frames corresponding to list of table names

  ),
  private = list(
    exportedData = list()   # will be hierarchical named list of data.frames (hierarchy per partitioning)
  ),
  private = list(
    Tags = c("data.frame","raw","data") # valid tags used in initialize; first one is default tag
  )
)

###############################
#
###### Implement VEExporter.CSV
#
###############################

#' @export
VEExporter.CSV <- R6::R6Class(
  # CSV implementation writes out CSV files using the Partition strategy
  # Depending on how much overlap, could also use this to implement parquet format
  "VEExporter.CSV",
  public = list(
    # implementing methods
    createTable = ve.exporter.csv.createTable,
    writeTable  = ve.exporter.csv.writeTable,
    readTable   = ve.exporter.csv.readTable,

    # methods
    initialize = ve.exporter.csv.initialize, # Set up output directory and partitioning; use super$initialize for partition
    connectionn = ve.exporter.csv.connection,# Set up output directory
    list=ve.exporter.csv.list,               # list tables existing within this exporter
    print=ve.exporter.csv.print,             # print list of tables
    data=ve.exporter.csv.data                # return list of data.frames corresponding to list of table names
  ),
  private = list(
    Directory = NULL,    # Default to "OutputDir/Export_<Timestamp>" in initializer
    Tags = c("csv") # later perhaps also parquet
  )
)

###############################
#
###### Implement VEExporter.DBI
#
###############################

#' @export
VEExporter.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection using the
  # Partition strategy: default "folder" partition is mapped to "name"
  "VEExporter.DBI",
  public = list( 
    # implementing methods
    createTable = ve.exporter.csv.createTable,
    writeTable  = ve.exporter.csv.writeTable,
    readTable   = ve.exporter.csv.readTable,

    # methods
    initialize = ve.exporter.dbi.initialize, # Set up output directory and partitioning; use super$initialize
                                             # DBI has a "Driver" parameter that will interpret the Connection parameter...
                                             # Driver defaults to SQLite; Connection is a list of arguments passed to
                                             # DBI::dbConnect via do.call.
    connection = ve.exporter.dbi.connection, # Update connection, prior to first write operation
    data=ve.exporter.dbi.data                # return list of data.frames corresponding to list of table names

  ),
  private = list(
    Connection = NULL,    # Default Database is "OutputDir/Results_<TimeStamp>.sqlite3
    Tags = c("sql","sqlite","excel","dbi") # sql and sqlite are probably the same; "dbi" unpacks a more complex connection string
  )
)


##################
# Exporter factory
##################

exporterList <- list(
  csv=VEExporter.CSV,
  dbi=VEExporter.DBI,
  data.frame=VEExporter.Dataframe,
  sqlite=VEExporter.DBI # shortcut - implies default connection
  # parquet=VEExporter.Parquet  # Apache arrow implementation for parquet
  # excel=VEExporter.Excel      # Native Excel implementation rather than DBI
)
# Instantiate one of these using exporterList[["csv"]]$new
# Still to create: parquet format, others...

#' Instantiate a VisionEval Exporter (VEExporter species) using a lookup name
#'
#' @description
#' `openExporter` creates a VEExporter object to receive data exported from model results or
#' query results.
#'
#' @details
#' With no parameters or unknown `exporter`, will return a list of available Exporters.
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
#' NOTE: Any connection or partition provided when calling openExporter will override anything
#'   defined in the global or Model environment.
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
#' @param exporter A character string selecting an exporter from the known list (see
#'   openExporter()). If none is provided, return a list of known exporter names.
#' @param connection A string or named list specific to the exporter type that will open a
#'   connection to the ' output location. See the documentation for each individual exporter.
#' @param partition A named character string specifying partition strategy for items being written.
#'   default partition is c(Scenario=folder,Group=folder,Table=name). See Details.
#' @param Model A VEModel object; if present, unfurnished settings for the desired exporter will be
#'   sought in the model configuration. Default is no Model, in which case default or global
#'   parameters will be used, if they are present, and otherwise connection defaults.
#' @param load A character vector identifying a .VEexport file (if relative, must have Model and
#'   file is sought in Model OutputDir). The file will contain the connection and partition
#'   information. The resulting exporter is read-only, but can be copied to a different connection
#'   with a possibly different partitioning scheme.
#' @return A VEResults object giving access to the VisionEval results in `path`
#' @export
openExporter <- function(exporter="Unknown",connection=NULL,partition=NULL,Model=NULL,load=NULL) {
  # Look up "exporter" as a character string
  # If exporter is already a VEExporter, just return it
  if ( inherits(exporter,"VEExporter") ) {
    exporter$connection(connection,Model) # reconfigure if we want to make other changes
    exporter$partition(partition,Model)
    return(exporter)
  }
  if ( ! exporter %in% names(exporterList) ) {
    # Report list of available exporters
    exporters <- c(
      "Available Exporters:",
      names(exporterList)
    )
    if ( exporter != "Unknown" ) exporters <- c(paste("Unknown exporter:",exporter),exporters)
    for ( e in exporters ) message(paste0(e,"\n"))
    stop("No exporter",call.=FALSE)
  }

  # If connection or partition is not supplied, the exporter class initialization will look up the
  #   defaults using config (tag in the "Exporter" block from Model and system and default visioneval.cnf in
  #   that order).See the docs for the specific exporter types.
  return( exporterList[[exporter]]$new(connection=connection,partition=partition,config=list(Model=Model,Tag=exporter)) )
}
