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
#       Name : a character vector of path elements describing the Table in the connection (appended to Table name)
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
    self$partition <- c(Scenario="folder",Group="name")
  } else {
    # remove "none" or "merge" elements and keep the rest
    self$partition <- partition[ grep("(merge)|(none)",partition,invert=TRUE) ]
    # collapse alternate names to standard names
    self$partition <- gsub("path","folder",self$partition)
    # get rid of (and warn about) any partition actions that are not defined
    badPartitions <- ! self$partition %in% c("name","folder") # eventually allow "database"
    if ( any(badPartitions) ) {
      writeLog(paste("Ignoring bad partitions:",paste(self$partitions[badPartitions],collapse=",")),Level="warn")
      self$partition <- self$partition[ ! badPartitions]
    }
  }
}

ve.partition.partition <- function(data,Table,Scenario=NULL,Group=NULL,Metadata=NULL) {
  # data is a data.frame possible with partitionable columns
  # Metadata should describe "data"
  # if Metadata is not provided either explicitly or as an attribute of data, then
  #   no metadata will be furnished for the table and writeMetadata will not add an entry for it

  if ( missing(Metadata) || is.null(Metadata) ) {
    Metadata <- attr(data,"Metadata")
  } # Metadata may still be NULL if no attribute

  # reduce self$partition to fields present in data (can't partition what is not there)
  use.partition <- self$partition[ which( names(self$partition) %in% names(data) ]

  # identify unique values in each of the partition columns
  partition.values <- lapply( names(use.partition), function(p) unique(data[[p]]) )
  names(partition.values) <- names(use.partition)
  locations <- expand.grid(partition.values) # yields a data.frame where columns are the partition fields and rows are the values

  # Build the partitions based on the locations...
  TableLocs <- list()
  locNames <- names(locations)
  pathNames <- names(use.partition)[use.partition=="path"]
  nameNames <- names(use.partition)[use.partition=="name"]

  for ( loc in 1:nrow(locations) ) {
    Partition <- list(
      # NOTE: dropping any partition field value that is NA (as if, just for this partition,
      # it's not there). Good partition descriptors won't use fields that might have any NA's.
      Path = { p <- unlist(locations[loc,pathNames]); names(p)<-pathNames; p[!is.na(p)] }
      Name = { n <- unlist(locations[loc,nameNames]); names(n)<-nameNames; n[!is.na(n)] }
      Table = Table,
    )
    TableString <- paste(
      paste(
        paste(Partition$Path,collapse="/"), Table, sep="/"
      ),
      sep="_",
      paste(Partition$Name,collapse="_")
    )
    
    locFields <- locations[loc,,drop=FALSE]
    locSelection <- rep(TRUE,nrow(data))
    for ( n in locNames ) {
      nValue <- locFields[[n]]
      locSelection <- locSelection & (data[[n]] != nValue) # knock off rows not matching this value
    }
    Range <- which(locSelection) # vector of data rows to include in this partition location
                         
    TableLocs[[TableString]] <- list(
      Range = Range,
      Partition = Partition,
      TableString = TableString,
      Metadata = Metadata
    )
  }
  return(TableLocs)
}

#' @export
VEPartition <- R6::R6Class(
  "VEPartition",
  public = list(
    # public data
    Partition = character(0)           # Describes fields to be partitioned into different output tables

    # methods
    initialize=ve.partition.init,        # initialize internal partition
    partition=ve.partition.partition,   # partition a data.frame into output tables
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
#    writeMetadata ## helper to write self$metadata()
#    list        ## log of tables that have been built during export
#    print       ## just cat the list
#    data        ## convert table locators (parameters, default=all) to flat list of data.frames

ve.export.print <- function(...) {
  cat(
  for ( n in self$list() ) { # just the locator names

ve.exporter.init <- function(tag=NULL,connection=NULL,partition=NULL) {
  # We may be able to live without subclassed Exporters
  # connection is a VEConnection object; if NULL, create a default one
  # partition is a VEPartition object; if NULL, use the default one for tag
  #   To force no partition, pass c() or character(0) - Table name will just get written/appended as is
  self$Connection <- VEConnection$new(tag,connection) # connection is a set of parameter structures
  self$Partition <- VEPartition$new(tag,partition)
}

ve.exporter.connection <- function(connection) {
}

ve.exporter.partition <- function(partition) {
}

# subclasses will do more with Connection and Partition prior to saving them
# May rewrite "folder" to "name" e.g.
# Folders will get created as data is written

ve.exporter.write <- function(data, Table, Metadata=NULL, Scenario=NULL, Group=NULL ) {
  # The ... parameters (which must be named) are used to control the partitioning
  # Optional Scenario and Group are injected into the internal metadata. It is recommended (to avoid
  #   table collisions like Marea) to specify the VEPartition using Global and Year rather than
  #   relying on Group. Scenario and Group, if present, are added as additional columns to Table
  #   if they are not already present. They will be run through the Partition to figure out
  #   what to do with them. Group can be compared internally to "Global" partition (and injected as
  #   a field if not present in data) and if it's not Global, subject it to the "Year" partition
  #   (and inject a "Year" field into data.
  # Metadata parameter, if provided, is a data.frame with a Name column and additional
  #   columns to specify things like Units and Description (or possibly other metadata from
  #   the field specification...). If Metadata is not provided, use row.names(data) as the
  #   Name field in the internal Metadata, and perhaps also push that into the written Table.
  # Table must be provided as the root name for the Table being written. Table is used
  #   to select the recipient for the partitioned data. The actual Table name written
  #   will be composed of a database element, path elements, name elements, the Table name
  #   and built by the connection object (adding any TablePrefix or TableSuffix elements
  #   configured in the connection.
  # NOTE: if data.partition is empty, we won't partition; we'll just write Table
  #   with its name (plus any connection adjustments for prefix/suffix/timestamp
  # TOTO: add the partitioning variables to the TableList we're building internally
  #   We may inject columns if there are new partition elements (set to NA if not used
  #   for the table
  # TODO: Append the data.partition to the TableList for metadata.creation
  # TODO: finish this implementation
  # So we can just throw data and the data.partition to VEPartition to get a list of
  #   "bites" that will get appended to the corresponding partition table (creating the
  #   table if it does not already exist.
  # Partitioner returns list of folder and name components (and perhaps the Database when
  #   we get to implementing different databases as partitions). Table prefix handed in
  #   through the connection gets jammed on the front of the table name (expansion of
  #   the table name prefix).
  # We don't have to submit any partition information, in which case the table is written
  #   to the connection root 
  # Should append the rows from data to the corresponding table
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

ve.exporter.metadata <- function() {
  # TODO: write a single metadata table that includes lists of what got written to which table
  # Essentially a flat dump of self$TableList
  self$write(self$list(namesOnly=FALSE),Table=
}

ve.exporter.list <- function(namesOnly=TRUE) {
  # namesOnly is intended for interactive use to list out the tables in the export (as written)
  # if this function is used internally, set namesOnly to False, or if its really deeply
  #   internal, just access self$TableList (the "metadata")
  if ( isTRUE(namesOnly) ) names(self$TableList) else self$TableList
}

ve.exporter.print <- function(...) {
  cat("Exporter Connection:",self$Connection)
  print(self$list(namesOnly=TRUE),...)
}

ve.exporter.data <- function(tables=NULL) { # tables: a nested named list of table identifiers to retrieve
  # Could look at the partitioning scheme to interpret "tables"
  # Generally won't pass anything, but we could as long as tables is a proper subset of self$TableList
  # Iterate down through tables and build a parallel list of nested data.frames
  # Each data frame is returned via readTable which reaches through the partitions (what does a
  # "folder" mean in terms of locating the table? - the individual exporters will need to handle that).
}

ve.exporter.metadata <- function() {
  # TODO: reformat self$TableList with a single list of tables that have been written
  #   to the data.store. Should list S/G/T/N plus TableName (one row per N), and include
  #   the Units actually written plus the N description. Might be easier to expand G to
  #   "Global"/Year or find some way to conflate those even though we want to partition
  #   them separately. Perhaps push the Global vs year down into the partition (by examining
  #   the field 
# helper function to write the Metadata
ve.exporter.writeMetadata(data, Table="Metadata") {
  self$write( self$metadata(), Table=Table)
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
    initialize=ve.exporter.init        # call from subclasses to establish internal connection and partition
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

#############################
#
###### Implement VEConnection
#
#############################

# The VEConnection is initialized from a "tag" and named list of onnection parameters that are
# interpreted as needed by each driver. Default settings for all of them will yield a valid
# exporter for:
#    data.frames (option to use data.tables, tibbles or something else) default is hierarchical names lists of data.frames
#    csv (option perahsp to use parquet or alternte csv driver) default is csv in subdirectory of OutputDir named after model
#    dbi (options to pick a specific driver) default is SQLite into a file in OutputDir named after model

#' @export
VEConnection <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VECnnection",
  public = list(
    # public data
    # TBD - probably none for base class
    # but could include TablePrefix, TableSuffix, addTimestamp flag

    # methods (each connecton type will implement its own version of these
    initialize=ve.connection.init,              # call from subclasses to establish internal connection and partition
    summary=ve.connection.summary,              # simplified text representation of the connection details
                                                # format and content depends on the connection class
    print=ve.connection.print,                  # cat's the summary
    raw=ve.connection.raw,                      # returns CSV folder or list of data.frames or DBI connection
                                                # specific value is class-specific and is enough to re-open and
                                                # review what is in the connection (e.g. to list actually written
                                                # tables)

    nameTable   = function(TableLoc) NULL,      # Turn the TableLoc into an actual table name suitable for creating/writing/reading
                                                # Exporter will add this name to the TableLoc Metadata
    createTable = function(data,table) NULL,    # Create or re-create a Table from scratch (includes append)
    writeTable  = function(data,table) NULL,    # Append data to existing table (restructure as needed)
    readTable   = function(table) NULL          # Read named table into a data.frame
                                                # Throughout, 'table' may be a TableLoc or a TableLocator string
  )
)

# TODO: The remainder of these are VEConnections

#######################################
#
###### Implement VEConnection.Dataframe
#
#######################################

#' @export
VEConnection.Dataframe <- R6::R6Class(
  # Accumulates data.frames in memory
  # Connection may create a different storage class (e.g. data table / tibble / data.frame or even arrow)
  #   Later, when $data is called, it will produce the desired type automatically
  "VEConnection.Dataframe",
  inherit = VEConnection,
  public = list(
    # methods
    # implementing methods
    nameTable   = ve.connection.df.nameTable,
    createTable = ve.connection.df.createTable,
    writeTable  = ve.connection.df.writeTable,
    readTable   = ve.connection.df.readTable,
  ),
  private = list(
    exportedData = list()   # will be hierarchical named list of data.frames (hierarchy per partitioning)
  ),
  private = list(
    Tags = c("data.frame","raw","data") # valid tags used in initialize; first one is default tag
  )
)

#################################
#
###### Implement VEConnection.CSV
#
#################################

#' @export
VEConnection.CSV <- R6::R6Class(
  # CSV implementation writes out CSV files using the Partition strategy
  # Depending on how much overlap, could also use this to implement parquet format
  "VEConnection.CSV",
  public = list(
    # implementing methods
    nameTable   = ve.connection.csv.nameTable,
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

#' @export
VEConnection.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection using the
  # Partition strategy: default "folder" partition is mapped to "name"
  "VEConnection.DBI",
  public = list( 
    # implementing methods
    nameTable   = ve.connection.dbi.nameTable,
    createTable = ve.connection.dbi.createTable,
    writeTable  = ve.connection.dbi.writeTable,
    readTable   = ve.connection.dbi.readTable,
  ),
  private = list(
    Tags = c("sql","sqlite","excel","dbi") # sql and sqlite are probably the same; "dbi" unpacks a more complex connection string
  )
)


##################
# Exporter factory
##################

# This is really the connection list...
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
