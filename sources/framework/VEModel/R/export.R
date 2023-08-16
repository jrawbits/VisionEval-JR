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
    self$partition <- character(0) # Default is not to partition
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

ve.partition.partition <- function(data,Table) {
  # data is a data.frame possible with partitionable columns
  # Table is the root name of the table for its locator
  # returns a list of table locators (lists) for each of the partitions

  # reduce self$partition to fields present in data (can't partition what is not there)
  use.partition <- self$partition[ which( names(self$partition) %in% names(data) ) ]

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
      Paths = { p <- unlist(locations[loc,pathNames]); names(p)<-pathNames; p[!is.na(p)] },
      Names = { n <- unlist(locations[loc,nameNames]); names(n)<-nameNames; n[!is.na(n)] },
      Table = Table
    )
    
    locFields <- locations[loc,,drop=FALSE]
    locSelection <- rep(TRUE,nrow(data))
    for ( n in locNames ) {
      nValue <- locFields[[n]]
      locSelection <- locSelection & (data[[n]] != nValue) # knock off rows not matching this value
    }
    Range <- which(locSelection) # vector of data rows to include in this partition location

    tableLoc <- list(
      Range = Range,
      Partition = Partition
    )
    # makeTableString defined below (used mostly by VEConnection)
    TableString <- makeTableString( Partition$Path, Partition$Name, Table ) # default format, no timestamp
    TableLocs[[TableString]] <- structure( tableLoc , class="VETableLocator")
  }
  return(TableLocs)
}

ve.partition.location <- function(data,Table) {
  return( list(
    structure(
      list(
        Range = 1:nrow(data),
        Partition = list(
          Paths = character(0),
          Names = character(0),
          Table = Table
        )
      ), class="VETableLocator"
    )
  ) )
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

ve.exporter.init <- function(tag=NULL,connection=NULL,partition=NULL) {
  # We may be able to live without subclassed Exporters
  # connection is a VEConnection object; if NULL, create a default one
  # partition is a VEPartition object; if NULL, use the default one for tag
  #   To force no partition, pass c() or character(0) - Table name will just get written/appended as is
  self$Connection <- VEConnection$new(tag,connection) # connection is a set of parameter structures
  self$Partition <- VEPartition$new(tag,partition)
}

# subclasses will do more with Connection and Partition prior to saving them
# May rewrite "folder" to "name" e.g.
# Folders will get created as data is written

ve.exporter.write <- function(data, Table, Metadata=NULL, Scenario=NULL, Group=NULL, overwrite=FALSE ) {
  # Table must be provided as the root name for the Table being written. Table is used to
  #   select the recipient for the partitioned data. The actual Table name written will be
  #   composed of the results of partitioning data into path elements, name elements, the
  #   Table name and built by the connection object (adding any TablePrefix or TableSuffix).
  Table <- paste0(self$TablePrefix,Table,self$TableSuffix)

  # Manage Metadata (which says what we've written into the Exporter)
  # Optional Scenario and Group are injected into the internal Metadata, keeping track of what
  #   has been exported - we won't re-export Scenario plus Group if they're already in the
  #   Exporter output. If either is not provided, the Table is written (and tracked in the
  #   TableLocations just by its Table name) - if the Table already exists, nothing will happen
  #   unless "overwrite=TRUE" which generally forces Tables to be re-started rather than added to
  #   which is the default.
  # Metadata parameter, if provided, is a data.frame with a Name column and additional
  #   columns to specify things like Units and Description (or possibly other metadata from
  #   the field specification...). If Metadata is not provided, use names(data) as the
  #   Name field in the internal Metadata and just have S/G/T/N in the Metadata. The presumption
  #   is that Scenario and Group, if present, must match Scenario and Group in Metadata.

  # NOTE: if Scenario and Group are not provided, we won't partition; we'll just write Table
  #   with its name (plus any connection adjustments for prefix/suffix/timestamp). That's for
  #   stuff like Query results or Metadata itself (neither of those get written to the connection
  #   Metadata and their "TableLocator" name has no Paths or Names element). What to do if the
  #   the table exists? Probably if the table exists, we should just overwrite it or
  #   fail (respecting the 'overwrite' parameter.

  locations <- if ( ! ( is.null(Scenario) || is.null(Group) ) ) { # no partitioning
    self$Partition$partition(data,Table)
  } else self$Partition$locate(data,Table) # creates a TableLocation 

  # Manage TableList
  # This is a list of tables (after partitioning) with a vector of field names in them so
  # we can make field adjustments when writing a new set of rows into that table.

  # Use the TableLocator string as names on the list. Then add fields (value=NA) to an
  # existing table if the new data has a previously-unseen field, or add fields to the
  # data (value=NA) if it is missing any fields required in the Table. Suggests the
  # VEConnection should have a function to add fields given a list of names (readTable,
  # cbind missing names with NA values, and rewrite the table, dropping it first:
  # dbRemoveTable,dbWriteTable). This is a convenience to prevent having to read back the
  # fields names from the connection. And since we're only doing it as we write, it would
  # be best to take the re-loaded database table and append the rows from the new data
  # directly to it, then rewrite the entirety. It will speed things up to have
  # VEExporter$write flag any new fields that we haven't seen go into the database.

  # Should append the rows from data to the corresponding table
  # Iterate over the locations, use location$Range to subset data, then write to the
  # Connection (if the table is not in TableList, flag Connection to create; otherwise
  # presume appending). This function will conform the names, and there is a parameter
  # on VEConnection$writeTable to pass names of new fields to be inserted into the table
  # (via a read/rewrite, or perhaps eventually some more efficient option); the
  # data.frame can be adjusted here prior to writing.
}

ve.exporter.list <- function(names=NULL, namesOnly=TRUE) {
  # namesOnly is intended for interactive use to list out the tables in the export (as written)
  # if this function is used internally, set namesOnly to False, or if its really deeply
  #   internal, just access self$TableList (the "metadata")
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
      self$TableList[names] # a list of data.frames
    }
  )
}

ve.exporter.print <- function(names=NULL,...) {
  cat("Exporter Connection:",self$Connection)
  print(self$list(names=names,Only=TRUE),...)
}

ve.exporter.data <- function(tables=NULL,format="data.frame") { # tables: a list of table strings
  # Generate a list of requested table data in the requested format (which will be
  # interpreted by self$Connection as it reads out the data).
  tables <- self$list(tables,namesOnly=TRUE)
  lapply( tables, function(t) {
    self$Connection$readTable(t,format=format)
  } ) # generates a list of formatted data from reading each requested table.
}

#' @import dplyr
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
}

ve.exporter.save <- function(filename) { # .VEexport file (.Rdata)
}

VEExporter <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEExporter",
  public = list(
    # public data
    Partition  = NULL,    # default is c(Scenario="merge",Group="merge",Table="name") # probably never want to change Table partition
    Connection = NULL,    # may be named list or character vector or string depending on derived class needs
    TableList  = list(),  # Names in this list are table IDs of created tables, elements of the list are lists of the
                          # fields that exist in the tables (name and type). Need to maintain fields so we can
                          # make existing table and new written data be conformant during $write (typically will be)
    Metadata   = NULL,    # data.frame of S/G/T/N/Metadata passed into $write (TableList prior to partitioning)
                          # Any N row in the Metadata for an S/G/T is a candidate for partitioning

    # methods
    initialize=ve.exporter.init,       # call from subclasses to establish internal connection and partition
    load=ve.exporter.load,             # called from init if load=savedExporter is provided (a file.path)
    save=ve.exporter.save,             # called e.g. by VEResultsList$export when the export is complete

    write=ve.exporter.write,           # write rows onto a table (creating if necessary)
    list=ve.exporter.list,             # list tables that have been created within this exporter
    print=ve.exporter.print,           # print list of table identifiers
    data=ve.exporter.data,             # return list of data.frames corresponding to list of table names (all or some of self$list)
                                       # generates a partitioned (nested) list of data.frames (each level is a "folder")

    # Use Prefix/Suffix to identify different exports
    TablePrefix = NULL,                         # if set, send to Table during $writeTable or $readTable
    TableSuffix = NULL,                         # if set, send to connection$writeTable or readTable

    # implementation methods
    # for the following, "table" is a table navigator (folder, folder, name) as stored in TableList
    createTable = function(data,table) NULL, # Create or re-create a Table from scratch (includes append)
    writeTable  = function(data,table) NULL, # Append data to existing table (restructure as needed)
    readTable   = function(table) NULL       # Read named table into a data.frame
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

ve.connection.init <- function(tag,...) {
}

ve.connection.summary <- function() {
}

ve.connection.print <- function() {
}

ve.connection.raw       <- function() {} # override in specific subclasses
ve.connection.nameTable <- function() {}
ve.connection.createTable <- function() {}
ve.connection.writeTable <- function() {}
ve.connection.readTable <- function() {}


VEConnection <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VECnnection",
  public = list(
    # public data

    # methods (each connecton type will implement its own version of these)
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
#   csv.table.file <- makeTableString( loc$Paths, loc$Names, Loc$Table, self$connectTime, tableSep="/", timeSep="_" )
#   # MyScenario/2019_Household_20230816084102
#
# e.g. SQL/DBI:
#   sql.table <- makeTableString( loc$Paths, loc$Names, Loc$Table, pathSep="_", tableSep="_" )
#   # MyScenario_2019_Household
#
# e.g. data.frame:
#   df.name <- makeTableString( Paths=c(), Names=loc$Names, Table=loc$Table )
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
    Paths <- Paths$Paths
    Names <- Paths$Names
    Table <- Paths$Table   # Table already had prefix/suffix attached
  } else if ( ! is.null(locString) ) {
    locator <- parseTableString(locString)[[1]] # Default parameters
    Paths <- locator$Paths
    Names <- locator$Names
    Table <- locator$Table # Table already had prefix/suffix attached
  } else{ # Prefix/Suffix should include separator if needed (suggest "+" or "!" or "@", depending on connection)
    Table <- paste0( tablePrefix,Table,tableSuffix )
  }
  paths <- paste(Paths,collapse=pathSep)
  names <- paste(Table,Names,collapse=nameSep)
  tableString <- paste(paths,names,sep=tableSep)
  if ( ! is.null(TimeStamp && ! any(is.na(TimeStamp)) ) ) {
    TimeStamp <- TimeStamp[1] # vector not allowed
    if ( isTRUE(TimeStamp) ) TimeStamp <- Sys.time() # system default string time format
    else if ( inherits(TimeStamp,"POSIXct") ) TimeStamp <- format(TimeStamp,"%Y%m%d%H%M")
    else TimeStamp <- as.character(TimeStamp) # may get "sanitized" by VEConnection for use as table name
  } else time <- character(0)
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

ve.connection.df.init      <- function() {}
ve.connection.df.raw       <- function() {} # override in specific subclasses
ve.connection.df.createTable <- function() {}
ve.connection.df.writeTable <- function() {}
ve.connection.df.readTable <- function() {}

VEConnection.Dataframe <- R6::R6Class(
  # Accumulates data.frames in memory
  # Connection may create a different storage class (e.g. data table / tibble / data.frame or even arrow)
  #   Later, when $data is called, it will produce the desired type automatically
  "VEConnection.Dataframe",
  inherit = VEConnection,
  public = list(
    # methods
    initialize  = ve.connection.df.init,
    # implementing methods
    nameTable   = function(loc) { makeTableString( Paths=c(), Names=loc$Names, Table=loc$Table ) },
    createTable = ve.connection.df.createTable,
    writeTable  = ve.connection.df.writeTable,
    readTable   = ve.connection.df.readTable
  ),
  private = list(
    exportedData = list(),              # will be hierarchical named list of data.frames (hierarchy per partitioning)
    Tags = c("data.frame","raw","data") # valid tags used in initialize; first one is default tag
  )
)

#################################
#
###### Implement VEConnection.CSV
#
#################################

ve.connection.csv.init      <- function() {}
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
      makeTableString(loc$Paths, loc$Names, Loc$Table, self$connectTime,
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

ve.connection.dbi.init      <- function() {}
ve.connection.dbi.raw       <- function() {} # override in specific subclasses
ve.connection.dbi.createTable <- function() {}
ve.connection.dbi.writeTable <- function() {}
ve.connection.dbi.readTable <- function() {}

VEConnection.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection
  # "path" partitions are mapped to "name"
  "VEConnection.DBI",
  public = list( 
    # methods
    initialize  = ve.connection.dbi.init,
    # implementing methods
    nameTable = function(loc) {
      makeTableString(loc$Paths, loc$Names, Loc$Table, pathSep="_", tableSep="_")
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

#' Instantiate a VisionEval Exporter using a lookup name to find the Connection
#'
#' @description
#' `openExporter` creates a VEExporter object to receive data exported from model results or
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
#' @param tag A character string tag selecting an exporter from the default list (see
#'   openExporter()). If none is provided, return a list of known exporter names.
#' @param driver A character string or RDBI back end driver (e.g. `RSQListe::SQlite`
#' @param connection A string or named list specific to the exporter type that will open a
#'   connection to the ' output location. See the documentation for each individual exporter.
#' @param file A file name from which to load a previously saved exporter (possibly with results)
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
openExporter <- function(tag="Unknown",driver=tag,file=NULL,connection=NULL,partition=NULL,Model=NULL,load=NULL) {

  # If connection is provided, pass it to VEConnection$new
  if ( is.character(connection) ) {
    connection <- VEConnection$new(load=connection)
  } else if ( ! inherits(connection,"VEConnection") ) {
    # Look up "tag" as a character string
    # If exporter is already a VEExporter, just return it
    if ( inherits(tag,"VEExporter") ) {
      exporter <- tag
    } else if ( is.character(tag) ) {
      # TODO: need to specify this all better:
      #   1. tag can get a default set of connection parameters (file names, directories)
      #   2. tag can be overridden in model or globally to provide connection details
      #   3. exporter can be opened from a saved file
      # Perhaps need to distinguish between exporter type and "configuration tag"
      #   Look for tag in model definitions for connecton - it should be complete
      #   If not found there, look in default connections
      #   "tag" yields both Connection and Partition definitions
      # Unknown tag yields a list of known default types.

      # Check to see if it really is a tag (in VEModel "Exporter" settings )
      if ( ! tag %in% names(connectionList) ) {
        # if not found in Exporter or Defaults, report the list
        # Report list of available exporters
        connections <- c(
          "Available Exporters:",
          names(connectionList)
        )
        if ( tag != "Unknown" ) exporters <- c(paste("Unknown exporter:",tag),exporters)
        for ( e in exporters ) message(paste0(e,"\n"))
        stop("No exporter",call.=FALSE)
      } else {
        connection <- connectionList[[tag]]
      }

    } else { stop("Invalid exporter tag",call.=FALSE) }
  }
    
  # If partition is provided,
  if ( is.character(partition) ) partition <- VEPartition$new(partition)
  

  return( connectionList[[tag]]$new(tag=tag,connection=connection,partition=partition,Model=Model) )
}
