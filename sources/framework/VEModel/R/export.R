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
#    initialize  ## Use the "exporter" parameter to find and create
#                ## partition vector with name of variable and a strategy from among
#                ## c("none","folder","name")
#                ## exporter may not support "folder" (e.g. DBI/SQL in which case we
#                ## should warn and fall back to "name")
#                ## initialize should have a low-level "connection" (folder for
#                ##   csv, database for DBI/SQL, file for SQLite)
#                ## Do we want to have changed connections/database be a partition method?
#    partition   ## Change the exporter partitioning strategy (can't call if anything has
#                ##   already been written) e.g. c(Group=name,Scenario=name,Table=name)
#                ##   CSV Default is folder for Scenario, name for Table, and merge for Group/Scenario
#                ##   SQL Default is merge for Scenario, name for Table, and merge for Group
#                ## Doing folder for SQL might one day allow us to write to a different DBI
#                ##   connection
#    write       ## a data.frame plus partition flags (perhaps passed as ...)
#                ## default for unsupplied parameters is to ignore them (treating them as 'merge"
#                ## and not checking if they are really there)
#    list        ## log of tables that have been built during export
#    print       ## just cat the list
#    data        ## convert selected table names into the corresponding data.frame

###########################
#
###### Implement VEExporter
#
###########################

#' @export
VEExporter <- R6::R6Class(
  # Default class does nothing
  # Calling its $data function will list available exporters
  "VEExporter",
  public = list(
    # public data
    Partition    = NULL, # default is c(Scenario="folder",Group="folder",Table="name") # probably never want to change Table partition

    # methods
    initialize=ve.exporter.init,     # Set up initial partition
    partition=ve.exporter.partition  # change partition scheme
    write=ve.exporter.write,
    list=ve.exporter.list,           # list tables existing within this exporter
    print=ve.exporter.print,         # print list of tables
    data=ve.exporter.data            # return list of data.frames corresponding to list of table names
  )
)

#' @export
VEExporter <- R6::R6Class(
  # Default class just accumulates data.frames in memory
  "VEExporter.Dataframe",
  inherit = VEExporter,
  public = list(
    # public data
    createTable  = ve.exporter.dataframe.create, # adds data.frame to internal list using partition scheme
    writeTable   = ve.exporter.dataframe.write,  # appends data.frame to existing internal list element using partition scheme

    # methods
    # can use base class write
    list=ve.exporter.dataframe.list,           # list tables existing within this exporter
    print=ve.exporter.dataframe.print,         # print list of tables
    data=ve.exporter.dataframe.data            # return list of data.frames corresponding to list of table names
  ),
  private = list(
    exportedData = list()   # will be hierarchical named list of data.frames (hierarchy per partitioning)
  )
)

#' @export
VEExporter.CSV <- R6::R6Class(
  # CSV implementation writes out CSV files using the Partition strategy
  "VEExporter.CSV",
  public = list(
    # implementing methods
    createTable = ve.exporter.csv.createTable,
    writeTable  = ve.exporter.csv.writeTable,

    # methods
    initialize = ve.exporter.csv.initialize, # Set up output directory and partitioning; use super$initialize for partition
    list=ve.exporter.csv.list,               # list tables existing within this exporter
    print=ve.exporter.csv.print,             # print list of tables
    data=ve.exporter.csv.data                # return list of data.frames corresponding to list of table names
  ),
  private = list(
    Directory = NULL,    # Default to "OutputDir/Export_<Timestamp>" in initializer
  )
)

#' @export
VEExporter.DBI <- R6::R6Class(
  # DBI implementation writes out to DBI connection using the
  # Partition strategy: default "folder" partition is mapped to "name"
  "VEExporter.DBI",
  public = list( 
    # implementing methods
    createTable = ve.exporter.dbi.createTable,
    writeTable  = ve.exporter.dbi.writeTable,

    # methods
    initialize = ve.exporter.dbi.initialize, # Set up output directory and partitioning; use super$initialize
                                             # DBI has a "Driver" parameter that will interpret the Connection parameter...
                                             # Driver defaults to SQLite; Connection is a list of arguments passed to
                                             # DBI::dbConnect via do.call.
    print=ve.exporter.dbi.print,             # print list of tables
    data=ve.exporter.dbi.data                # return list of data.frames corresponding to list of table names
  ),
  private = list(
    Connection = NULL,    # Default Database is "OutputDir/Results_<TimeStamp>.sqlite3
  )
)


##################
# Exporter factory
##################

exporterList <- c(
  csv=VEExporter.CSV,
  dbi=VEExporter.DBI,
  data.frame=VEExporter.Dataframe,
  sqlite=VEExporter.DBI # shortcut
)
# Still to create: parquet format, others...

#' Instantiate a VisionEval Exporter (VEExporter species) using a lookup name
#'
#' @description
#' `newExporter` creates a VEExporter object to receive data exported from model results or
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
#' specified on an partition level, the actual table will just be called "Table". The partition
#' scheme c(Scenario="folder",Group="name",Table="folder) will create a Scenario folder, Table
#' folders within that, and the name the tables within after the Group. Not every exporter format
#' supports "folder" partitions (e.g. SQL, where flat tables are the order of the day). If "merge"
#' is specified for everything, it is the same as saying
#' c("Scenario"="merge",Group="merge",Table="name") - that is, there will always be one "name"
#' partition.
#'
#' @param exporter A character string selecting an exporter from the known list (see newExporter())
#' @param connection A string specific to the exporter type that will open a connection to the
#' output location
#' @param partition A named character string specifying partition strategy for items being written.
#'   default partition is c(Scenario=folder,Group=folder,Table=name). See Details.
#' @param ... Named exporter-specific parameters passed on to the exporter's initialize function.
#'   See the documentation for individual exporter types.
#' @return A VEResults object giving access to the VisionEval results in `path`
#' @export
newExporter <- function(exporter="",driver=NULL,connection=NULL,partition=NULL,...) {
  # Look up "exporter" as a character string
  # If exporter is already a VEExporter, just return it
  if ( inherits(exporter,"VEExporter") ) {
    exporter$partition(partition)
    return(exporter)
  }
  if ( missing(exporter) ) exporter <- "Unknown"
  if ( ! exporter %in% names(exporterList) ) ) {
    # Pretend exporter whose data is a list of available exporters
    exporters <- c(
      "Available Exporters:",
      names(exporterList)
    )
    if ( exporter != "Unknown" ) exporters <- c(paste("Unknown exporter:",exporter),exporters)
    for ( e in exporters ) message(paste0(e,"\n")
    stop("No exporter",call.=FALSE)
  }
  exporterClass <- exporterList[exporter] # R6 class name lookup
  return( exporterClass$new(driver=driver,connection=connection,partition=partition,...) ) # TODO: Check syntax/correctness
}