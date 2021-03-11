# Test.R
# Comprehensively test VEModel and related interfaces
# Also provides working examples of the API

# function: create_test_environment
# Expects to run from <pkg>/tests; looks in parent directory
# Creates a temporary directory and initializes it as a ve.runtime
# Sets the working directory to that temporary location

# function: pseudo_package
# 
if ( ! requireNamespace("pkgload",quietly=TRUE) ) {
  stop("Missing required package: 'pkgload'")
}
if ( ! requireNamespace("visioneval",quietly=TRUE) ) {
  stop("Missing required package: 'visioneval'")
}

setup <- function(ve.runtime=NULL) {
  if ( ! is.character(ve.runtime) ) {
    ve.runtime <- Sys.getenv("VE_RUNTIME",unset=NA)
    if ( ! is.na(ve.runtime ) ) {
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- NA
      }
    }
    if ( is.na(ve.runtime) ) {
      ve.runtime <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)[1]
      if ( ! dir.exists(ve.runtime) ) {
        ve.runtime <- normalizePath(tempfile(pattern="runtime",tmpdir="tests"),winslash="/",mustWork=FALSE)
        dir.create(ve.runtime)
      }
    }
  }
  ve.runtime <- normalizePath(ve.runtime,winslash="/",mustWork=TRUE)
  Sys.setenv(VE_RUNTIME=ve.runtime)
  pkgload::load_all()
  ve.env <- VEModel::runtimeEnvironment()
  ve.env$ve.runtime <- ve.runtime; # override default from package load (working directory)
  setwd(ve.env$ve.runtime)

  if ( ! dir.exists("models") ) dir.create("models")
}

takedown <- function() {
  start.dir <- NA
  ve.runtime <- NA
  if ( isNamespaceLoaded("VEModel") ) {
    ve.env <- VEModel::runtimeEnvironment()
    if ( exists("ve.runtime",envir=ve.env,inherits=FALSE) ) {
      ve.runtime <- ve.env$ve.runtime
    }
    if ( exists("start.dir",envir=ve.env,inherits=FALSE) ) {
      start.dir <- ve.env$start.dir
    }
  }
  if ( "package:VEModel" %in% search() ) detach("package:VEModel")
  unloadNamespace("VEModel")
  if ( ! is.na(start.dir) ) setwd(start.dir)
  if ( ! is.na(ve.runtime) ) {
    message("To remove runtime directory:")
    message("unlink('",ve.runtime,"',recursive=TRUE)")
  }
}

rewind <- function() {
  cat("Rewinding...")
  takedown()
  loadhistory(".Rhistory") # get rid of rep("n",a.million.times) and other debugging leftovers
  setup()
}

cleanup <- function() {
  takedown()
  runtimes <- grep("^(tests/)runtime.*",list.dirs("tests"),value=TRUE)
  message("Removing:")
  print(runtimes)
  if ( length(runtimes)>0 && isTRUE(askYesNo("Remove runtimes?")) ) unlink(runtimes,recursive=TRUE)
}

testStep <- function(msg) {
  cat("",paste(msg,collapse="\n"),"",sep="\n")
}

test_model <- function(log="warn") {
  owd <- getwd()
  tryCatch(
    {
      testStep("Clearing previous model, if any")
      if ( dir.exists("models/JRSPM") ) {
        message("Clearing runtime environment")
        unlink("models/JRSPM",recursive=TRUE)
      }
      testStep("Installing model from package")
      rs <- installModel("VERSPM","JRSPM",log=log)
      testStep("Running model")
      rs$run(log=log)
      return(rs)
    },
    error=function(e) { cat(conditionMessage(e),"\n"); takedown(); stop(e) },
    finally=setwd(owd)
  )
  return("Failed to run.")
}

test_results <- function (log="warn") {
  testStep("Opening model and pulling out results and selection...")
  jr <- openModel("JRSPM")
  rs <- jr$results()
  sl <- rs$select() # Get full field list
  # Check error: No results or selection if the model has not been run.

  # Test display units, select speeds, create unit conversion
  testStep("Creating and Writing Display Units...")
  un <- rs$list(details=TRUE)[,c("Group","Table","Name","Units")]
  spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
  spd$DisplayUnits <- "MI/HR"
  cat("Writing display_units.csv into ")
  display_units_file <- file.path(
      jr$modelPath,
      visioneval::getRunParameter("ParamDir",Param_ls=jr$RunParam_ls),
      visioneval::getRunParameter("DisplayUnitsFile",Param_ls=jr$RunParam_ls)
    )
  cat(display_units_file,"\n")
  write.csv(spd,file=display_units_file)

  testStep("Selecting speed fields...")
  sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
  print(sl$fields())

  testStep("Showing currently defined UNITS/DISPLAYUNITS")
  print(rs$units())

  # Clean up the fields to add the geography fields in the Marea Table
  testStep("Adding geography fields to selection...")
  sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
  print(sl$fields())
  print(rs$units())

  testStep("Extracting speed fields using DISPLAY units")
  sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

  testStep("Exporting speed fields using DATASTORE units")
  sl$export(prefix="Datastore",convertUnits=FALSE) # Using DATASTORE units

  # Do some erroneous things to make sure we get suitable errors.
}

test_query <- function(log="warn") {
  # Process the standard query list for the test model
  testStep("Opening test model and caching its results...")
  jr <- openModel("JRSPM")
  rs <- jr$results()

  testStep("Show query directory (may be empty)...")
  jr$query()

  testStep("Create an empty query object and print it...")
  # create a query object
  qry <- jr$query("Test-Query")
  cat("Query valid:",qry$valid(),"\n")
  cat("Print qry$checkResults\n")
  print(qry$checkResults)
  cat("Print query\n")
  print(qry)

  testStep("Add a query formulated as a list element...")
  spec <- list(
    Name = "UrbanHhDvmt",
    Summarize = list(
      Expr = "sum(UrbanHhDvmt)",
      Units = c(
        UrbanHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By = "Marea",
      Table = "Marea"
    ),
    Units = "Miles per day",
    Description = "Daily vehicle miles traveled by households residing in the urban area"
  )
  qry$add(spec)
  qry$print()

  testStep("Names of specifications in added query...")
  print(qry$names())    # List names of QuerySpecifications in order
  testStep("Print function for added query...")
  print(class(qry))
  print(qry)

  testStep("Construct bare query...")
  spec <- VEQuerySpec$new()
  cat("Bare query is valid: ")
  print(spec$valid())   # Should return FALSE

  testStep("Add spec details to bare query using $update...")
  spec$update(
    Name = "UrbanHhDvmt_MixNbrhd",
    Description = "Daily vehicle miles traveled by households residing in mixed use in the urban area",
    Units = "Miles per day", # Purely advisory...
    Summarize = list(
      Expr = "sum(Dvmt[LocType == 'Urban' & IsUrbanMixNbrhd == '1'])",
      Units = c(
        Dvmt = "MI/DAY",        # Will force to this unit, with conversion if needed
        LocType = "",           # Leaving it blank says use Datastore default
        IsUrbanMixNbrhd = "",
        Marea = ""
      ),
      By = "Marea",
      Table = "Household"
    )
  )
  print(spec)
  return("Done testing")

  testStep("Add updated spec to Query...")
  qry$add(spec)
  print(qry)

  testStep("Print query with details...")
  print(qry,details=TRUE)

  testStep("Copy the updated query spec and adjust the copy with update...")

  testStep("Complete the initial query by adding one more 'Summarize'...")

  testStep("Add a 'Function' query specification...")

  testStep("Save the query and see it in the directory...")

  testStep("Open the query in a different object from the file...")

  testStep("Add some more queries using the add and update functions...")

  testStep("Run the query on the model...")
  qry$run(jr)
  testStep("Run the query on the results...")
  qry$run(rs)

  # Throw some additional specific broken queries at it to see if errors are correct.
}

rewind()
