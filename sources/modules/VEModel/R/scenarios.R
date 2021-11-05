# scenarios.R
#' @include environment.R
#' @include models.R
NULL

# Documentation for VEModelScenarios
#' VEModelScenarios class for managing scenarios within a model
#'
#' Documentation yet to come for various functions (plus some
#' implementation).
#'
#' @name VEModelScenarios
NULL

self=private=NULL

# Build a scenario management object
ve.scenario.init <- function( baseModel=NULL, fromFile=FALSE ) {
  self$baseModel <- baseModel
  self$scenarioDir <- self$baseModel$setting("ScenarioDir")
  self$scenarioPath <- normalizePath(file.path(self$baseModel$modelPath,self$scenarioDir))
  if ( dir.exists(self$scenarioPath) ) {
    self$load(fromFile=fromFile)
  }
}

# Load scenario's visioneval.cnf (constructing self$RunParam_ls and self$loadParam_ls)
ve.scenario.load <- function(fromFile=FALSE) {
  if ( ! fromFile && ! is.null(self$modelStages) ) return(NULL) # do not reload model stages

  # Reload scenario configuration file and then build the scenario stages
  self$loadedParam_ls <- if ( dir.exists(self$scenarioPath) ) {
    visioneval::loadConfiguration(ParamDir=self$scenarioPath, mustWork=FALSE)
  } else list()

  # Layer in the base model run parameters as basis for scenarios
  # Must ignore keys that we may read for Scenarios that are distinct from base model
  scenarioParams <- c("ModelStates","Categories","Scenarios")
  baseParam_ls <- baseModel$RunParam_ls[ ! names(baseModel$RunParam_ls) %in% scenarioParams ]

  # Now add the loaded scenario parameters (scenarioParams, but possibly others)
  modelParam_ls <- visioneval::mergeParameters(self$baseModel$RunParam_ls,self$loadedParam_ls)

  # Load different types of scenarios and build ModelStages for them
  writeLog("Loading model Scenarios",Level="info")
  self$modelStages <- NULL
  # Explicit ModelStages are defined first (available to use as StartFrom for Category stages)
  # These will use the top-level StartFrom for the scenarios
  if ( "ModelStages" %in% names(modelParam_ls) ) {
    writeLog(paste("Parsing explicit Scenarios from",self$scenarioDir),Level="info")
    # TODO: overlay self$RunParam_ls on self$baseModel$RunParam_ls
    modelStages <- lapply(names(modelParam_ls$ModelStages), # Use pre-defined structures
      # At a minimum, must provide Dir or Config
      function(stage) {
        obj <- modelParam_ls$ModelStages[[stage]] # Get the stageParam_ls structure
        writeLog(paste("Model Stage:",stage),Level="info")
        VEModelStage$new(
          Name=stage,
          Model=self$baseModel,
          ScenarioDir=self$scenarioPath,
          modelParam_ls=modelParam_ls,
          StageIsScenario=TRUE,
          stageParam_ls=obj
        )
      }
    )
  } else modelStages <- list()
  self$modelStages <- modelStages

  if ( all("Categories","Scenarios") %in% names(modelParam_ls) ) {
    writeLog(paste("Parsing Category combination Scenarios from",self$ScenarioDir),Level="info")

    # Get CategorySettings if any and overlay on self$RunParam_ls for use in building these stages
    # Only present to support setting a StartFrom from among the explicit ModelStages in the
    # ScenarioDir - that stage will always use a StartFrom from the BaseModel (or have no start
    # from). All tahe Category/Scenario stages will StartFrom the CategorySetting/StartFrom
    if ( "StartFrom" %in% names(modelParam_ls$Categories) ) {
      categoryParam_ls <- visioneval::addParameterSource(list(StartFrom=modelParam_ls$Categories$StartFrom),Source="Category Setting")
      modelParam_ls <- visioneval::mergeParameters(modelParam_ls,categoryParam_ls)
      # Set the StartFrom (if any) to use for the individual categories
      # Supports starting from an explicit ModelStage defined in ScenarioDir
    }

    # TODO: if a required Category/Scenario input directory is missing, verify the input
    # directories report scenarios or levels that are not present, or are missing files,
    # or that have extra files. We'll still set up whatever model stages we can (and allow them to
    # run) but we will note that we tried to define additional scenarios but were missing input
    # files (or, for explicit ModelStages, the indicated scenario parameters were not runnable;
    # No file checking is done for explicit ModelStages.). That could come up as a warning when we
    # do scenarios$stages().

    # Once we've loaded a model with such "defective" scenarios, we can retrieve
    # BaseModel$scenarios() and use the "build" and "verify" functions to see if everything is
    # complete and present.

    # TODO: Figure out which Category/Level constitutes each ModelStage. Zero level is injected
    # into the category implicitly: we only permute the non-zero levels, but we do include all the
    # variations of "just this category's levels", add one more category, add another category,
    # etc. The StartFrom stage has no Levels, and if we ask for one we always get zero. That will
    # be true throughout. So the ModelStage lets us add a Level (which includes a Scenario
    # index/name. the Level indictor for that Scenario, and an InputPath entry). Later, we can ask
    # for levels for a list of Scenario index/names and get back the numbers if present, or zero
    # if not, so each Model stage will generate its descriptors based on giving it a list of all
    # scenarios known to the visualizer and we'll have a complete set in VEData.

    # Categories key needs these elements:
    #   Levels Array
    #     Each Category element in the array
    #       Name: Label to display above the category selection box in visualizer
    #       Description: long form explanation of what will be varied
    #       Levels:
    #         Inputs: an array of Scenario Name/Level pairs, each of which has
    #           Name: Name of Scenario
    #           Level: Level Name of Scenario for this Category Level
    # Scenarios key needs these elements:
    #   Name: Short tag to key to the Category that includes the Scenario
    #   Label: to display and cross-index to Categories (promoted to Category Name if
    #   auto-generating Categories one-to-one with Scenarios)
    #   Description: long form explanation of what will be varied
    #   ScenarioRoot: root directory within Scenario directory for Level inputs to this Scenario
    #   (defaults to Name).
    #   Files: array of input file names belonging to this scenario (that will vary with Level)
    #   Levels: Array of level objects containing:
    #     Name: Short tag to key to Category Level
    #     Label: (short human description)
    #     Description: Long explanation of how the files vary in this level from the base case
    #     LevelDir: subdirectory within ScenarioDir/ScenarioRoot that gets added to InputPath
    #     (defaults to Name)
    # No base level is included in either Categories or Scenarios - the StartFrom stage provides
    # all the base level information.

    # TODO: expand.grid is our friend - need to include the zero levels.
    # We can prepend NA to the vector of values and we just ignore those as we compile the list of
    # Scenario properties for building model stages - zero length list (pure base case) means do
    # not create a ModelStage, otherwise for each Category (column) and Level (value of column)
    # that is not NA, build a list of the Scenario-Level item. Ultimately for the Scenario-Level
    # need the Scenario Name (for use by visualizer) and level Name, plus the ScenarioDir/LevelDir
    # (can just compose - don't need to keep ScenarioDir separately). The ModelStage Dir (and Name)
    # will be built by flattening character renditions of Scenario Name and Level Name
    # (SN-SL+...).

    # Look at previous BuildScenarios for ideas - it's too complex and messy, but has the idea of
    # filtering the scenarios by categories - doesn't have the "StartFrom" idea obviously. Does
    # check for presence of required files (which are attached to "scenarios",
    # not "categories") Set that up from the extended example.

    # TODO: visit each scenario that is part of the category level included in the stage and add its
    # InputPath and descriptor (Scenario Tag, Level) to the ModelStage Levels list.
    
    scenarioList <- list()
    # Process Categories
    for ( category in modelParam_ls$Categories ) {
      writeLog(paste("Processing category",category$Name),Level="info")

      # Construct levels for this category
      levels <- category$Levels

      catLevel <- lapply(
        levels,
        function(level) {
          catLevelName <- paste0(category,"-",level$Name)
          list(
            Name=catLevelName,
            Description=paste0("(Category: ",category$Label,") ",level$Description),
            InputPath=file.path(self$scenarioPath,catLevelName)
          )
        }
      )
# TODO: Include a "build" function to construct (or verify) the input directories. Copy any
# missing files from the Base Model inputs (use the InputDir reported for each file in the
# BaseModel input path). Report on files present but not listed in the Files key, and delete
# them if they do not differ from the Base Model. Any missing files get copied into place. Also
# generate (perhaps a separate function) a report on which files in the Scenario levels differ
# from the base model (and from other Scenario levels), and perhaps even go deeper looking at
# which columns differ and creating a table with "before-and-after" values for each of the
# fields that are changed.
ve.scenario.build <- function(copyInputs=TRUE) {
  # if not copyInputs, just build the Category/Scenario folders but put nothing
  # in them can re-run with copyInputs==TRUE to populate files later
}

# Print summary information about scenarios
ve.scenario.print <- function(details=FALSE) {
  # does self$scenarioConfig exist?
  # list its folder scenarios (just the name)
  # list its categories (how many files and levels in each)
  # list number of ModelStages present in self$scenarioConfig (from build)
  # details adds in:
  #   under categories, list files and levels
  #   under model stages, list how many are in each run status
  # print(scenarios$verify()) to get diagnostics on broken stuff
}

# List available inputs for each scenario and (if details) whether it has a local version
# TODO: do we need this? Verify should report deviant cases
ve.scenario.inputs <- function(scenario=NULL,category=NULL,details=TRUE) {
  # if "scenario" is a character vector, only show those folder scenarios
  # if "category" is a character vector, only show those categories
  # Folder scenarios list/compare files from overall StartFrom (if any) else just files in folder
  #   Use baseModel$inputs(stage=StartFrom)
  #   List each folder scenario, inspect its InputPath and if the file is present there, mark it as "Used"
  # Category scenarios list all files in Category StartFrom and tags them with the Category they
  #   are associated with (from the Categories configuration, only one possible Category per file)
}

# TODO: do we need this?
ve.scenario.categories <- function(category=NULL,details=FALSE) {
  # TODO: list categories
  # Show category name by default (not informative)
  # If "category" is a character vector, only show those categories
  #   (and set details=TRUE if missing)
  # With details return a data.frame:
  #   TRUE == all details (== c("levels","files"))
  #   "levels" == add one row for each distinct set of level columns (LevelName, Label, Description)
  #   "files" == add one row with File name for each distinct file in the category
  #   if details is a character vector with both "levels" and "files", list files within each
  #     level and in addition to the file name, list out its directory, its size, and its
  #     modification date.
}

# TODO: do we need this?
ve.scenario.list <- function(scenario=NULL, details=FALSE) {
  # TODO: list scenarios
  # Show scenario names by default (character vector)
  # If "scenario" parameter is a character vector, only show those scenarios
  #   (and set details=TRUE if missing)
  # With details return a data.frame:
  #  Show if it is a "Folder" scenario or a "Category" scenario
  #  Show scenario (stage) RunStatus (check baseModel stages - should (re-)load baseModel)
  # Can subset details by providing a character string instead of a logical
  #  TRUE == all details
  #  "status" == name plus run status
  #  "type" == folder/category
}

ve.scenario.save <- function(overwrite=TRUE) {
  # write scenarioConfig to a file
  writeSetup(self$loadedParam_ls,self$scenarioPath,overwrite=overwrite)
}

#' @export
VEModelScenarios <- R6::R6Class(
  "VEModelScenarios",
  public = list(
    # Data elements
    baseModel = NULL,                   # Model object to which scenarios are attached
    scenarioDir = NULL,                 # Name of the current scenario directory (within baseModel$modelPath)
    scenarioPath = NULL,                # Normalized full path to scenaro directory
    loadedParam_ls = NULL,              # Scenario parameters as loaded from configFile (or to be rewritten)
    RunParam_ls = NULL,                 # RunParam_ls for Scenarios (runtime)
    modelStages = list(),               # list of VEModelStage object, built during $load, empty if undefined/invalid
    startFrom = NULL,                   # ModelStage to start from (from config, set during $load)
    invalidStages = list(),             # List of diagnostics (generated during "load" by calling "verify")

    # Functions
    initialize=ve.scenario.init,        # Initializes VEModelScenarios object
    load=ve.scenario.load,              # loads ScenarioDir/ScenarioConfig
    stages=ve.scenario.stages,          # Returns list of VEModelStage representing scenarios
    verify=ve.scenario.verify,          # Returns scenario diagnostics
    build=ve.scenario.build,            # If Category/Scenario defined, will create any missing directories/files then return verify
    reportable=ve.scenario.reportable,  # Returns TRUE if the supplied stage name is the Scenario StartFrom stage, else FALSE
    print=ve.scenario.print,            # Display scenario configuration
    inputs=ve.scenario.inputs,          # Set/View list of inputs by category (or just list of files if no categories)
    categories=ve.scenario.categories,  # Return categories, or replace/update them (optionally save config to .csv files)
    list=ve.scenario.list,              # List out the scenario configuration and stages (details optional)
    save=ve.scenario.save               # Save the in-memory configuration back out to the config (mostly after build)
  )
)
