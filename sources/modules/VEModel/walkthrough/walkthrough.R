# Run initial setup
source("Setup.R")
# Loads VEModel (creating a VisionEval runtime environment)
# also defines "makeMiniModel" function, used below

# install models
# Show available models
installModel()

# Show avaialble variants for one of the models
installModel("VERSPM",var="") # "var" is short for "variant" - you can spell it out

# Install the base variant as "VERSPM" (with a confirm dialog)
installModel("VERSPM") # default if running interactively is to ask for a "y" to confirm installation
dir("models") # Note that installed name includes the variant: VERSPM-base

# Can try VERPAT too
installModel("VERPAT",modelPath="VERPAT",confirm=FALSE) # base variant, but with name we chose

# See what we've got
dir("models")

# opening models
vrb <- openModel("VERSPM-base")
print(vrb)

# inspecting model inputs
inputs <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
print(inputs)
input.dir <- unique(vrb$dir(inputs=TRUE,shorten=FALSE))
required.files <- unique(file.path(input.dir,inputs[,"FILE"]))

# inspecting model stages
print(vrb$modelStages)  # list of stage objects - only one in "base" model

vrs <- openModel("VERSPM-run") # pre-created and run in setup.R
print(vrs$modelStages) # Three stages - we'll get back to stages

# Use a mini-model to illustrate run operations
mini <- makeMiniModel(vrb)

mini$dir()              # List the contents of the model
mini$dir(inputs=TRUE)   # List just the model input directories
mini$dir(inputs=TRUE,all.files=TRUE) # List all the input files...

mini$run("reset")    # throw away existing results and re-run
mini$dir()           # notice presence of results directory
mini$dir(results=T,all.files=TRUE)

mini$run("save")     # move existing results into an archive and re-run
mini$dir()

mini$run("continue") # re-run any stage that is not "Run Complete" - does nothing here
mini$run()           # same as vr$run("continue")

# examine model parameters
# default visioneval and VEModel parameters
visioneval::defaultVERunParameters()

# parameters defined in ve.runtime (initially, none)
viewSetup(fromFile=TRUE)

# parameters defined in vrs (Staged) model
viewSetup(vrs,fromFile=TRUE)

# parameters defined in a vrs (Staged) model stage configuration file
viewSetup(vrs$modelStages[[2]],fromFile=TRUE)

# Let's change the overall runtime configuration for the base VERSPM, altering the Seed parameter
# NOTE: VERSPM-base does not define Seed - it will use the runtime or the VE default
viewSetup(mini$modelStages[[1]]) # Runtime settings for mini model
print(mini$setting("Seed"))
print(mini$setting("Seed",source=TRUE))

viewSetup(fromFile=TRUE)

updateSetup(inFile=TRUE,Seed=2.3) # if inFile==FALSE, just update working set of parameters in memory
writeSetup(overwrite=TRUE)
viewSetup(fromFile=TRUE) # changes not yet recorded

getSetup(reload=TRUE)
viewSetup(fromFile=TRUE) # changes reloaded from master file

# To apply the new setting, we need to re-open the model
mini$configure()
print(mini$setting("Seed"))
print(mini$setting("Seed",source=TRUE))

# Now put the settings back:
getwd() # should be walkthrough runtime
dir() # should have a visioneval.cnf
unlink("visioneval.cnf") # get rid of configuration
getSetup(reload=TRUE) # reload runtime setup

# Probably the Seed should be set in the model not the runtime, so do this:
updateSetup(mini,inFile=TRUE,Seed=1.5)
writeSetup(mini,overwrite=TRUE)
mini$configure()
print(mini$setting("Seed")) # Show value of runtime setting
print(mini$setting("Seed",source=TRUE)) # show where the setting came from
viewSetup(mini$modelStages[[1]])

# Run the model again, saving the values one more time
mini$run("save")
mini$dir(results=TRUE,archive=TRUE)

# Add stages to the mini model
mini$modelStages # list the one stage constructed from the root folder

# Recreate the mini model with two stages, for base and future years
mini.1 <- Mini$copy("BARE-Stages",results=FALSE)

# Two ways to make stages. First, just with configuration files
# The model script will be run once for each stage, using the same input
#   files. Stage specific information can be placed in a model subdirectory
#   (Next to "inputs" and "defs") or it can be gathered in other places.
# Items like "defs" will be found if they are set for the model. In the
#   mini.1 model, all the "inputs" are also located at the model level.

# Just make two more configurations for the stages (base and future year)
# We'll put them in a directory of their own
config.dir <- file.path(mini.1$modelPath,"config")
if ( dir.exists(config.dir) ) unlink(config.dir,recursive=TRUE)
create.dir( config.dir )

StageParam_ls <- list(
  Scenario    = "MiniModel Base Year",
  Description = "Minimal model base year, construted programatically",
  Years       = c("2010"),
  Reportable  = TRUE # need this to extract/query if a later stage starts from this one
)
yaml::write_yaml(StageParam_ls,file.path(config.dir,"BaseYear.cnf"))

StageParam_ls <- list(
  Scenario    = "MiniModel Future Year",
  Description = "Minimal model future year, construted programatically",
  Years       = c("2038"),
  StartFrom   = "BaseYear" # match name of earlier stage in ModelStages (see below)
  # Standard VERSPM future year requires access to BaseYear results.
)
yaml::write_yaml(StageParam_ls,file.path(config.dir,"FutureYear.cnf"))

# Now we'll update the base model configuration to connect the stages
modelParam_ls <- list(
  Model       = "Staged Mini Model Test",
  Region      = "RVMPO",
  State       = "OR",
  BaseYear    = "2010",
  ModelStages = list(
    "BaseYear" = list( 
      Dir = "BaseYear",
      Config = "config/BaseYear.cnf"
    ),
    "FutureYear" = list(
      Dir = "FutureYear",
      Config = "config/FutureYear.cnf"
    )
  )
)
yaml::write_yaml(modelParam_ls,file.path(mini.1$modelPath,"visioneval.cnf"))

# Reload the updated model configuration
mini.1$configure()

# Let's see what we've got
print(mini.1)
print(unique(mini.1$list(inputs=TRUE,details=c("STAGE","FILE","INPUTDIR"))))

mini.1$run("reset")
mini.1$dir()

# Now let's go back and do "implicit stages" from the original mini
mini.2 <- mini.1$copy("BARE-Imp-Stages")

dir.create( stage1.dir <- file.path(mini.2$modelPath,"BaseYear") )
StageParam_ls <- list(
  Scenario    = "MiniModel Base Year",
  Description = "Minimal model base year, construted programatically",
  Years       = c("2010"),
  Reportable  = TRUE # need this to extract/query if a later stage starts from this one
)
yaml::write_yaml(StageParam_ls,file.path(stage1.dir,"visioneval.cnf"))

dir.create( stage2.dir <- file.path(mini.2$modelPath,"FutureYear") )
StageParam_ls <- list(
  Scenario    = "MiniModel Future Year",
  Description = "Minimal model future year, construted programatically",
  Years       = c("2038"),
  StartFrom   = "BaseYear" # match name of earlier stage in ModelStages (see below)
  # Standard VERSPM future year requires access to BaseYear results.
)
yaml::write_yaml(StageParam_ls,file.path(stage2.dir,"visioneval.cnf"))

modelParam_ls <- list(
  Model       = "Implicit Staged Mini Model Test",
  Region      = "RVMPO",
  State       = "OR",
  BaseYear    = "2010"
  ModelStages = list(
    "BaseYear" = list  ( Dir = "BaseYear"   ), # Dir need not match name of stage
    "FutureYear" = list( Dir = "FutureYear" )
  )
)
yaml::write_yaml(modelParam_ls,file.path(mini.2$modelPath,"visioneval.cnf"))

# Now re-run implicit stages
mini.2$configure()
mini.2$run("reset")
mini.2$dir()

# Finally, let's add a stage to mini.2 with a different scenario
# Note that this stage is added IN MEMORY ONLY - if we configure (reload)
#  the model, the extra stage will be lost.
# The use case is for doing a quick test as we are here, and also for
#  programatically injecting a new scenario into a base model based on
#  combinations of inputs
mini.2$addstage(
    list(
      Name="AltFuture",
      Dir="AltFuture"
    ),
    Scenario="Mini-Model Alternative Future",
    Description="Scramble an input file",
    StartFrom="BaseYear",
    BaseYear=mini.2$setting("BaseYear"),
    Years=mini.2$setting("Years",stage="FutureYear")
  )
dir.create( stage3.dir   <- file.path(mini.2$modelPath,"AltFuture") )
dir.create( stage3.input <- file.path( stage3.dir,mini.2$setting("InputDir") ) )

# Create an alternate future input (extra 3% on hh_pop distributions)
# For a real "in memory" stage, this scenario file would exist somewhere else, and we would
#   locate it by setting an InputPath parameter saying where to find an InputDir with the
#   updated files in it.
mini.files <- unique(mini.2$list(reset=TRUE,inputs=TRUE,shorten=FALSE,details=c("FILE","INPUTDIR")))
mini.files <- file.path(mini.files$INPUTDIR,mini.files$FILE)
scenario.input <- grep(
  "hh_pop",
  mini.files,
  value=TRUE
)
adj.input <- read.csv(scenario.input)
adj.names <- grep("^Age",names(adj.input),value=TRUE)
adj.input[adj.input$Year="2038", adj.names] <- adj.input[adj.input$Year="2038", adj.names] * 1.03
write.csv(adj.input,row.names=FALSE,file=file.path(stage3.input,basename(scenario.input)))

# Show the input files again:
mini.files <- unique(mini.2$list(reset=TRUE,inputs=TRUE,shorten=TRUE,details=c("FILE","INPUTDIR","STAGE")))

# TODO: Use the pre-run full version of VERSPM and a few query tweaks

# Extracting results (straight from the test suite)
  # Basic extraction
  # Setting display_units
  # Filtering Group/Table/Name
  # Setting OutputDir (sub-directory of model's results)

# Querying results (from the test suite; using the long query list for the model)
  # Defining query specifications
    # Writing a file
    # Building in memory and saving
  # Running a query (set of specifications)
  # What comes up (summary of scenarios)
  # Using a different geography

# TODO: Haven't had time to test out converting DatastoreType (unclear if H5 is even working)
# Copying Datastores
  # Flattening
  # Converting DatastoreType

# TODO: LoadModel
  # Building stages across "models"
  # Using LoadModel for post-mortem debugging
# From the original BaseYear bare model, create an entirely separate model copy
#  in which we specify LoadModel

