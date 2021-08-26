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

# stages
  # inspecting stages (in memory)
  # defining stages
    # implicitly as a model subfolder
    # explicitly in model visioneval.cnf
  # stage visioneval.cnf

# StartFrom stages
  # structure of configuration
  # options
    # stage meta-parameters
    # stage directory
    # stage visioneval.cnf (via Config)
    # stage inputs
    # stage results
    # stage descriptors

# Copying Datastores
  # Flattening
  # Converting DatastoreType
# LoadModel
  # Building stages across "models"
  # Using LoadModel for post-mortem debugging
# Extracting results
  # Basic extraction
  # Setting display_units
  # Filtering Group/Table/Name
  # Setting OutputDir (sub-directory of model's results)
# Querying results
  # Defining query specifications
    # Writing a file
    # Building in memory and saving
  # Running a query (set of specifications)
  # What comes up (summary of scenarios)
  # Using a different geography
# Using model stages to make scenarios
  # Adding a scenario stage to a model
    # StartFrom
    # InputDir
    # run_model.R
    # Years, Scenario, Description
  # Where do scenario results to?
  # How to extract or query scenario results