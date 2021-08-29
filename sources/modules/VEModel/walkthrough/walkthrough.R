# See nearby Readme.md for instructions on the walkthrough
# Copy a few lines at a time into your R interpreter of choice to see what happens
# You can run this file non-interactively (e.g. via "source") but it won't print
# out all the interesting stuff...

###########################
# INITIAL WALKTHROUGH SETUP
###########################

# Run initial setup
source("Setup.R")
# Loads VEModel (creating a VisionEval runtime environment)
# also defines "makeMiniModel" function, used below

search()            # Notice VEModel package, but not visioneval
loadedNamespaces()  # Notice "visioneval" among the loaded namespaces
# framework functions need namespace resolution, visioneval::frameworkFunction

#########################
# INSTALL STANDARD MODELS
#########################

# The standard models are the same ones we distribute in the current VisionEval
#  but they have been restructured into "Next Generation" model setups

# install models
# Show available models
installModel()

# Show avaialble variants for one of the models
installModel("VERSPM",var="") # "var" is short for "variant" - you can spell it out

# Install the base variant as "VERSPM" (with a confirm dialog)
dir("models") # Setup.R already installed and ran "VERSPM-run" (using the "pop" variant - more on that below)

installModel("VERSPM") # default if running interactively is to ask for a "y" to confirm installation
# If no variant is specified, you get "base"

dir("models") # Note that the installed name includes the variant: VERSPM-base
# Can try VERPAT too
installModel("VERPAT",modelPath="MYRPAT",confirm=FALSE) # base variant, but with name we chose

# See what we've got
dir("models")

# opening models
vrb <- openModel("VERSPM-base")
print(vrb) # Initialized
mwr <- model.with.results <- openModel("VERSPM-run")
print(mwr)  # Run Complete (has results)mi

################################
# FIRST VIEW OF MODEL STRUCTURES
################################

# Tour the model structures in file explorer
shell.exec("models")

# inspecting model inputs
inputs <- vrb$list(inputs=TRUE,details=c("FILE","INPUTDIR"))
print(inputs[1:10,])
required.files <- unique(file.path(inputs$INPUTDIR,inputs$FILE))
print(required.files[1:10]) # full paths

# Hack for shortening model paths:
sub(runtimeEnvironment()$ve.runtime,"",required.files)

# inspecting model stages
print(vrb)  # list of stage objects - only one in "base" model

# Here's a version of VERSPM with stages - we'll come back to that
vrs <- installModel("VERSPM",variant="pop",confirm=FALSE) # VERSPM-run was pre-created from "pop" variant and run in setup.R
print(vrs) # Three stages - we'll get back to that

##########################
# EXTRACTING MODEL RESULTS
##########################

# Dump all of a model's results:
results <- mwr$results()
if ( is.list(results) ) { # TRUE if there are multiple Reportable stages
  results <- results[[1]]  # or select a different stage by name or index
}
print(results)
# Result extraction must be done stage-by-stage

# If you want to extract from the whole model, you can flatten the Datastore (see below)
#   into a new directory. but be careful if you have a bunch of stages that fill up a 2050
#   future year group with variants of the same data; you'll only get the data from the
#   final one.

# Queries, on the other hand, work just fine with modelStages as we'll see below.

# Here's the basic extraction of everything
results$extract()
mwr$dir(output=TRUE)
mwr$dir(output=TRUE,all.files=TRUE)

# See what is selected
print(results)
sl <- results$select() # Get full field list
print(
  head(
    capture.output( print(sl) ),
    n=12
  )
)

#######################################
# SELECTING GROUPS, TABLES AND DATASETS
#######################################

# Do some basic field extraction - list fields
print(sl$groups())
print(sl$tables())
fld <- sl$fields()
print(fld[sample(length(fld),20)])

# Select some subsets by group, table or field name...
# Can we easily identify group names, table names, field names and zero in on selecting them?
sl$select( sl$find(Group="Years") )
print(sl$groups())
sl$select( sl$find(Group=sl$groups()[1]) )         # Just the first ones
print(sl$groups())

sl$select( sl$find(Group=sl$groups()[1],Table=c("Household","Vehicle")) )
print(sl)
print(head(capture.output(print(sl,details=TRUE)),n=12))

sl$extract()
results$extract() # WARNING: Uses "sl" selection
sl$all() # deselect everything

########################
# CHANGING DISPLAY UNITS
########################

# Programatically set up the display_units file with a useful conversion
un <- results$list(details=TRUE)[,c("Group","Table","Name","Units")]
spd <- un[ grepl("MI/",un$Units)&grepl("sp",un$Name,ignore.case=TRUE), ]
spd$DisplayUnits <- "MI/HR"
print(spd)

# Put the display_units file in a useful place (model 'defs' directory)
display_units_file <- file.path(
  mwr$modelPath,
  mwr$setting("ParamDir"),
  mwr$setting("DisplayUnitsFile") # defaults to 'display_units.csv'
)
cat(display_units_file,"\n")
write.csv(spd,file=display_units_file)

# Select speed fields...
sl$all() # re-select everything
sl$select( with(spd,paste(Group,Table,Name,sep="/")) )
print(sl)

# Showing currently defined UNITS/DISPLAYUNITS (via sl$results)
print(sl$results$units())

# Showing currently defined UNITS/DISPLAYUNITS (directly from results)
print(results$units())

# Add the geography fields in the Marea Table
sl$add( sl$find("^(Marea|Azone|Bzone)$",Group="Years",Table="Marea") )
print(sl$fields())
print(results$units())

# Extracting speed fields using DISPLAY units
sl$extract(prefix="DisplayUnits")                 # Using DISPLAY units

# Extract speed fields using DATASTORE units
sl$export(prefix="Datastore",convertUnits=FALSE)  # Using DATASTORE units

shell.exec(results$resultsPath)
# In general, it is better to use queries to do the extraction and unit conversion...

################################
# FLATTEN OR CONVERT A DATASTORE
################################

# to do this right with a staged model, you need to iterate over the
# "Reportable" stages and copy their results one by one.

test.dir <- file.path(getwd(),"Test-Results-Copy")
if ( dir.exists(test.dir) ) unlink(test.dir,recursive=TRUE)
dir.create(test.dir)
results$copy(test.dir,Flatten=c(TRUE,TRUE)) # Force use of "Flatten" even if result is already flat
tr <- openResults(test.dir)
print(tr)

# results$copy(test.dir,Flatten=FALSE) # exact copy of just this stage (not previous stages)
# results$copy(test.dir,DatastoreType="H5") # convert result to H5 Datastore
# NOTE: it is not clear that the H5 DatastoreType still works at all

#############################
# CLEARING OUTPUTS (EXTRACTS)
#############################

print(mwr$dir())
print(mwr$dir(outputs=TRUE))

mwr$clear(outputOnly=TRUE, force=FALSE) # interactive clearing of outputs

############################################
# BUILD AND RUN A NEXT-GENERATION MINI-MODEL
############################################

# Use a mini-model to illustrate run operations
# Open Setup.R to see what happens - we'll open the individual files
print(vrb)
mini <- makeMiniModel(vrb)

# Tour the mini model structure in File Explorer
shell.exec(mini$modelPath)

# Tour the model using VEModel exploration functions

mini$dir()              # List the summary contents of the model
# TODO 8/27/2021: $dir does not work for staged models (need to handle stage directories)

mini$dir(inputs=TRUE)   # List just the model input directories
mini$dir(inputs=TRUE,all.files=TRUE) # List all the input files...

mini$run("reset")    # throw away existing results and re-run

mini$dir()           # notice presence of results directory

mini$run("save")     # move existing results into an archive and re-run
mini$dir()           # notice presence of archive directory
mini$dir(archive=TRUE) # call out just the archive directories
mini$dir(archive=TRUE,all.files=TRUE) # List all the files in the archive directories

mini$run("continue") # re-run any stage that is not "Run Complete" - does nothing here
mini$run()           # same as vr$run("continue")

###########################################
# RUNTIME PARAMETERS (GLOBAL, MODEL, STAGE)
###########################################

# examine model parameters
# default visioneval and VEModel parameters
# other parameters exist (e.g. a stage's RunPath) but they do not have defaults
viewSetup(Param_ls=visioneval::defaultVERunParameters())

# parameters defined in ve.runtime (initially, none)
viewSetup(fromFile=TRUE)

# parameters defined in model configuration
viewSetup(mini,fromFile=TRUE)

# all parameters
viewSetup(mini)

# parameters defined in stage configuration file
# note: there are additional parameters in the stage, even for a one-stage model
viewSetup(mini$modelStages[[1]])

# inspect parameters in configuration file versus loaded model
print(names(getSetup(mini,fromFile=TRUE))) # in the file
print(names(getSetup(mini)))               # all constructed parameters in memory

# easier to inspect constructed parameter with the model's "setting" function
mini$setting() # list the names
mini$setting(setting="",) # List all the names and values (warning: potentially huge)

##########################
# ADDING STAGES TO A MODEL
##########################

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
  Description = "Minimal model base year, constructed programatically",
  Years       = c("2010"),
  Reportable  = TRUE # need this to extract/query if a later stage starts from this one
)
yaml::write_yaml(StageParam_ls,file.path(config.dir,"BaseYear.cnf"))

StageParam_ls <- list(
  Scenario    = "MiniModel Future Year",
  Description = "Minimal model future year, constructed programatically",
  Years       = c("2038"),
  StartFrom   = "BaseYear" # match name of earlier stage in ModelStages (see below)
  # Standard VERSPM future year requires access to BaseYear results.
)
yaml::write_yaml(StageParam_ls,file.path(config.dir,"FutureYear.cnf"))

# Now we'll update the base model configuration to connect the stages

# We could have updated the setup using the updateSetup functions we saw earlier, but for
#  extensive changes, it's simpler just to re-write the entire list.
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

###################################
# STAGED MODEL WITH IMPLICIT STAGES
###################################

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

# Now re-run the two implicit stages
mini.2$configure()
mini.2$run("reset")
mini.2$dir()

# Finally, let's add a stage to mini.2 with a different scenario
# Note that this stage is added IN MEMORY ONLY, but we can still run the model
# The use case is for doing a quick test as we are here, and also for
#  programatically injecting a new scenario into a base model based on
#  combinations of inputs, where we are only interested in the model results

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
# add stage automatically "configures" the model

# Even though the stage definition is in memory, the specific inputs for
#  the alternative scenario are maintained in a directory.

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

# Finally, run the new stage

mini.2$run() # Watch closely: will only run the new stage!

# Show the input files again:
mini.files <- unique(mini.2$list(reset=TRUE,inputs=TRUE,shorten=TRUE,details=c("FILE","INPUTDIR","STAGE")))

####################################
# BUILDING AND RUNNING MODEL QUERIES
####################################

# Simple query illustration
vrb <- openModel("VERSPM-run")
vrb$query()
shell.exec(vrb$modelPath)

qr <- vrb$query( vrb$query()[1] )
qr$run(vrb)
vrb$dir(output=TRUE)

#######################################
# CHANGING MODEL PARAMETERS FROM R CODE
#######################################

# Let's change the overall runtime configuration for the mini model, altering the Seed parameter
# NOTE: mini does not define Seed - it will use the VE default

viewSetup(mini$modelStages[[1]]) # Runtime settings for mini model
mini$setting("Seed")
mini$setting("Seed",source=TRUE)

viewSetup(fromFile=TRUE)
updateSetup(Seed=2.3)                # update working set of parameters in memory
mini$configure(fromFile=FALSE)       # use updated parameter to rebuild model
viewSetup(fromFile=TRUE)             # No settings since file didn't exist
viewSetup()                          # Created Seed=2.3 in memory

mini$setting("Seed")
mini$setting("Seed",source=TRUE)     # source is "interactive"

updateSetup(inFile=TRUE,Seed=2.3)    # if inFile==TRUE, update file parameters in memory
viewSetup(fromFile=TRUE)             # Staged change - not yet in file

file.exists("visioneval.cnf")        # FALSE - not saved yet
writeSetup(overwrite=TRUE)           # Commit the file changes to the configuration file
file.exists("visioneval.cnf")        # TRUE - now saved
getSetup(reload=TRUE)                # reload global parameter file
viewSetup(fromFile=TRUE)

# To apply the new setting from the configuration file, we need to re-open the model
# Default is to revisit configuration file (see mini$configure(fromFile=FALSE) above
mini$configure()          
mini$setting("Seed")

# Now put the settings back the way they were
getwd() # should be walkthrough runtime directory
dir() # should have a visioneval.cnf
unlink("visioneval.cnf") # get rid of configuration
getSetup(reload=TRUE) # reload runtime setup

# The Seed parameter should probably be set in the model not globally, so do this:
updateSetup(mini,inFile=TRUE,Seed=1.5)  # Change the configuration
writeSetup(mini,overwrite=TRUE)         # Save it in the model's configuration file
mini$configure()                        # Reload the model from its configuration

print(mini$setting("Seed"))             # Show value of runtime setting
print(mini$setting("Seed",source=TRUE)) # show where the setting came from
viewSetup(mini$modelStages[[1]])

# Run the model again, saving the values one more time
mini$run("save")
mini$dir(results=TRUE,archive=TRUE)

#######################
# LOADING ANOTHER MODEL
#######################

# shell.exec("models/BARE-load")

loadModel <- openModel("BARE-load")  # Constructed in makeMiniModel(...)
print(loadModel)
viewSetup(loadModel,fromFile=TRUE)
loadModel$run()
# Now notice that the data from the original mini model was copied into this Datastore
loadModel$dir(results=TRUE,all.files=TRUE)
