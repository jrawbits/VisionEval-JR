# Functions to build module packages

# Generally, building a module package follows the old "build/build-modules.R"

# Need to set up the environment (different for RStudio add-in versus command line ve.build()).
# where to we put src, ve.pkg
# where do we look for extdata (and put the data that result from estimation)

# Nuancing the module load (sourcing the script ourselves rather than relying on pkgload/Roxygen implicitly)
#   Permits finding an Estimation function and checking for illegal objects
#   Module should not allow any free-standing objects in R space that are not known functions (tightly control structure)
#     However we should allow helper furnctions - just no non-function objects.
#   So there will be a helper function om VEBuild to parse the module script and reject unknown items
#   Should the module be allowed to create an environment when the package loads? A function to populate that?
#   Should the package startup identify modules internally? We're initially just tracking modules via DESCRIPTION field...

# Should new estimation architecture go module-by-module or expect to do everything for the package?
# Probably needs to be the latter; so in building, we'll assemble all the available <module>Estimate functions
#   then run those. The extdata to perform estimation is pulled from one place for all modules.

# To handle estimation specifications - should there be a separate <Module>EstimateSpecification file
# that forces documentation of the input files? Then the framework estimator can visit each module in
# the package (build that list as part of package data?) and it locates the inputs and passes that
# block of data to the actual estimation function (perhaps just a named list of data.frames...). That
# will wrap up the specifications in a place where we can assemble documentation.
# Perhaps the builder will update DESCRIPTION rather than relying on the author to build it.

# Also consider situations where one module's estimator depends on output from an earlier model's
# estimator. Also consider package dependencies where one package's estimator depends on the presence
# of another package - that will show up in the Imports/Depends section of DESCRIPTION and we need
# to load those packages prior to running the Estimate function for the package.

# So the estimator can be a series of "pre-modules" that can be checked/run as part of the model run
#   -- All the following steps are based on the ModelScript - we will estimate/input/intiialize/run based
#      on runModule, runPython, runQuery. Standalone estimation of a Package is possible too (e.g. for localdata
#      outside the model folder (i.e. in runtime)
#   -- Estimation functions run first (works on localdata/data; does not use Datastore)
#      Report where each package's data comes from (optional, unless data is being rebuilt)
#   -- Model Input loading runs next (load all inputs into Datastore)
#      Uses an input data spec to map conventional .csv names to a tabular data source
#   -- Initialize functions run next (module-specific, then package-wide, works on Datastore)
#      Check what is happening in Initialize: does it actually read input files or is it working on the Datastore?
#   -- Model runModule/runPython functions run in sequence (reading/writing Datastore)
#   -- Add a runQuery model step that can do either a simple export or one of the canned queries
#      Looks for query by name in the model queries folder
#      Generates into outputs folder as CSV by default, but can have global export spec or runQuery specific one

# Set up build environment
#   - src folder into which full package is constructed
#   - ve-lib for installed package
#   - ve-pkgs for CRAN-like local repository (source/binary, current R version)
# Build packages from a parent directory of multiple packages
# Build a single package (starting with VE pre-build, then doing regular R package build)
#   0. Identify Module scripts in package (from Description)
#   1. Update Namespace, Collate and Function docs (Roxygen)
#   2. Load each Module script and queue up key functions
#      a. Package module_docs will call framework document module for each module script
#         Move documentModule function definition to this package
#         Will loop over identify Module scripts and run documentModule for each (generating empty docs if no block present)
#      b. Package Estimation will call each Module Estimate function if prosent
#         Although the framework estimate can just look for <module>Estimate functions in the package namespace
#         Framework estimate has a parameter / search path to look for extdata
#         Framework estimate can also get estimate specification for required extdata (move that documentation into
#         the module and allow it to be interrogated, and to generate skeleton extdata files)
#         Individual Module estimation will always look in the one place first located, and fail with errors if the
#           the estimation data is wront or incomplete
#         Let the <Package>Initialize module also have a <Package>InitializeEstimate function to perform package-wide
#           checking (e.g. look for all the different module's estimation files and report if they are not present.
#         Use extdata for the package (option to locate extdata elsewhere for later when we do the package estimation)
#      c. Add a test harness here to run tests on the module? Do that through standard R build process?
#      d. Finally, perform all the package operations
#   3. Once the VE-specific pre-build stuff is done, do the regular R package build
#   4. If running on Windows, build binary and source packages (or perhaps always do that - can we do cross-platform builds?)
#      Place them in a local repository

# Probably want a separate function for installing
#   1. Install a package from the local repository into the local ve-lib (optional, perhaps a separate function)

# Construct an RStudio add-in to do VE package build
#   Perhaps limit that to a single package (or just diagnose based on working directory contents)
#   Should it have a dialog or other means to configure R package build (work with VE_HOME perhaps)
#   Perhaps break out build and install steps as separate add-in elements

# Other functions for some of the other build process steps
#   - puse packages to external CRAN-liek repository (requires permissions)
#   - VEBuild should be able to handle multiple local repositories, associated with different build locations
#     Or it can build everything from different folders into a single local repository
#   - Build Inventory (to work through the sample model scripts in this package and inventory the Inp/Set files and fields)
#   - Create offline installers
#     - Just the VE packages (no dependencies)
#     - Get all the dependencies (do this as part of building the offline installer)
#       The local ve-lib will always get the dependencies when each package is installed -- either from online or from
#       a full local package repo (see next)
#     - For offline installation, generate either a source contriburl or a pre-installed Windows library