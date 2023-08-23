# Set up model and scenarios
# Directory Tree:
# models/MyModel
#   visioneval.cnf (rolls up old run_parameters.json and initializeModel)
#   defs
#     ... (The usual cast of characters)
#     display_units.csv (Specify display/export units for certain fields)
#   scripts
#     run_model.R
#   inputs
#     ... (the usual cast of characters for Base Year and default future years)
#   scenarios
#     visioneval.cnf (defines scenario names and subdirectories and "BaseScenario" for inputs)
#     scenario_1 (or however you want to call them
#       visioneval.cnf (optional, usually easiest just to put it scenarios/visioneval.cnf)
#       ... (The subset of base input files for which values are different in this scenario)
#     scenario_2 (and so
#       ...
#     ...        (and you can have as many scenarios as you want)

mod <- openModel("MyModel")  # open the model, configurated as above
mod$plan(workers=2)          # run scenarios in parallel - limiting factor is usually RAM, not CPUs
                             # we can pretty easily hack this to use arbitrary connections to cloud servers
                             # Let me know if you want to try that out and we'll work out the details...
                             #   See the 'futures' package for details on connections and setup
mod$run()                    # runs all the scenarios, posting status messages
print(mod)                   # see status - in particular scenarios that failed to run
results <- mod$results()     # extract results; supports selecting subsets - inquire for details

# basic export
exp_csv <- results$export()             # put all the tables in a CSV file and return the exporter
print(exp_csv)                          # summarize tables created
exp_sql <- results$export("sql")        # put the results in an SQLite database
print(exp_sql)
exp.df <- results$export("data.frame") # put the results into R data.frames and return an exporter
print(exp_df)

# You can create an "Exporters" block in visioneval.cnf and override defaults for "sql" (e.g. if you
# want to use Access or MySQL or PostgreSQL) or create an entirely new class (needs to use an internal
# driver like DBI): might use that to create an ODBC connection to an Excel spreadsheet, for example.

df_ls <- results$extract()            # shortcut for export("data.frame"); returns a list of data.frames
exp_df <- attr(df_ls,"Exporter")      # but also has the Exporter as an R attribute

dt_ls <- results$extract(format="data.table")           # in case you prefer data.tables to work with
dt_sql_ls <- results$extract("sql",format="data.table") # in case you want an SQLite database as backing store
                                                        # extract still gives you the list of all the data.tables
dtibble_ls <- results$extract(format="tibble")          # or tibbles
print(names(dtibble_ls))

# reading back from an export
dtibble_ls <- exp.df$data(format="tibble")

# partitioning differently
# The default is to create one table per scenario per year, identified either in the table name (SQL or data.frames)
# or in sub-folders (CSV). But the partitioning is entirely flexible.
# "Paths" are folder-like (and prepended to the base table name); "Names" are name-like (and appended to the base
# table name. So here's an alternative scheme that partitions with Year grouped into folders (and all the scenarios
# by Year within each table):
exp_csv <- mod$export("csv",partition=c(Year="path",Scenario="merge",Global="path"))
print(exp_csv$list()) # list of tables created...
exp_csvtable_df <- exp_csv$data("2038/Household") # Can elevate a subset of tables back into R (get names from exporter$list()).

# In the CSV world, the tables are broken into sub-directories by scenario and individual tables
# are created for each Year in the output data.
#   partition=c(Year="name",Scenario="path") # implied, always, that Global="path"

# In the SQL world, the default is to mash up one set of tables for each table type (Households, Vehicles, etc) for all
# the Scenarios and Years - the "Global" tables, some of which have the same names as "Year" tables (e.g. Marea or
# Bzone), are always broken out into different tables.
#   partition=c(Scenario="path",Year="name")
# The following variants lead to subtle differences in the exported Table names:
#   partition=c(Scenario="path",Year="path")
#   partition=c(Year="path",Scenario="name")
# The following puts everything in one table (Household, Vehicle, etc.), with Global broken out always
#   partition=c(Year="merge",Scenario="merge") # push all of that into one table
#   partition=character(0)                     # Same as "merge everything" partition

# Same export mechanism works with queries (but no partitioning)
qr <- mod$query("Full-Query")
qr$run()
qr$export("csv") # creates a table at the root of the model's OutputDir ("outputs" inside model results directory)

mod$dir(outputs=TRUE, all.files=TRUE)
