# Output.R

ve.init.output <- function() { # parameters yet to come - hook to model
  NULL
}

ve.output.index <- function() {
  # Check that there is a model state
  # message("indexing model stages...")
  if ( length(self$modelState)==0 || ! any(unlist(lapply(self$modelState,length))>0) ) {
    stop("Model does not appear to have been run yet.")
  }

  # message("Processing model stages...")
  Index <- data.frame()
  Inputs <- data.frame()
  for ( stage in self$stageCount ) {
    ms <- self$modelState[[stage]]
    if ( length(ms)==0 ) next
    ds <- (ms$Datastore)
    model.path <- file.path(basename(dirname(self$modelPath[stage])),basename(self$modelPath[stage]))

    # TODO: change this to parse the model from run_model.R if modelstate does not exist
    # Use

    # message("Processing ",basename(self$modelPath[stage]))
    # NOTE: Datastore element ds of ModelState is a data.frame.
    #       The attributes column contains a list for each row
    # Datastore elements with a "FILE" attribute are inputs; we want the outputs
    # the non-FILE elements are creations living in the Datastore (i.e. not inputs => outputs)
    InputIndex <- sapply(ds$attributes, attributeExist, "FILE")
    Description <- sapply(ds$attributes, attributeGet, "DESCRIPTION",simplify=TRUE) # should yield a character vector
    Module <- sapply(ds$attributes, attributeGet, "MODULE",simplify=TRUE) # should yield a character vector
    Units <- sapply(ds$attributes, attributeGet, "UNITS",simplify=TRUE) # should yield a character vector

    # Build parallel data.frame for Inputs
    # message("Input data frame...")
    File <- sapply(ds$attributes, attributeGet, "FILE",simplify=TRUE) # should yield a character vector
    inputs <- data.frame(
      Module = Module[InputIndex],
      Name = ds$name[InputIndex],
      File = File[InputIndex],
      Description = Description[InputIndex],
      Units = Units[InputIndex],
      Stage = rep(as.character(stage),length(which(InputIndex))),
      Path = model.path
    )
    Inputs <- rbind(Inputs,inputs)
    # message("Length of inputs:",nrow(inputs))

    # message("Output data frame...")
    Description <- Description[!InputIndex]
    Module <- Module[!InputIndex]
    Units <- Units[!InputIndex]
    splitGroupTableName <- strsplit(ds[!InputIndex, "groupname"], "/")
    if ( length(Description) != length(splitGroupTableName) ) stop("Inconsistent table<->description correspondence")
    # message("Length of outputs:",length(splitGroupTableName))

    maxLength <- max(unlist(lapply(splitGroupTableName, length)))
    if ( maxLength != 3 ) {
      warning("Model state ",self$modelPath[stage],"is incomplete (",maxLength,")")
      next
    }
    splitGroupTableName <- lapply(splitGroupTableName , function(x) c(x, rep(NA, maxLength-length(x))))

    # Add modelPath and Description to Index row
    # message("Adding Description and modelPath")
    PathGroupTableName <- list()
    for ( i in 1:length(splitGroupTableName) ) {
      PathGroupTableName[[i]] <- c(
        splitGroupTableName[[i]],
        Description[i],
        Units[i],
        Module[i],
        as.character(stage),
        model.path
      )
    }
    if ( any((cls<-lapply(PathGroupTableName,class))!="character") ) {
      bad.class <- which(cls!="character")
      print( PathGroupTableName[[bad.class[1]]] )
      print( length(bad.class) )
      stop("Non-character vector in Datastore index row")
    }

    # Using 'do.call' turns each element of the splitGroupTableName list into one argument for rbind.data.frame
    # By contrast, calling rbind.data.frame(splitGroupTableName) simply converts the list (a single argument) into a
    # data.frame (so each element becomes one column) Explanation:
    # https://www.stat.berkeley.edu/~s133/Docall.html
    # message("Adding to output data.frame")
    GroupTableName <- data.frame()
    GroupTableName <- do.call(rbind.data.frame, PathGroupTableName)
    colnames(GroupTableName) <- c("Group", "Table", "Name","Description", "Units","Module","Stage","Path")
    # message("length of output data:",nrow(GroupTableName))

    # GroupTableName is now a data.frame with five columns
    # complete.cases blows away the rows that have any NA values
    # (each row is a "case" in stat lingo, and the "complete" ones have a non-NA value for each
    # column)
    # message("Adding inputs to Inputs data.frame")
    ccases <- complete.cases(GroupTableName)
    GroupTableName <- GroupTableName[ccases,]
    # message("Length of complete.cases:",nrow(GroupTableName))
    Index <- rbind(Index,GroupTableName)
    # message("length of Index:",nrow(Index))
  }
  # message("Attaching ve.inputs attribute to Index")
  self$modelIndex <- Index
  self$modelInputs <- Inputs
  invisible(list(Index=self$modelIndex,Inputs=self$modelInputs))
}

# TODO: change this so the fields are always sought within the
# Selected groups and tables (so with no tables selected, we'll
# get all group/table/field combinations).
ve.output.select <- function( what, details=FALSE ) {
  # interactive utility to select groups, tables or fields
  # 'what' can be "groups","tables" or "fields" (either as strings or names without quotes)
  # 'details' = FALSE (default) will present just the item name
  # 'details' = TRUE will present all items details
  # Interactive dialog will pre-select whatever is already selected (everything if
  #   nothing has been selected yet (either by assignment or earlier
  #   invocation of ve.output.select)
  sub.what <- substitute(what)
  if ( class(sub.what) == "name" ) {
    what <- deparse(sub.what)
  }
  if ( class(what) != "character" ) {
    message("What to select must be 'groups','tables' or 'names'")
    invisible(character(0))
  }
  if ( ! interactive() ) {
    message("VEModel$select(",paste(what,collapse=","),") called from non-interactive session.")
    message("In a script, just assign desired selection to VEModel$groups (or tables or fields)")
    invisible(character(0))
  }
  what <- what[1] # if there's a vector, use the first element
  select.from <- which(c("groups","tables","fields") %in% what)
  select.from <- prepSelect(self,what,details)
  # select.from is a list with two elements:
  #  "names" which is a character vector of names corresponding to "choices" (just the name)
  #  "choices" which are the text lines that appear in the display
  #            (pasted text with name, details)
  #  "selected" which are the subset of the strings in "choices" that are already selected
  if ( is.null(select.from) ) {
    message("Unknown entity to select from:",paste(what,collapse=","))
    invisible(character(0))
  }
  selected <- select.list(choices=select.from$choices,preselect=select.from$selected,multiple=TRUE,
    title=paste("Select",paste(toupper(substring(what,1,1)),substring(what,2),sep=""),sep=" "))
  self[[what]] <- select.from$names[ select.from$choices %in% selected ] # character(0) if none selected => selects all
  invisible(self[[what]]) # print result to see what actually got selected.
}

ve.output.groups <- function(groups) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxGroups <- unique(self$modelIndex[,c("Group","Stage")])
  row.names(idxGroups) <- NULL
  if ( ! missing(groups) ) {
    years <- ( tolower(groups) %in% c("years","year") ) # magic shortcut
    if ( any(years) ) {
      # Expand literal "Years" into all the year-like groups (name is exactly 4 digits)
      groups <- c( groups[!years], grep("^[[:digit:]]{4}$",idxGroups$Group,value=TRUE) )
    }
    if ( is.character(groups) && length(groups)>0 ) {
      self$groupsSelected <- groups[ groups %in% idxGroups$Group ]
    } else {
      self$groupsSelected <- character(0)
    }
  }
  if ( length(self$groupsSelected)==0 ) {
    idxGroups$Selected <- "Yes"
  } else {
    idxGroups$Selected <- ifelse(idxGroups$Group %in% self$groupsSelected,"Yes","No")
  }
  return(idxGroups)
}

ve.group.selected <- function(test.group,groups) {
  return( test.group %in% groups$Group[groups$Selected=="Yes"] )
}

ve.output.tables <- function(tables) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxTables <- unique(self$modelIndex[,c("Group","Table","Stage")])
  row.names(idxTables) <- NULL
  if ( ! missing(tables) ) {
    if ( is.character(tables) && length(tables)>0 ) {
      self$tablesSelected <- tables[ tables %in% idxTables$Table ]
    } else {
      self$tablesSelected <- character(0)
    }
  }
  group.selected <- ve.group.selected(idxTables$Group,self$groups)
  if ( length(self$tablesSelected)==0 ) {
    idxTables$Selected <- ifelse( group.selected, "Yes", "No (!Group)" )
  } else {
    idxTables$Selected <- ifelse(
      idxTables$Table %in% self$tablesSelected,
      ifelse( group.selected,
        "Yes","No (!Group)"
      ),
      "No")
  }
  return(idxTables)
}

ve.table.selected <- function(test.table,tables) {
  return ( test.table %in% tables$Table[tables$Selected=="Yes"] )
}

ve.output.fields <- function(fields) {
  # extract fields from the index where groups and tables are selected
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  idxFields <- self$modelIndex[,c("Group","Table","Name","Stage")]
  row.names(idxFields) <- NULL
  if ( ! missing(fields) ) {
    if ( is.character(fields) && length(fields)>0 ) {
      self$fieldsSelected <- fields[ fields %in% idxFields$Name ]
    } else {
      self$fieldsSelected <- character(0)
    }
  }
  table.selected <- ve.table.selected(idxFields$Table,self$tables)
  group.selected <- ve.group.selected(idxFields$Group,self$groups)
  tg.selected <- table.selected & group.selected
  if ( length(self$fieldsSelected)==0 ) {
    idxFields$Selected <- ifelse( tg.selected, "Yes", "No (!Table)" )
  } else {
    idxFields$Selected <- ifelse(
      idxFields$Name %in% self$fieldsSelected,
      ifelse( tg.selected,
        "Yes","No (!Table)"
      ),
      "No")
  }
  return(idxFields)
}

ve.field.selected <- function(test.field,fields) {
  return ( test.field %in% fields$Name[fields$Selected=="Yes"] )
}

ve.output.list <- function(selected=TRUE, pattern="", details=FALSE) {
  # Show details about model fields
  # selected = TRUE shows just the selected fields
  # selected = FALSE shows all fields (not just unselected)
  # pattern matches (case-insensitive regexp) some portion of field name
  # details = TRUE returns a data.frame self$modelIndex (units, description)
  # detail = FALSE returns just the "Name" vector from self$modelIndex
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  filter <- if ( missing(selected) || selected ) {
    self$fields$Selected=="Yes"
  } else {
    rep(TRUE,nrow(self$modelIndex))
  }
  if ( ! missing(pattern) && is.character(pattern) && nzchar(pattern) ) {
    filter <- filter & grepl(pattern,self$modelIndex$Name,ignore.case=TRUE )
  }
  if ( missing(details) || ! details ) {
    ret.fields <- c("Name")
  } else {
    ret.fields <- names(self$modelIndex)
  }
  ret.value <- self$modelIndex[ filter, ret.fields, drop=TRUE ]
  if ( class(ret.value)!='character' ) ret.value <- ret.value[order(ret.value$Stage, ret.value$Group, ret.value$Name),]
  return(unique(ret.value))
}

ve.output.inputs <- function( fields=FALSE, module="", filename="" ) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  if ( ! missing(fields) && fields ) {
    ret.fields <- c("File","Name","Description","Units","Module","Stage","Path")
  } else {
    ret.fields <- c("Module","File","Stage","Path")
  }

  filter <- rep(TRUE,nrow(self$modelInputs))
  if ( !missing(module) && nzchar(module) ) {
    filter <- filter & grepl(module,self$modelInputs$Module)
  }
  if ( !missing(filename) && nzchar(filename) ) {
    filter <- filter & grepl(filename,self$modelInputs$File)
  }

  ret.value <- unique(self$modelInputs[ filter, ret.fields ])
  return( ret.value[order(ret.value$Stage,ret.value$File),] )
}

ve.output.units <- function() {
  NULL
}

# Build data.frames based on selected groups, tables and dataset names
ve.output.extract <- function(
  stage=NULL,
  saveTo="output",
  overwrite=FALSE,
  quiet=FALSE
) {
  if ( ! all(file.exists(file.path(self$modelPath,"ModelState.Rda"))) ) {
    stop("Model has not been run yet.")
  }
  if ( is.null(stage) ) stage <- self$stageCount # Last one should have everything
  saving <- is.character(saveTo) && nzchar(saveTo)[1]
  
  visioneval::assignDatastoreFunctions(self$runParams$DatastoreType)
  fields <- ( self$fields )

  extract <- fields[ ( fields$Selected=="Yes" & fields$Stage==stage ) ,c("Name","Table","Group","Stage")]

  tables <- split( extract$Name, list(extract$Table,extract$Group,extract$Stage) )
  tables <- tables[which(sapply(tables,length)!=0)]
  DataSpecs <- lapply( names(tables), function(T.G.S) {
        TGS <- unlist(strsplit(T.G.S,"\\."))
        stage <- as.integer(TGS[3])
        mp <- self$modelPath[stage]
        ms <- self$modelState[[stage]]
        dstoreloc <- file.path(mp,ms$DatastoreName)
        df <- data.frame(
          Name  = tables[[T.G.S]],
          Table = TGS[1],
          Group = TGS[2],
          Loc   = dstoreloc
        )
        list(
          Data=df,
          File=paste0(paste(gsub("\\.","_",T.G.S),format(ms$LastChanged,"%Y-%m-%d_%H%M%S"),sep="_"),".csv"),
          Stage=stage
        )
      }
    )
  results <- lapply(DataSpecs, function(d) {
        if (!quiet && saving ) message("Extracting data for Table ",d$Data$Table[1]," in Group ",d$Data$Group[1])
        # Do this in a for-loop rather than faster "apply" to avoid dimension and class/type problems.
        # TODO: make sure this works for earlier stages where not all fields will be defined...
        ds.ext <- list()
        for ( fld in 1:nrow(d$Data) ) {
          dt <- d$Data[fld,]
          ds.ext[[dt$Name]] <- readFromTable(Name=dt$Name,Table=dt$Table,Group=dt$Group,DstoreLoc=dt$Loc,ReadAttr=FALSE)
        }
        return( data.frame(ds.ext) )
      }
    )
  files <- sapply(DataSpecs, function(x) x$File)
  stages <- sapply(DataSpecs, function(x) x$Stage)
  names(results) <- files
  if ( saving ) {
    mapply(
      names(results),
      stages,
      FUN=function(f,s) {
        data <- results[[f]]
        out.path <- file.path(self$modelPath[s],saveTo)
        if ( ! dir.exists(out.path) ) dir.create(out.path,recursive=TRUE)
        fn <- file.path(out.path,f)
        write.csv(data,file=fn)
        if (!quiet) message("Write output file: ",gsub(ve.runtime,"",fn))
      }
    )
  } else {
    names(results) <- sub("\\.[^.]*$","",names(results))
    if (!quiet) message("Returning extracted data as invisible list of data.frames\n(quiet=TRUE to suppress this message)")
  }
  invisible(results)
}

ve.output.print <- function() {
  # Update for output
  cat("Model:",self$modelName,"\n")
  cat("Path:\n")
  print(self$modelPath)
  cat("Datastore Type:",self$runParams$DatastoreType,"\n")
  cat("Status:", self$status,"\n")
  self$status
}

ve.output.query <- function() {
  if ( ! private$queryObject ) {
    query <- VEQuery$new() # parameters TBD
    if ( query.isValid() ) {
      private$queryObject <- query
    } else {
      private$queryObject <- NULL
    }
  }
  return(private$queryObject)
}

# Here is the VEOutput R6 class
# One of these is constructed by VEModel$output

VEOutput <- R6::R6Class(
  "VEOutput",
  public = list(
    initialize=ve.init.output,
    model=NULL,                     # Back-reference to the VEModel for this output
    select=ve.output.select,
    extract=ve.output.extract,
    list=ve.output.list,
    search=ve.output.list,
    inputs=ve.output.inputs,
    print=ve.output.print,
    units=ve.output.units,          # Set units on field list (modifies private$modelIndex)
    query=ve.output.query           # Create a VEQuery object from named query file, or menu
  ),
  active = list(
    groups=ve.output.groups,
    tables=ve.output.tables,
    fields=ve.output.fields
  ),
  private = list(
    queryObject=NULL,               # object to manage queries for this output
    outputPath=NULL,                # root for extract
    modelInputs=NULL,
    modelIndex=NULL,
    ModelState=NULL,
    groupsSelected=character(0),
    tablesSelected=character(0),
    fieldsSelected=character(0),
    index=ve.output.index
  )
)
