# Integration layer for optimized emuR database configuration



if (!exists("database.schema.suffix")) {
  emuDB.suffix = '_emuDB'
  session.suffix = '_ses'
  bundle.dir.suffix = '_bndl'
  bundle.annotation.suffix = '_annot'
  database.schema.suffix = '_DBconfig.json'
  database.cache.suffix = '_emuDBcache.sqlite'
}

# Cache for frequently accessed configurations
.dbConfigCache <- new.env(parent = emptyenv())

###########################################
# Optimized DBconfig file handling

#' Load database configuration
#'
#' @param obj Either a character (basePath) or corpus object
#' @return List with database configuration
#' @keywords internal
#' @noRd
load_DBconfig <- function(obj) {
  # Determine basePath based on input type
  if (is.character(obj)) {
    basePath <- obj
  } else if (S7::S7_inherits(obj, corpus)) {
    # S7 corpus class
    basePath <- obj@basePath
  } else if (is.list(obj) && !is.null(obj$basePath)) {
    # emuDBhandle
    basePath <- obj$basePath
  } else if (is.list(obj) && !is.null(obj@basePath)) {
    basePath <- obj@basePath
  } else {
    cli::cli_abort("obj must be a basePath (character) or corpus/emuDBhandle object")
  }
  
  # Find the DBconfig file
  config_files <- list.files(basePath, pattern = "_DBconfig\\.json$", full.names = TRUE)
  if(length(config_files) == 0) {
    cli::cli_abort("No DBconfig file found in {.path {basePath}}")
  }
  dbCfgPath <- config_files[1]
  
  # Check cache first
  cache_key <- dbCfgPath
  if(exists(cache_key, .dbConfigCache, inherits = FALSE)) {
    cached <- .dbConfigCache[[cache_key]]
    # Check if file has been modified
    if(file.mtime(dbCfgPath) <= attr(cached, "mtime")) {
      return(cached)
    }
  }

  if(!file.exists(dbCfgPath)) {
    cli::cli_abort("{.path {dbCfgPath}} does not exist. Check 'name' entry in DBconfig file.")
  }

  # Read the config
  DBconfig <- read_json_fast(dbCfgPath, simplifyVector = FALSE)

  # Validate against schema (soft-warn unless reindeer.schema_strict is TRUE)
  .validate_against_schema(DBconfig, "dbconfig.schema.json", file_path = dbCfgPath)

  # Cache the config
  attr(DBconfig, "mtime") <- file.mtime(dbCfgPath)
  .dbConfigCache[[cache_key]] <- DBconfig

  DBconfig
}

#####################################################
# Helper functions with optimizations

get_levelNameForAttributeName <- function(corpusObj, attributeName) {
  DBconfig <- load_DBconfig(corpusObj)

  # Vectorized search through level definitions
  for(lvlD in DBconfig$levelDefinitions) {
    # Extract all attribute names at once
    aNames <- vapply(lvlD$attributeDefinitions, `[[`, character(1), "name")
    if(attributeName %in% aNames) {
      return(lvlD$name)
    }
  }
  return(NULL)
}

get_allAttributeNames <- function(corpusObj) {
  DBconfig <- load_DBconfig(corpusObj)

  # Vectorized extraction of all attribute names
  unlist(lapply(DBconfig$levelDefinitions, function(lvlD) {
    vapply(lvlD$attributeDefinitions, `[[`, character(1), "name")
  }))
}

get_linkLevelChildrenNames <- function(schema, superlevelName) {
  # Vectorized extraction
  links <- schema$linkDefinitions
  if(length(links) == 0) return(character(0))

  # Extract matching sublevel names efficiently
  superlevels <- vapply(links, `[[`, character(1), "superlevelName")
  sublevels <- vapply(links, `[[`, character(1), "sublevelName")
  sublevels[superlevels == superlevelName]
}

# Optimized path expansion using iteration instead of recursion
expand_linkPath <- function(p) {
  pLen <- length(p)
  if(pLen == 1) return(list())

  # Pre-allocate result list
  expPath <- vector("list", pLen - 1)
  for(i in seq_len(pLen - 1)) {
    expPath[[i]] <- p[seq_len(pLen - i + 1)]
  }
  expPath
}

# Optimized hierarchy path building
build_allHierarchyPaths <- function(schema) {
  # Use hash table for unique paths
  pathSet <- new.env(parent = emptyenv())

  for(ld in schema$levelDefinitions) {
    pathes <- build_sublevelPathes(schema, ld$name)
    for(p in pathes) {
      expanded <- expand_linkPath(p)
      for(exp_p in expanded) {
        # Use path as key for uniqueness
        key <- paste(exp_p, collapse = "|")
        pathSet[[key]] <- exp_p
      }
    }
  }

  # Convert back to list
  as.list(pathSet)
}

# Optimized recursive path building with memoization
build_sublevelPathes <- local({
  memo <- new.env(parent = emptyenv())

  function(DBconfig, levelName) {
    # Check memo cache
    key <- paste(levelName, collapse = "|")
    if(exists(key, memo)) {
      return(memo[[key]])
    }

    chNames <- get_linkLevelChildrenNames(DBconfig, levelName)

    if(length(chNames) == 0) {
      pathes <- list(levelName)
    } else {
      pathes <- list()
      for(chName in chNames) {
        chPathes <- build_sublevelPathes(DBconfig, chName)
        # Pre-allocate and vectorize path construction
        for(chPath in chPathes) {
          pathes[[length(pathes) + 1L]] <- c(levelName, chPath)
        }
      }
    }

    # Cache result
    memo[[key]] <- pathes
    pathes
  }
})

build_levelPathes <- function(corpusObj) {
  DBconfig <- load_DBconfig(corpusObj)

  # Pre-allocate list and use unlist
  do.call(c, lapply(DBconfig$levelDefinitions, function(l) {
    build_sublevelPathes(DBconfig, l$name)
  }))
}

get_hierPathsConnectingLevels <- function(corpusObj, levelName1, levelName2) {
  allHierPaths <- build_allHierarchyPaths(load_DBconfig(corpusObj))

  # Vectorized filtering
  conHierPaths <- list()

  for(p in allHierPaths) {
    pFirst <- p[1]
    pLast <- p[length(p)]

    # Check both directions at once
    if((pFirst == levelName1 && pLast == levelName2) ||
       (pFirst == levelName2 && pLast == levelName1)) {
      conHierPaths[[length(conHierPaths) + 1]] <- p
    }
  }

  conHierPaths
}

build_extLinkDefinitions <- function(corpusObj) {
  pathes <- build_levelPathes(corpusObj)
  lds <- list()

  for(p in pathes) {
    pLen <- length(p)
    # Vectorized inner loop
    for(i in seq_len(pLen)) {
      lds[[length(lds) + 1L]] <- p[i:pLen]
    }
  }
  lds
}

find_segmentLevels <- function(corpusObj, attrName) {
  lvlNm <- get_levelNameForAttributeName(corpusObj, attrName)
  extLnkDefs <- build_extLinkDefinitions(corpusObj)

  # Use set for unique values
  segLvlSet <- character(0)

  for(extLnkDef in extLnkDefs) {
    if(extLnkDef[1] == lvlNm && length(extLnkDef) > 1) {
      # Vectorized type checking
      for(trgLvlNm in extLnkDef[-1]) {
        trgLd <- get_levelDefinition(corpusObj, trgLvlNm)
        if(!is.null(trgLd) && trgLd$type == 'SEGMENT') {
          segLvlSet <- unique(c(segLvlSet, trgLvlNm))
        }
      }
    }
  }

  segLvlSet
}

# Optimized level definition lookup using indexing
get_levelDefinition <- function(corpusObj, name) {
  DBconfig <- load_DBconfig(corpusObj)

  # Early return with vectorized search
  for(ld in DBconfig$levelDefinitions) {
    if(ld$name == name) return(ld)
  }
  NULL
}

# Store function with cache invalidation
#' Store database configuration
#' @param obj corpus object or emuDBhandle
#' @param dbConfig configuration list
#' @param basePath optional basePath override
#' @keywords internal
#' @noRd
store_DBconfig <- function(obj, dbConfig, basePath = NULL) {
  # Extract basePath and dbName
  if (is.null(basePath)) {
    if (S7::S7_inherits(obj, corpus)) {
      basePath <- obj@basePath
      dbName <- obj@dbName
    } else if (is.list(obj) && !is.null(obj$basePath)) {
      basePath <- obj$basePath
      dbName <- obj$dbName
    } else {
      cli::cli_abort("Cannot extract basePath from obj")
    }
  } else {
    # Extract dbName from basePath
    dbName <- sub("_emuDB$", "", basename(basePath))
  }

  dbCfgPath <- file.path(basePath, paste0(dbName, database.schema.suffix))

  # Use more efficient JSON writing
  # null = "null": a bare R NULL (e.g. a generator$package/$version left NA
  # by an unresolvable DSP function, round-tripped through
  # read_json_fast(..., simplifyVector = FALSE) as a real NULL) must
  # re-serialize as JSON `null`, not jsonlite's default `{}` empty-object
  # stand-in for NULL - the schema declares those fields `["string","null"]`.
  json <- jsonlite::toJSON(dbConfig, auto_unbox = TRUE,
                           force = TRUE, pretty = TRUE, null = "null")

  # Validate before writing - write paths are always strict
  .validate_against_schema(as.character(json), "dbconfig.schema.json",
                           file_path = dbCfgPath, write = TRUE)

  writeLines(json, dbCfgPath, useBytes = TRUE)

  # Invalidate cache
  cache_key <- dbCfgPath
  if(exists(cache_key, .dbConfigCache)) {
    rm(list = cache_key, envir = .dbConfigCache)
  }
}

# ==============================================================================
# TEST HELPERS (internal)
# ==============================================================================

#' Create the ae demo database for testing
#'
#' Creates the emuR demo database in a temp directory and returns its path.
#' Used internally by tests and examples.
#'
#' @param verbose Logical; show progress
#' @return Character path to the ae_emuDB directory
#' @keywords internal
#' @noRd
create_ae_db <- function(verbose = FALSE) {
  tmp_dir <- tempfile("reindeer_ae_")
  dir.create(tmp_dir, recursive = TRUE)
  if (verbose) {
    emuR::create_emuRdemoData(dir = tmp_dir)
  } else {
    suppressMessages(emuR::create_emuRdemoData(dir = tmp_dir))
  }
  file.path(tmp_dir, "emuR_demoData", "ae_emuDB")
}
