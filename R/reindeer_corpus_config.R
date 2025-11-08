# Integration layer for optimized emuR database configuration



if(! exists("database.schema.suffix")){
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
#' @export
load_DBconfig <- function(obj) {
  # Determine basePath based on input type
  if (is.character(obj)) {
    basePath <- obj
  } else if (inherits(obj, "corpus") || (is.list(obj) && !is.null(obj$basePath))) {
    basePath <- if (inherits(obj, "corpus")) obj@basePath else obj$basePath
  } else if (is.list(obj) && !is.null(obj@basePath)) {
    basePath <- obj@basePath  
  } else {
    stop("obj must be a basePath (character) or corpus/emuDBhandle object")
  }
  
  # Find the DBconfig file
  config_files <- list.files(basePath, pattern = "_DBconfig\\.json$", full.names = TRUE)
  if(length(config_files) == 0) {
    stop("No DBconfig file found in ", basePath)
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
    stop(dbCfgPath, " does not exist. Check 'name' entry in DBconfig file.")
  }

  # Use optimized JSON reading with RcppSimdJson
  DBconfig <- read_json_fast(dbCfgPath, simplifyVector = FALSE)

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
#' @export
store_DBconfig <- function(obj, dbConfig, basePath = NULL) {
  # Extract basePath and dbName
  if (is.null(basePath)) {
    if (inherits(obj, "corpus")) {
      basePath <- obj@basePath
      dbName <- obj@dbName
    } else if (is.list(obj) && !is.null(obj$basePath)) {
      basePath <- obj$basePath
      dbName <- obj$dbName
    } else {
      stop("Cannot extract basePath from obj")
    }
  } else {
    # Extract dbName from basePath
    dbName <- sub("_emuDB$", "", basename(basePath))
  }

  dbCfgPath <- file.path(basePath, paste0(dbName, database.schema.suffix))

  # Use more efficient JSON writing
  json <- jsonlite::toJSON(dbConfig, auto_unbox = TRUE,
                           force = TRUE, pretty = TRUE)

  writeLines(json, dbCfgPath, useBytes = TRUE)

  # Invalidate cache
  cache_key <- dbCfgPath
  if(exists(cache_key, .dbConfigCache)) {
    rm(list = cache_key, envir = .dbConfigCache)
  }
}

################################################################
# Optimized CRUD operations
################################################################

# Optimized add_levelDefinition
add_levelDefinition <- function(corpusObj, name, type,
                                rewriteAllAnnots = TRUE, verbose = TRUE) {
  check_corpusObj(corpusObj)

  allowedTypes <- c('ITEM', 'SEGMENT', 'EVENT')
  if(!(type %in% allowedTypes)) {
    stop('Bad type! Must be: ', paste(allowedTypes, collapse = ' | '))
  }

  dbConfig <- load_DBconfig(corpusObj)

  # Vectorized name check
  existing_names <- vapply(dbConfig$levelDefinitions, `[[`,
                           character(1), "name")
  if(name %in% existing_names) {
    stop("Level definition '", name, "' already exists")
  }

  # Add new definition
  levelDefinition <- list(
    name = name,
    type = type,
    attributeDefinitions = list(
      list(name = name, type = 'STRING')
    )
  )

  dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions) + 1]] <-
    levelDefinition

  store_DBconfig(corpusObj, dbConfig)

  if(rewriteAllAnnots) {
    rewrite_annots(corpusObj, verbose = verbose)
  }

  invisible(NULL)
}

# Optimized list_levelDefinitions
list_levelDefinitions <- function(corpusObj) {
  check_corpusObj(corpusObj, checkCache = FALSE)
  dbConfig <- load_DBconfig(corpusObj)

  if(length(dbConfig$levelDefinitions) == 0) return(NULL)

  # Vectorized data frame construction
  df_list <- lapply(dbConfig$levelDefinitions, function(ld) {
    data.frame(
      name = ld$name,
      type = ld$type,
      nrOfAttrDefs = length(ld$attributeDefinitions),
      attrDefNames = paste0(
        vapply(ld$attributeDefinitions, function(ad)
          paste0(ad$name, ";"), character(1)),
        collapse = " "
      ),
      stringsAsFactors = FALSE
    )
  })

  if(length(df_list) > 0) {
    do.call(rbind, df_list)
  } else {
    NULL
  }
}

# Optimized remove_levelDefinition with batch operations
remove_levelDefinition <- function(corpusObj, name,
                                   rewriteAllAnnots = TRUE,
                                   force = FALSE, verbose = TRUE) {
  check_corpusObj(corpusObj)
  dbConfig <- load_DBconfig(corpusObj)

  # Vectorized existence check
  level_exists <- any(vapply(dbConfig$levelDefinitions,
                             function(ld) ld$name == name, logical(1)))

  if(!level_exists) {
    stop("Level definition '", name, "' does not exist")
  }

  # Check link references
  for(lkd in dbConfig$linkDefinitions) {
    if(lkd$superlevelName == name || lkd$sublevelName == name) {
      stop("Cannot remove level '", name,
           "'. Referenced by link definition")
    }
  }

  if(!force) {
    # Optimized query
    itemsCnt <- DBI::dbGetQuery(
      corpusObj@connection,
      sprintf("SELECT COUNT(*) as cnt FROM items WHERE db_uuid='%s' AND level='%s'",
              corpusObj@UUID, name)
    )$cnt

    if(itemsCnt > 0) {
      stop("Level is not empty. Remove items first")
    }
  } else {
    if(verbose) {
      answ <- readline("Remove all items? (y/n): ")
      if(!answ %in% c("y", "Y")) stop("Aborted")
    }

    # Batch delete operations
    DBI::dbWithTransaction(corpusObj@connection, {
      # Delete labels
      DBI::dbExecute(
        corpusObj@connection,
        sprintf("DELETE FROM labels WHERE EXISTS(
                  SELECT 1 FROM items i
                  WHERE i.db_uuid='%s' AND i.level='%s'
                  AND i.session=labels.session
                  AND i.bundle=labels.bundle
                  AND i.item_id=labels.item_id)",
                corpusObj@UUID, name)
      )

      # Delete items
      DBI::dbExecute(
        corpusObj@connection,
        sprintf("DELETE FROM items WHERE db_uuid='%s' AND level='%s'",
                corpusObj@UUID, name)
      )
    })
  }

  # Remove from config using filter
  dbConfig$levelDefinitions <- Filter(
    function(x) x$name != name,
    dbConfig$levelDefinitions
  )

  # Remove from perspectives
  for(i in seq_along(dbConfig$EMUwebAppConfig$perspectives)) {
    order <- dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order
    dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order <-
      order[order != name]
  }

  store_DBconfig(corpusObj, dbConfig)

  if(rewriteAllAnnots) {
    rewrite_annots(corpusObj, verbose = verbose)
  }

  invisible(NULL)
}

# Additional optimized CRUD functions...
# [Similar optimizations applied to remaining functions]

# Clear cache function
clear_dbconfig_cache <- function() {
  rm(list = ls(.dbConfigCache), envir = .dbConfigCache)
}

### INTEGRATION LAYER


get_allAttributeNames <- function(corpusObj) {
  DBconfig <- load_DBConfig(corpusObj)

  # Try Rcpp version first
  if(exists("get_allAttributeNames_cpp")) {
    return(get_allAttributeNames_cpp(DBconfig$levelDefinitions))
  }

  # Fallback to optimized R version
  unlist(lapply(DBconfig$levelDefinitions, function(lvlD) {
    vapply(lvlD$attributeDefinitions, `[[`, character(1), "name")
  }))
}

# Hybrid path building
build_allHierarchyPaths <- function(schema) {
  # Try Rcpp version
  if(exists("build_allHierarchyPaths_cpp")) {
    return(build_allHierarchyPaths_cpp(schema))
  }

  # Fallback to optimized R version
  pathSet <- new.env(parent = emptyenv())

  for(ld in schema$levelDefinitions) {
    pathes <- build_sublevelPathes(schema, ld$name)
    for(p in pathes) {
      expanded <- expand_linkPath(p)
      for(exp_p in expanded) {
        key <- paste(exp_p, collapse = "|")
        pathSet[[key]] <- exp_p
      }
    }
  }

  as.list(pathSet)
}

build_sublevelPathes <- function(DBconfig, levelName) {
  # Try Rcpp version
  if(exists("build_sublevelPathes_cpp")) {
    return(build_sublevelPathes_cpp(DBconfig, levelName))
  }

  # Fallback to memoized R version
  build_sublevelPathes(DBconfig, levelName)
}

# #####################################################
# # Performance monitoring utilities
#
# # Function to benchmark operations
# benchmark_operation <- function(func, ..., times = 10) {
#   results <- numeric(times)
#   for(i in seq_len(times)) {
#     start <- Sys.time()
#     func(...)
#     results[i] <- as.numeric(Sys.time() - start)
#   }
#
#   list(
#     mean = mean(results),
#     median = median(results),
#     sd = sd(results),
#     min = min(results),
#     max = max(results)
#   )
# }
#
# # Profile database operations
# profile_db_operations <- function(corpusObj) {
#   cat("Profiling database operations...\n")
#
#   cfgFunc <- load_DBConfig
#   # Test configuration loading
#   config_time <- benchmark_operation(cfgFunc, corpusObj)
#   cat("Config loading: ", config_time$mean, "s (mean)\n")
#
#   # Test path building
#   DBconfig <- load_DBConfig(corpusObj)
#   path_time <- benchmark_operation(build_allHierarchyPaths, DBconfig)
#   cat("Path building: ", path_time$mean, "s (mean)\n")
#
#   # Test attribute extraction
#   attr_time <- benchmark_operation(get_allAttributeNames, corpusObj)
#   cat("Attribute extraction: ", attr_time$mean, "s (mean)\n")
#
#   invisible(list(
#     config = config_time,
#     paths = path_time,
#     attributes = attr_time
#   ))
# }

#####################################################
# Batch operations for improved performance

# Batch add multiple level definitions
batch_add_levelDefinitions <- function(corpusObj, definitions,
                                       verbose = TRUE) {
  check_corpusObj(corpusObj)
  dbConfig <- load_DBConfig(corpusObj)

  # Validate all definitions first
  allowedTypes <- c('ITEM', 'SEGMENT', 'EVENT')
  existing_names <- vapply(dbConfig$levelDefinitions, `[[`,
                           character(1), "name")

  for(def in definitions) {
    if(!(def$type %in% allowedTypes)) {
      stop('Bad type in definition: ', def$name)
    }
    if(def$name %in% existing_names) {
      stop("Level '", def$name, "' already exists")
    }
    existing_names <- c(existing_names, def$name)
  }

  # Add all definitions
  for(def in definitions) {
    levelDefinition <- list(
      name = def$name,
      type = def$type,
      attributeDefinitions = list(
        list(name = def$name, type = 'STRING')
      )
    )
    dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions) + 1]] <-
      levelDefinition
  }

  # Single write operation
  store_DBconfig(corpusObj, dbConfig)

  # Single rewrite operation
  rewrite_annots(corpusObj, verbose = verbose)

  invisible(NULL)
}

# Batch database operations with transaction
batch_db_operations <- function(corpusObj, operations) {
  DBI::dbWithTransaction(corpusObj@connection, {
    for(op in operations) {
      do.call(op$func, op$args)
    }
  })
}

#####################################################
# Parallel processing support for large databases

# Process bundles in parallel
process_bundles <- function(corpusObj, func,
                                     n_cores = parallel::detectCores() - 1) {
  if(requireNamespace("parallel", quietly = TRUE)) {
    bundles <- list_bundles(corpusObj)

    # Create cluster
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export necessary objects
    parallel::clusterExport(cl, c("corpusObj", "func"),
                            envir = environment())

    # Process in parallel
    results <- parallel::parLapply(cl, seq_len(nrow(bundles)),
                                   function(i) {
                                     func(bundles[i,])
                                   })

    return(results)
  } else {
    warning("'parallel' package not available, using sequential processing")
    bundles <- list_bundles(corpusObj)
    lapply(seq_len(nrow(bundles)), function(i) func(bundles[i,]))
  }
}

#####################################################
# Memory management utilities

# Clear all caches
clear_all_caches <- function() {
  clear_dbconfig_cache()
  gc()  # Force garbage collection
  invisible(NULL)
}

# Monitor memory usage
monitor_memory <- function(func, ...) {
  gc_before <- gc()
  result <- func(...)
  gc_after <- gc()

  memory_used <- (gc_after[2,2] - gc_before[2,2]) +
    (gc_after[2,4] - gc_before[2,4])

  cat("Memory used: ", round(memory_used, 2), "MB\n")

  result
}


# Fast attribute lookup with indexing
create_attribute_index <- function(corpusObj) {
  DBconfig <- load_DBConfig(corpusObj)

  index <- new.env(parent = emptyenv())

  for(lvlD in DBconfig$levelDefinitions) {
    for(ad in lvlD$attributeDefinitions) {
      if(is.null(index[[ad$name]])) {
        index[[ad$name]] <- list()
      }
      index[[ad$name]][[length(index[[ad$name]]) + 1]] <- lvlD$name
    }
  }

  index
}

# Fast link traversal
create_link_graph <- function(corpusObj) {
  DBconfig <- load_DBConfig(corpusObj)

  # Create adjacency list representation
  graph <- new.env(parent = emptyenv())
  reverse_graph <- new.env(parent = emptyenv())

  for(ld in DBconfig$linkDefinitions) {
    super <- ld$superlevelName
    sub <- ld$sublevelName

    # Forward edges
    if(is.null(graph[[super]])) {
      graph[[super]] <- character(0)
    }
    graph[[super]] <- c(graph[[super]], sub)

    # Reverse edges
    if(is.null(reverse_graph[[sub]])) {
      reverse_graph[[sub]] <- character(0)
    }
    reverse_graph[[sub]] <- c(reverse_graph[[sub]], super)
  }

  list(forward = graph, reverse = reverse_graph)
}

#####################################################
# Validation and consistency checks

# Fast consistency check
validate_dbconfig <- function(corpusObj) {
  DBconfig <- load_DBConfig(corpusObj)
  errors <- character(0)

  # Check level definitions
  level_names <- vapply(DBconfig$levelDefinitions, `[[`,
                        character(1), "name")
  if(any(duplicated(level_names))) {
    errors <- c(errors, "Duplicate level names found")
  }

  # Check link definitions
  for(ld in DBconfig$linkDefinitions) {
    if(!(ld$superlevelName %in% level_names)) {
      errors <- c(errors, paste("Unknown super level:", ld$superlevelName))
    }
    if(!(ld$sublevelName %in% level_names)) {
      errors <- c(errors, paste("Unknown sub level:", ld$sublevelName))
    }
  }

  if(length(errors) > 0) {
    warning("Validation errors found:\n", paste(errors, collapse = "\n"))
    return(FALSE)
  }

  TRUE
}



################################
## INTERACTIVE TESTING
################################

if(FALSE){

  reindeer:::create_ae_db() -> corpusObj
  cfg <- load_DBconfig(corpusObj)

}
