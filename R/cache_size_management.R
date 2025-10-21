# ==============================================================================
# CACHE SIZE MONITORING AND MANAGEMENT
# ==============================================================================
#
# Unified infrastructure for monitoring and managing cache sizes across:
# - Persistent quantify/enrich caches
# - Simulation result caches
# - Draft annotation caches
#
# Provides warnings when caches become large and utilities for cleanup.

# ==============================================================================
# SIZE UTILITIES
# ==============================================================================

#' Format bytes as human-readable size
#'
#' @param bytes Number of bytes
#' @return Character string with human-readable size
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' format_bytes(1024)           # "1.00 KB"
#' format_bytes(1048576)        # "1.00 MB"
#' format_bytes(1073741824)     # "1.00 GB"
#' }
format_bytes <- function(bytes) {
  if (is.na(bytes) || bytes == 0) {
    return("0 B")
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  unit_idx <- floor(log(bytes, 1024)) + 1
  unit_idx <- min(unit_idx, length(units))

  size <- bytes / (1024 ^ (unit_idx - 1))

  sprintf("%.2f %s", size, units[unit_idx])
}

#' Parse human-readable size to bytes
#'
#' @param size_string Character string like "500 MB", "2 GB", or "500MB"
#' @return Number of bytes (unnamed numeric)
#' @keywords internal
parse_size_string <- function(size_string) {
  size_string <- trimws(toupper(size_string))

  # Extract number and unit (handle with or without space)
  if (grepl("\\s", size_string)) {
    parts <- strsplit(size_string, "\\s+")[[1]]
  } else {
    # No space - split number from unit
    match <- regexpr("[A-Z]+$", size_string)
    if (match == -1) {
      stop("Invalid size format. Use format like '500 MB' or '2 GB'")
    }
    number_part <- substr(size_string, 1, match - 1)
    unit_part <- substr(size_string, match, nchar(size_string))
    parts <- c(number_part, unit_part)
  }

  if (length(parts) != 2) {
    stop("Invalid size format. Use format like '500 MB' or '2 GB'")
  }

  number <- as.numeric(parts[1])
  unit <- parts[2]

  multipliers <- c(
    "B" = 1,
    "KB" = 1024,
    "MB" = 1024^2,
    "GB" = 1024^3,
    "TB" = 1024^4
  )

  if (!unit %in% names(multipliers)) {
    stop("Unknown unit: ", unit, ". Use B, KB, MB, GB, or TB")
  }

  unname(number * multipliers[unit])
}

#' Get file size
#'
#' @param path Path to file or directory
#' @param recursive If TRUE and path is directory, get total size recursively
#' @return Size in bytes
#' @keywords internal
get_file_size <- function(path, recursive = FALSE) {
  if (!file.exists(path)) {
    return(0)
  }

  if (dir.exists(path)) {
    if (recursive) {
      files <- list.files(path, full.names = TRUE, recursive = TRUE,
                          include.dirs = FALSE)
      if (length(files) == 0) return(0)
      sum(file.info(files)$size, na.rm = TRUE)
    } else {
      files <- list.files(path, full.names = TRUE, recursive = FALSE)
      sum(file.info(files)$size, na.rm = TRUE)
    }
  } else {
    file.info(path)$size
  }
}

# ==============================================================================
# CACHE SIZE CHECKING
# ==============================================================================

#' Check cache size and warn if large
#'
#' @param cache_path Path to cache file or directory
#' @param cache_type Type of cache ("quantify", "simulation", "draft", "general")
#' @param warn_threshold Warning threshold (bytes or size string like "500 MB")
#' @param max_threshold Maximum threshold (bytes or size string)
#' @param verbose Show informational messages
#' @return List with size info and whether thresholds exceeded
#' @keywords internal
check_cache_size <- function(cache_path,
                              cache_type = "general",
                              warn_threshold = "500 MB",
                              max_threshold = "2 GB",
                              verbose = TRUE) {

  # Parse thresholds if strings
  if (is.character(warn_threshold)) {
    warn_threshold <- parse_size_string(warn_threshold)
  }
  if (is.character(max_threshold)) {
    max_threshold <- parse_size_string(max_threshold)
  }

  # Get cache size
  cache_size <- get_file_size(cache_path, recursive = TRUE)

  # Determine status
  warn_exceeded <- cache_size >= warn_threshold
  max_exceeded <- cache_size >= max_threshold

  result <- list(
    path = cache_path,
    size_bytes = cache_size,
    size_formatted = format_bytes(cache_size),
    warn_threshold = warn_threshold,
    max_threshold = max_threshold,
    warn_exceeded = warn_exceeded,
    max_exceeded = max_exceeded,
    cache_type = cache_type
  )

  # Issue warnings if needed
  if (verbose) {
    if (max_exceeded) {
      cli::cli_warn(c(
        "{.strong Cache size critical}: {cache_type} cache is very large",
        "i" = "Current size: {.val {result$size_formatted}}",
        "i" = "Maximum threshold: {.val {format_bytes(max_threshold)}}",
        "!" = "Consider cleaning up old cache files with {.code clean_cache()}"
      ))
    } else if (warn_exceeded) {
      cli::cli_alert_warning(
        "{cache_type} cache is large: {result$size_formatted} (threshold: {format_bytes(warn_threshold)})"
      )
    }
  }

  invisible(result)
}

#' Get cache directory size summary
#'
#' @param cache_dir Cache directory path
#' @param pattern File pattern to match (e.g., "\\.sqlite$")
#' @return Data frame with file sizes
#' @keywords internal
get_cache_dir_summary <- function(cache_dir, pattern = NULL) {

  if (!dir.exists(cache_dir)) {
    return(data.frame(
      file = character(0),
      size_bytes = numeric(0),
      size_formatted = character(0),
      modified = character(0),
      stringsAsFactors = FALSE
    ))
  }

  files <- if (is.null(pattern)) {
    list.files(cache_dir, full.names = TRUE, recursive = FALSE)
  } else {
    list.files(cache_dir, pattern = pattern, full.names = TRUE, recursive = FALSE)
  }

  if (length(files) == 0) {
    return(data.frame(
      file = character(0),
      size_bytes = numeric(0),
      size_formatted = character(0),
      modified = character(0),
      stringsAsFactors = FALSE
    ))
  }

  file_info <- file.info(files)

  data.frame(
    file = basename(files),
    path = files,
    size_bytes = file_info$size,
    size_formatted = sapply(file_info$size, format_bytes),
    modified = as.character(file_info$mtime),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# CACHE-SPECIFIC SIZE CHECKS
# ==============================================================================

#' Check quantify/enrich persistent cache size
#'
#' @param corpus_obj A corpus object
#' @param warn_threshold Warning threshold (default: 500 MB)
#' @param max_threshold Maximum threshold (default: 2 GB)
#' @param verbose Show messages
#' @return Cache size information
#' @export
check_quantify_cache_size <- function(corpus_obj,
                                       warn_threshold = "500 MB",
                                       max_threshold = "2 GB",
                                       verbose = TRUE) {

  cache_dir <- get_quantify_cache_dir(corpus_obj)

  check_cache_size(
    cache_dir,
    cache_type = "Quantify/enrich",
    warn_threshold = warn_threshold,
    max_threshold = max_threshold,
    verbose = verbose
  )
}

#' Check simulation cache sizes
#'
#' @param simulation_store Simulation store directory
#' @param warn_threshold Warning threshold (default: 1 GB)
#' @param max_threshold Maximum threshold (default: 5 GB)
#' @param verbose Show messages
#' @return Cache size information
#' @export
check_simulation_cache_size <- function(simulation_store,
                                         warn_threshold = "1 GB",
                                         max_threshold = "5 GB",
                                         verbose = TRUE) {

  if (!dir.exists(simulation_store)) {
    if (verbose) {
      cli::cli_alert_info("Simulation store directory does not exist")
    }
    return(invisible(NULL))
  }

  check_cache_size(
    simulation_store,
    cache_type = "Simulation",
    warn_threshold = warn_threshold,
    max_threshold = max_threshold,
    verbose = verbose
  )
}

#' Check draft annotation cache sizes
#'
#' @param corpus_obj A corpus object
#' @param warn_threshold Warning threshold (default: 500 MB)
#' @param max_threshold Maximum threshold (default: 2 GB)
#' @param verbose Show messages
#' @return Cache size information
#' @export
check_draft_cache_size <- function(corpus_obj,
                                    warn_threshold = "500 MB",
                                    max_threshold = "2 GB",
                                    verbose = TRUE) {

  cache_dir <- get_draft_cache_dir(corpus_obj)

  check_cache_size(
    cache_dir,
    cache_type = "Draft annotation",
    warn_threshold = warn_threshold,
    max_threshold = max_threshold,
    verbose = verbose
  )
}

#' Check all cache sizes for a corpus
#'
#' @param corpus_obj A corpus object
#' @param simulation_store Optional simulation store directory
#' @param verbose Show messages
#' @return List with all cache sizes
#' @export
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/db_emuDB")
#' check_all_cache_sizes(corp)
#' }
check_all_cache_sizes <- function(corpus_obj,
                                   simulation_store = NULL,
                                   verbose = TRUE) {

  if (verbose) {
    cli::cli_h2("Cache Size Summary")
  }

  # Check quantify cache
  quantify_info <- check_quantify_cache_size(corpus_obj, verbose = verbose)

  # Check draft cache
  draft_info <- check_draft_cache_size(corpus_obj, verbose = verbose)

  # Check simulation cache if provided
  simulation_info <- if (!is.null(simulation_store)) {
    check_simulation_cache_size(simulation_store, verbose = verbose)
  } else {
    NULL
  }

  # Calculate total
  total_size <- quantify_info$size_bytes + draft_info$size_bytes +
    (simulation_info$size_bytes %||% 0)

  if (verbose) {
    cli::cli_alert_info("Total cache size: {.val {format_bytes(total_size)}}")
  }

  invisible(list(
    quantify = quantify_info,
    draft = draft_info,
    simulation = simulation_info,
    total_bytes = total_size,
    total_formatted = format_bytes(total_size)
  ))
}

# ==============================================================================
# CACHE CLEANUP UTILITIES
# ==============================================================================

#' Remove old cache files by age
#'
#' @param cache_dir Cache directory
#' @param days_old Remove files older than this many days
#' @param pattern File pattern to match
#' @param dry_run If TRUE, only show what would be deleted
#' @param verbose Show messages
#' @return Number of files deleted
#' @keywords internal
remove_old_cache_files <- function(cache_dir,
                                    days_old = 30,
                                    pattern = "\\.sqlite$",
                                    dry_run = FALSE,
                                    verbose = TRUE) {

  if (!dir.exists(cache_dir)) {
    if (verbose) {
      cli::cli_alert_info("Cache directory does not exist")
    }
    return(0)
  }

  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    if (verbose) {
      cli::cli_alert_info("No cache files found")
    }
    return(0)
  }

  # Get file ages
  file_info <- file.info(files)
  file_ages <- as.numeric(Sys.time() - file_info$mtime, units = "days")

  old_files <- files[file_ages > days_old]

  if (length(old_files) == 0) {
    if (verbose) {
      cli::cli_alert_info("No cache files older than {days_old} day{?s}")
    }
    return(0)
  }

  # Calculate space to be freed
  old_size <- sum(file.info(old_files)$size)

  if (verbose) {
    if (dry_run) {
      cli::cli_alert_info(
        "Would delete {length(old_files)} file{?s} ({format_bytes(old_size)})"
      )
      cli::cli_ul(basename(old_files))
    } else {
      cli::cli_alert_warning(
        "Deleting {length(old_files)} cache file{?s} older than {days_old} day{?s} ({format_bytes(old_size)})"
      )
    }
  }

  if (!dry_run) {
    unlink(old_files)
    if (verbose) {
      cli::cli_alert_success("Deleted {length(old_files)} file{?s}")
    }
  }

  length(old_files)
}

#' Clean quantify/enrich cache
#'
#' @param corpus_obj A corpus object
#' @param days_old Remove files older than this many days (default: 30)
#' @param dry_run If TRUE, only show what would be deleted
#' @param verbose Show messages
#' @return Number of files deleted
#' @export
clean_quantify_cache <- function(corpus_obj,
                                  days_old = 30,
                                  dry_run = FALSE,
                                  verbose = TRUE) {

  cache_dir <- get_quantify_cache_dir(corpus_obj)

  if (verbose) {
    cli::cli_h3("Cleaning quantify/enrich cache")
  }

  remove_old_cache_files(cache_dir, days_old, pattern = "\\.rds$|\\.qs$",
                          dry_run = dry_run, verbose = verbose)
}

#' Clean draft annotation cache
#'
#' @param corpus_obj A corpus object
#' @param days_old Remove files older than this many days (default: 30)
#' @param dry_run If TRUE, only show what would be deleted
#' @param verbose Show messages
#' @return Number of files deleted
#' @export
clean_draft_cache <- function(corpus_obj,
                               days_old = 30,
                               dry_run = FALSE,
                               verbose = TRUE) {

  cache_dir <- get_draft_cache_dir(corpus_obj)

  if (verbose) {
    cli::cli_h3("Cleaning draft annotation cache")
  }

  remove_old_cache_files(cache_dir, days_old, pattern = "\\.sqlite$",
                          dry_run = dry_run, verbose = verbose)
}

#' Clean simulation cache
#'
#' @param simulation_store Simulation store directory
#' @param days_old Remove files older than this many days (default: 30)
#' @param dry_run If TRUE, only show what would be deleted
#' @param verbose Show messages
#' @return Number of files deleted
#' @export
clean_simulation_cache <- function(simulation_store,
                                    days_old = 30,
                                    dry_run = FALSE,
                                    verbose = TRUE) {

  if (verbose) {
    cli::cli_h3("Cleaning simulation cache")
  }

  remove_old_cache_files(simulation_store, days_old, pattern = "\\.sqlite$",
                          dry_run = dry_run, verbose = verbose)
}

#' Clean all caches for a corpus
#'
#' @param corpus_obj A corpus object
#' @param simulation_store Optional simulation store directory
#' @param days_old Remove files older than this many days (default: 30)
#' @param dry_run If TRUE, only show what would be deleted
#' @param verbose Show messages
#' @return Named list with number of files deleted from each cache
#' @export
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/db_emuDB")
#'
#' # Dry run - see what would be deleted
#' clean_all_caches(corp, dry_run = TRUE)
#'
#' # Actually delete files older than 30 days
#' clean_all_caches(corp)
#'
#' # Delete files older than 7 days
#' clean_all_caches(corp, days_old = 7)
#' }
clean_all_caches <- function(corpus_obj,
                              simulation_store = NULL,
                              days_old = 30,
                              dry_run = FALSE,
                              verbose = TRUE) {

  if (verbose) {
    cli::cli_h2("Cache Cleanup")
    if (dry_run) {
      cli::cli_alert_info("DRY RUN - no files will be deleted")
    }
  }

  quantify_deleted <- clean_quantify_cache(corpus_obj, days_old, dry_run, verbose)
  draft_deleted <- clean_draft_cache(corpus_obj, days_old, dry_run, verbose)

  simulation_deleted <- if (!is.null(simulation_store)) {
    clean_simulation_cache(simulation_store, days_old, dry_run, verbose)
  } else {
    0
  }

  total <- quantify_deleted + draft_deleted + simulation_deleted

  if (verbose) {
    if (dry_run) {
      cli::cli_alert_info("Would delete {total} file{?s} total")
    } else {
      cli::cli_alert_success("Deleted {total} file{?s} total")
    }
  }

  invisible(list(
    quantify = quantify_deleted,
    draft = draft_deleted,
    simulation = simulation_deleted,
    total = total
  ))
}

#' Get cache directory path for quantify
#'
#' @param corpus_obj A corpus object
#' @return Path to cache directory
#' @keywords internal
get_quantify_cache_dir <- function(corpus_obj) {
  # This should match the location used in quantify/enrich
  file.path(corpus_obj@basePath, ".quantify_cache")
}

#' List cache files with size information
#'
#' @param corpus_obj A corpus object
#' @param cache_type Type of cache: "quantify", "draft", or "all"
#' @return Data frame with cache file information
#' @export
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/db_emuDB")
#' list_cache_files(corp, "draft")
#' list_cache_files(corp, "all")
#' }
list_cache_files <- function(corpus_obj, cache_type = "all") {

  cache_type <- match.arg(cache_type, c("all", "quantify", "draft"))

  results <- list()

  if (cache_type %in% c("all", "quantify")) {
    quantify_dir <- get_quantify_cache_dir(corpus_obj)
    quantify_files <- get_cache_dir_summary(quantify_dir, pattern = "\\.rds$|\\.qs$")
    if (nrow(quantify_files) > 0) {
      quantify_files$type <- "quantify"
      results$quantify <- quantify_files
    }
  }

  if (cache_type %in% c("all", "draft")) {
    draft_dir <- get_draft_cache_dir(corpus_obj)
    draft_files <- get_cache_dir_summary(draft_dir, pattern = "\\.sqlite$")
    if (nrow(draft_files) > 0) {
      draft_files$type <- "draft"
      results$draft <- draft_files
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      file = character(0),
      path = character(0),
      size_bytes = numeric(0),
      size_formatted = character(0),
      modified = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  combined <- do.call(rbind, results)
  rownames(combined) <- NULL

  # Sort by size descending
  combined[order(combined$size_bytes, decreasing = TRUE), ]
}
