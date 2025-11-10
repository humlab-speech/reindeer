#' Cache Management Utilities
#'
#' Functions for managing the quantify() results cache.
#'
#' @name cache_utils
NULL

#' Get cache statistics
#'
#' Returns information about the current cache state including size,
#' number of entries, and format distribution.
#'
#' @param cache_dir Cache directory path. If NULL, uses default location.
#'
#' @return A list with cache statistics
#'
#' @examples
#' \dontrun{
#' stats <- cache_summary()
#' print(stats)
#' }
#'
#' @export
cache_summary <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "reindeer_cache")
  }
  
  cache_file <- file.path(cache_dir, "quantify_cache.sqlite")
  
  if (!file.exists(cache_file)) {
    cli::cli_alert_info("No cache found at {.path {cache_file}}")
    return(invisible(NULL))
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Get summary statistics
  stats <- DBI::dbGetQuery(conn, "
    SELECT 
      COUNT(*) as total_entries,
      SUM(size_bytes) as total_size_bytes,
      format,
      COUNT(*) as format_count
    FROM cache
    GROUP BY format
  ")
  
  total_stats <- DBI::dbGetQuery(conn, "
    SELECT 
      COUNT(*) as total_entries,
      SUM(size_bytes) as total_size_bytes,
      MIN(created_at) as oldest_entry,
      MAX(accessed_at) as most_recent_access
    FROM cache
  ")
  
  result <- list(
    cache_location = cache_file,
    total_entries = total_stats$total_entries,
    total_size_mb = round(total_stats$total_size_bytes / (1024^2), 2),
    format_distribution = stats,
    oldest_entry = as.POSIXct(total_stats$oldest_entry, origin = "1970-01-01"),
    most_recent_access = as.POSIXct(total_stats$most_recent_access, origin = "1970-01-01")
  )
  
  # Print formatted output
  cli::cli_h2("Cache Summary")
  cli::cli_alert_info("Location: {.path {result$cache_location}}")
  cli::cli_alert_info("Total entries: {result$total_entries}")
  cli::cli_alert_info("Total size: {result$total_size_mb} MB")
  
  if (nrow(stats) > 0) {
    cli::cli_h3("Format Distribution")
    for (i in seq_len(nrow(stats))) {
      format_name <- stats$format[i]
      count <- stats$format_count[i]
      pct <- round(100 * count / result$total_entries, 1)
      cli::cli_text("  {format_name}: {count} ({pct}%)")
    }
  }
  
  cli::cli_text("")
  cli::cli_text("Oldest entry: {format(result$oldest_entry, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_text("Most recent access: {format(result$most_recent_access, '%Y-%m-%d %H:%M:%S')}")
  
  invisible(result)
}

#' Clear cache
#'
#' Removes all entries from the cache or entries matching specific criteria.
#'
#' @param cache_dir Cache directory path. If NULL, uses default location.
#' @param older_than Remove entries older than this many days (optional)
#' @param format Remove entries of specific format: "qs" or "rds" (optional)
#'
#' @examples
#' \dontrun{
#' # Clear entire cache
#' clear_cache()
#' 
#' # Clear entries older than 30 days
#' clear_cache(older_than = 30)
#' 
#' # Clear only RDS format entries
#' clear_cache(format = "rds")
#' }
#'
#' @export
clear_cache <- function(cache_dir = NULL, older_than = NULL, format = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "reindeer_cache")
  }
  
  cache_file <- file.path(cache_dir, "quantify_cache.sqlite")
  
  if (!file.exists(cache_file)) {
    cli::cli_alert_info("No cache found at {.path {cache_file}}")
    return(invisible(NULL))
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Build query
  where_clauses <- character()
  
  if (!is.null(older_than)) {
    cutoff_time <- as.integer(Sys.time()) - (older_than * 24 * 60 * 60)
    where_clauses <- c(where_clauses, sprintf("accessed_at < %d", cutoff_time))
  }
  
  if (!is.null(format)) {
    where_clauses <- c(where_clauses, sprintf("format = '%s'", format))
  }
  
  where_sql <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }
  
  # Count entries to be deleted
  count_query <- sprintf("SELECT COUNT(*) as count FROM cache %s", where_sql)
  to_delete <- DBI::dbGetQuery(conn, count_query)$count
  
  if (to_delete == 0) {
    cli::cli_alert_info("No entries match the criteria")
    return(invisible(0))
  }
  
  # Delete entries
  delete_query <- sprintf("DELETE FROM cache %s", where_sql)
  DBI::dbExecute(conn, delete_query)
  
  cli::cli_alert_success("Removed {to_delete} cache entr{?y/ies}")
  
  invisible(to_delete)
}

#' Convert cache format
#'
#' Converts cache entries from RDS to qs format for improved performance.
#' This requires the qs package to be installed.
#'
#' @param cache_dir Cache directory path. If NULL, uses default location.
#' @param from_format Source format to convert from (default: "rds")
#' @param to_format Target format to convert to (default: "qs")
#' @param batch_size Number of entries to convert at once (default: 100)
#'
#' @examples
#' \dontrun{
#' # Convert all RDS entries to qs
#' convert_cache_format()
#' 
#' # Convert with smaller batch size
#' convert_cache_format(batch_size = 50)
#' }
#'
#' @export
convert_cache_format <- function(cache_dir = NULL, 
                                  from_format = "rds",
                                  to_format = "qs",
                                  batch_size = 100) {
  
  if (to_format == "qs" && !requireNamespace("qs", quietly = TRUE)) {
    cli::cli_abort("qs package required for cache conversion. Install with: install.packages('qs')")
  }
  
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "reindeer_cache")
  }
  
  cache_file <- file.path(cache_dir, "quantify_cache.sqlite")
  
  if (!file.exists(cache_file)) {
    cli::cli_alert_info("No cache found at {.path {cache_file}}")
    return(invisible(NULL))
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Get entries to convert
  entries <- DBI::dbGetQuery(conn, sprintf("
    SELECT cache_key, result_blob 
    FROM cache 
    WHERE format = '%s' OR format IS NULL
  ", from_format))
  
  if (nrow(entries) == 0) {
    cli::cli_alert_info("No entries to convert")
    return(invisible(0))
  }
  
  cli::cli_alert_info("Converting {nrow(entries)} cache entr{?y/ies} from {from_format} to {to_format}")
  
  # Process in batches
  n_batches <- ceiling(nrow(entries) / batch_size)
  converted <- 0
  failed <- 0
  
  for (batch in seq_len(n_batches)) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(entries))
    
    cli::cli_progress_step(
      "Processing batch {batch}/{n_batches}",
      msg_done = "Batch {batch}/{n_batches} complete"
    )
    
    for (i in start_idx:end_idx) {
      tryCatch({
        # Deserialize with old format
        obj <- if (from_format == "rds") {
          unserialize(entries$result_blob[[i]])
        } else if (from_format == "qs") {
          qs::qdeserialize(entries$result_blob[[i]])
        }
        
        # Reserialize with new format
        new_blob <- if (to_format == "qs") {
          qs::qserialize(obj, preset = "fast")
        } else {
          serialize(obj, NULL)
        }
        
        # Update in database
        DBI::dbExecute(conn, "
          UPDATE cache 
          SET result_blob = ?, format = ?, size_bytes = ?
          WHERE cache_key = ?
        ", params = list(list(new_blob), to_format, length(new_blob), entries$cache_key[i]))
        
        converted <- converted + 1
      }, error = function(e) {
        if (batch == 1 && i == start_idx) {
          cli::cli_alert_warning("Failed to convert entry: {entries$cache_key[i]}")
          cli::cli_alert_info("Error: {conditionMessage(e)}")
        }
        failed <- failed + 1
      })
    }
  }
  
  if (failed > 0) {
    cli::cli_alert_warning("{failed} entr{?y/ies} failed to convert")
  }
  
  cli::cli_alert_success("Successfully converted {converted} cache entr{?y/ies}")
  
  # Show size comparison
  if (converted > 0) {
    old_size <- sum(vapply(entries$result_blob, length, integer(1)))
    new_stats <- DBI::dbGetQuery(conn, sprintf("
      SELECT SUM(size_bytes) as total_size
      FROM cache
      WHERE format = '%s'
    ", to_format))
    new_size <- new_stats$total_size
    
    savings_pct <- round(100 * (1 - new_size / old_size), 1)
    cli::cli_alert_info(
      "Storage: {round(old_size / 1024^2, 1)} MB → {round(new_size / 1024^2, 1)} MB ({savings_pct}% savings)"
    )
  }
  
  invisible(converted)
}
