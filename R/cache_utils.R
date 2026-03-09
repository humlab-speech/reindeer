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
#' @examplesIf interactive()
#' stats <- cache_summary()
#' print(stats)
#'
#' @keywords internal
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
#' @examplesIf interactive()
#' # Clear entire cache
#' clear_cache()
#'
#' # Clear entries older than 30 days
#' clear_cache(older_than = 30)
#'
#' # Clear only RDS format entries
#' clear_cache(format = "rds")
#'
#' @keywords internal
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
  
  # Build query with parameterized conditions
  where_clauses <- character()
  params <- list()
  
  if (!is.null(older_than)) {
    cutoff_time <- as.integer(Sys.time()) - (older_than * 24 * 60 * 60)
    where_clauses <- c(where_clauses, "accessed_at < ?")
    params <- c(params, list(cutoff_time))
  }
  
  if (!is.null(format)) {
    where_clauses <- c(where_clauses, "format = ?")
    params <- c(params, list(format))
  }
  
  where_sql <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }
  
  # Only pass params when there are placeholders
  params_arg <- if (length(params) == 0) NULL else params
  
  # Count entries to be deleted
  count_query <- paste("SELECT COUNT(*) as count FROM cache", where_sql)
  to_delete <- DBI::dbGetQuery(conn, count_query, params = params_arg)$count
  
  if (to_delete == 0) {
    cli::cli_alert_info("No entries match the criteria")
    return(invisible(0))
  }
  
  # Delete entries
  delete_query <- paste("DELETE FROM cache", where_sql)
  DBI::dbExecute(conn, delete_query, params = params_arg)
  
  cli::cli_alert_success("Removed {to_delete} cache entr{?y/ies}")
  
  invisible(to_delete)
}

