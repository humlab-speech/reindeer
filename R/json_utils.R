# ==============================================================================
# OPTIMIZED JSON UTILITIES WITH RcppSimdJson
# ==============================================================================
#
# This module provides optimized JSON reading using RcppSimdJson, which is
# significantly faster than jsonlite for parsing JSON files. We keep jsonlite
# for writing operations to maintain compatibility and feature parity.
#
# Performance benefits:
# - 2-10x faster JSON parsing (depending on file size and complexity)
# - Lower memory usage
# - Better handling of large JSON files
#
# Strategy:
# - READ: Use RcppSimdJson::fload() for file reading (fast)
# - WRITE: Use jsonlite for writing (feature-complete, pretty printing)
#

#' Read JSON file with optimized parser
#'
#' Reads a JSON file using RcppSimdJson for optimal performance. Falls back
#' to jsonlite if RcppSimdJson encounters issues.
#'
#' @param path Character; path to JSON file
#' @param simplifyVector Logical; simplify JSON arrays to R vectors (default: TRUE)
#' @param ... Additional arguments (for compatibility with jsonlite::read_json)
#'
#' @return Parsed JSON data as R object
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- read_json_fast("config.json")
#' data <- read_json_fast("data.json", simplifyVector = FALSE)
#' }
#' @noRd
read_json_fast <- function(path, simplifyVector = TRUE, ...) {
  assertthat::assert_that(
    assertthat::is.string(path),
    msg = "path must be a character string"
  )
  assertthat::assert_that(
    file.exists(path),
    msg = sprintf("JSON file not found: %s", path)
  )
  assertthat::assert_that(
    assertthat::is.flag(simplifyVector),
    msg = "simplifyVector must be TRUE or FALSE"
  )

  tryCatch({
    # Use RcppSimdJson for fast parsing
    result <- RcppSimdJson::fload(path, max_simplify_lvl = "data_frame")

    # If simplifyVector is FALSE, we need to de-simplify
    # RcppSimdJson always simplifies, so fall back to jsonlite for this case
    if (!simplifyVector) {
      return(jsonlite::read_json(path, simplifyVector = FALSE, ...))
    }

    return(result)
  }, error = function(e) {
    # Fall back to jsonlite if RcppSimdJson fails
    cli::cli_alert_warning(
      "RcppSimdJson failed, falling back to jsonlite: {e$message}"
    )
    jsonlite::read_json(path, simplifyVector = simplifyVector, ...)
  })
}




# ==============================================================================
# HYBRID STRATEGY HELPER
# ==============================================================================

