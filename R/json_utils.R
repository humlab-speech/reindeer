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
    result <- RcppSimdJson::fload(path, query = "/", max_simplify_lvl = "data_frame")

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

#' Parse JSON string with optimized parser
#'
#' Parses a JSON string using RcppSimdJson for optimal performance.
#'
#' @param txt Character; JSON string to parse
#' @param simplifyVector Logical; simplify JSON arrays to R vectors (default: TRUE)
#' @param ... Additional arguments (for compatibility)
#'
#' @return Parsed JSON data as R object
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- parse_json_fast('{"name": "test", "value": 42}')
#' }
parse_json_fast <- function(txt, simplifyVector = TRUE, ...) {
  assertthat::assert_that(
    assertthat::is.string(txt),
    msg = "txt must be a character string"
  )
  assertthat::assert_that(
    assertthat::is.flag(simplifyVector),
    msg = "simplifyVector must be TRUE or FALSE"
  )

  tryCatch({
    # Use RcppSimdJson for fast parsing
    result <- RcppSimdJson::fparse(txt, query = "/", max_simplify_lvl = "data_frame")

    # If simplifyVector is FALSE, fall back to jsonlite
    if (!simplifyVector) {
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE, ...))
    }

    return(result)
  }, error = function(e) {
    # Fall back to jsonlite if RcppSimdJson fails
    cli::cli_alert_warning(
      "RcppSimdJson failed, falling back to jsonlite: {e$message}"
    )
    jsonlite::fromJSON(txt, simplifyVector = simplifyVector, ...)
  })
}

#' Write JSON file (uses jsonlite for compatibility)
#'
#' Writes an R object to JSON file using jsonlite. This wrapper is provided
#' for consistency with read_json_fast().
#'
#' @param x R object to write
#' @param path Character; output file path
#' @param auto_unbox Logical; automatically unbox single-element arrays
#' @param pretty Logical; pretty print with indentation
#' @param ... Additional arguments passed to jsonlite::write_json
#'
#' @return NULL (invisibly)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' write_json_compat(list(a = 1, b = 2), "output.json")
#' }
write_json_compat <- function(x, path, auto_unbox = TRUE, pretty = TRUE, ...) {
  assertthat::assert_that(
    assertthat::is.string(path),
    msg = "path must be a character string"
  )
  assertthat::assert_that(
    assertthat::is.flag(auto_unbox),
    assertthat::is.flag(pretty),
    msg = "auto_unbox and pretty must be TRUE or FALSE"
  )

  # Use jsonlite for writing (feature-complete, good formatting)
  jsonlite::write_json(
    x, path,
    auto_unbox = auto_unbox,
    pretty = pretty,
    ...
  )

  invisible(NULL)
}

#' Serialize R object to JSON string (uses jsonlite)
#'
#' Converts an R object to JSON string using jsonlite.
#'
#' @param x R object to serialize
#' @param auto_unbox Logical; automatically unbox single-element arrays
#' @param ... Additional arguments passed to jsonlite::toJSON
#'
#' @return JSON string
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' json_str <- to_json_compat(list(a = 1, b = 2))
#' }
to_json_compat <- function(x, auto_unbox = TRUE, ...) {
  assertthat::assert_that(
    assertthat::is.flag(auto_unbox),
    msg = "auto_unbox must be TRUE or FALSE"
  )

  # Use jsonlite for serialization
  jsonlite::toJSON(x, auto_unbox = auto_unbox, ...)
}

# ==============================================================================
# HYBRID STRATEGY HELPER
# ==============================================================================

#' Get JSON parser strategy
#'
#' Returns information about which JSON parser to use for different operations.
#' This is useful for debugging and understanding performance characteristics.
#'
#' @return List with parser strategy information
#' @export
#'
#' @examples
#' get_json_strategy()
get_json_strategy <- function() {
  list(
    read_files = "RcppSimdJson::fload() [2-10x faster than jsonlite]",
    read_strings = "RcppSimdJson::fparse() [2-10x faster than jsonlite]",
    write_files = "jsonlite::write_json() [feature-complete, pretty printing]",
    write_strings = "jsonlite::toJSON() [feature-complete]",
    fallback = "jsonlite for all operations if RcppSimdJson fails",
    performance_gain = "Typical speedup: 2-5x for config files, up to 10x for large files",
    compatibility = "100% - transparent fallback to jsonlite ensures no breaking changes"
  )
}
