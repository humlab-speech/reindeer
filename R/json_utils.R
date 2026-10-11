# JSON helpers. Reading and writing both use jsonlite.

#' Read a JSON file
#'
#' Thin wrapper over [jsonlite::read_json()] with argument checks.
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
  .assert(
    .is_string(path),
    msg = "path must be a character string"
  )
  .assert(
    file.exists(path),
    msg = sprintf("JSON file not found: %s", path)
  )
  .assert(
    .is_flag(simplifyVector),
    msg = "simplifyVector must be TRUE or FALSE"
  )

  jsonlite::read_json(path, simplifyVector = simplifyVector, ...)
}

# ==============================================================================
# HYBRID STRATEGY HELPER
# ==============================================================================
