# Argument checks (replace assertthat). `.assert(cond, ..., msg = )` aborts with
# a classed `reindeer_validation_error` unless every condition is TRUE.
.assert <- function(..., msg = "assertion failed") {
  ok <- vapply(list(...), function(x) isTRUE(all(x)) && length(x) > 0, logical(1))
  if (!all(ok)) {
    cli::cli_abort("{msg}", class = c("reindeer_validation_error", "reindeer_error"),
                   call = rlang::caller_env())
  }
  invisible(TRUE)
}
.is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
.is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
.is_count <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x > 0 && x == trunc(x)
}
