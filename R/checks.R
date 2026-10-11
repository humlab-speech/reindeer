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

# Accept old argument spellings with a deprecation warning.
# `dots` is list(...); `map` maps old name -> new name. Returns the list with
# old names replaced by new ones (the caller still decides what to do with them).
.rename_args <- function(dots, map, fn = NULL) {
  old <- intersect(names(dots), names(map))
  if (length(old) == 0L) return(dots)
  cli::cli_warn(
    c("{.arg {old}} {?is/are} deprecated; use {.arg {unname(map[old])}}.",
      if (!is.null(fn)) c("i" = "In {.fn {fn}}.")),
    class = c("reindeer_deprecated", "reindeer_warning"))
  names(dots)[match(old, names(dots))] <- unname(map[old])
  dots
}
