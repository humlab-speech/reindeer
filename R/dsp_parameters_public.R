#' @include reindeer_signal_extensions_dt.R reindeer_enrich.R
NULL

#' Inspect age/gender-aware DSP parameters
#'
#' Returns the DSP parameter row that would be selected for a given
#' speaker's age and gender, drawn from the empirical / LOESS-smoothed
#' defaults table that powers [enrich()] and [quantify()] when
#' `.use_metadata = TRUE`. Use this to preview what windowSize, minF,
#' nominalF1/F2/F3 etc. will be passed to a DSP routine before you run
#' it, or to compare two speakers side-by-side.
#'
#' Pass `age` and `gender` directly to look up a single row, or pass a
#' `corpus_obj` to get one row per bundle (resolved through the usual
#' session/bundle inheritance chain).
#'
#' @param age Numeric age in years. Required unless `corpus_obj` is supplied.
#' @param gender Character: "Female", "Male", or "Unspecified" (case-
#'   insensitive; "F"/"M" also accepted). Required unless `corpus_obj`
#'   is supplied.
#' @param corpus_obj Optional corpus object. When given, the function
#'   pulls Age/Gender for every bundle and returns one row per bundle.
#' @return A tibble with one row per requested (age, gender) — or one
#'   row per bundle when `corpus_obj` is supplied. Columns mirror the
#'   internal DSPP table (windowSize, minF, maxF, nominalF1, nominalF2,
#'   nominalF3, plus any others present).
#' @examplesIf interactive()
#' # Single speaker
#' dsp_parameters(age = 35, gender = "Female")
#' # Compare two
#' rbind(
#'   dsp_parameters(age = 8,  gender = "Male"),
#'   dsp_parameters(age = 45, gender = "Male")
#' )
#' # Whole corpus
#' dsp_parameters(corpus_obj = corp)
#' @export
dsp_parameters <- function(age = NULL, gender = NULL, corpus_obj = NULL) {
  dspp <- dspp_metadataParameters_dt()
  dspp <- tibble::as_tibble(dspp)

  if (!is.null(corpus_obj)) {
    if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
      cli::cli_abort("{.arg corpus_obj} must be a corpus object")
    }
    md <- get_metadata(corpus_obj)
    have_age    <- "Age"    %in% names(md)
    have_gender <- "Gender" %in% names(md)
    if (!have_age || !have_gender) {
      cli::cli_abort(c(
        "Corpus is missing Age and/or Gender metadata.",
        "i" = "Run {.code load_metadata(corpus_obj)} or set them with {.code set_metadata()}."
      ))
    }
    md$.Age <- suppressWarnings(as.numeric(md$Age))
    md$.Gender <- .normalize_gender(md$Gender)
    rows <- lapply(seq_len(nrow(md)), function(i) {
      r <- .lookup_dspp_row(dspp, md$.Age[i], md$.Gender[i])
      cbind(tibble::tibble(session = md$session[i], bundle = md$bundle[i],
                            Age = md$.Age[i], Gender = md$.Gender[i]),
            r)
    })
    return(do.call(rbind, rows))
  }

  if (is.null(age) || is.null(gender)) {
    cli::cli_abort("Supply both {.arg age} and {.arg gender}, or pass {.arg corpus_obj}.")
  }
  gender <- .normalize_gender(gender)
  .lookup_dspp_row(dspp, as.numeric(age), gender)
}

# --- helpers ------------------------------------------------------------

.normalize_gender <- function(g) {
  g <- as.character(g)
  out <- ifelse(toupper(g) %in% c("F", "FEMALE"), "Female",
        ifelse(toupper(g) %in% c("M", "MALE"),   "Male",
                                                  "Unspecified"))
  out
}

.lookup_dspp_row <- function(dspp, age, gender) {
  if (is.na(age) || is.na(gender)) {
    return(dspp[0, , drop = FALSE])
  }
  sub <- dspp[as.character(dspp$Gender) == gender, , drop = FALSE]
  if (nrow(sub) == 0) {
    return(dspp[0, , drop = FALSE])
  }
  # nearest Age row
  idx <- which.min(abs(sub$Age - age))
  sub[idx, , drop = FALSE]
}
