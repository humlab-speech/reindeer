#' @include reindeer_signal_extensions_dt.R
NULL

#' Derive DSP parameters from bundle metadata
#'
#' Internal helper. End users should call [dsp_parameters()] instead;
#' kept exported for companion packages (`erodex`) that share the
#' age/gender derivation logic.
#'
#' Age/Gender are resolved to literature-derived, LOESS-smoothed norms
#' from the internal `DSPP` table (via `dspp_metadataParameters_dt()`),
#' matched to the DSP function's formal arguments. This is the same
#' lookup [dsp_parameters()] previews, so the preview equals what is
#' actually applied.
#'
#' @param dsp_fun A DSP function whose formal arguments are matched against
#'   the resolved norms.
#' @param metadata Named list of bundle metadata (expects `Age`, `Gender`).
#' @param metadata_fields Character vector of extra metadata field names to
#'   map straight onto matching formals of `dsp_fun`.
#' @param user_params Named list of user overrides (win over derived norms).
#' @return A named list of DSP parameters to pass to `dsp_fun`.
#' @keywords internal
#' @noRd
derive_dsp_parameters <- function(dsp_fun, metadata, metadata_fields, user_params) {

  fun_formals <- names(formals(dsp_fun))
  params <- list()
  meta_list <- as.list(metadata)

  gender <- if ("Gender" %in% names(meta_list)) .normalize_gender(meta_list$Gender) else NA
  age    <- if ("Age" %in% names(meta_list)) suppressWarnings(as.numeric(meta_list$Age)) else NA_real_

  # Age + Gender -> literature-derived norms from the DSPP table. Each DSPP
  # column is delivered to whatever the routine calls that parameter (see
  # .dsp_formal_map). dsp_parameters() previews exactly this lookup.
  if (!is.na(age) && !is.na(gender)) {
    dspp <- tibble::as_tibble(dspp_metadataParameters_dt())
    row  <- .lookup_dspp_row(dspp, age, gender)

    if (nrow(row) == 1L) {
      for (i in which(.dsp_formal_map$formal %in% fun_formals)) {
        formal <- .dsp_formal_map$formal[i]
        val <- row[[.dsp_formal_map$dspp[i]]]
        if (length(val) == 1L && !is.na(val)) {
          # DSP routines are .Call-based and need doubles, not DSPP integers.
          params[[formal]] <- if (is.numeric(val)) as.double(val) else val
        }
      }
    } else {
      cli::cli_warn(
        c("No DSPP norm row for Age {age}, Gender {gender}; using DSP defaults.",
          i = 'Preview with {.code dsp_parameters(age = {age}, gender = "{gender}")}.'),
        class = c("reindeer_metadata_warning", "reindeer_warning"))
    }
  }

  # Routines with a `gender` switch (forest, ksv, mhs) only honour explicit
  # norms when gender is "u": with "m"/"f" they use their own fixed ranges.
  # So: derived norms -> "u" (our age-aware values win); Gender alone -> m/f.
  if ("gender" %in% fun_formals) {
    if (length(params) > 0L) {
      params$gender <- "u"
    } else if (!is.na(gender)) {
      params$gender <- if (identical(as.character(gender), "Female")) "f" else "m"
    }
  }
  derived <- names(params)

  # Any remaining requested metadata fields map straight onto matching formals.
  for (field in metadata_fields) {
    if (field %in% names(meta_list) && field %in% fun_formals && !field %in% names(params)) {
      params[[field]] <- meta_list[[field]]
    }
  }

  # User params override.
  out <- utils::modifyList(params, user_params)
  attr(out, "derived") <- derived
  out
}

#' Reindeer norm name -> routine formal
#'
#' Each DSPP column (Age/Gender norm) can be called something else by a
#' superassp routine. One row per (routine formal, DSPP column).
#' @noRd
.dsp_formal_map <- data.frame(
  formal = c("minF", "minimum_f0", "maxF", "maximum_f0",
             "numFormants", "number_of_formants",
             "maxFormantHz", "maxHzFormant",
             "nominalF1", "nominalF2", "nominalF3", "windowSize"),
  dspp   = c("minF", "minF", "maxF", "maxF",
             "numFormants", "numFormants",
             "maxFormantHz", "maxFormantHz",
             "nominalF1", "nominalF2", "nominalF3", "windowSize"),
  stringsAsFactors = FALSE
)

#' Derive DSP params per bundle
#'
#' Applies `derive_dsp_parameters()` one bundle at a time. The underlying
#' function has a single-row contract; passing a whole metadata table
#' recycles vectorised `Age`/`Gender` against the DSPP table and returns
#' an arbitrary row. This wrapper resolves one params list per
#' (session, bundle) and returns a tibble with a `dsp_params` list-column.
#'
#' @param metadata A tibble with `session`, `bundle`, `Age`, `Gender`
#'   columns (one row per bundle).
#' @return Tibble with `session`, `bundle`, and a `dsp_params` list-column
#'   whose entries are the fully merged parameter lists (user overrides
#'   already applied).
#' @noRd
.derive_dsp_params_per_bundle <- function(dsp_fun, metadata, metadata_fields,
                                          user_params) {
  n <- nrow(metadata)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- derive_dsp_parameters(
      dsp_fun = dsp_fun,
      metadata = metadata[i, , drop = FALSE],
      metadata_fields = metadata_fields,
      user_params = user_params
    )
  }
  tibble::tibble(
    session = metadata$session,
    bundle = metadata$bundle,
    dsp_params = out
  )
}

#' Speaker metadata for a set of bundles, inheritance resolved
#'
#' Bundle > session > database precedence (the same COALESCE as
#' [get_metadata()]), restricted to `fields` and `bundles`. This is the one
#' reader the DSP paths use, so session- and database-level Age/Gender reach
#' the routine and not only values written on the bundle itself.
#'
#' @param corpus_obj A corpus.
#' @param bundles Data frame with `session` and `bundle` columns.
#' @param fields Metadata field names to fetch.
#' @return Tibble with `session`, `bundle` and one character column per field
#'   that has a value somewhere.
#' @noRd
.speaker_metadata <- function(corpus_obj, bundles, fields = c("Age", "Gender")) {
  con <- get_corpus_connection(corpus_obj)
  fields <- unique(fields)
  values <- paste(rep("(?)", length(fields)), collapse = ", ")
  long <- DBI::dbGetQuery(con, sprintf("
    WITH f(field_name) AS (VALUES %s)
    SELECT b.session, b.name AS bundle, f.field_name AS key,
           COALESCE(mb.field_value, ms.field_value, md.field_value) AS value
    FROM bundle b
    CROSS JOIN f
    LEFT JOIN metadata_bundle mb ON mb.db_uuid = b.db_uuid AND mb.session = b.session
      AND mb.bundle = b.name AND mb.field_name = f.field_name
    LEFT JOIN metadata_session ms ON ms.db_uuid = b.db_uuid AND ms.session = b.session
      AND ms.field_name = f.field_name
    LEFT JOIN metadata_database md ON md.db_uuid = b.db_uuid AND md.field_name = f.field_name
    WHERE b.db_uuid = ?
      AND COALESCE(mb.field_value, ms.field_value, md.field_value) IS NOT NULL", values),
    params = c(as.list(fields), list(corpus_obj@.uuid)))

  needed <- paste(bundles$session, bundles$bundle, sep = "\x01")
  long <- long[paste(long$session, long$bundle, sep = "\x01") %in% needed, , drop = FALSE]
  if (nrow(long) == 0L) {
    return(tibble::tibble(session = character(), bundle = character()))
  }
  wide <- data.table::dcast(data.table::as.data.table(long), session + bundle ~ key,
                            value.var = "value")
  tibble::as_tibble(wide)
}
