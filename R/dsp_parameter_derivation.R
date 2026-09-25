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
#' @export
derive_dsp_parameters <- function(dsp_fun, metadata, metadata_fields, user_params) {

  # Get formal arguments of DSP function
  fun_formals <- names(formals(dsp_fun))

  # Start with empty parameter list
  params <- list()

  # Extract metadata values
  meta_list <- as.list(metadata)

  # Age/Gender -> literature-derived DSP norms from the DSPP table.
  # dsp_parameters() previews exactly this; both route through the same
  # .lookup_dspp_row()/.normalize_gender() helpers so they cannot diverge.
  if ("Gender" %in% names(meta_list) && "Age" %in% names(meta_list)) {
    age    <- suppressWarnings(as.numeric(meta_list$Age))
    gender <- .normalize_gender(meta_list$Gender)

    if (!is.na(age) && !is.na(gender)) {
      dspp <- tibble::as_tibble(dspp_metadataParameters_dt())
      row  <- .lookup_dspp_row(dspp, age, gender)

      if (nrow(row) == 1L) {
        # Pull every DSPP norm column the DSP function actually accepts.
        norm_cols <- setdiff(intersect(names(row), fun_formals), c("Age", "Gender"))
        for (col in norm_cols) {
          if (!is.na(row[[col]])) params[[col]] <- row[[col]]
        }
      } else {
        cli::cli_warn(
          c("No DSPP norm row for Age {age}, Gender {gender}; using DSP defaults.",
            i = 'Preview with {.code dsp_parameters(age = {age}, gender = "{gender}")}.'),
          class = c("reindeer_metadata_warning", "reindeer_warning"))
      }
    }
  }

  # Any remaining requested metadata fields map straight onto matching formals.
  for (field in metadata_fields) {
    if (field %in% names(meta_list) && field %in% fun_formals) {
      params[[field]] <- meta_list[[field]]
    }
  }

  # A wrapper with no parameters beyond `...` cannot receive age/gender norms.
  # superassp >= 3.0.0 exposes the wrapped routine's formals (nominalF1,
  # windowSize, ...); older builds expose only (listOfFiles, ...), which made
  # metadata-driven DSP degrade to plain defaults without a word. Say so once
  # per session instead.
  if (length(params) == 0L &&
      all(fun_formals %in% c("listOfFiles", "...")) &&
      "..." %in% fun_formals &&
      !isTRUE(getOption("reindeer.norm_warning_shown", FALSE))) {
    options(reindeer.norm_warning_shown = TRUE)
    cli::cli_warn(
      c("DSP routine exposes no parameters, so Age/Gender norms are not applied.",
        i = "The wrapper only accepts {.code listOfFiles} and {.code ...}.",
        i = "superassp >= 3.0.0 exposes the wrapped routine's formals; older builds do not.",
        i = "Reinstall it from GitHub: {.run remotes::install_github(\"humlab-speech/superassp\")}."),
      class = c("reindeer_metadata_warning", "reindeer_warning"))
  }

  # Merge with user params (user params override)
  utils::modifyList(params, user_params)
}

#' Derive DSP params per bundle
#'
#' Applies [derive_dsp_parameters()] one bundle at a time. The underlying
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
