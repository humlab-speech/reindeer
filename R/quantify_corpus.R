#' @include corpus_class.R segment_list_quantify.R reindeer_corpus_config.R
NULL

# ==============================================================================
# quantify.corpus: corpus-wide DSP materialization + ssffTrackDefinitions
# provenance. See specs/2026-09-24-quantify-unification-design.md.
# ==============================================================================

#' Resolve a DSP function's identity (name/package/version) for provenance
#'
#' `dsp_fun_name` is the caller's deparsed expression (e.g.
#' `"superassp::trk_formant_forest"` or `"trk_formant_forest"` if the
#' package was attached). A qualified expression is parsed directly;
#' a bare name falls back to the function's own defining namespace.
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @return List with `function`, `package` (`NA` if unresolvable),
#'   `version` (`NA` if `package` is `NA` or has no installed version).
#' @noRd
.resolve_dsp_identity <- function(dsp_fun, dsp_fun_name) {
  qualified <- regmatches(
    dsp_fun_name,
    regexec("^([[:alnum:].]+)::([[:alnum:]._]+)$", dsp_fun_name)
  )[[1]]

  if (length(qualified) == 3) {
    pkg <- qualified[2]
    fn_name <- qualified[3]
  } else {
    fn_name <- dsp_fun_name
    pkg <- tryCatch({
      env_name <- environmentName(topenv(environment(dsp_fun)))
      if (nzchar(env_name) && !env_name %in% c("R_GlobalEnv", "base")) {
        env_name
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
  }

  version <- if (!is.na(pkg)) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  list(`function` = fn_name, package = pkg, version = version)
}

#' Build the `generator` block recorded on an `ssffTrackDefinitions` entry
#'
#' `args` contains only what the caller explicitly passed to `quantify()` —
#' never metadata-derived or per-bundle-resolved values (those have no home
#' in a single global track definition; see the design spec's "Non-goals").
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @param user_params Named list of explicit caller args (`list(...)` at the
#'   `quantify.corpus` method boundary).
#' @return A `generator` list ready to embed in an `ssffTrackDefinitions[i]`.
#' @noRd
.build_generator_block <- function(dsp_fun, dsp_fun_name, user_params) {
  identity <- .resolve_dsp_identity(dsp_fun, dsp_fun_name)
  list(
    `function` = identity$`function`,
    package = identity$package,
    version = identity$version,
    args = if (length(user_params)) user_params else list(),
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}
