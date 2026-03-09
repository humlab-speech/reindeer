#'reindeer: An extension of the capabilities of emuR to work with speech data in
#'a nordic climate
#'
#' The purpose of this package is to provide a systematic framework for speech
#' corpora managemnet that retains emuR compatability, but provides provides
#' expanded ability to store metadata associated with the recording, use the
#' metadata to guide signal processing when appropriate, and support the user in
#' future archiving and FAIR use by automating the creation, and continnous
#' updating, of standard compliant metadata files for corpora.
#'
#'@name reindeer
#'
#' @importFrom Rdpack reprompt
#' @importFrom Rcpp sourceCpp
#' @importFrom stats end setNames start
#' @importFrom utils data head modifyList object.size
#' @useDynLib reindeer, .registration = TRUE

NULL

# Suppress R CMD check NOTEs for data.table/NSE variables
utils::globalVariables(c(
  ".", "..dsp_param_names", "..dsp_params", "..prep_param_names",
  "..prep_params", "..result_cols",
  "Age", "Age_lower", "Age_upper", "Gender", "Parameter", "Setting",
  "Study", "Study identifier", "identifier", "Study participants",
  "bundle", "cache_key", "cached_result",
  "check_corpusObj", "convert_emu_to_eaf",
  "data", "end", "end_item_seq_idx", "extension",
  "field_name", "field_type", "file_exists", "file_group_id",
  "from_id", "full_path", "head",
  "item_id", "level", "list_bundles", "load_DBConfig",
  "minF", "modifyList", "name", "nominalF1", "nominalF2", "nominalF3",
  "object.size", "ref_seq_idx", "result", "rewrite_annots",
  "sample_dur", "sample_end", "sample_rate", "sample_start",
  "seg_idx", "seq_idx", "session", "setNames", "sha1", "signal_file",
  "start", "start_item_id", "start_item_seq_idx",
  "to_id", "type", "weight", "windowSize"
))

# Null coalescing operator (internal, no roxygen to avoid Rd name issue)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

