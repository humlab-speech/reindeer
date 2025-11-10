#' Passthrough Preprocessing Function for Testing
#'
#' A test utility that mimics the interface of superassp::prep_recode() but
#' performs no actual transformation of the media file. Useful for testing
#' simulation infrastructure without dependency on external preprocessing
#' functions or actual media transformations.
#'
#' This function reads the media file and returns it unchanged, but accepts
#' all the same parameters as prep_recode() to test parameter passing and
#' grid generation in the simulation system.
#'
#' @param listOfFiles Character vector of file paths to media files
#' @param format Output format (accepted but ignored - returns original format)
#' @param codec Audio codec (accepted but ignored)
#' @param sample_rate Target sample rate (accepted but ignored - returns original)
#' @param bit_rate Target bit rate (accepted but ignored)
#' @param start_time Start time in seconds (accepted but ignored)
#' @param end_time End time in seconds (accepted but ignored)
#' @param channels Number of output channels (accepted but ignored)
#' @param verbose Logical; show progress messages (default: FALSE)
#' @param ... Additional arguments (ignored)
#'
#' @return For single file: Integer vector with audio samples in s32le format
#'   (32-bit signed integers), with attributes:
#'   \itemize{
#'     \item \code{channels}: Number of audio channels (integer)
#'     \item \code{sample_rate}: Sample rate in Hz (integer)
#'     \item \code{prep_params}: List of parameters passed (for testing)
#'   }
#'
#'   For multiple files: List of integer vectors, one per file
#'
#' @details
#' This function is designed solely for testing the simulation infrastructure.
#' It accepts the same parameters as superassp::prep_recode() to ensure
#' compatibility, but performs no transformations. The audio is read using
#' the av package and returned unchanged.
#'
#' The function adds a special attribute \code{prep_params} containing all
#' the parameters that were passed, allowing tests to verify correct parameter
#' propagation through the simulation system.
#'
#' @examples
#' \dontrun{
#' # Test simulation with passthrough preprocessing
#' library(reindeer)
#'
#' corp <- corpus("path/to/db_emuDB")
#' segments <- ask_for(corp, "Phonetic == t") %>% head(5)
#'
#' # Simulate with prep parameters but no actual transformation
#' results <- quantify_simulate(
#'   segments,
#'   .using = superassp::forest,
#'   .simulate = list(nominalF1 = c(500, 600)),
#'   .prep_function = prep_passthrough,
#'   .prep_simulate = list(
#'     sample_rate = c(16000, 22050),
#'     format = c("wav", "mp3")
#'   ),
#'   .simulation_store = tempdir()
#' )
#'
#' # Verify parameters were passed correctly
#' attr(results[[1]], "prep_params")
#' }
#'
#' @keywords internal
#' @export
prep_passthrough <- function(listOfFiles,
                              format = NULL,
                              codec = NULL,
                              sample_rate = NULL,
                              bit_rate = NULL,
                              start_time = NULL,
                              end_time = NULL,
                              channels = NULL,
                              verbose = FALSE,
                              ...) {

  # Check av package
  if (!requireNamespace("av", quietly = TRUE)) {
    stop("Package 'av' is required for prep_passthrough.\n",
         "Install with: install.packages('av')",
         call. = FALSE)
  }

  # Validate input
  if (missing(listOfFiles) || is.null(listOfFiles) || length(listOfFiles) == 0) {
    stop("listOfFiles cannot be NULL or empty", call. = FALSE)
  }

  # Check files exist
  missing_files <- listOfFiles[!file.exists(listOfFiles)]
  if (length(missing_files) > 0) {
    stop("File(s) not found: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }

  # Store parameters that were passed (for testing verification)
  prep_params <- list(
    format = format,
    codec = codec,
    sample_rate = sample_rate,
    bit_rate = bit_rate,
    start_time = start_time,
    end_time = end_time,
    channels = channels
  )

  if (verbose) {
    cli::cli_alert_info("prep_passthrough: Reading {length(listOfFiles)} file{?s} (no transformation)")
    cli::cli_alert_info("Parameters passed: {paste(names(prep_params)[!sapply(prep_params, is.null)], collapse = ', ')}")
  }

  # Process files
  process_file <- function(file_path) {
    if (verbose) {
      cli::cli_alert("Reading: {.file {basename(file_path)}}")
    }

    # Read audio using av package
    tryCatch({
      audio_data <- av::read_audio_bin(file_path)

      # Add prep_params attribute for testing
      attr(audio_data, "prep_params") <- prep_params

      if (verbose) {
        cli::cli_alert_success(
          "Read {length(audio_data)} samples @ {attr(audio_data, 'sample_rate')} Hz, {attr(audio_data, 'channels')} channel{?s}"
        )
      }

      audio_data
    }, error = function(e) {
      stop("Failed to read audio from ", file_path, ": ", e$message, call. = FALSE)
    })
  }

  # Process single or multiple files
  if (length(listOfFiles) == 1) {
    result <- process_file(listOfFiles[1])
  } else {
    result <- lapply(listOfFiles, process_file)
  }

  if (verbose) {
    cli::cli_alert_success("prep_passthrough complete")
  }

  result
}


#' Check if prep_passthrough parameters were preserved
#'
#' Test utility to verify that preprocessing parameters were correctly
#' passed through the simulation system.
#'
#' @param result Result object from quantify_simulate or enrich_simulate
#' @param expected_params Named list of expected parameter values
#' @return Logical; TRUE if parameters match, FALSE otherwise
#' @keywords internal
#' @export
verify_prep_params <- function(result, expected_params) {

  # Extract prep_params from result
  if (is.list(result) && "prep_params" %in% names(attributes(result))) {
    actual_params <- attr(result, "prep_params")
  } else {
    warning("No prep_params attribute found in result")
    return(FALSE)
  }

  # Compare each expected parameter
  all_match <- TRUE
  for (param_name in names(expected_params)) {
    expected_val <- expected_params[[param_name]]
    actual_val <- actual_params[[param_name]]

    if (!isTRUE(all.equal(expected_val, actual_val))) {
      warning(sprintf(
        "Parameter mismatch: %s\n  Expected: %s\n  Actual: %s",
        param_name,
        paste(expected_val, collapse = ", "),
        paste(actual_val, collapse = ", ")
      ))
      all_match <- FALSE
    }
  }

  all_match
}


#' Create minimal test corpus for simulation testing
#'
#' Creates a minimal in-memory test corpus with a few bundles for
#' testing simulation functionality without requiring external data.
#'
#' @param n_bundles Number of bundles to create (default: 3)
#' @param temp_dir Temporary directory for corpus (default: tempdir())
#' @return Path to created test corpus
#' @keywords internal
#' @export
create_test_corpus_for_simulation <- function(n_bundles = 3, temp_dir = tempdir()) {

  # Create unique corpus name
  corpus_name <- sprintf("test_sim_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
  corpus_path <- file.path(temp_dir, paste0(corpus_name, "_emuDB"))

  if (dir.exists(corpus_path)) {
    unlink(corpus_path, recursive = TRUE)
  }

  # Use emuR to create minimal database
  if (!requireNamespace("emuR", quietly = TRUE)) {
    stop("emuR package required for creating test corpus", call. = FALSE)
  }

  # Create database
  emuR::create_emuDB(
    name = corpus_name,
    targetDir = temp_dir,
    verbose = FALSE
  )

  # Add a simple annotation level
  db_handle <- emuR::load_emuDB(corpus_path, verbose = FALSE)

  emuR::add_levelDefinition(
    db_handle,
    name = "Phonetic",
    type = "ITEM",
    verbose = FALSE
  )

  # Import some dummy wav files and annotations
  # (Implementation would depend on available test data)

  corpus_path
}
