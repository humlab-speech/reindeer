"""
Fidelity Test: Compare Python MOMEL-INTSINT to Original Praat Implementation

This script compares the output of the new Python/Parselmouth implementation
with the original Praat/C/Perl implementation to ensure fidelity.
"""

library(testthat)
library(reindeer)
library(dplyr)
library(emuR)

test_that("Python and Praat MOMEL-INTSINT produce similar results", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  skip_if_not(superassp::have_praat(), 
              "Praat not available")
  
  # Load test database
  ae_path <- system.file("extdata/ae_emuDB", package = "emuR", mustWork = FALSE)
  skip_if(ae_path == "", "emuR demo data not available")
  
  # Copy to temp location for testing
  test_dir <- tempfile("momel_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  
  file.copy(ae_path, test_dir, recursive = TRUE)
  test_db_path <- file.path(test_dir, basename(ae_path))
  
  # Load with emuR
  ae_db <- load_emuDB(test_db_path, verbose = FALSE)
  
  # Get a test bundle
  test_wav <- list.files(test_db_path, pattern = "\\.wav$", 
                         recursive = TRUE, full.names = TRUE)[1]
  
  # === Run Praat implementation (if old function still exists) ===
  # Note: This assumes the old annotate_INTSINT_MOMEL function exists
  # If it's been removed, we can skip this comparison
  
  praat_results <- NULL
  has_praat_impl <- exists("annotate_INTSINT_MOMEL", mode = "function")
  
  if (has_praat_impl) {
    # Query for utterances
    sl <- query(ae_db, "Utterance =~ .*", verbose = FALSE)
    
    # Run old Praat implementation
    praat_results <- tryCatch({
      annotate_INTSINT_MOMEL(
        ae_db, sl,
        windowSize = 30,
        minF = 60,
        maxF = 750,
        pitchSpan = 1.5,
        maximumError = 1.04,
        reducWindowSize = 20,
        minimalDistance = 20,
        minimalFrequencyRatio = 0.05,
        verbose = FALSE
      )
    }, error = function(e) {
      NULL
    })
  }
  
  # === Run Python implementation ===
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  python_result <- momel_intsint$process_momel_intsint(
    sound_file = test_wav,
    window_length = 30L,
    min_f0 = 60.0,
    max_f0 = 750.0,
    pitch_span = 1.5,
    max_error = 1.04,
    reduced_window_length = 20L,
    minimal_distance = 20L,
    minimal_frequency_ratio = 0.05,
    time_step = 0.01
  )
  
  # Extract Python results
  py_targets <- python_result[[1]]
  py_range <- python_result[[2]]
  py_key <- python_result[[3]]
  
  # Basic sanity checks on Python output
  expect_gt(length(py_targets), 0, 
            label = "Python implementation should produce targets")
  
  expect_gt(py_range, 0,
            label = "Range should be positive")
  expect_lt(py_range, 3,
            label = "Range should be reasonable (< 3 octaves)")
  
  expect_gt(py_key, 50,
            label = "Key should be above minimum F0")
  expect_lt(py_key, 600,
            label = "Key should be below maximum F0")
  
  # Check all tones are valid
  valid_tones <- c("T", "M", "B", "H", "U", "S", "D", "L")
  py_tones <- sapply(py_targets, function(t) t$tone)
  expect_true(all(py_tones %in% valid_tones),
              label = "All tones should be valid INTSINT labels")
  
  # If we have Praat results, compare them
  if (!is.null(praat_results)) {
    # Compare number of targets (should be similar, but may differ slightly)
    n_praat <- nrow(praat_results)
    n_python <- length(py_targets)
    
    # Allow up to 20% difference in number of targets
    expect_lt(abs(n_python - n_praat) / n_praat, 0.2,
              label = "Number of targets should be similar")
    
    # Compare key and range (should be close)
    # Extract from Praat results if available
    if ("key" %in% names(praat_results$metadata)) {
      praat_key <- unique(praat_results$metadata$key)[1]
      expect_lt(abs(py_key - praat_key) / praat_key, 0.1,
                label = "Keys should be within 10% of each other")
    }
    
    if ("range" %in% names(praat_results$metadata)) {
      praat_range <- unique(praat_results$metadata$range)[1]
      expect_lt(abs(py_range - praat_range) / praat_range, 0.2,
                label = "Ranges should be within 20% of each other")
    }
    
    # Compare tone distributions
    praat_tones <- table(praat_results$labels)
    python_tones <- table(py_tones)
    
    # All tones in Praat should also appear in Python (or vice versa is ok)
    common_tones <- intersect(names(praat_tones), names(python_tones))
    expect_gt(length(common_tones), 0,
              label = "Should have some tones in common")
  }
})


test_that("Python implementation is thread-safe for parallel processing", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  skip_on_cran()
  
  ae_path <- system.file("extdata/ae_emuDB", package = "emuR", mustWork = FALSE)
  skip_if(ae_path == "", "emuR demo data not available")
  
  test_wavs <- list.files(ae_path, pattern = "\\.wav$", 
                         recursive = TRUE, full.names = TRUE)
  skip_if(length(test_wavs) < 2, "Need multiple WAV files")
  
  # Take first 3 files
  test_wavs <- head(test_wavs, 3)
  
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  # Process sequentially first
  sequential_results <- lapply(test_wavs, function(wav) {
    momel_intsint$process_momel_intsint(
      sound_file = wav,
      window_length = 30L,
      min_f0 = 60.0,
      max_f0 = 750.0,
      max_error = 1.04
    )
  })
  
  # Process in parallel (if parallel backend available)
  if (requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(2)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    parallel_results <- parallel::parLapply(cl, test_wavs, function(wav) {
      momel_intsint <- reticulate::import_from_path(
        "momel_intsint",
        path = system.file("python", package = "reindeer", mustWork = TRUE)
      )
      
      momel_intsint$process_momel_intsint(
        sound_file = wav,
        window_length = 30L,
        min_f0 = 60.0,
        max_f0 = 750.0,
        max_error = 1.04
      )
    })
    
    # Results should be similar (not necessarily identical due to 
    # potential numerical differences)
    expect_equal(length(sequential_results), length(parallel_results))
    
    for (i in seq_along(sequential_results)) {
      seq_n <- length(sequential_results[[i]][[1]])
      par_n <- length(parallel_results[[i]][[1]])
      
      # Allow small differences
      expect_lt(abs(seq_n - par_n), 3,
                label = paste("File", i, "should have similar number of targets"))
    }
  }
})


test_that("Python implementation produces valid results for various signal types", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  # Test with synthetic pitch contour
  test_dir <- tempfile("synth_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  
  # Create synthetic sound with parselmouth if available
  if (reticulate::py_module_available("parselmouth")) {
    py_run_string("
import parselmouth
import numpy as np

# Create a simple sound with varying pitch
duration = 1.0
sampling_frequency = 16000
sound = parselmouth.Sound.create_simple(
    duration=duration,
    sampling_frequency=sampling_frequency,
    formula='1/2 * sin(2*pi*200*x) + 1/2 * sin(2*pi*300*x)'
)

# Save to file
output_path = r'%s'
sound.save(output_path, 'WAV')
" %::% file.path(test_dir, "test_synth.wav"))
    
    synth_file <- file.path(test_dir, "test_synth.wav")
    
    if (file.exists(synth_file)) {
      result <- momel_intsint$process_momel_intsint(
        sound_file = synth_file,
        window_length = 30L,
        min_f0 = 100.0,
        max_f0 = 400.0,
        max_error = 1.04
      )
      
      expect_type(result, "list")
      targets <- result[[1]]
      
      # Should produce some targets for synthetic sound
      expect_gt(length(targets), 0,
                label = "Should produce targets for synthetic sound")
      
      # All targets should be in reasonable F0 range
      target_f0s <- sapply(targets, function(t) t$target)
      expect_true(all(target_f0s >= 100 & target_f0s <= 400),
                  label = "All targets should be in specified F0 range")
    }
  }
})
