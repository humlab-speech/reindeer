test_that("Python MOMEL-INTSINT implementation works", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  
  # Create test database
  ae_path <- system.file("extdata/ae_emuDB", package = "emuR", mustWork = FALSE)
  skip_if(ae_path == "", "emuR demo data not available")
  
  # Import Python module
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  # Test on a sample audio file
  test_wav <- list.files(ae_path, pattern = "\\.wav$", 
                        recursive = TRUE, full.names = TRUE)[1]
  skip_if(is.na(test_wav), "No WAV files found")
  
  # Run Python implementation
  result <- momel_intsint$process_momel_intsint(
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
  
  # Check results structure
  expect_type(result, "list")
  expect_length(result, 3)
  
  intsint_targets <- result[[1]]
  range_val <- result[[2]]
  key_val <- result[[3]]
  
  # Check that we got some targets
  expect_gt(length(intsint_targets), 0)
  
  # Check range and key are reasonable
  expect_gt(range_val, 0)
  expect_lt(range_val, 3)
  expect_gt(key_val, 50)
  expect_lt(key_val, 600)
  
  # Check target structure
  first_target <- intsint_targets[[1]]
  expect_true("time" %in% names(first_target))
  expect_true("tone" %in% names(first_target))
  expect_true("target" %in% names(first_target))
  expect_true("estimate" %in% names(first_target))
  
  # Check tone is valid INTSINT label
  valid_tones <- c("T", "M", "B", "H", "U", "S", "D", "L")
  expect_true(first_target$tone %in% valid_tones)
})


test_that("draft_momel_intsint creates valid suggestions", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  
  skip("Requires full corpus infrastructure")
  
  # This test would require:
  # corp <- corpus(test_db_path)
  # bundles <- corp[".*", ".*"]
  # suggestions <- draft_momel_intsint(corp, bundles, verbose = FALSE)
  # 
  # expect_s7_class(suggestions, "EventSuggestion")
  # expect_gt(nrow(suggestions@annotations), 0)
  # expect_equal(suggestions@level_name, "Intsint")
  # expect_equal(suggestions@level_type, "EVENT")
})


test_that("Python implementation handles edge cases", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  # Test helper functions
  
  # Test octave conversion
  expect_equal(momel_intsint$octave(440), log(440, 2))
  expect_equal(momel_intsint$linear(momel_intsint$octave(440)), 440)
  
  # Test with various F0 ranges
  f0_values <- c(0, 0, 100, 150, 200, 150, 100, 0, 0)
  f0_array <- reticulate::np_array(f0_values)
  
  # This should not crash
  targets <- momel_intsint$momel(
    f0_values = f0_array,
    window_length = 3L,
    min_f0 = 60.0,
    max_f0 = 300.0,
    max_error = 1.04,
    reduced_window_length = 2L,
    minimal_distance = 1.0,
    minimal_frequency_ratio = 0.05
  )
  
  expect_type(targets, "list")
})


test_that("Glitch elimination works correctly", {
  skip_if_not(reticulate::py_module_available("parselmouth"),
              "Parselmouth not available")
  
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )
  
  # Create F0 contour with obvious glitch
  # Normal values around 100 Hz, with a spike to 200 Hz
  f0_with_glitch <- c(100, 100, 100, 200, 100, 100, 100)
  f0_array <- reticulate::np_array(f0_with_glitch)
  
  result <- momel_intsint$eliminate_glitches(f0_array)
  
  # The glitch at index 3 should be removed (set to 0)
  # Note: Python uses 0-indexing
  expect_equal(result[4], 0)  # R uses 1-indexing
  expect_equal(result[1], 100)
  expect_equal(result[7], 100)
})
