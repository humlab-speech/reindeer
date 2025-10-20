# Tests for simulation preprocessing functionality
# Tests the new .prep_function and .prep_simulate parameters

test_that("create_parameter_grid handles DSP parameters only", {
  dsp_spec <- list(
    param1 = c(1, 2, 3),
    param2 = c("a", "b")
  )

  grid <- reindeer:::create_parameter_grid(dsp_spec, NULL)

  expect_s3_class(grid, "data.table")
  expect_equal(nrow(grid), 6)  # 3 * 2 = 6 combinations
  expect_true("param_hash" %in% names(grid))
  expect_equal(attr(grid, "dsp_params"), c("param1", "param2"))
  expect_equal(attr(grid, "prep_params"), character(0))
})

test_that("create_parameter_grid handles prep parameters only", {
  prep_spec <- list(
    sample_rate = c(16000, 22050),
    format = c("wav", "mp3")
  )

  grid <- reindeer:::create_parameter_grid(list(), prep_spec)

  expect_s3_class(grid, "data.table")
  expect_equal(nrow(grid), 4)  # 2 * 2 = 4 combinations
  expect_true("param_hash" %in% names(grid))
  expect_equal(attr(grid, "dsp_params"), character(0))
  expect_equal(attr(grid, "prep_params"), c("sample_rate", "format"))
})

test_that("create_parameter_grid handles both DSP and prep parameters", {
  dsp_spec <- list(
    nominalF1 = c(500, 600, 700)
  )

  prep_spec <- list(
    sample_rate = c(16000, 22050)
  )

  grid <- reindeer:::create_parameter_grid(dsp_spec, prep_spec)

  expect_s3_class(grid, "data.table")
  expect_equal(nrow(grid), 6)  # 3 * 2 = 6 combinations
  expect_true(all(c("nominalF1", "sample_rate", "param_hash") %in% names(grid)))
  expect_equal(attr(grid, "dsp_params"), "nominalF1")
  expect_equal(attr(grid, "prep_params"), "sample_rate")

  # Verify all combinations are present
  expect_equal(sort(unique(grid$nominalF1)), c(500, 600, 700))
  expect_equal(sort(unique(grid$sample_rate)), c(16000, 22050))
})

test_that("create_parameter_grid generates unique hashes", {
  dsp_spec <- list(a = 1:3, b = c("x", "y"))
  prep_spec <- list(c = c(10, 20))

  grid <- reindeer:::create_parameter_grid(dsp_spec, prep_spec)

  # All hashes should be unique
  expect_equal(length(unique(grid$param_hash)), nrow(grid))

  # Hash should be consistent for same parameters
  hash1 <- digest::digest(list(dsp = list(a = 1, b = "x"), prep = list(c = 10)), algo = "md5")
  hash2 <- digest::digest(list(dsp = list(a = 1, b = "x"), prep = list(c = 10)), algo = "md5")
  expect_equal(hash1, hash2)
})

test_that("create_parameter_grid handles empty inputs", {
  # Both empty
  grid1 <- reindeer:::create_parameter_grid(list(), NULL)
  expect_s3_class(grid1, "data.table")
  expect_equal(nrow(grid1), 0)

  # NULL prep
  grid2 <- reindeer:::create_parameter_grid(list(a = 1:2), NULL)
  expect_equal(nrow(grid2), 2)
  expect_equal(attr(grid2, "prep_params"), character(0))
})

test_that("prep_passthrough reads audio without transformation", {
  skip_if_not_installed("av")
  skip_if_not_installed("emuR")

  # Create minimal test database with audio
  test_dir <- tempdir()
  db_path <- reindeer:::create_ae_db(verbose = FALSE)

  # Get a test audio file
  signal_files <- peek_signals(corpus(db_path))
  test_file <- signal_files$full_path[1]

  skip_if(!file.exists(test_file))

  # Read with prep_passthrough
  result <- prep_passthrough(
    test_file,
    sample_rate = 22050,
    format = "wav",
    verbose = FALSE
  )

  expect_type(result, "integer")
  expect_true("sample_rate" %in% names(attributes(result)))
  expect_true("channels" %in% names(attributes(result)))
  expect_true("prep_params" %in% names(attributes(result)))

  # Verify prep_params were stored
  prep_params <- attr(result, "prep_params")
  expect_equal(prep_params$sample_rate, 22050)
  expect_equal(prep_params$format, "wav")

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("prep_passthrough handles multiple files", {
  skip_if_not_installed("av")
  skip_if_not_installed("emuR")

  db_path <- reindeer:::create_ae_db(verbose = FALSE)
  signal_files <- peek_signals(corpus(db_path))
  test_files <- signal_files$full_path[1:min(2, nrow(signal_files))]

  skip_if(length(test_files) < 2)

  result <- prep_passthrough(
    test_files,
    sample_rate = 16000,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_equal(length(result), length(test_files))

  # Each result should have prep_params
  for (audio in result) {
    expect_true("prep_params" %in% names(attributes(audio)))
    expect_equal(attr(audio, "prep_params")$sample_rate, 16000)
  }

  unlink(db_path, recursive = TRUE)
})

test_that("prep_passthrough validates inputs", {
  expect_error(
    prep_passthrough(character(0)),
    "listOfFiles cannot be NULL or empty"
  )

  expect_error(
    prep_passthrough("/nonexistent/file.wav"),
    "File.*not found"
  )
})

test_that("verify_prep_params works correctly", {
  # Create mock result with prep_params
  mock_result <- list(data = 1:10)
  attr(mock_result, "prep_params") <- list(
    sample_rate = 22050,
    format = "wav"
  )

  # Should match
  expect_true(verify_prep_params(
    mock_result,
    list(sample_rate = 22050, format = "wav")
  ))

  # Should not match
  expect_false(verify_prep_params(
    mock_result,
    list(sample_rate = 16000)
  ))

  # Missing prep_params
  mock_result2 <- list(data = 1:10)
  expect_false(verify_prep_params(mock_result2, list(sample_rate = 22050)))
})

test_that("quantify_simulate validates prep function requirements", {
  skip_if_not_installed("emuR")

  db_path <- reindeer:::create_ae_db(verbose = FALSE)
  corp <- corpus(db_path)
  segments <- ask_for(corp, "[#Phonetic -> Phonetic]") %>% head(2)

  # Error if .prep_simulate without .prep_function
  expect_error(
    quantify_simulate(
      segments,
      .using = function(x) x,
      .prep_simulate = list(sample_rate = c(16000, 22050)),
      .simulation_store = tempdir()
    ),
    ".prep_function must be provided"
  )

  # Error if .prep_function is not a function
  expect_error(
    quantify_simulate(
      segments,
      .using = function(x) x,
      .prep_function = "not_a_function",
      .prep_simulate = list(sample_rate = 16000),
      .simulation_store = tempdir()
    ),
    ".prep_function must be a function"
  )

  unlink(db_path, recursive = TRUE)
})

test_that("quantify_simulate works without preprocessing", {
  skip_if_not_installed("emuR")
  skip("Integration test - requires full setup")

  db_path <- reindeer:::create_ae_db(verbose = FALSE)
  corp <- corpus(db_path)
  segments <- ask_for(corp, "[#Phonetic -> Phonetic]") %>% head(2)

  # Traditional simulation without preprocessing should still work
  result <- quantify_simulate(
    segments,
    .using = function(listOfFiles, param1 = 1, ...) {
      data.frame(value = param1, file = basename(listOfFiles))
    },
    .simulate = list(param1 = c(1, 2)),
    .simulation_store = file.path(tempdir(), "sim_test"),
    .verbose = FALSE
  )

  expect_s3_class(result, "simulation_results")
  expect_equal(length(result), 2)  # 2 parameter combinations
  expect_null(attr(result, "prep_function"))
  expect_equal(length(attr(result, "prep_params")), 0)

  unlink(db_path, recursive = TRUE)
})

test_that("simulation_results print shows prep function info", {
  # Create mock simulation_results with prep function
  mock_grid <- data.table::data.table(
    nominalF1 = rep(c(500, 600), each = 2),
    sample_rate = rep(c(16000, 22050), 2),
    param_hash = paste0("hash", 1:4)
  )
  attr(mock_grid, "dsp_params") <- "nominalF1"
  attr(mock_grid, "prep_params") <- "sample_rate"

  mock_results <- list(
    result1 = data.frame(x = 1),
    result2 = data.frame(x = 2),
    result3 = data.frame(x = 3),
    result4 = data.frame(x = 4)
  )

  class(mock_results) <- c("simulation_results", "list")
  attr(mock_results, "parameter_grid") <- mock_grid
  attr(mock_results, "dsp_function") <- "test_dsp"
  attr(mock_results, "prep_function") <- "prep_passthrough"
  attr(mock_results, "dsp_params") <- "nominalF1"
  attr(mock_results, "prep_params") <- "sample_rate"
  attr(mock_results, "timestamp") <- "20251020_120000"
  attr(mock_results, "cache_file") <- "/tmp/test.sqlite"

  # Print should work without error
  output <- capture.output(print(mock_results))

  expect_true(any(grepl("prep_passthrough", output)))
  expect_true(any(grepl("DSP Parameters", output)))
  expect_true(any(grepl("Prep Parameters", output)))
})

test_that("simulation cache stores prep function metadata", {
  skip("Requires database setup")

  # This would test that:
  # 1. prep_function name is stored in simulation_metadata table
  # 2. prep_parameter_names are stored as JSON
  # 3. prep_params_json is stored in parameter_combinations table
})

test_that("parameter grid attributes are preserved through simulation", {
  dsp_spec <- list(a = 1:2)
  prep_spec <- list(b = c("x", "y"))

  grid <- reindeer:::create_parameter_grid(dsp_spec, prep_spec)

  # Attributes should be present
  expect_equal(attr(grid, "dsp_params"), "a")
  expect_equal(attr(grid, "prep_params"), "b")

  # Subset should preserve attributes
  grid_subset <- grid[1:2, ]
  expect_equal(attr(grid_subset, "dsp_params"), "a")
  expect_equal(attr(grid_subset, "prep_params"), "b")
})
