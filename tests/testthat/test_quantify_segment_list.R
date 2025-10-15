test_that("segment_list class validation works", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("S7")
  
  # Create ae database for testing
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  # Load database
  ae_db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(ae_db$connection))
  
  # Get query result
  query_result <- emuR::query(ae_db, "Phonetic == n", resultType = "tibble")
  
  # Convert to segment_list
  seg_list <- as_segment_list(query_result, 
                               db_uuid = ae_db$UUID,
                               db_path = ae_path)
  
  # Test class
  expect_true(inherits(seg_list, "segment_list"))
  expect_true(is_segment_list(seg_list))
  
  # Test properties
  expect_equal(seg_list@db_uuid, ae_db$UUID)
  expect_equal(seg_list@db_path, ae_path)
  
  # Test required columns
  required_cols <- c("labels", "start", "end", "db_uuid", "session", 
                     "bundle", "start_item_id", "end_item_id", "level", 
                     "attribute", "start_item_seq_idx", "end_item_seq_idx",
                     "type", "sample_start", "sample_end", "sample_rate")
  expect_true(all(required_cols %in% names(seg_list)))
})

test_that("segment_list can be created from emuR::query result", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  ae_db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(ae_db$connection))
  
  # Query using emuR
  query_result <- emuR::query(ae_db, "Phonetic == t", resultType = "tibble")
  
  # Convert to segment_list
  seg_list <- as_segment_list(query_result, 
                               db_uuid = ae_db$UUID,
                               db_path = ae_path)
  
  # Verify data matches
  expect_equal(nrow(seg_list), nrow(query_result))
  expect_equal(seg_list$labels, query_result$labels)
  expect_equal(seg_list$start, query_result$start)
  expect_equal(seg_list$end, query_result$end)
})

test_that("ask_for returns segment_list", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  # Query using ask_for
  seg_list <- ask_for(ae_path, "Phonetic == t")
  
  # Test class
  expect_true(is_segment_list(seg_list))
  
  # Test that it has valid db_path
  expect_true(nchar(attr(seg_list, "basePath")) > 0)
  
  # Test data
  expect_gt(nrow(seg_list), 0)
  expect_true(all(seg_list$labels == "t"))
})

test_that("quantify works with fake DSP function", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  # Create corpus
  corp <- corpus(ae_path, verbose = FALSE)
  
  # Get segments
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  # Create a fake DSP function that returns a data.frame
  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(
      f0_mean = 120.5,
      f0_sd = 15.2,
      intensity = 70.3
    )
  }
  
  # Apply quantify
  result <- quantify(seg_list, fake_dsp, .parallel = FALSE, .verbose = FALSE)
  
  # Check result
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  
  # Check that segment info is preserved
  expect_true("labels" %in% names(result))
  expect_true("start" %in% names(result))
  expect_true("end" %in% names(result))
  
  # Check that DSP results are included
  expect_true("f0_mean" %in% names(result))
  expect_true("f0_sd" %in% names(result))
  expect_true("intensity" %in% names(result))
  
  # Check values
  expect_equal(unique(result$f0_mean), 120.5)
})

test_that("quantify works with .at parameter for time points", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  # Create a fake DSP that returns multiple frames (simulating SSFF track)
  fake_dsp_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    # Simulate 10 frames
    n_frames <- 10
    obj <- data.frame(
      f1 = seq(500, 600, length.out = n_frames),
      f2 = seq(1500, 1600, length.out = n_frames),
      f3 = seq(2500, 2600, length.out = n_frames)
    )
    class(obj) <- c("AsspDataObj", "data.frame")
    obj
  }
  
  # Extract at specific time points
  result <- quantify(seg_list, fake_dsp_track, 
                    .at = c(0.25, 0.5, 0.75), 
                    .parallel = FALSE, 
                    .verbose = FALSE)
  
  # Check that we get 3 rows per segment
  n_segs <- nrow(seg_list)
  expect_equal(nrow(result), n_segs * 3)
  
  # Check that .time_point column exists
  expect_true(".time_point" %in% names(result))
  
  # Check time points
  expect_setequal(unique(result$.time_point), c(0.25, 0.5, 0.75))
})

test_that("quantify handles empty segment list", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  
  # Get empty segment list
  seg_list <- ask_for(corp, "Phonetic == zzz")
  seg_list@db_path <- ae_path
  
  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = 1)
  }
  
  # Should return empty tibble
  result <- quantify(seg_list, fake_dsp, .parallel = FALSE, .verbose = FALSE)
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("quantify error handling works", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  # Test with invalid .at parameter
  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = 1)
  }
  
  expect_error(
    quantify(seg_list, fake_dsp, .at = c(-0.5, 0.5)),
    ".at.*between 0 and 1"
  )
  
  expect_error(
    quantify(seg_list, fake_dsp, .at = c(0.5, 1.5)),
    ".at.*between 0 and 1"
  )
})

test_that("quantify with metadata derivation works", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  
  # Add some metadata
  corp["0000", "msajc003"] <- list(Gender = "Male", Age = 25)
  
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  # Fake DSP that accepts Gender and Age parameters
  fake_dsp_with_params <- function(listOfFiles, beginTime, endTime, 
                                    Gender = "Unknown", Age = 0,
                                    toFile = FALSE, verbose = FALSE, ...) {
    data.frame(
      gender_used = Gender,
      age_used = Age,
      value = 1
    )
  }
  
  # Apply with metadata
  result <- quantify(seg_list, fake_dsp_with_params, 
                    .use_metadata = TRUE,
                    .parallel = FALSE,
                    .verbose = FALSE)
  
  # Check that metadata was used
  expect_true("gender_used" %in% names(result))
  expect_true("age_used" %in% names(result))
  
  # At least some segments should have the metadata
  # (Note: only msajc003 bundle has metadata set)
  msajc003_rows <- result[result$bundle == "msajc003", ]
  if (nrow(msajc003_rows) > 0) {
    expect_true(any(msajc003_rows$gender_used == "Male"))
    expect_true(any(msajc003_rows$age_used == 25))
  }
})

test_that("quantify parallel processing works", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    Sys.sleep(0.01)  # Simulate some work
    data.frame(value = runif(1))
  }
  
  # Test parallel
  time_parallel <- system.time({
    result_parallel <- quantify(seg_list, fake_dsp, 
                                .parallel = TRUE,
                                .workers = 2,
                                .verbose = FALSE)
  })
  
  # Test sequential
  time_sequential <- system.time({
    result_sequential <- quantify(seg_list, fake_dsp, 
                                  .parallel = FALSE,
                                  .verbose = FALSE)
  })
  
  # Both should produce results
  expect_gt(nrow(result_parallel), 0)
  expect_gt(nrow(result_sequential), 0)
  expect_equal(nrow(result_parallel), nrow(result_sequential))
  
  # Note: Parallel might not always be faster for small datasets with overhead,
  # but both methods should work
})

test_that("segment_list print and summary methods work", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  seg_list <- ask_for(ae_path, "Phonetic == t")
  
  # Test print
  expect_output(print(seg_list), "Segment List")
  expect_output(print(seg_list), "Database:")
  expect_output(print(seg_list), "Segments:")
  
  # Test summary
  expect_output(summary(seg_list), "Segment List Summary")
  expect_output(summary(seg_list), "Number of segments:")
  expect_output(summary(seg_list), "Duration range:")
})

test_that("quantify preserves segment ordering", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  seg_list <- ask_for(corp, "Phonetic =~ .*")
  seg_list@db_path <- ae_path
  
  # Take first few segments
  seg_list_subset <- seg_list[1:5, ]
  
  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = runif(1))
  }
  
  result <- quantify(as_segment_list(seg_list_subset, 
                                     db_uuid = seg_list@db_uuid,
                                     db_path = seg_list@db_path), 
                    fake_dsp, 
                    .parallel = FALSE, 
                    .verbose = FALSE)
  
  # Check ordering is preserved
  expect_equal(result$start_item_id, seg_list_subset$start_item_id)
  expect_equal(result$labels, seg_list_subset$labels)
})

test_that("quantify handles list output from DSP", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae_emuDB", package = "emuR")
  skip_if(ae_path == "", "ae demo data not available")
  
  corp <- corpus(ae_path, verbose = FALSE)
  seg_list <- ask_for(corp, "Phonetic == n")
  seg_list@db_path <- ae_path
  
  # DSP that returns a list
  fake_dsp_list <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    list(
      f0 = c(120, 125, 130),
      intensity = c(70, 72, 71),
      quality = "good"
    )
  }
  
  result <- quantify(seg_list, fake_dsp_list, 
                    .parallel = FALSE, 
                    .verbose = FALSE)
  
  # Check result structure
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  
  # List should be converted to data frame columns
  expect_true(any(grepl("f0|intensity|quality", names(result))))
})
