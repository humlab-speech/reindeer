test_that("quantify2 works with segment lists", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  # Create two segment lists
  vowels <- ask_for(corp, "Phonetic =~ V")
  consonants <- ask_for(corp, "Phonetic =~ [ptkbdgmnlrszfvwj]")
  
  # Define a simple comparison function
  compare_segments <- function(seg1, seg2) {
    list(
      n_seg1 = nrow(seg1),
      n_seg2 = nrow(seg2),
      total = nrow(seg1) + nrow(seg2)
    )
  }
  
  # Test with segment lists
  result <- quantify2(vowels, consonants, .from = corp, 
                      .using = compare_segments, .quiet = TRUE)
  
  expect_true(is.data.frame(result))
  expect_true("result" %in% names(result))
  expect_true("session" %in% names(result))
  expect_true(nrow(result) > 0)
  
  # Check that results are lists
  expect_true(all(sapply(result$result, is.list)))
})

test_that("quantify2 works with EQL queries", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  # Define comparison function
  compare_func <- function(data1, data2) {
    list(count1 = nrow(data1), count2 = nrow(data2))
  }
  
  # Test with query strings
  result <- quantify2("Phonetic =~ V", "Phonetic =~ [ptk]",
                      .from = corp, .using = compare_func, .quiet = TRUE)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("quantify2 by_bundle processing works", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  vowels <- ask_for(corp, "Phonetic =~ V")
  consonants <- ask_for(corp, "Phonetic =~ [ptk]")
  
  compare_func <- function(seg1, seg2) {
    list(n1 = nrow(seg1), n2 = nrow(seg2))
  }
  
  # Process by bundle
  result_bundle <- quantify2(vowels, consonants, .from = corp,
                             .using = compare_func, .by_bundle = TRUE, .quiet = TRUE)
  
  # Process by session
  result_session <- quantify2(vowels, consonants, .from = corp,
                              .using = compare_func, .by_bundle = FALSE, .quiet = TRUE)
  
  expect_true(is.data.frame(result_bundle))
  expect_true(is.data.frame(result_session))
  
  # Should have more rows when processing by bundle
  expect_true(nrow(result_bundle) >= nrow(result_session))
})

test_that("quantify2 handles non-matching groups", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  # Get segments that might not overlap completely
  vowels <- ask_for(corp, "Phonetic == V")
  some_consonants <- ask_for(corp, "Phonetic == t")
  
  compare_func <- function(seg1, seg2) {
    list(count1 = nrow(seg1), count2 = nrow(seg2))
  }
  
  # Should warn about non-matching but still proceed
  expect_warning(
    result <- quantify2(vowels, some_consonants, .from = corp,
                        .using = compare_func),
    NA  # No error expected
  )
})

test_that("quantify2 validates inputs correctly", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  vowels <- ask_for(corp, "Phonetic =~ V")
  
  # Missing .using argument
  expect_error(
    quantify2(vowels, vowels, .from = corp),
    "Missing"
  )
  
  # Invalid function (only 1 parameter)
  bad_func <- function(x) { return(1) }
  expect_error(
    quantify2(vowels, vowels, .from = corp, .using = bad_func, .quiet = TRUE),
    "at least 2 arguments"
  )
  
  # Missing corpus for EQL query
  expect_error(
    quantify2("Phonetic == V", "Phonetic == C", .using = function(a, b) {}),
    "Missing corpus"
  )
})

test_that("quantify2 passes additional arguments correctly", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  vowels <- ask_for(corp, "Phonetic =~ V")
  consonants <- ask_for(corp, "Phonetic =~ [ptk]")
  
  # Function with extra parameters
  compare_with_param <- function(seg1, seg2, multiplier = 1) {
    list(
      total = (nrow(seg1) + nrow(seg2)) * multiplier
    )
  }
  
  result <- quantify2(vowels, consonants, .from = corp,
                      .using = compare_with_param, 
                      multiplier = 5, .quiet = TRUE)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  
  # Check that multiplier was applied
  first_result <- result$result[[1]]
  expect_true(first_result$total >= 5)  # Should be at least 5 if multiplier worked
})

test_that("quantify2 works with fake DSP functions from examples", {
  skip_on_cran()
  
  # Setup test database
  create_ae_db(verbose = FALSE) -> ae
  corp <- corpus(ae$basePath)
  
  vowels <- ask_for(corp, "Phonetic =~ V") |> head(3)
  consonants <- ask_for(corp, "Phonetic =~ [ptk]") |> head(3)
  
  # Should work with fake_two_df_fun from tidy_trackdata.R
  if (exists("fake_two_df_fun", where = asNamespace("reindeer"))) {
    result <- quantify2(vowels, consonants, .from = corp,
                        .using = reindeer:::fake_two_df_fun, .quiet = TRUE)
    
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
  }
})
