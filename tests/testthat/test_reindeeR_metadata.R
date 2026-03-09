# Legacy metadata tests
# These test the old get_metadata(emuDBhandle) API which was replaced by
# the optimized metadata system (tested in test_metadata_optimized.R).

library(testthat)

test_that("Metadata is collected correctly for a database", {
  skip("Legacy metadata API — superseded by optimized metadata system")
})

test_that("Metadata may be set to NA and signal processing still works", {
  skip("Legacy metadata API — superseded by optimized metadata system")
})
