# Legacy signal extensions tests
# These test deprecated functions (dspp_metadataParameters, match_parameters,
# add_trackDefinition, getSamples, readTrackData) that have been removed.
# Signal processing is now tested via quantify() in test_quantify_segment_list.R.

library(testthat)

test_that("Signal extensions tests are skipped (deprecated functions removed)", {
  skip("Legacy signal extensions — functions removed in optimization phase")
})
