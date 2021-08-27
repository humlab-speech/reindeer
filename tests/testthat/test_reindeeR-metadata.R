#context("Bundle (session and database) metadata")
library(testthat)
library(wrassp)


test_that("Metadata is collected correctly for a database",{

  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db() -> emuDBhandle
  reindeer:::make_dummy_metafiles(emuDBhandle)
  fl = emuR::list_files(emuDBhandle,"wav")
  md <- reindeer::get_metadata(emuDBhandle)

  expect_equal(nrow(md),nrow(fl))


})



