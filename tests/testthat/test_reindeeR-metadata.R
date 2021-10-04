#context("Bundle (session and database) metadata")
library(testthat)
library(wrassp)


test_that("Metadata is collected correctly for a database",{

  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
  reindeer:::make_dummy_metafiles(emuDBhandle)
  fl = emuR::list_files(emuDBhandle,"wav")
  md <- reindeer::get_metadata(emuDBhandle)

  testthat::expect_equal(nrow(md),nrow(fl))

})


test_that("Metadata may be set to NA and signal processing still works",{

  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db(verbose=FALSE) -> emuDBhandle
  reindeer:::make_dummy_metafiles(emuDBhandle)
  fl = emuR::list_files(emuDBhandle,"wav")
  reindeer::add_metadata(emuDBhandle,list(Age=NA,Gender=NA),bundle="msajc003")
  md <- reindeer::get_metadata(emuDBhandle)

  testthat::expect_equal(nrow(md),nrow(fl))

  reindeer::add_metadata(emuDBhandle,list(Gender=NA),bundle="msajc012")
  md <- reindeer::get_metadata(emuDBhandle)

  testthat::expect_equal(nrow(md),nrow(fl))

  reindeer::add_metadata(emuDBhandle,list(Gender=NA),bundle="msajc015")
  md <- reindeer::get_metadata(emuDBhandle)

  testthat::expect_equal(nrow(md),nrow(fl))

  #Now try generating a formant-type measure
  reindeer::add_trackDefinition(emuDBhandle,"F",columnName = "fm",onTheFlyFunctionName = "forest",verbose=FALSE)

  fmsFiles <- list_files(emuDBhandle,"fms")
  testthat::expect_equal(nrow(fmsFiles),nrow(fl))

  #Now try generating a formant-type measure
  reindeer::add_trackDefinition(emuDBhandle,"f0",columnName = "F0",onTheFlyFunctionName = "ksvF0",verbose=FALSE)

  f0Files <- list_files(emuDBhandle,"f0")
  testthat::expect_equal(nrow(f0Files),nrow(fl))

})




