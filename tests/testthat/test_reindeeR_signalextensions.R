context("Code extending the signal handling abilities of emuR")

reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db() -> emuDBhandle
reindeer:::make_dummy_metafiles(emuDBhandle)
fl = emuR::list_files(emuDBhandle,"wav")
unlink(emuR::list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])

#add_trackDefinition(emuDBhandle,"fm","fm","fm",onTheFlyFunctionName = "praat_formant_burg",onTheFlyOptLogFilePath = "/Users/frkkan96/Desktop/test")

test_that("Signals can be created using wrassp functions",{

  reindeer::add_trackDefinition(emuDBhandle,"rms","rms","rms",onTheFlyFunctionName = "rmsana")
  fe <- reindeer::list_ssffTrackDefinitions(emuDBhandle)$fileExtension
  flrms <- reindeer::list_files(emuDBhandle,"rms")
  expect_true("rms" %in% fe)
  #Make sure one
  expect_equal(dim(flrms),dim(fl),label="One rms file should be created for each wav file2")
})
