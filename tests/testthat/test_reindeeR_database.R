
require(testthat)
require(reindeer)

test_that("Importation of speech signal files works",{
  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db(verbose = FALSE) -> emuDBhandle
  prolonged <- "../signalfiles/prolonged_a/"
  #prolonged <- "tests/signalfiles/prolonged_a/"
  expect_true(dir.exists(prolonged))

  reindeer::import_recordings(emuDBhandle,dir=prolonged,targetSessionName = "prolonged",verbose = FALSE)

  expect_error(reindeer::import_recordings(emuDBhandle,dir=prolonged,targetSessionName = "prolongedError",verbose = FALSE,speech.channel = 3))

  pw <- list_files(emuDBhandle,sessionPattern = "prolonged",fileExtension = "wav")

  expect_true(nrow(pw) == 3)

  expect_true(nrow(list_sessions(emuDBhandle)) == 3  )

  expect_true(nrow(list_bundles(emuDBhandle,session = "prolonged")) == 3 )

  signalfile <- wrassp::read.AsspDataObj(pw$absolute_file_path[[1]])

  expect_true(ncol(signalfile$audio) == 1)

})


test_that("Importation of EGG and speech signal files works",{
  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db(verbose = FALSE) -> emuDBhandle
  egg <- "../signalfiles/EGG/Session 1/"
  #egg <- "tests/signalfiles/EGG/Session 1/"
  expect_true(dir.exists(egg))

  reindeer::import_recordings(emuDBhandle,dir=egg,targetSessionName="EGG", verbose = FALSE,egg.channel = 2)

  pw <- list_files(emuDBhandle,sessionPattern = "EGG",fileExtension = "wav")

  expect_true(nrow(pw) == 1)

  expect_true(nrow(list_sessions(emuDBhandle)) == 2 )

  expect_true(nrow(list_bundles(emuDBhandle,session = "EGG")) == 1 )

  signalfile <- wrassp::read.AsspDataObj(pw$absolute_file_path[[1]])

  expect_true(ncol(signalfile$audio) == 1)

  pe <- list_files(emuDBhandle,sessionPattern = "EGG",fileExtension = "egg")

  expect_true(nrow(pe) == 1)

  expect_true(nrow(list_bundles(emuDBhandle,session = "EGG")) == 1 )

  eggfile <- wrassp::read.AsspDataObj(pe$absolute_file_path[[1]])

  expect_true(ncol(eggfile$audio) == 1)


})

