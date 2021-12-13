
require(testthat)
require(reindeer)

test_that("Importation of speech signal files works",{
  reindeer:::unlink_emuRDemoDir()
  reindeer:::create_ae_db(verbose = FALSE) -> emuDBhandle
  sounds <- file.path("..","signalfiles","generated")

  expect_true(dir.exists(sounds))

  reindeer::import_recordings(emuDBhandle,dir=sounds,targetSessionName = "generated",verbose = FALSE)

  expect_equal(nrow(list_sessions(emuDBhandle)) , 2  )

  expect_error(reindeer::import_recordings(emuDBhandle,dir=sounds,targetSessionName = "generated2",verbose = FALSE,speech.channel = 3))

  pw <- list_files(emuDBhandle,sessionPattern = "generated",fileExtension = "wav")

  expect_equal(nrow(pw),  3)

  expect_equal(nrow(list_bundles(emuDBhandle,session = "generated")) , 3 )

  #Check that all files are mono sound files, and readable as such
  for(i in 1:nrow(pw)){

    signalfile <- wrassp::read.AsspDataObj(pw$absolute_file_path[[i]])

    expect_equal(ncol(signalfile$audio) , 1)

  }



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

