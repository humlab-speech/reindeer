context("Bundle (session and database) metadata")

## Setup
reindeer:::create_ae_db() -> ae_test
#
teardown({

  reindeer:::unlink_emuRDemoDir()
})


test_that("Metadata is collected correctly by get_metadata",{
  reindeer:::make_dummy_test_metafiles(ae_test)

  res <- reindeer::get_metadata(ae_test)
  resnames <- names(as.data.frame(res))
  namesShouldBe <- c("session", "bundle", "Session.Date", "Session.Time", "Participant.ID", "Researcher", "Gender", "Condition", "Setup")
  outputShouldBe <- openxlsx::read.xlsx(file.path("..","expected_meta.xlsx"),sheet="bundles")


  expect_named(res,namesShouldBe)
  expect_equal(res,outputShouldBe)

}

)

test_that("Import of metadata from an Excel file produces an exected result",{
  unlink_emuRDemoDir()
  reindeer:::create_ae_db() -> ae_test
  reindeer:::make_dummy_metafiles(ae_test)
  dummyRes <- reindeer::get_metadata(ae_test)

  reindeer:::detach_demo_db(ae_test)
  create_ae_test_db() -> ae_test

  import_metadata(ae_test,file.path("..","expected_meta.xlsx"))
  importRes <- reindeer::get_metadata(ae_test)
  importRes <- importRes %>%
    dplyr::select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    dplyr::arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)

  dummyRes <- dummyRes %>%
    dplyr::select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    dplyr::arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)


  expect_identical(na.omit(dummyRes), na.omit(importRes))
  expect_equal(as.list(table(is.na(dummyRes))),list(`FALSE`=122,`TRUE`=4))
  expect_equal(as.list(table(is.na(importRes))),list(`FALSE`=122,`TRUE`=4))
}
)

test_that("Test of the digest adding function",{
  reindeer::add_digests(ae_test,algorithm="md5")
  reindeer::add_digests(ae_test)
  reindeer::add_digests(ae_test,algorithm="sha512")

  md <- reindeer::get_metadata(ae_test)

  expect_false(all(is.na(md[c("Bundle.Duration.ms","Bundle.md5_checksum","Bundle.sha512_checksum")])))
}
)


