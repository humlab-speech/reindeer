context("Bundle and session metadata")
library(emuR)
library(dplyr)
library(openxlsx)




test_that("Metadata is collected correctly by get_metadata",{
  make_dummy_metafiles(ae_test)

  res <- get_metadata(ae_test,add.metadata = NULL)
  resnames <- names(as.data.frame(res))
  namesShouldBe <- c("session", "bundle", "Session.Date", "Session.Time", "Participant.ID", "Researcher", "Gender", "Condition", "Setup")
  outputShouldBe <- openxlsx::read.xlsx(file.path("..","expected_meta.xlsx"),sheet="bundles")


  expect_named(res,namesShouldBe)
  expect_equal(res,outputShouldBe)

}

)

test_that("Import of metadata from an Excel file produces an exected result",{
  make_dummy_metafiles(ae_test)
  dummyRes <- get_metadata(ae_test,add.metadata = NULL)

  unlink_emuRDemoDir()
  create_ae_db() -> ae_test

  import_metadata(ae_test,file.path("..","expected_meta.xlsx")) -> metaFiles
  importRes <- get_metadata(ae_test,add.metadata = NULL)
  importRes <- importRes %>%
    select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)

  dummyRes <- dummyRes %>%
    select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)

  #expect_length(metaFiles,14+2) # 14 bundle files and two session files.


  expect_identical(na.omit(dummyRes),
               na.omit(importRes))
  expect_equal(as.list(table(is.na(dummyRes))),list(`FALSE`=122,`TRUE`=4))
  expect_equal(as.list(table(is.na(importRes))),list(`FALSE`=122,`TRUE`=4))
}
)



