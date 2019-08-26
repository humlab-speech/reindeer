context("Bundle and session metadata")
library(emuR)
library(dplyr)




test_that("Test that metadata is collected by get_metadata",{
  get_metadata(ae_test,add.metadata = NULL) -> res
  resnames <- names(as.data.frame(res))
  resnames <- c("session", "bundle", "file", "absolute_file_path", "Session.DateStr",
                "Session.Date", "Session.Time", "Gender", "Participant.ID", "Recording.Date")
  namesShouldBe <- c("session", "bundle", "file", "absolute_file_path","Session.DateStr","Session.Date", "Session.Time","Gender", "Participant.ID", "Recording.Date")

  expect_is(resnames,"character")
  expect_setequal(resnames,namesShouldBe )
}

)





