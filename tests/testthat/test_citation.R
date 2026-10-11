test_that("citation() takes its version from DESCRIPTION", {
  skip_if_not(nzchar(system.file(package = "reindeer")))
  cit <- format(citation("reindeer"), style = "text")
  expect_match(cit, as.character(utils::packageVersion("reindeer")), fixed = TRUE)
  expect_no_match(cit, "Extend emuR")
})
