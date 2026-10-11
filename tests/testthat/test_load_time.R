test_that("library(reindeer) does not load ggplot2 or Rdpack", {
  skip_if_not(workers_can_load_reindeer(), "reindeer is not installed")
  out <- system2(file.path(R.home("bin"), "Rscript"),
                 c("-e", shQuote("suppressMessages(library(reindeer)); cat(c('ggplot2','Rdpack','grid') %in% loadedNamespaces())")),
                 stdout = TRUE)
  expect_equal(out, "FALSE FALSE FALSE")
})
