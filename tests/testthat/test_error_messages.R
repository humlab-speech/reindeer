# ==============================================================================
# Lint guard: every R/ file must use cli::cli_abort/cli_warn instead of plain
# stop()/warning() (Item 8). Files under R/deprecated/ are exempt.
# ==============================================================================

library(testthat)
library(reindeer)

test_that("no plain stop() or warning() remain in R/ (excluding deprecated/)", {
  pkg_root <- testthat::test_path("..", "..")
  files <- list.files(file.path(pkg_root, "R"),
                      pattern = "\\.R$", full.names = TRUE,
                      recursive = TRUE)
  files <- files[!grepl("/deprecated/", files, fixed = TRUE)]

  offenders <- character()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]                          # strip comments
    src <- gsub('"[^"]*"', "", src)                            # strip strings
    src <- gsub("'[^']*'", "", src)
    hits <- grep("\\b(stop|warning)\\(", src, value = TRUE)
    if (length(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(f), ": ", hits))
    }
  }
  expect_equal(
    length(offenders), 0L,
    info = if (length(offenders)) {
      paste(c("Use cli::cli_abort / cli::cli_warn instead:", offenders),
            collapse = "\n")
    } else {
      "ok"
    }
  )
})
