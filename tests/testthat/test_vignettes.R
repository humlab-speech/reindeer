# Every vignette must knit, and no showcase output may be empty (a 0-row table
# in a tutorial means the example silently stopped demonstrating anything).

vignette_dir <- function() {
  d <- testthat::test_path("..", "..", "vignettes")
  if (dir.exists(d)) normalizePath(d) else ""
}

test_that("vignettes knit and show no empty results", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("superassp")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("openxlsx")
  vd <- vignette_dir()
  skip_if(!nzchar(vd), "vignettes not available")
  old <- options(reindeer.auto_cmdi = TRUE)
  on.exit(options(old), add = TRUE)
  for (f in list.files(vd, pattern = "\\.Rmd$", full.names = TRUE)) {
    out <- tempfile(fileext = ".md")
    expect_no_error(
      rmarkdown::render(f, output_format = "md_document", output_file = out,
                        quiet = TRUE, envir = new.env()))
    txt <- if (file.exists(out)) readLines(out, warn = FALSE) else character()
    empty <- grep("A tibble: 0 [x×]|^\\s*#>\\s*<0 rows>|data frame with 0 columns and 0 rows",
                  txt, value = TRUE)
    expect_length(empty, 0L)
    expect_false(any(grepl("^\\s*#> Error", txt)), info = basename(f))
  }
})
