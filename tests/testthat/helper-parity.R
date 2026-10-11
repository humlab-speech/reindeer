# emuR parity harness. Golden outputs come from tools/make-eql-golden.R.
# Rows are compared UNSORTED: emuR row order is part of the contract.

eql_golden <- function() {
  readRDS(testthat::test_path("fixtures", "eql_golden.rds"))$queries
}

# Character vector of mismatching columns ("rows" = row-count mismatch,
# "error" = reindeer errored); character(0) means parity.
eql_parity_problems <- function(corp, eql, tol = 1e-9) {
  exp <- eql_golden()[[eql]]$rows
  got <- tryCatch(as.data.frame(reindeer::collect(reindeer::query(corp, eql))),
                  error = function(e) NULL)
  if (is.null(got)) return("error")
  if (nrow(got) != nrow(exp)) return("rows")
  if (nrow(exp) == 0) return(character())
  bad <- character()
  for (col in c("session", "bundle", "labels")) {
    if (!identical(as.character(got[[col]]), as.character(exp[[col]]))) bad <- c(bad, col)
  }
  for (col in c("start", "end")) {
    if (!isTRUE(all.equal(as.numeric(got[[col]]), as.numeric(exp[[col]]), tolerance = tol)))
      bad <- c(bad, col)
  }
  for (col in c("start_item_id", "end_item_id")) {
    if (!isTRUE(all.equal(as.integer(got[[col]]), as.integer(exp[[col]])))) bad <- c(bad, col)
  }
  bad
}

expect_eql_parity <- function(corp, eql, tol = 1e-9) {
  testthat::expect_identical(eql_parity_problems(corp, eql, tol), character(), info = eql)
}
