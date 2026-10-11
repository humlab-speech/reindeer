# Re-scan the golden EQL queries and rewrite `known_failures` in
# tests/testthat/test_eql_parity.R. Run from the package root after a parity fix.
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-parity.R")
eql_golden <- function() readRDS("tests/testthat/fixtures/eql_golden.rds")$queries
corp <- corpus(reindeer:::create_ae_db(), verbose = FALSE)
res <- vapply(names(eql_golden()), function(q) paste(eql_parity_problems(corp, q), collapse = ","), "")
bad <- nzchar(res)
wp <- ifelse(res == "start,end", "A6: WP3.1", "A6/A7: WP3.2-3.4")[bad]
esc <- function(s) encodeString(s, quote = '"')
body <- if (any(bad)) paste0("  ", esc(names(res)[bad]), " = ", esc(wp), collapse = ",\n") else ""
f <- "tests/testthat/test_eql_parity.R"
s <- paste(readLines(f), collapse = "\n")
s <- sub("known_failures <- list\\(.*?\\n\\)\\n", paste0("known_failures <- list(\n", body, "\n)\n"), s)
writeLines(s, f)
cat(sum(bad), "known;", sum(!bad), "passing\n")
print(table(res[bad]))
