# Documentation contracts: the docs are executable assertions.
#
# These tests exist because the package's examples and vignettes never execute
# in CI, which let two classes of drift go unnoticed:
#   * documented `pkg::fun` references that do not exist (T7), and
#   * documented columns that the returned object does not have (T8).

pkg_root <- function() testthat::test_path("..", "..")

#' Extract `pkg::fun` references from a character vector
pkg_fun_refs <- function(lines) {
  m <- regmatches(
    lines,
    gregexpr("\\b([A-Za-z][A-Za-z0-9.]*)::([A-Za-z._][A-Za-z0-9._]*)", lines,
             perl = TRUE)
  )
  refs <- unlist(m, use.names = FALSE)
  if (length(refs) == 0) return(data.frame(pkg = character(), fun = character()))
  parts <- strsplit(refs, "::", fixed = TRUE)
  data.frame(
    pkg = vapply(parts, `[`, character(1), 1L),
    fun = vapply(parts, `[`, character(1), 2L),
    stringsAsFactors = FALSE
  )
}

test_that("every package::function reference in the docs resolves", {
  skip("PENDING WP1.9 - see dev/REINDEER_REMEDIATION_PLAN.md")
  root <- pkg_root()
  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE),
    file.path(root, "README.md")
  )
  files <- files[file.exists(files)]
  expect_gt(length(files), 10)

  refs <- pkg_fun_refs(unlist(lapply(files, readLines, warn = FALSE),
                             use.names = FALSE))
  expect_gt(nrow(refs), 20)

  # Only packages whose namespace we can inspect are checked; a reference to an
  # uninstalled companion cannot be verified here (CI installs them; see
  # .github/workflows/R-CMD-check.yaml).
  refs$installed <- vapply(
    refs$pkg,
    function(p) {
      if (identical(p, "reindeer")) return(TRUE)  # the package under test
      requireNamespace(p, quietly = TRUE)
    },
    logical(1)
  )
  checked <- unique(refs[refs$installed, c("pkg", "fun")])
  skip_if(nrow(checked) == 0, "no inspectable packages referenced")

  missing <- list()
  for (i in seq_len(nrow(checked))) {
    pkg <- checked$pkg[i]
    fun <- checked$fun[i]
    exports <- if (identical(pkg, "reindeer")) {
      getNamespaceExports(asNamespace("reindeer"))
    } else {
      getNamespaceExports(pkg)
    }
    if (!fun %in% exports) {
      missing[[length(missing) + 1L]] <- paste0(pkg, "::", fun)
    }
  }

  expect_equal(
    length(missing), 0,
    info = paste0(
      "Documentation references functions that do not exist:\n  ",
      paste(unique(unlist(missing)), collapse = "\n  ")
    )
  )
})

#' Columns named in the `\item` bullets of an Rd `\value` section
documented_columns <- function(rd_path) {
  txt <- readLines(rd_path, warn = FALSE)
  start <- grep("^\\\\value\\{", txt)
  expect_length(start, 1L)
  rest <- txt[(start + 1L):length(txt)]
  end <- grep("^\\}", rest)[1]
  body <- if (!is.na(end)) rest[seq_len(end - 1L)] else rest
  items <- grep("^\\\\item", body, value = TRUE)
  tokens <- regmatches(items, gregexpr("\\\\code\\{([^}]*)\\}", items))
  tokens <- unlist(tokens, use.names = FALSE)
  tokens <- sub("^\\\\code\\{", "", tokens)
  tokens <- sub("\\}$", "", tokens)
  # Column names are lowercase identifiers; this drops SEGMENT/EVENT/ITEM and
  # prose such as `lazy = FALSE`.
  tokens[grepl("^[a-z][a-z0-9_.]*$", tokens)]
}

test_that("columns documented for query() exist in the result", {
  skip("PENDING WP1.9 - see dev/REINDEER_REMEDIATION_PLAN.md")
  docs <- documented_columns(file.path(pkg_root(), "man", "query.Rd"))
  expect_gt(length(docs), 5)  # guard against a vacuous parse

  segs <- query(demo_corpus(), "Phoneme =~ .+") |> collect()
  missing <- setdiff(docs, names(segs))
  expect_equal(
    missing, character(0),
    info = paste0("query() docs promise columns that are absent: ",
                  paste(missing, collapse = ", "))
  )
})

test_that("columns documented for segment_list exist in the result", {
  skip("PENDING WP1.9 - see dev/REINDEER_REMEDIATION_PLAN.md")
  docs <- documented_columns(file.path(pkg_root(), "man", "segment_list.Rd"))
  expect_gt(length(docs), 5)

  segs <- query(demo_corpus(), "Phoneme =~ .+") |> collect()
  missing <- setdiff(docs, names(segs))
  expect_equal(
    missing, character(0),
    info = paste0("segment_list docs promise columns that are absent: ",
                  paste(missing, collapse = ", "))
  )
})
