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
    gregexpr("\\b([A-Za-z][A-Za-z0-9.]*)::([A-Za-z._][A-Za-z0-9._]*[A-Za-z0-9_])", lines,
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
  root <- pkg_root()
  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE),
    file.path(root, "README.md")
  )
  files <- files[file.exists(files)]
  expect_gt(length(files), 10)

  # The vignette YAML header names the knitr engine ("knitr::rmarkdown"); that
  # is a declaration, not a function reference.
  lines <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  lines <- lines[!grepl("\\\\VignetteEngine\\{", lines)]

  refs <- pkg_fun_refs(lines)
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

#' Columns named in an Rd page
#'
#' Two shapes appear in this package: `\code{name}` bullet lists (`query.Rd`,
#' under `\value`) and `name: description` bullet lists (`segment_list.Rd`,
#' under `\section{Structure}`). Both are parsed, then filtered to lowercase
#' identifiers so SEGMENT/EVENT/ITEM and prose bullets drop out.
documented_columns <- function(rd_path) {
  txt <- readLines(rd_path, warn = FALSE)
  starts <- grep("^\\\\value\\{|^\\\\section\\{Structure\\}", txt)
  expect_gt(length(starts), 0)

  body <- character()
  for (s in starts) {
    rest <- txt[(s + 1L):length(txt)]
    end <- grep("^\\}", rest)[1]
    body <- c(body, if (!is.na(end)) rest[seq_len(end - 1L)] else rest)
  }

  items <- grep("^\\\\item", body, value = TRUE)
  expect_gt(length(items), 0)

  from_code <- unlist(regmatches(items, gregexpr("\\\\code\\{([^}]*)\\}", items)))
  from_code <- sub("^\\\\code\\{", "", from_code)
  from_code <- sub("\\}$", "", from_code)

  from_colon <- sub("^\\\\item\\s+([A-Za-z0-9_.]+):.*$", "\\1", items)
  from_colon <- from_colon[grepl("^[A-Za-z0-9_.]+$", from_colon)]

  tokens <- unique(c(from_code, from_colon))
  tokens[grepl("^[a-z][a-z0-9_.]*$", tokens)]
}

test_that("columns documented for query() exist in the result", {
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
