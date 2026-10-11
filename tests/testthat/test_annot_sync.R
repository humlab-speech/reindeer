# WP4.5: incremental cache sync on open.

annot_path <- function(corp, ses, b) {
  file.path(corp@basePath, paste0(ses, "_ses"), paste0(b, "_bndl"), paste0(b, "_annot.json"))
}
edit_label <- function(f, from, to) {
  txt <- readLines(f, warn = FALSE)
  writeLines(sub(sprintf('"value" *: *"%s"', from), sprintf('"value": "%s"', to), txt), f)
  Sys.setFileTime(f, Sys.time() + 5)
}

test_that("reopening sees an external annotation edit without a full rebuild", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  path <- corp@basePath
  n_t <- nrow(collect(query(corp, "Phonetic == t")))
  expect_gt(n_t, 0)
  edit_label(annot_path(corp, "0000", "msajc003"), "t", "TT")
  rm(corp); gc()
  corp2 <- corpus(path, verbose = FALSE)
  expect_equal(nrow(collect(query(corp2, "Phonetic == TT"))), 1L)
  expect_equal(nrow(collect(query(corp2, "Phonetic == t"))), n_t - 1L)
})

test_that("unchanged bundles are not re-read on reopen", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  counts <- reindeer:::sync_emuDB_cache(corp@basePath)
  expect_equal(counts$added + counts$updated + counts$removed, 0L)
  expect_gt(counts$unchanged, 0L)
})

test_that("added and removed bundles are picked up", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  base <- corp@basePath
  n_b <- length(unique(collect(query(corp, "Phonetic =~ .*"))$bundle))
  src <- file.path(base, "0000_ses", "msajc003_bndl")
  dst <- file.path(base, "0000_ses", "copy1_bndl")
  dir.create(dst)
  file.copy(list.files(src, full.names = TRUE), dst)
  for (f in list.files(dst, full.names = TRUE)) {
    file.rename(f, file.path(dst, sub("msajc003", "copy1", basename(f))))
  }
  expect_equal(reindeer:::sync_emuDB_cache(base)$added, 1L)
  unlink(src, recursive = TRUE)
  expect_equal(reindeer:::sync_emuDB_cache(base)$removed, 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(base, "ae_emuDBcache.sqlite"))
  on.exit(DBI::dbDisconnect(con))
  b <- DBI::dbGetQuery(con, "SELECT name FROM bundle")$name
  expect_true("copy1" %in% b)
  expect_false("msajc003" %in% b)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM items WHERE bundle = 'msajc003'")$n, 0)
})

test_that("a touched but identical file is not re-parsed", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  f <- annot_path(corp, "0000", "msajc003")
  Sys.setFileTime(f, Sys.time() + 10)
  counts <- reindeer:::sync_emuDB_cache(corp@basePath)
  expect_equal(counts$updated, 0L)
})

test_that("the quick argument warns and is ignored", {
  skip_if_no_emuR()
  p <- create_isolated_ae_db()
  expect_warning(corpus(p, quick = FALSE), class = "reindeer_deprecated")
})
