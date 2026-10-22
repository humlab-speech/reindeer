# Coverage for import_bundle()/import_session(): consistent import
# (create + metadata + media + auto track generation from the corpus'
# registered generator recipes). See
# specs/2026-09-28-import-bundle-session-design.md.

library(testthat)
library(reindeer)

test_that("import_bundle creates a new session/bundle, writes metadata, and imports media", {
  ae <- create_isolated_ae_corpus()
  src_wav <- reindeer:::peek_signals(ae)$full_path[1]

  result <- import_bundle(ae, "NewSession", "NewBundle", media = src_wav,
                           metadata = list(Age = 42, Gender = "F"),
                           tracks = FALSE, verbose = FALSE)

  expect_true(S7::S7_inherits(result, reindeer::corpus))

  bundle_dir <- file.path(ae@basePath, "NewSession_ses", "NewBundle_bndl")
  expect_true(dir.exists(bundle_dir))
  expect_true(file.exists(file.path(bundle_dir, "NewBundle.wav")))

  meta <- get_metadata(ae, "NewSession", "NewBundle")
  expect_equal(as.character(meta$Age), "42")
  expect_equal(as.character(meta$Gender), "F")
})

test_that("import_bundle replays registered track generators for the new bundle only", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  existing_full_path <- sigs$full_path[1]
  src_wav <- sigs$full_path[2]

  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE)

  existing_rms <- sub("\\.wav$", ".rms", existing_full_path)
  expect_true(file.exists(existing_rms))
  mtime_before <- file.info(existing_rms)$mtime

  import_bundle(ae, "NewSession2", "NewBundle2", media = src_wav,
                metadata = list(Age = 30, Gender = "M"),
                tracks = TRUE, verbose = FALSE)

  new_rms <- file.path(ae@basePath, "NewSession2_ses", "NewBundle2_bndl", "NewBundle2.rms")
  expect_true(file.exists(new_rms))
  expect_equal(file.info(existing_rms)$mtime, mtime_before)
})

test_that("import_bundle with tracks = FALSE creates the bundle but generates no tracks", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  src_wav <- sigs$full_path[1]

  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE)

  import_bundle(ae, "NoTrackSession", "NoTrackBundle", media = src_wav,
                tracks = FALSE, verbose = FALSE)

  new_rms <- file.path(ae@basePath, "NoTrackSession_ses", "NoTrackBundle_bndl", "NoTrackBundle.rms")
  expect_false(file.exists(new_rms))
})

test_that("import_bundle generates tracks with DSP defaults when metadata is missing", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  src_wav <- sigs$full_path[1]

  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE)

  expect_no_error(
    import_bundle(ae, "NoMetaSession", "NoMetaBundle", media = src_wav,
                  tracks = TRUE, verbose = FALSE)
  )

  new_rms <- file.path(ae@basePath, "NoMetaSession_ses", "NoMetaBundle_bndl", "NoMetaBundle.rms")
  expect_true(file.exists(new_rms))
})

test_that("import_bundle warns but still returns when a generator's package is unresolvable", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  src_wav <- sigs$full_path[1]

  quantify(ae, name = "ghost", fileExtension = "ghost",
           generator = list(`function` = "no_such_fn", package = "no_such_pkg_xyz"))

  expect_warning(
    result <- import_bundle(ae, "GhostSession", "GhostBundle", media = src_wav,
                            tracks = TRUE, verbose = FALSE),
    "GhostSession/GhostBundle"
  )
  expect_true(S7::S7_inherits(result, reindeer::corpus))
})

test_that("import_session applies session-level metadata to every bundle, with per-bundle override", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  src_wav <- sigs$full_path[1]

  import_session(ae, "Session9",
                 metadata = list(Age = 50, Gender = "F"),
                 bundles = list(
                   list(bundle = "B1", media = src_wav),
                   list(bundle = "B2", media = src_wav, metadata = list(Gender = "M"))
                 ),
                 tracks = FALSE, verbose = FALSE)

  m1 <- get_metadata(ae, "Session9", "B1")
  m2 <- get_metadata(ae, "Session9", "B2")
  expect_equal(as.character(m1$Age), "50")
  expect_equal(as.character(m1$Gender), "F")
  expect_equal(as.character(m2$Age), "50")
  expect_equal(as.character(m2$Gender), "M")
})

test_that("import_session continues importing remaining bundles after one fails", {
  ae <- create_isolated_ae_corpus()
  sigs <- reindeer:::peek_signals(ae)
  src_wav <- sigs$full_path[1]

  expect_warning(
    import_session(ae, "Session10",
                   bundles = list(
                     list(bundle = "Good1", media = src_wav),
                     list(bundle = "Bad", media = "/no/such/file.wav"),
                     list(bundle = "Good2", media = src_wav)
                   ),
                   tracks = FALSE, verbose = FALSE),
    "Bad"
  )

  expect_true(dir.exists(file.path(ae@basePath, "Session10_ses", "Good1_bndl")))
  expect_true(dir.exists(file.path(ae@basePath, "Session10_ses", "Good2_bndl")))
})

test_that("import_media() imports many files, checks them first, applies metadata", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  wavs <- list.files(corp@basePath, "msajc003\\.wav$", recursive = TRUE, full.names = TRUE)
  src <- withr::local_tempdir()
  a <- file.path(src, "newA.wav"); b <- file.path(src, "newB.wav")
  file.copy(wavs[1], a); file.copy(wavs[1], b)

  expect_error(import_media(corp, c(a, file.path(src, "nope.wav")), session = "9999"),
               class = "reindeer_import_error")
  expect_false(dir.exists(file.path(corp@basePath, "9999_ses")))   # nothing written

  res <- import_media(corp, c(a, b), session = "9999",
                      metadata = data.frame(bundle = c("newA", "newB"), Speaker = c("x", "y")))
  expect_equal(res$bundle, c("newA", "newB"))
  m <- as.data.frame(get_metadata(corp))
  expect_equal(m$Speaker[m$bundle == "newB"], "y")
  expect_true(file.exists(file.path(corp@basePath, "9999_ses", "newA_bndl", "newA.wav")))
  expect_error(import_media(corp, a, session = "9999"), class = "reindeer_import_error")
  expect_error(import_media(corp, a, session = "S", annotations = "x.TextGrid"), "not supported")
})

test_that("corpus(create = TRUE) builds a full cache, and a stale cache is replaced", {
  root <- tempfile("create_"); dir.create(root)
  path <- file.path(root, "pilot_emuDB")
  corp <- corpus(path, create = TRUE)
  want <- reindeer:::load_DBconfig(corp)$UUID
  cache <- file.path(path, "pilot_emuDBcache.sqlite")
  cached_uuid <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), cache)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT uuid FROM emu_db")$uuid
  }
  expect_identical(cached_uuid(), want)

  # A cache without emu_db (what 3.1.1 wrote on create) is rebuilt; on Windows
  # this failed with "table session already exists".
  unlink(cache)
  con <- DBI::dbConnect(RSQLite::SQLite(), cache)
  DBI::dbExecute(con, "CREATE TABLE session (db_uuid TEXT, name TEXT)")
  DBI::dbDisconnect(con)
  corpus(path)
  expect_identical(cached_uuid(), want)
})
