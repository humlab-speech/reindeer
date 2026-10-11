# Tests opt in to automatic FAIR regeneration explicitly (it is on by default).
options(reindeer.auto_cmdi = FALSE)

# Shared test fixtures for reindeer test suite
# Auto-sourced by testthat before any test file runs.

# ==============================================================================
# ae demo database fixtures
# ==============================================================================

#' Get or create the ae demo database (emuR demo data)
#'
#' Creates the ae database once per test session using a shared tempdir.
#' Returns the path to the ae_emuDB directory.
#' Tests that modify the database should call `create_isolated_ae_db()` instead.
#'
#' @return Character path to ae_emuDB directory
get_shared_ae_path <- function() {
  shared_dir <- file.path(tempdir(), "reindeer_test_shared")
  ae_path <- file.path(shared_dir, "emuR_demoData", "ae_emuDB")

  if (!dir.exists(ae_path)) {
    dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
    suppressMessages(emuR::create_emuRdemoData(dir = shared_dir))
  }

  ae_path
}

#' Create an isolated ae database for tests that modify data
#'
#' Creates a fresh copy in a unique temp directory. Use this when the test
#' writes metadata, adds tracks, or otherwise mutates the database.
#'
#' @param verbose Logical
#' @return Character path to ae_emuDB directory
create_isolated_ae_db <- function(verbose = FALSE) {
  # Copy of a per-session template whose SQLite cache is already built:
  # opening it costs ~0.06 s instead of ~2 s for a cold build.
  tpl <- ae_template_path()
  dest <- tempfile("reindeer_ae_")
  dir.create(dest)
  file.copy(tpl, dest, recursive = TRUE)
  file.path(dest, basename(tpl))
}

ae_template_path <- function() {
  if (is.null(.ae_template$path) || !dir.exists(.ae_template$path)) {
    p <- reindeer:::create_ae_db(verbose = FALSE)
    suppressMessages(reindeer::corpus(p, verbose = FALSE))
    gc()  # release the SQLite connection before the dir is copied
    .ae_template$path <- p
  }
  .ae_template$path
}
.ae_template <- new.env()

#' Load the shared ae database as an emuR handle
#'
#' @return An emuDBhandle
load_shared_ae_handle <- function() {
  ae_path <- get_shared_ae_path()
  emuR::load_emuDB(ae_path, verbose = FALSE)
}

#' Create a shared ae corpus (S7 object)
#'
#' @param quick Ignored (kept so old callers keep working).
#' @return A reindeer corpus object
create_shared_ae_corpus <- function(quick = TRUE) {
  ae_path <- get_shared_ae_path()
  reindeer::corpus(ae_path, verbose = FALSE)
}

#' Create an isolated ae corpus (for tests that mutate data)
#'
#' @return A reindeer corpus object
create_isolated_ae_corpus <- function() {
  ae_path <- create_isolated_ae_db(verbose = FALSE)
  reindeer::corpus(ae_path, verbose = FALSE)
}

# ==============================================================================
# Minimal empty database fixture
# ==============================================================================

#' Create a minimal empty emuDB for lightweight tests
#'
#' @param name Database name
#' @return Character path to the emuDB directory
create_minimal_db <- function(name = "test_db") {
  test_dir <- tempfile(paste0("reindeer_", name, "_"))
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  db_path <- file.path(test_dir, paste0(name, "_emuDB"))

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = name, targetDir = test_dir, verbose = FALSE)
  db_path
}

# ==============================================================================
# Skip helpers
# ==============================================================================

#' Skip test if emuR is not installed
skip_if_no_emuR <- function() {
  skip_if_not_installed("emuR")
}

#' Skip test if superassp is not installed
skip_if_no_superassp <- function() {
  skip_if_not_installed("superassp")
}

#' Can a `future::multisession` worker load reindeer?
#'
#' Workers start fresh R sessions and load packages from `.libPaths()`. During
#' `devtools::test()` the package is only *loaded*, not installed, so worker-based
#' code paths cannot run. Tests that exercise them must check this first.
#'
#' @return Logical
workers_can_load_reindeer <- function() {
  any(file.exists(file.path(.libPaths(), "reindeer", "DESCRIPTION")))
}
