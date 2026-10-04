# ==============================================================================
# Locating, validating, and provisioning the Artic web application
# ==============================================================================
#
# Artic (https://github.com/humlab-speech/artic) is a fork of EMU-webApp and
# remains an independent, standalone application. reindeer only consumes a
# prebuilt `dist/` directory: a pinned copy bundled in the package, one fetched
# by install_artic(), or one the user points at explicitly. reindeer never
# builds the app and never forks it.

# Features a dist must advertise for review()'s playlist to work. annotate()
# needs none of these beyond a basic build.
.artic_required_features <- function() c("bundleList", "timeAnchors")

#' Abort with a reindeer Artic/web-app condition
#' @keywords internal
#' @noRd
.artic_abort <- function(message, ..., call = NULL, .envir = parent.frame()) {
  cli::cli_abort(message, ..., call = call, .envir = .envir,
                 class = c("reindeer_artic_error", "reindeer_error"))
}

#' Root of the downloaded Artic cache
#' @keywords internal
#' @noRd
.artic_cache_root <- function() {
  file.path(tools::R_user_dir("reindeer", which = "cache"), "artic")
}

#' Read the build manifest shipped inside a dist directory
#' @keywords internal
#' @noRd
.artic_read_manifest <- function(dir) {
  p <- file.path(dir, "artic-manifest.json")
  if (!file.exists(p)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(p, simplifyVector = FALSE), error = function(e) NULL)
}

#' Features advertised by a dist directory
#' @keywords internal
#' @noRd
.artic_features <- function(dir) {
  man <- .artic_read_manifest(dir)
  if (is.null(man)) {
    return(character())
  }
  as.character(unlist(man$features %||% list(), use.names = FALSE))
}

#' Is a directory a usable Artic dist?
#' @keywords internal
#' @noRd
.artic_is_dist <- function(dir) {
  !is.null(dir) && length(dir) == 1L && nzchar(dir) &&
    dir.exists(dir) && file.exists(file.path(dir, "index.html"))
}

#' Newest downloaded Artic build in the cache, if any
#' @keywords internal
#' @noRd
.artic_cached_dirs <- function() {
  root <- .artic_cache_root()
  if (!dir.exists(root)) {
    return(character())
  }
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  dirs[vapply(dirs, .artic_is_dist, logical(1))]
}

#' Candidate Artic dist locations, most explicit first
#' @keywords internal
#' @noRd
.artic_candidates <- function() {
  pkg <- system.file(package = "reindeer")
  if (nzchar(pkg) && basename(pkg) == "inst") {
    pkg <- dirname(pkg)  # pkgload resolves the package root to <pkg>/inst
  }
  cands <- c(
    getOption("reindeer.artic.dir", NULL),
    Sys.getenv("ARTIC_DIR", unset = ""),
    system.file("artic/dist", package = "reindeer"),
    .artic_cached_dirs(),
    if (nzchar(pkg)) file.path(dirname(pkg), "artic", "dist") else character(),
    if (nzchar(pkg)) file.path(pkg, "artic", "dist") else character(),
    file.path(getwd(), "artic", "dist")
  )
  unique(cands[nzchar(cands) & !is.na(cands)])
}

#' Locate a usable Artic dist
#'
#' Resolution order: explicit \code{appDir}, \code{options(reindeer.artic.dir)},
#' \code{ARTIC_DIR}, the copy bundled with reindeer
#' (\code{system.file("artic/dist")}), the \code{install_artic()} cache, then
#' sibling \code{artic/dist} checkouts relative to the package or working
#' directory.
#'
#' @param appDir Optional explicit dist directory (wins over everything).
#' @param require Character vector of manifest features the caller needs.
#' @return The resolved dist path.
#' @keywords internal
#' @noRd
find_artic <- function(appDir = NULL, require = character(), quiet = FALSE) {
  # An explicit directory is an override: honor it or fail, never fall through.
  if (!is.null(appDir)) {
    if (!.artic_is_dist(appDir)) {
      .artic_abort("No Artic dist (index.html) at {.path {appDir}}.")
    }
    .artic_check_features(appDir, require, quiet)
    return(appDir)
  }
  for (cand in .artic_candidates()) {
    if (!.artic_is_dist(cand)) {
      next
    }
    .artic_check_features(cand, require, quiet)
    return(cand)
  }
  .artic_abort(c(
    "Artic (the annotation web app) was not found.",
    "i" = "Set {.code options(reindeer.artic.dir = \"/path/to/artic/dist\")} or {.envvar ARTIC_DIR}.",
    "i" = "Or run {.fn install_artic} to fetch the build pinned to this reindeer version."
  ))
}

#' Fail when a dist lacks a required feature
#' @keywords internal
#' @noRd
.artic_check_features <- function(dir, require, quiet) {
  if (length(require) == 0L || quiet) {
    return(invisible(TRUE))
  }
  missing <- setdiff(require, .artic_features(dir))
  if (length(missing) > 0L) {
    .artic_abort(c(
      "The Artic build at {.path {dir}} is too old for this operation.",
      "x" = "Missing feature{?s}: {.val {missing}}.",
      "i" = "Run {.fn install_artic} or point {.code options(reindeer.artic.dir = )} at a current build."
    ))
  }
  invisible(TRUE)
}

#' The Artic build pinned to this reindeer version
#' @keywords internal
#' @noRd
.artic_pin <- function() {
  p <- system.file("artic/pin.json", package = "reindeer")
  if (!nzchar(p) || !file.exists(p)) {
    p <- file.path(getwd(), "inst", "artic", "pin.json")
  }
  if (!file.exists(p)) {
    .artic_abort(c(
      "This reindeer build ships no pinned Artic artifact.",
      "i" = "Use {.code install_artic(url = )} or {.code install_artic(local = )}."
    ))
  }
  jsonlite::read_json(p, simplifyVector = FALSE)
}

#' Describe the Artic build in use
#'
#' Reports where the app was found and what the build manifest says about it.
#'
#' @param appDir Optional explicit dist directory.
#' @return A list with \code{path}, \code{manifest}, and \code{features}.
#' @examplesIf interactive()
#' artic_info()
#' @export
artic_info <- function(appDir = NULL) {
  path <- find_artic(appDir = appDir, quiet = TRUE)
  man <- .artic_read_manifest(path)
  list(
    path = path,
    manifest = man,
    features = .artic_features(path)
  )
}

#' Download or register a local Artic build
#'
#' reindeer serves Artic from a prebuilt \code{dist/} directory. This installs
#' one into the per-user cache used by \code{\link{find_artic}}. It never runs
#' implicitly: \code{annotate()} and \code{review()} only look for a build and
#' point at this function when none is found.
#'
#' @param ref Release tag or version to fetch from the pinned repository.
#'   Ignored when \code{url} or \code{local} is given.
#' @param url Explicit artifact URL (a \code{.tar.gz} of the dist contents).
#' @param local Path to a locally built dist directory to copy into the cache.
#'   Use this for offline or air-gapped installs, or for a custom build.
#' @param sha256 Expected SHA-256 of the downloaded artifact. Defaults to the
#'   value pinned in the shipped \code{pin.json} when \code{url}/\code{ref}
#'   come from the pin.
#' @param dest Destination directory (defaults to a versioned cache slot).
#' @param overwrite Replace an existing destination.
#' @param quiet Suppress download/progress messages.
#' @return Invisibly, the path to the installed dist.
#' @examplesIf interactive()
#' install_artic(local = "../artic/dist")
#' @export
install_artic <- function(ref = NULL, url = NULL, local = NULL, sha256 = NULL,
                          dest = NULL, overwrite = FALSE, quiet = FALSE) {
  if (!is.null(local) && !is.null(url)) {
    .artic_abort("{.arg local} and {.arg url} are mutually exclusive.")
  }

  if (!is.null(local)) {
    if (!.artic_is_dist(local)) {
      .artic_abort("{.path {local}} is not a built Artic dist (no index.html).")
    }
    id <- basename(normalizePath(local, mustWork = FALSE))
    dest <- dest %||% file.path(.artic_cache_root(), id)
    if (dir.exists(dest) && !overwrite) {
      .artic_abort(c(
        "Destination {.path {dest}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ))
    }
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(list.files(local, full.names = TRUE), dest,
                    recursive = TRUE, copy.date = TRUE)
    if (!all(ok)) {
      .artic_abort("Failed to copy Artic build from {.path {local}}.")
    }
    return(invisible(dest))
  }

  pin <- tryCatch(.artic_pin(), error = function(e) NULL)
  if (is.null(url) && !is.null(pin)) {
    url <- pin$url
    if (is.null(sha256)) sha256 <- pin$sha256
  }
  if (is.null(url)) {
    .artic_abort(c(
      "No Artic artifact to install.",
      "i" = "Pass {.arg url} (release artifact) or {.arg local} (a built dist)."
    ))
  }

  id <- ref %||% (pin$version %||% basename(url))
  dest <- dest %||% file.path(.artic_cache_root(), id)
  if (dir.exists(dest) && !overwrite) {
    .artic_abort(c(
      "Destination {.path {dest}} already exists.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."
    ))
  }

  tmp <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp), add = TRUE)
  if (!quiet) cli::cli_alert_info("Downloading Artic from {.url {url}}")
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = quiet)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    .artic_abort(c(
      "Could not download the Artic artifact.",
      "i" = "Check the URL and network access, or use {.code install_artic(local = )}."
    ))
  }

  if (!is.null(sha256)) {
    got <- digest::digest(file = tmp, algo = "sha256", serialize = FALSE)
    if (!identical(tolower(got), tolower(sha256))) {
      .artic_abort(c(
        "Artic artifact checksum mismatch.",
        "x" = "expected {.val {sha256}}, got {.val {got}}.",
        "i" = "Refusing to install a corrupted or unexpected build."
      ))
    }
  }

  stage <- tempfile("artic-stage-")
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  utils::untar(tmp, exdir = stage)
  root <- if (file.exists(file.path(stage, "index.html"))) {
    stage
  } else {
    hits <- list.files(stage, pattern = "^index.html$", recursive = TRUE,
                       full.names = TRUE)
    if (length(hits) == 0L) {
      .artic_abort("The Artic artifact did not contain an index.html.")
    }
    dirname(hits[[1]])
  }

  if (dir.exists(dest)) unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(list.files(root, full.names = TRUE), dest,
                  recursive = TRUE, copy.date = TRUE)
  if (!all(ok)) {
    .artic_abort("Failed to install Artic into {.path {dest}}.")
  }
  if (!quiet) cli::cli_alert_success("Installed Artic {.val {id}} at {.path {dest}}")
  invisible(dest)
}
