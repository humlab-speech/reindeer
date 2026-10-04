#!/usr/bin/env Rscript

# Sync a trimmed, pinned Artic build into inst/artic/dist.
#
# Artic stays the source of truth: build it there (npm run build), then run
# this script to vendor the dist into reindeer so annotate()/review() work in
# every deployment (offline, R-universe, conda, Docker, HPC). demoDBs/ is
# dropped because Artic disables the demo button once it connects over the
# WebSocket protocol.
#
# Usage:
#   Rscript tools/sync-artic-dist.R [path/to/artic]   # default: ../artic

args <- commandArgs(trailingOnly = TRUE)
artic <- if (length(args) >= 1L) args[[1]] else "../artic"
artic <- normalizePath(artic, mustWork = TRUE)
dist <- file.path(artic, "dist")
if (!file.exists(file.path(dist, "index.html"))) {
  stop("No built Artic dist at ", dist, " - run `npm run build` in ", artic, " first.")
}

pkg <- normalizePath(".")
target <- file.path(pkg, "inst", "artic", "dist")
unlink(target, recursive = TRUE)
dir.create(target, recursive = TRUE, showWarnings = FALSE)

# Copy everything except demo databases and OS junk.
entries <- list.files(dist, full.names = TRUE)
entries <- entries[!basename(entries) %in% c("demoDBs", ".DS_Store")]
ok <- file.copy(entries, target, recursive = TRUE)
if (!all(ok)) stop("Failed to copy some Artic dist entries.")
invisible(ok)

pkgjson <- jsonlite::fromJSON(file.path(artic, "package.json"))
# Drop OS junk that file.copy carries along recursively.
unlink(list.files(target, pattern = "^\\.DS_Store$", recursive = TRUE,
                  full.names = TRUE, all.files = TRUE))
sha <- tryCatch(
  system2("git", c("-C", artic, "rev-parse", "--short", "HEAD"), stdout = TRUE),
  error = function(e) NA_character_
)
features <- c("bundleList", "timeAnchors", "generatorTracks", "fromIndexTracks")
manifest <- list(
  name = "artic",
  version = pkgjson$version,
  gitSha = sha,
  builtAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  protocol = list(name = "EMU-webApp-websocket-protocol", version = "0.0.2"),
  features = features
)
jsonlite::write_json(manifest, file.path(target, "artic-manifest.json"),
                     auto_unbox = TRUE, pretty = TRUE)

# License + provenance notice (Artic is MIT; reindeer is GPL-2+).
dir.create(file.path(pkg, "inst", "artic"), recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(artic, "LICENSE"))) {
  invisible(file.copy(file.path(artic, "LICENSE"), file.path(pkg, "inst", "artic", "LICENSE"),
                      overwrite = TRUE))
}
writeLines(c(
  "This directory contains a prebuilt Artic distribution.",
  "Artic is MIT licensed: https://github.com/humlab-speech/artic",
  sprintf("Source: https://github.com/humlab-speech/artic @ %s", sha),
  sprintf("Built: %s", manifest$builtAt),
  "demoDBs/ is intentionally excluded; the reviewer sessions disable it."
), file.path(pkg, "inst", "artic", "NOTICE"))

jsonlite::write_json(
  list(version = pkgjson$version, gitSha = sha, features = features,
       manifest = "dist/artic-manifest.json"),
  file.path(pkg, "inst", "artic", "pin.json"),
  auto_unbox = TRUE, pretty = TRUE
)

cat("Synced Artic", pkgjson$version, "->", target, "\n")
