# Demo script for auto-sync system
# Run this to test the auto-sync functionality

library(emuR)
source("R/reindeeR_autosync.R")
source("R/reindeeR_autosync_wrappers.R")
source("R/reindeeR_cmdi.R")

# Load test database
db <- load_emuDB("/Users/frkkan96/Downloads/ae_emuDB")

cat("\n=== DEMO: AUTO-SYNC SYSTEM ===\n\n")

# 1. Enable auto-sync
cat("1. Enabling auto-sync...\n")
enable_auto_sync(
  db,
  enable = TRUE,
  sync_eaf = TRUE,
  sync_cmdi = TRUE,
  align_items = TRUE,
  cmdi_profile = "speech-corpus",
  verbose = TRUE
)

cat("\n2. Adding bundle metadata...\n")
# This should trigger CMDI sync
write_bundle_metadata(
  db,
  session = "0000",
  bundle = "msajc003",
  metadata = list(
    participant = list(
      id = "P001",
      age = 45,
      gender = "Female",
      language = "English"
    ),
    recording = list(
      date = "2023-09-15",
      location = "Laboratory"
    )
  ),
  verbose = TRUE
)

cat("\n3. Checking sync status...\n")
config <- load_sync_config(db)
cat("  Auto-sync enabled:", config$enabled, "\n")
cat("  EAF sync:", config$sync_eaf, "\n")
cat("  CMDI sync:", config$sync_cmdi, "\n")
cat("  CMDI profile:", config$cmdi_profile, "\n")

cat("\n4. Manual sync check...\n")
result <- sync_database(db, verbose = TRUE)

cat("\n5. Verifying generated files...\n")
eaf_file <- file.path(db$basePath, "0000_ses", "msajc003_bndl", "msajc003.eaf")
cmdi_file <- file.path(db$basePath, paste0(db$dbName, "_cmdi.xml"))

cat("  EAF file exists:", file.exists(eaf_file), "\n")
cat("  CMDI file exists:", file.exists(cmdi_file), "\n")

if (file.exists(cmdi_file)) {
  cat("  CMDI file size:", file.info(cmdi_file)$size, "bytes\n")
}

cat("\n=== DEMO COMPLETE ===\n")
