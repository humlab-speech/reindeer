# Automatic Synchronization System for EMU Databases

## Overview

The reindeer package provides an automatic synchronization system that keeps your EMU database files in sync:

1. **EAF Sync**: Automatically updates `.eaf` files when `_annot.json` files change
2. **CMDI Sync**: Automatically updates CMDI metadata files when database structure or metadata changes

This ensures that your ELAN-compatible EAF files and CLARIN-compatible CMDI metadata are always up-to-date without manual intervention.

---

## Quick Start

### Enable Auto-Sync

```r
library(reindeer)
library(emuR)

# Load your database
db <- load_emuDB("/path/to/database_emuDB")

# Enable automatic synchronization
enable_auto_sync(
  db,
  enable = TRUE,
  sync_eaf = TRUE,        # Sync EAF files on annotation changes
  sync_cmdi = TRUE,       # Sync CMDI on database/metadata changes
  align_items = TRUE,     # Align ITEMs with time info in EAF
  cmdi_profile = "speech-corpus",
  verbose = TRUE
)
```

**Output:**
```
✔ Auto-sync enabled for database
ℹ EAF files will sync on _annot.json changes
ℹ CMDI will sync on database changes
```

### What Happens Next?

Once enabled, synchronization happens automatically when:

1. **Annotation files** (`_annot.json`) are modified
   - EAF files are regenerated
   
2. **Metadata files** (`.meta_json`) are created or modified
   - CMDI file is updated with new participant info
   
3. **Database configuration** is changed
   - CMDI file is updated with new structure
   
4. **Sessions or bundles** are added/removed
   - CMDI file reflects new corpus composition

---

## How It Works

### Change Detection

The system uses two mechanisms to detect changes:

1. **MD5 Checksums**: Each file's content is hashed
2. **State Tracking**: Previous checksums stored in `.sync_state.json`

When a file changes, its checksum differs from the stored value, triggering sync.

### State Files

Two hidden files in your database directory track sync state:

- **`.sync_config.json`**: Stores sync preferences (EAF/CMDI enabled, profile, etc.)
- **`.sync_state.json`**: Stores file checksums and last sync times

These files are automatically created when you enable auto-sync.

---

## Configuration

### Full Configuration Options

```r
enable_auto_sync(
  db,
  enable = TRUE,              # Master switch
  sync_eaf = TRUE,            # Auto-sync EAF files
  sync_cmdi = TRUE,           # Auto-sync CMDI metadata
  align_items = TRUE,         # How to handle ITEMs in EAF
  cmdi_profile = "speech-corpus",  # CMDI profile to use
  verbose = TRUE              # Show sync messages
)
```

### CMDI Profile Options

Choose a profile based on your corpus type:

```r
# General media corpus (default)
enable_auto_sync(db, cmdi_profile = "media-corpus")

# Speech corpus with participants
enable_auto_sync(db, cmdi_profile = "speech-corpus")

# Speech corpus (DLU variant)
enable_auto_sync(db, cmdi_profile = "speech-corpus-dlu")
```

### Disable Auto-Sync

```r
enable_auto_sync(db, enable = FALSE)
```

---

## Manual Sync

Sometimes you want to trigger sync manually:

### Sync Everything

```r
# Sync both EAF and CMDI
sync_database(db, verbose = TRUE)
```

### Sync Only EAF Files

```r
sync_database(db, sync_eaf = TRUE, sync_cmdi = FALSE)
```

### Sync Only CMDI

```r
sync_database(db, sync_eaf = FALSE, sync_cmdi = TRUE)
```

### Force Sync (Ignore Change Detection)

```r
# Regenerate all EAF and CMDI files regardless of changes
sync_database(db, force = TRUE)
```

---

## Working with Metadata

### Bundle-Level Metadata

Add or update metadata for a specific bundle:

```r
# Write metadata (auto-syncs CMDI by default)
write_bundle_metadata(
  db,
  session = "0000",
  bundle = "msajc003",
  metadata = list(
    participant = list(
      id = "P001",
      age = 45,
      gender = "Female"
    ),
    recording = list(
      date = "2023-09-15",
      stimulus = "Reading passage A"
    )
  ),
  verbose = TRUE
)
```

**Output:**
```
✔ Metadata written for 0000/msajc003
ℹ CMDI sync triggered by: metadata
✔ CMDI updated: database_cmdi.xml
```

### Session-Level Metadata

Add metadata for an entire session:

```r
write_session_metadata(
  db,
  session = "0000",
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
    ),
    session = list(
      task = "Reading task",
      duration_minutes = 30
    )
  )
)
```

### Merge vs. Replace

By default, new metadata is **merged** with existing metadata:

```r
# First write
write_bundle_metadata(db, "0000", "bundle1", 
  list(participant = list(age = 25)))

# Second write - adds gender, keeps age
write_bundle_metadata(db, "0000", "bundle1", 
  list(participant = list(gender = "Female")))
# Result: {age: 25, gender: "Female"}

# Replace completely
write_bundle_metadata(db, "0000", "bundle1",
  list(participant = list(gender = "Female")),
  merge = FALSE)
# Result: {gender: "Female"} (age is gone)
```

---

## Database Structure Changes

### Adding Sessions

```r
# Add session with auto-sync
add_session_with_sync(db, "0001", verbose = TRUE)
```

**Output:**
```
✔ Session added: 0001
ℹ CMDI sync triggered by: structure
✔ CMDI updated: database_cmdi.xml
```

### Removing Sessions

```r
remove_session_with_sync(db, "0001", verbose = TRUE)
```

### Updating Configuration

```r
# Modify database configuration
config <- db$config
config$levelDefinitions[[1]]$name <- "NewName"

update_config_with_sync(db, config, verbose = TRUE)
```

---

## Batch Operations

When updating many bundles, use batch operations to sync only once:

```r
# Prepare updates
updates <- list(
  list(
    session = "0000",
    bundle = "bundle1",
    metadata = list(participant = list(age = 25))
  ),
  list(
    session = "0000",
    bundle = "bundle2",
    metadata = list(participant = list(age = 30))
  ),
  list(
    session = "0000",
    bundle = "bundle3",
    metadata = list(participant = list(age = 35))
  )
)

# Apply all updates with single sync at the end
batch_update_metadata(db, updates, verbose = TRUE)
```

**Output:**
```
ℹ Updating metadata for 3 bundle(s)...
✔ Batch update complete
ℹ CMDI sync triggered by: metadata
✔ CMDI updated: database_cmdi.xml
```

---

## Integration with Workflows

### Example: Import and Annotate Workflow

```r
library(reindeer)
library(emuR)

# 1. Load database and enable auto-sync
db <- load_emuDB("/path/to/corpus_emuDB")
enable_auto_sync(db, verbose = FALSE)

# 2. Import recordings (creates sessions/bundles)
import_recordings(db, "/path/to/audio", targetSessionName = "session1")
# → CMDI automatically updates with new structure

# 3. Add participant metadata
write_session_metadata(db, "session1", list(
  participant = list(id = "P001", age = 35, gender = "Female")
))
# → CMDI automatically updates with participant info

# 4. Annotate in ELAN (external)
# User opens EAF files and annotates...

# 5. Import annotations back
# (assuming you have a function to import from EAF to _annot.json)
import_eaf_to_annot(db, "session1", "bundle1")
# → EAF automatically syncs

# 6. Work with annotations in R
query_result <- query(db, "Phonetic == 'a:'")
# Annotations stay in sync

# 7. Manual sync check
sync_database(db, verbose = TRUE)
```

### Example: Corpus Curation

```r
# Load corpus
db <- load_emuDB("/path/to/corpus_emuDB")
enable_auto_sync(db, verbose = TRUE)

# Curate metadata from external sources
participants <- read.csv("participants.csv")

# Update all bundles
for (i in 1:nrow(participants)) {
  p <- participants[i, ]
  
  write_bundle_metadata(
    db,
    session = p$session,
    bundle = p$bundle,
    metadata = list(
      participant = list(
        id = p$id,
        age = p$age,
        gender = p$gender,
        language = p$language
      )
    ),
    trigger_sync = FALSE  # Don't sync each time
  )
}

# Single sync at the end
sync_database(db, sync_cmdi = TRUE, verbose = TRUE)
```

---

## Monitoring Sync Activity

### Check Sync Status

```r
# Load sync configuration
config <- load_sync_config(db)

if (!is.null(config) && config$enabled) {
  cat("Auto-sync is ENABLED\n")
  cat("  EAF sync:", config$sync_eaf, "\n")
  cat("  CMDI sync:", config$sync_cmdi, "\n")
  cat("  CMDI profile:", config$cmdi_profile, "\n")
} else {
  cat("Auto-sync is DISABLED\n")
}
```

### View Sync State

```r
# Load sync state
state <- load_sync_state(db)

# Check how many files are tracked
cat("Tracking", length(state$annot_checksums), "annotation files\n")
cat("Tracking", length(state$metadata_checksums), "metadata files\n")
cat("Last full scan:", state$last_full_scan, "\n")
```

### Detect Changes Without Syncing

```r
# Check which bundles have changed
changed <- detect_annot_changes(db)

if (!is.null(changed)) {
  cat("Changed bundles:\n")
  print(changed)
} else {
  cat("No changes detected\n")
}
```

---

## Advanced Usage

### Disable Sync for Specific Operations

```r
# Temporarily disable sync
enable_auto_sync(db, enable = FALSE, verbose = FALSE)

# Do bulk operations without triggering sync
for (bundle in bundles) {
  # ... modify annotations ...
}

# Re-enable and do single sync
enable_auto_sync(db, enable = TRUE, verbose = FALSE)
sync_database(db, force = TRUE, verbose = TRUE)
```

### Custom Sync Triggers

```r
# Create custom function that modifies database
my_custom_operation <- function(db, ...) {
  # Your code here
  # ...
  
  # Trigger sync at the end
  auto_sync_check(db)
}
```

### Conditional Sync

```r
# Only sync if significant changes occurred
if (major_annotation_changes) {
  sync_database(db, sync_eaf = TRUE, sync_cmdi = FALSE)
}

if (metadata_updated) {
  sync_database(db, sync_eaf = FALSE, sync_cmdi = TRUE)
}
```

---

## Troubleshooting

### Problem: Sync Not Triggering

**Check if auto-sync is enabled:**
```r
config <- load_sync_config(db)
print(config$enabled)
```

**Re-enable:**
```r
enable_auto_sync(db, enable = TRUE, verbose = TRUE)
```

### Problem: EAF Files Not Updating

**Check EAF sync is enabled:**
```r
config <- load_sync_config(db)
print(config$sync_eaf)
```

**Force EAF regeneration:**
```r
sync_database(db, sync_eaf = TRUE, sync_cmdi = FALSE, force = TRUE)
```

### Problem: CMDI File Not Updating

**Check CMDI sync is enabled:**
```r
config <- load_sync_config(db)
print(config$sync_cmdi)
```

**Force CMDI regeneration:**
```r
sync_database(db, sync_eaf = FALSE, sync_cmdi = TRUE, force = TRUE)
```

### Problem: Sync Taking Too Long

**Disable verbose output:**
```r
enable_auto_sync(db, verbose = FALSE)
```

**Use batch operations:**
```r
# Instead of:
for (bundle in bundles) {
  write_bundle_metadata(db, session, bundle, meta)  # Syncs each time
}

# Do this:
updates <- lapply(bundles, function(b) {
  list(session = session, bundle = b, metadata = meta)
})
batch_update_metadata(db, updates)  # Syncs once
```

---

## Best Practices

### 1. Enable at Project Start

```r
# First thing when starting work
db <- load_emuDB("/path/to/db")
enable_auto_sync(db)
```

### 2. Use Batch Operations

When updating many bundles, use `batch_update_metadata()` to sync only once.

### 3. Disable for Bulk Imports

```r
enable_auto_sync(db, enable = FALSE)
# ... bulk import ...
enable_auto_sync(db, enable = TRUE)
sync_database(db, force = TRUE)
```

### 4. Check Sync Status Regularly

```r
# At the end of work session
sync_database(db, verbose = TRUE)
```

### 5. Preserve CMDI Metadata

The system preserves user-edited CMDI metadata (title, description, author) when regenerating. To update these, either:

- Edit the CMDI XML file directly
- Use `create_cmdi_metadata()` with explicit parameters
- The next auto-sync will preserve your edits

### 6. Version Control

Add sync state files to `.gitignore`:
```
.sync_config.json
.sync_state.json
```

But consider committing generated files:
```
*.eaf
*_cmdi.xml
```

---

## Performance Considerations

### Change Detection is Fast

- MD5 checksums are computed only for modified files
- State tracking minimizes file system operations
- Typical overhead: <100ms for databases with <1000 bundles

### Sync is Incremental

- Only changed bundles get new EAF files
- CMDI only updates when database/metadata changes
- No unnecessary file writes

### Optimize for Large Corpora

For databases with >10,000 bundles:

```r
# Use batch operations
batch_update_metadata(db, updates)

# Disable verbose output
enable_auto_sync(db, verbose = FALSE)

# Manual sync for large changes
enable_auto_sync(db, enable = FALSE)
# ... make many changes ...
enable_auto_sync(db, enable = TRUE)
sync_database(db, force = TRUE)
```

---

## Technical Details

### File Locations

```
database_emuDB/
├── .sync_config.json      # Sync configuration
├── .sync_state.json       # Change detection state
├── database_cmdi.xml      # Generated CMDI file
├── 0000_ses/
│   ├── .meta_json         # Session metadata
│   ├── bundle1_bndl/
│   │   ├── bundle1_annot.json
│   │   ├── bundle1.eaf    # Generated EAF file
│   │   └── .meta_json     # Bundle metadata
│   └── bundle2_bndl/
│       └── ...
└── ...
```

### Sync Triggers

| Event | EAF Sync | CMDI Sync |
|-------|----------|-----------|
| `_annot.json` modified | ✓ | - |
| `.meta_json` created/modified | - | ✓ |
| `_DBconfig.json` modified | - | ✓ |
| Session added/removed | - | ✓ |
| Bundle added/removed | ✓ (for that bundle) | ✓ |

### Dependencies

The auto-sync system requires:
- `jsonlite`: JSON file handling
- `xml2`: CMDI XML generation
- `tools`: MD5 checksums
- `cli`: User feedback

---

*Auto-Sync Documentation v1.0*  
*Date: November 1, 2025*  
*For: reindeer R package v0.2.2+*
