# Corpus Class Implementation Summary

## Overview
This document summarizes the revised corpus S7 class implementation with metadata management and media import capabilities.

## Changes Made

### 1. Enhanced Corpus S7 Class (`R/reindeer-corpus.R`)

#### Core Features
- **Automatic cache building**: Corpus construction now automatically builds/updates the SQLite cache file
- **Metadata gathering**: Scans `.meta_json` files at database, session, and bundle levels during construction
- **Flexible construction**: Can be created from either a file path or emuDBhandle object

#### Constructor Pattern
```r
VISP <- corpus("path/to/database_emuDB", verbose = TRUE)
# or
VISP <- corpus(emuDBhandle_object)
```

### 2. Bundle List S7 Class (`R/reindeer-corpus.R`)

A new S7 class that extends tibble to represent query results with metadata:
- Inherits from `tbl_df`
- Requires `session` and `bundle` columns
- Includes inherited metadata from database/session/bundle levels

### 3. Subsetting Operations

#### Reading Bundle Metadata: `corpus[i, j]`
```r
# Specific bundle
VISP["Session1", "Bundle1"]

# All bundles in a session  
VISP["Session1", ]

# Bundle across sessions (if unique)
VISP[, "Bundle1"]
# or
VISP["Bundle1"]

# Regex patterns
VISP["Sess.*", "Bund.*"]
```

Returns a `bundle_list` object (tibble) with session, bundle, and all metadata columns.

#### Writing Metadata: `corpus[i, j] <- value`

**Database-level metadata** (all bundles):
```r
VISP[] <- list(Institution = "University", Project = "Speech")
```

**Session-level metadata**:
```r
VISP["Session1", ] <- list(Age = 30, Gender = "Female")
```

**Bundle-level metadata**:
```r
VISP["Session1", "Bundle1"] <- list(Age = 25, Gender = "Male")
```

**Using regex** (updates all matching):
```r
VISP["Sess.*1", ] <- list(Condition = "Control")
```

### 4. Media Import: `corpus[i, j] <- file_path`

#### Simple Import
```r
# Import to mediafileExtension (usually "wav")
VISP["Session1", "Bundle1"] <- "path/to/audio.mp3"
```

#### Multi-channel Import with Track Specification
```r
# Explicit channel mapping:
# - Channel 1 → mediafileExtension
# - Channel 2 → .egg file
# - Channel 3 → ignored (NULL)
# - Channel 4 → .flow file
VISP["Session1", "Bundle1"] <- c("path/to/multichannel.mp3", "egg", NULL, "flow")

# Or with file path at different position:
VISP["Session1", "Bundle1"] <- c("egg", "path/to/file.mp3", NULL, "flow")
```

**Requirements**:
- No regex patterns allowed for media import (exact names only)
- Bundle must exist before importing media
- Track extensions ("egg", "flow") must be defined in database config
- Requires `wrassp` package for audio file handling

### 5. Enhanced Summary Method

The `summary(corpus)` method now provides comprehensive information matching emuR's output:

```r
summary(VISP)
```

**Displays**:
- Database name, UUID, path
- Session, bundle, item, label, and link counts
- SSFF track definitions
- Level definitions with attributes
- Link definitions (hierarchy)
- Metadata diagnostics (fields, types, coverage)

### 6. Metadata Management System (`R/reindeeR_metadata_optimized.R`)

#### SQLite Schema for Metadata Caching
- `metadata_fields`: Tracks all known metadata fields and types
- `metadata_database`: Database-level defaults
- `metadata_session`: Session-level defaults
- `metadata_bundle`: Bundle-specific metadata

#### Metadata Inheritance
Metadata follows precedence: **Bundle > Session > Database**

#### Ground Truth: `.meta_json` Files
- Database: `<dbname>.meta_json` in database root
- Session: `<sessionname>.meta_json` in session directory
- Bundle: `<bundlename>.meta_json` in bundle directory

All modifications update both the `.meta_json` files (ground truth) and the SQLite cache.

#### Internal Functions
- `gather_metadata_internal()`: Scans all `.meta_json` files during corpus construction
- `process_metadata_list()`: Inserts metadata into appropriate cache tables
- `serialize_metadata_value()` / `deserialize_metadata_value()`: Handle type conversion
- `register_metadata_field()`: Tracks metadata fields and types

### 7. Helper Functions (`R/reindeer-corpus.R`)

- `get_corpus_connection()`: Gets SQLite connection to cache file
- `get_metadata_for_patterns()`: Retrieves metadata for bundles matching regex patterns
- `get_metadata_field_values()`: Gets values for a single field with inheritance
- `corpus_assign_metadata()`: Routes metadata assignment to appropriate level
- `set_metadata_database/session/bundle()`: Writes metadata at each level
- `corpus_import_media()`: Handles media file import with channel mapping
- `import_media_to_bundle()`: Performs actual media import using wrassp

### 8. Configuration Loading (`R/reindeer_corpus_config.R`)

Modified `load_DBconfig()` to handle multiple input types:
- Character (basePath)
- Corpus objects
- emuDBhandle objects

Uses polymorphism instead of S7 dispatch to avoid loading order issues.

## Database Metadata Storage (`.meta_json` Location)

**Changed from DBconfig to separate file**:
- OLD: Database defaults stored in `metadataDefaults` field of `_DBconfig.json`  
- NEW: Database defaults stored in `<dbname>.meta_json` in database root directory

This change improves compatibility with other EMU tools that may not recognize custom DBconfig fields.

## Integration Status

### Completed
✅ Corpus S7 class definition with all properties  
✅ Bundle_list S7 class  
✅ Subsetting operators `[` and `[<-`  
✅ Metadata reading with inheritance  
✅ Metadata writing to .meta_json files  
✅ SQLite metadata caching schema  
✅ Enhanced summary() method  
✅ Media import infrastructure  
✅ Multi-channel audio handling  
✅ Helper functions for all operations  

### Testing Required
⚠️ Full integration testing with real database  
⚠️ Regex pattern matching for sessions/bundles  
⚠️ Media import with various audio formats  
⚠️ Metadata type validation and conversion  
⚠️ Interaction with emuR::query() results  

### Known Issues
🔧 Package loading order: There are function name conflicts during `devtools::load_all()` suggesting some files may be sourced rather than properly loaded. This needs investigation to ensure clean package loading.

## Usage Examples

### Complete Workflow
```r
library(reindeer)

# Create or load corpus
VISP <- corpus("path/to/ae_emuDB", verbose = TRUE)

# View corpus info
print(VISP)
summary(VISP)

# Get bundles with metadata
all_bundles <- VISP[, ]  # All bundles
session1_bundles <- VISP["0000", ]  # All in session "0000"
specific_bundle <- VISP["0000", "msajc003"]  # Specific bundle

# Set database-wide defaults
VISP[] <- list(Institution = "LMU", Project = "AE-Study")

# Set session metadata
VISP["0000", ] <- list(Speaker = "AE01", Age = 32, Gender = "Female")

# Set bundle-specific metadata
VISP["0000", "msajc003"] <- list(Condition = "Read", Quality = "Good")

# Import media
VISP["0000", "msajc003"] <- "recordings/msajc003.wav"

# Import multi-channel with EGG
VISP["0000", "msajc003"] <- c("recordings/msajc003_multi.wav", "egg", NULL)

# Check metadata was applied
bundle_info <- VISP["0000", "msajc003"]
print(bundle_info)
```

### Working with Query Results
```r
# Perform query
result <- query_opt(VISP, "Phonetic == t")

# Add metadata to results (biographize)
result_with_meta <- biographize(result, VISP)

# Now has speaker info, age, gender, etc.
```

## File Structure

```
R/
├── reindeer-corpus.R              # Main corpus class & operators
├── reindeeR_metadata_optimized.R  # Metadata management system
├── reindeer_corpus_config.R       # Config loading (modified)
└── reindeeR_database.R            # Media import reference

Database Structure:
database_emuDB/
├── database.meta_json             # Database-level defaults (NEW)
├── database_DBconfig.json         # Database configuration
├── database_emuDBcache.sqlite     # SQLite cache (with metadata tables)
├── Session1_ses/
│   ├── Session1.meta_json         # Session-level metadata
│   ├── Bundle1_bndl/
│   │   ├── Bundle1.meta_json      # Bundle-level metadata
│   │   ├── Bundle1_annot.json     # Annotations
│   │   ├── Bundle1.wav            # Media file
│   │   └── Bundle1.egg            # Additional track (if present)
│   └── ...
└── ...
```

## Performance Considerations

- **Metadata caching**: Metadata is cached in SQLite for fast access
- **Lazy loading**: Corpus connection is established on-demand
- **Batch operations**: Metadata writes are transaction-protected
- **Regex matching**: Pattern-based operations scan in-memory then query cache

## Future Enhancements

1. **Validation**: Add metadata field validation against schema
2. **Export/Import**: Excel export/import for bulk metadata editing (partially implemented)
3. **Type coercion**: Automatic type conversion with user confirmation
4. **Field discovery**: Suggest similar field names when typos detected
5. **Batch import**: Import media for multiple bundles at once
6. **Track management**: List and validate track definitions before import

## Dependencies

- S7: For class definitions
- DBI/RSQLite: For cache management
- jsonlite: For .meta_json file handling
- wrassp: For audio file import
- cli: For user interface
- stringr, dplyr, tibble: For data manipulation

## Testing Recommendations

1. Create test database with multiple sessions and bundles
2. Test metadata inheritance at each level
3. Test regex patterns for session/bundle matching
4. Test media import with various formats
5. Test interaction with query results
6. Test Excel export/import workflow
7. Verify `.meta_json` files are correctly written
8. Verify SQLite cache stays synchronized

## Documentation

All exported functions have roxygen documentation. Key user-facing documentation:
- `?corpus`: Corpus class construction and usage
- `?bundle_list`: Bundle list class
- `?summary.corpus`: Enhanced summary method
- Export/import functions in `reindeeR_metadata_optimized.R`

---

**Date**: 2025-10-14  
**Author**: Implementation based on user requirements for enhanced metadata and media management in reindeer package
