# reindeer 0.2.5 (Development)

## Major New Features 🎉

### Corpus Creation from Scratch

* `corpus()` now supports creating new EMU databases with `create = TRUE` parameter
* Automatically appends `_emuDB` suffix if not present
* Provides helpful error messages when database doesn't exist
* Uses `emuR::create_emuDB()` internally for full compatibility

### Dynamic Session and Bundle Creation

* New function `create_session_and_bundle()` for programmatic corpus building
* Automatically creates proper directory structure (`<session>_ses/<bundle>_bndl/`)
* Generates minimal `_annot.json` files
* Updates SQLite cache automatically
* Auto-creation when importing media to non-existent sessions/bundles

### Name Validation

* New `validate_name()` function ensures valid session/bundle names
* Blocks regex special characters for literal names
* Prevents path separators and problematic characters
* Provides clear validation error messages

## Bug Fixes 🐛

### Bracket Assignment Operator

* **Fixed critical issue**: `corpus[i, j] <- value` now works correctly
* Resolved S7/S3 method dispatch conflict by adjusting class order
* Added "corpus" as first class in hierarchy for proper S3 dispatch
* Class order now: `corpus < reindeer::corpus < S7_object`
* Bracket notation now works for both metadata assignment and media import

### Media Import

* `corpus_import_media()` now validates names properly
* Auto-creates missing sessions/bundles before import
* Better error handling and progress messages

## Improvements

### Error Messages

* Significantly improved error messages throughout
* Helpful suggestions when database path doesn't exist
* Clear guidance for name validation failures
* Better context in all error conditions

### Documentation

* Added comprehensive roxygen documentation for new functions
* Created `CORPUS_CREATION_IMPLEMENTATION.md` technical guide
* Updated function examples with corpus creation workflows
* Improved parameter descriptions

## Internal Changes

* New file `R/corpus_creation.R` with creation utilities
* Modified `R/corpus_class.R` constructor to support `create` parameter
* Fixed `R/corpus_methods.R` subsetting operator registration
* Updated `R/zzz.R` to register S7 `[` method in `.onLoad()`
* Enhanced `R/corpus_metadata_io.R` with validation and auto-creation

## Breaking Changes

None - fully backward compatible. Existing code continues to work unchanged.

## Usage Example

```r
library(reindeer)

# Create new corpus from scratch
VISP <- corpus("VISP", create = TRUE)

# Add metadata
add_metadata(VISP, list(
  Project = "VISP",
  Language = "Swedish"
))

# Create session and bundle
create_session_and_bundle(VISP, "Svenska", "Annie")

# Import media (auto-creates if needed)
# VISP["Svenska", "Erik"] <- "path/to/audio.wav"
```

---

## Code Quality Improvements 🧹

### Repository Cleanup
* **Major cleanup**: Removed 5,942 lines of deprecated code (-28.5% of codebase)
  - Deleted 7 deprecated files marked with `_DELETE_` prefix
  - All deprecated code was internal, non-exported (no breaking changes)
  - Reduces maintenance burden and improves code clarity

### Naming Standardization
* **Consistent snake_case naming** across all files (100% compliance)
  - Renamed 10 files: `reindeeR_*` → `reindeer_*`
  - Fixed typo: `emuR_develoment_utils.R` → `emur_development_utils.R`
  - Expanded abbreviation: `signalextensions` → `signal_extensions`
  - Git history preserved (all tracked as renames, not deletions)

### Impact Metrics
* R files: 72 → 65 (-9.7%)
* Lines of code: 20,819 → 14,877 (-28.5%)
* File naming consistency: ~60% → 100%
* All changes backward compatible

### Large File Refactoring (Phase 3)
* **File size reduction**: Split 4 large files (>1,000 lines) into 11 focused modules
  - `reindeer-corpus.R` (1,910 lines) → 4 modules (corpus_class, corpus_methods, corpus_metadata_io, corpus_database)
  - `reindeer_simulation.R` (1,778 lines) → 3 modules (simulation_infrastructure, simulation_core, simulation_cache)
  - `reindeer_query_optimized.R` (1,296 lines) → 2 modules (query_parser, query_executor)
  - `reindeer_metadata_optimized.R` (1,247 lines) → 2 modules (metadata_core, metadata_import_export)
* **Total refactored**: 6,231 lines reorganized into maintainable modules
* **Largest file**: Now 1,062 lines (was 1,910 lines)
* **Benefits**: Improved code organization, easier navigation, clearer separation of concerns
* **No breaking changes**: All functionality preserved, fully backward compatible

### Documentation
* `CLEANUP_PHASE1_SUMMARY.md` - Detailed cleanup impact
* `CLEANUP_PHASE2_PLAN.md` - Future naming improvements
* `CODE_QUALITY_IMPROVEMENT_SUMMARY.md` - Complete metrics
* `PHASE3_REFACTORING_PLAN.md` - Large file refactoring plan
* `PHASE3_PROGRESS.md` - Refactoring progress tracking

---
# reindeer 0.2.4

## Major Performance Improvements ⚡

* **RcppSimdJson Integration** - 2-10x faster JSON parsing
  - All JSON reading operations now use RcppSimdJson for massive speedups
  - Hybrid strategy: RcppSimdJson for reading, jsonlite for writing
  - Automatic fallback to jsonlite ensures 100% compatibility
  - Database loading: 2.5x faster
  - Metadata gathering: 3-4x faster
  - Draft cache operations: 3.3x faster
  - Large files: up to 10x faster

* **Complete qs Migration** - 3-4x faster cache serialization
  - All cache operations now use qs format by default
  - Smaller cache files (~50% reduction)
  - Better handling of complex R objects
  - Simulation caches optimized
  - Draft annotation caches optimized

## Bug Fixes 🐛

* **Fixed Critical Test Failures** - All 47 tests now passing
  - Fixed infinite recursion in `build_emuDB_cache()` for empty databases
  - Fixed SQL parameter binding errors in draft cache system
  - Fixed qs deserialization issues (blob indexing)
  - Fixed type mismatch in error_occurred (INTEGER vs LOGICAL)
  - Fixed NOT NULL constraint in draft_annotations table

## New Features

* **Cache Management System**
  - `manage_cache()`: User-friendly cache management interface
  - Actions: status, list, clean (with dry-run support)
  - Works across all cache types (quantify, draft, simulation)
  - Automatic size monitoring with warnings
  - `check_all_cache_sizes()`: Monitor cache sizes
  - `clean_all_caches()`: Clean old cache files

* **JSON Utilities** (internal)
  - `read_json_fast()`: Optimized JSON file reading
  - `parse_json_fast()`: Optimized JSON string parsing
  - `write_json_compat()`: Compatible JSON writing
  - `to_json_compat()`: Compatible JSON serialization
  - `get_json_strategy()`: Inspect parser strategy

## Input Validation

* **Comprehensive assertthat Integration**
  - 30+ validation checks across core functions
  - `corpus()` constructor: 4 validation checks
  - `quantify()`: 5 validation blocks
  - `store_draft_annotations()`: 7 validations
  - `initialize_draft_cache()`: 2 validations
  - `manage_cache()`: Full input validation
  - Clear, informative error messages

## Documentation 📚

* **New Vignettes**
  - `getting_started.Rmd`: Comprehensive beginner's guide (634 lines)
  - `cache_management.Rmd`: Cache optimization guide (287 lines)

* **Enhanced pkgdown Site**
  - New "Cache Management" reference section
  - Reorganized article navigation
  - Better function discoverability
  - Ready for deployment

## Testing 🧪

* **Expanded Test Coverage** - 15% increase
  - New test file: `test_manage_cache.R` (12 scenarios, 30+ assertions)
  - Edge case coverage (empty/single segment lists)
  - Input validation tests
  - All assertthat checks verified
  - Total: 230+ tests, 0 failures

## Benchmarking

* **New Benchmark Suite**
  - `benchmarking/benchmark_json.R`: JSON parsing performance
  - Comprehensive file size testing
  - Real-world scenario simulation
  - Visualization of speedup
  - Documents 2-10x improvements

## Code Quality

* **Major Cleanup**
  - Removed 1,843 lines of deprecated code
  - Net reduction: 521 lines (after adding features)
  - Improved documentation formatting
  - Consistent error handling patterns

## Performance Summary

| Operation | Before | After | Speedup |
|-----------|--------|-------|---------|
| Corpus loading | 5ms | 2ms | 2.5x |
| Metadata (100 files) | 150ms | 40ms | 3.75x |
| Cache serialization | Base R | qs | 3-4x |
| JSON parsing (large) | 50ms | 5ms | 10x |

**Cumulative Impact:**
- Loading corpus with 100 bundles: ~3.6x faster overall
- Processing 1000 annotations: ~700ms saved
- Large corpora (10k bundles): ~11 seconds saved

## Breaking Changes

None - 100% backward compatible

---

# reindeer 0.2.3

## New Features

* **Automatic Synchronization System**
  - `enable_auto_sync()`: Configure automatic sync for database
  - `sync_database()`: Manually trigger synchronization
  - Automatic EAF file updates when `_annot.json` files change
  - Automatic CMDI updates when database structure or metadata changes
  - Change detection using MD5 checksums and state tracking
  - Sync state persisted in `.sync_config.json` and `.sync_state.json`

* **Metadata Management Functions**
  - `write_bundle_metadata()`: Write bundle-level `.meta_json` with auto-sync
  - `write_session_metadata()`: Write session-level `.meta_json` with auto-sync
  - `batch_update_metadata()`: Efficiently update multiple bundles at once
  - Merge mode for incremental metadata updates

* **Database Modification Wrappers**
  - `add_session_with_sync()`: Add session and trigger CMDI update
  - `remove_session_with_sync()`: Remove session and update CMDI
  - `update_config_with_sync()`: Update configuration and sync CMDI
  - All wrappers respect auto-sync configuration

* **Change Detection**
  - `detect_annot_changes()`: Find modified annotation files
  - `detect_metadata_changes()`: Find modified metadata files
  - `detect_config_changes()`: Detect database configuration changes
  - Efficient checksumming to avoid unnecessary syncs

## Documentation

* Added comprehensive auto-sync guide (`inst/doc/AUTO_SYNC_SYSTEM.md`)
  - Quick start and configuration
  - Metadata management workflows
  - Batch operations
  - Monitoring and troubleshooting
  - Performance optimization
  - Best practices
  - Complete usage examples

## Architecture

* Modular auto-sync system:
  - `reindeeR_autosync.R`: Core sync engine and change detection
  - `reindeeR_autosync_wrappers.R`: Database modification wrappers
  - State management with JSON persistence
  - Configurable sync triggers (EAF/CMDI independent)
  - Preserves user-edited CMDI metadata on regeneration

## Workflow Integration

* **Corpus Curation**: Auto-update CMDI when adding participant metadata
* **Annotation Pipeline**: Auto-generate EAF files for ELAN as annotations change
* **Database Evolution**: CMDI stays current as corpus structure evolves
* **Batch Processing**: Efficient bulk operations with single sync

## Performance

* Incremental sync: Only changed files are processed
* MD5-based change detection: Fast checksumming
* Batch-friendly: Defer sync until after multiple operations
* Typical overhead: <100ms for most operations

# reindeer 0.2.2

## New Features

* Added `create_cmdi_metadata()` function for generating CLARIN-compliant CMDI XML files
  - Supports multiple CMDI profiles (media-corpus, speech-corpus, speech-corpus-dlu)
  - Automatically collects metadata from database structure
  - Reads participant information from .meta_json files at session/bundle level
  - Generates comprehensive metadata including participants, resources, annotations
  - Includes placeholders for planned metadata additions

* Added CMDI validation script (`inst/scripts/validate-cmdi.sh`)
  - Validates XML well-formedness
  - Checks CMDI namespace compliance
  - Verifies required elements
  - Detects placeholder fields
  - Assesses metadata completeness
  - Validates resource references

## Documentation

* Added comprehensive CMDI generation guide (`inst/doc/CMDI_METADATA_GENERATION.md`)
  - Complete function usage examples
  - .meta_json file format specifications
  - CMDI profile descriptions
  - Placeholder field specifications (PROJECT.json, PUBLICATIONS.json, ETHICS.json, QUALITY.json)
  - Integration guide for CLARIN repositories
  - Best practices and troubleshooting

* Added metadata templates (`inst/templates/`)
  - session_meta_template.json
  - bundle_meta_template.json
  - PROJECT.json

## Metadata Infrastructure

* Session-level metadata support via `.meta_json` files
  - Participant demographics (age, gender, language, dialect)
  - Recording details (date, location, equipment, sample rate)
  - Session information (task, duration, notes)

* Bundle-level metadata support
  - Stimulus information
  - Repetition tracking
  - Annotation quality metrics

* Planned additions (with placeholders):
  - Project funding information
  - Related publications
  - Ethical approval details
  - Quality control procedures

## CLARIN Integration

* Full CMDI 1.2 specification compliance
* Three supported profiles:
  - media-corpus (clarin.eu:cr1:p_1387365569699)
  - SpeechCorpusWithParticipants (clarin.eu:cr1:p_1392642184799)
  - SpeechCorpus-DLU (clarin.eu:cr1:p_1381926654456)

* Generated CMDI files ready for upload to:
  - The Language Archive (TLA)
  - LINDAT/CLARIAH-CZ
  - Other CLARIN repositories

# reindeer 0.2.1

## Documentation

* Added comprehensive EAF validation documentation for `convert_emu_to_eaf()` function
  - **EMUR_TO_EAF_VALIDATION_GUIDE.md**: Complete EAF 3.0 specification and validation rules
  - **EMUR_TO_EAF_CHECKLIST.md**: Quick reference validation checklist
  - **CONVERT_FUNCTION_FIXES_REQUIRED.md**: Implementation requirements and testing guide

* These documents ensure the `convert_emu_to_eaf()` function produces valid EAF 3.0 files for all emuR annotation files, supporting both `align_items=TRUE` and `align_items=FALSE` modes

## Validation

* Documentation includes:
  - Complete EAF 3.0 specification requirements
  - Mode-specific conversion rules (ALIGNABLE_ANNOTATION vs REF_ANNOTATION)
  - Test cases with expected outputs
  - Common errors and troubleshooting guide
  - Integration with EAF validator tools
  - Priority-ordered implementation requirements

## Key Features Documented

* **align_items=TRUE**: ITEMs receive start/end times from dominated SEGMENTS/POINTs → `ALIGNABLE_ANNOTATION`
* **align_items=FALSE**: ITEMs become symbolic references → `REF_ANNOTATION`
* Proper TIME_ORDER generation with unique TIME_SLOT_IDs
* LINGUISTIC_TYPE definitions with correct TIME_ALIGNABLE settings
* TIER hierarchy with proper PARENT_REF and CONSTRAINTS attributes
* Time slot sharing for Time_Subdivision constraints
* PREVIOUS_ANNOTATION chains for Symbolic_Subdivision

# reindeer 0.2.0

Previous release (see prior commit history)
