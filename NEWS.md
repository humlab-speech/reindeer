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
