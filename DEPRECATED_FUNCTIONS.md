# Deprecated Functions and Files Analysis

Generated: 2025-10-16

## Summary

This document identifies functions and files that are no longer used by the modernized S7-based system.

## Files Marked for Deletion

### 1. **R/tidy_classes-s7.R** - OLD S7 IMPLEMENTATION
**Status:** DELETE  
**Reason:** Superseded by new S7 classes in:
- `R/reindeer-corpus.R` (corpus class)
- `R/reindeer_segment_list.R` (segment_list classes)
- `R/reindeer_transcription_system_optimized.R` (suggestion classes)

### 2. **R/tidy_transcriptions.R** - OLD TRANSCRIPTION SYSTEM
**Status:** DELETE  
**Reason:** Replaced by optimized transcription system in `R/reindeer_transcription_system_optimized.R`
**Unused functions:**
- suggest.character
- suggest.emuDBhandle
- suggest.data.frame

### 3. **R/tidy_trackdata.R** - OLD TRACK DATA SYSTEM
**Status:** DELETE  
**Reason:** Replaced by new quantify/enrich system
**Unused functions:**
- fake_two_df_fun
- fake_voice_report
- double_fake_voice_report
- ask_for_legacy
- relate_to
- harvest
- prepare.character
- prepare.data.frame
- prepare.emuDBhandle
- describe_level
- quantify.character
- quantify.data.frame
- quantify.segmentlist
- quantify.emuDBhandle
- provide_perspective
- readtrack

### 4. **R/reindeeR_annotate.R** - OLD PRAAT-BASED ANNOTATION
**Status:** DELETE  
**Reason:** Replaced by Python/Parselmouth implementation in `R/reindeeR_annotate_python.R`
**Unused functions:**
- annotate_voiceactivity

### 5. **R/reindeeR_metadata.R** - NON-OPTIMIZED METADATA
**Status:** DELETE  
**Reason:** Superseded by `R/reindeeR_metadata_optimized.R`

### 6. **R/reindeeR_signalextensions.R** - OLD SIGNAL HANDLING
**Status:** DELETE  
**Reason:** Replaced by data.table-based version `R/reindeeR_signalextensions_dt.R`
**Unused functions:**
- get_ssffObject
- sampleRates
- get_trackdata2

### 7. **R/reindeer_transcription_system.R** - NON-OPTIMIZED TRANSCRIPTION
**Status:** DELETE  
**Reason:** Superseded by `R/reindeer_transcription_system_optimized.R`

## Unused Functions in Active Files

### R/RcppExports.R
**Status:** KEEP FILE, functions may be called from C++
- get_linkLevelChildrenNames_cpp
- expand_linkPath_cpp
- find_segmentLevels_cpp
- get_levelDefinition_cpp
- get_hierPathsConnectingLevels_cpp

### R/reindeer_annotate_momel.R
**Status:** REVIEW - May be used for MOMEL/INTSINT
- annotate_momel_intsint (may be legacy, check against Python version)

### R/reindeeR_annotate_python.R
**Status:** KEEP - Testing functions
- draft_intsint_momel (appears to be the active implementation)
- has_python_annotations (utility function)
- test_python_annotations (testing function)

### R/reindeer_corpus_config.R
**Status:** REVIEW - Configuration utilities
These functions may be internal utilities:
- get_allAttributeNames
- get_hierPathsConnectingLevels
- find_segmentLevels
- batch_add_levelDefinitions
- batch_db_operations
- process_bundles
- clear_all_caches
- monitor_memory
- create_attribute_index
- create_link_graph
- validate_dbconfig

### R/reindeeR_database.R
**Status:** REVIEW - Export utilities
- snapshot (may be useful for backups)
- convert_emu_to_eaf (export functionality)

### R/reindeer_lazy_segment_list.R
**Status:** IMPLEMENTED BUT NOT YET USED
These are part of the lazy evaluation system:
- collect.default
- collect.lazy_segment_list
- print.lazy_segment_list
- summary.lazy_segment_list
- as.data.frame.lazy_segment_list
- needs_collect

**Note:** Lazy evaluation is implemented but ask_for() doesn't use it yet. This is a future optimization.

### R/reindeeR_metadata_optimized.R
**Status:** KEEP - Active functions
- get_metadata_field (may be internal)
- summary.corpus (should be used!)

### R/reindeer_segment_list.R
**Status:** KEEP - Constructor utilities
- as_segment_list (type conversion)
- is_segment_list (type checking)
- is_extended_segment_list (type checking)

### R/reindeeR_signalextensions_dt.R
**Status:** REVIEW
- dspp_metadataParameters_dt (appears to be the data.table version)

### R/reindeer_simulation.R
**Status:** KEEP - Simulation system
These are part of the simulation infrastructure:
- compute_signal_hash (internal utility)
- quantify_simulate (internal to quantify with .simulate)
- list_simulations (utility function)
- print.simulation_results (S3 method)
- summary.simulation_results (S3 method)
- enrich_simulate (internal to enrich with .simulate)
- print.simulation_tracks (S3 method)

### R/reindeeR_trackUtilities.R
**Status:** REVIEW
- bin_time (may be utility function)

### R/reindeer_transcription_system_optimized.R
**Status:** KEEP - Parallel processing
- transcribe_parallel (may be used internally)

### R/reindeer-corpus.R
**Status:** REVIEW
- get_handle (conversion to emuR handle?)

### R/tidy_trackdata_helpers.R
**Status:** REVIEW
- clear_tidy_cache (cache management)

## Action Items

### Immediate Deletion (7 files)
1. Delete `R/tidy_classes-s7.R`
2. Delete `R/tidy_transcriptions.R`
3. Delete `R/tidy_trackdata.R`
4. Delete `R/reindeeR_annotate.R`
5. Delete `R/reindeeR_metadata.R`
6. Delete `R/reindeeR_signalextensions.R`
7. Delete `R/reindeer_transcription_system.R`

### Review Required
- Functions in `reindeer_corpus_config.R` - determine if they're internal utilities
- Functions in `reindeer_lazy_segment_list.R` - connect to ask_for() for lazy evaluation
- `summary.corpus` in `reindeeR_metadata_optimized.R` - ensure it's properly exported and used
- Simulation print/summary methods - ensure they're properly registered
- `get_handle()` - determine if emuR compatibility is needed

### Testing Required
- Verify all core workflows work without deleted files
- Ensure no indirect dependencies on deleted functions
- Check that all S3 methods are properly registered
