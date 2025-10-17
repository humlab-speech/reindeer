# Deprecated Files

**These files are marked for deletion**

All files in this directory have been superseded by modernized implementations
and are no longer used by the package.

## Files and Replacements

- `_DELETE_tidy_classes-s7.R` → Replaced by `reindeer-corpus.R`, `reindeer_segment_list.R`, `reindeer_transcription_system_optimized.R`
- `_DELETE_tidy_transcriptions.R` → Replaced by `reindeer_transcription_system_optimized.R`
- `_DELETE_tidy_trackdata.R` → Replaced by `reindeer_enrich.R` and `tidy_trackdata_helpers.R`
- `_DELETE_reindeeR_annotate.R` → Replaced by `reindeeR_annotate_python.R`
- `_DELETE_reindeeR_metadata.R` → Replaced by `reindeeR_metadata_optimized.R`
- `_DELETE_reindeeR_signalextensions.R` → Replaced by `reindeeR_signalextensions_dt.R`
- `_DELETE_reindeer_transcription_system.R` → Replaced by `reindeer_transcription_system_optimized.R`

## Action Required

After confirming all tests pass, delete this entire directory:
```bash
rm -rf R/deprecated
```

Marked for deletion: 2025-10-16
