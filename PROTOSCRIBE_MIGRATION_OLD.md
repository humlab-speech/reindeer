# Migration Status: Draft Annotation Functions → protoscribe

**Date**: 2025-10-23  
**Status**: Phase 1 Complete

## What Happened

Draft annotation functionality has been extracted to a new **protoscribe** package.
This implements a clean producer-consumer architecture where:

- **protoscribe** (new package): Produces Suggestion objects from audio files
- **reindeer** (this package): Will consume Suggestions via workflow functions

## Files Copied to protoscribe

The following files were copied to protoscribe (originals remain here for now):

### Python Integration
- `inst/python/annotation_wrappers.py`
- `inst/python/annotate_periods.py`
- `inst/python/momel_intsint.py`

These Python files are now maintained in protoscribe. They will remain here 
temporarily for backward compatibility.

### Core Concepts Extracted
From `R/reindeer_transcription_system_optimized.R`:
- Suggestion S7 class definitions (simplified, no corpus object)
- EventSuggestion, SegmentSuggestion, ItemSuggestion classes

From `R/reindeeR_annotate_python.R` and `R/reindeer_annotate_momel.R`:
- Draft generation logic (simplified, no caching)
- `draft_periods()` → protoscribe::draft_periods()
- `draft_momel_intsint()` → protoscribe::draft_momel_intsint()

## What Stays in reindeer

**All existing functionality remains:**
- Corpus management (corpus class)
- Query system (ask_for, query)
- Signal processing (quantify, enrich)
- Metadata system (gather_metadata, biographize)
- Simulation infrastructure

**Current draft functions still work:**
- `draft_periods()` - Still functional as-is
- `draft_momel_intsint()` - Still functional as-is
- No breaking changes yet

## Phase 2: What Will Be Added to reindeer

In a future update, reindeer will gain workflow functions:

```r
# New functions to be added:
assess(corpus, suggestion)      # Validate Suggestion against corpus
prepare(corpus, suggestion)     # Create levels/attributes in corpus
transcribe(corpus, suggestion)  # Apply Suggestion to database
reverse(corpus, log)            # Rollback transcription

# Draft caching will move here:
# - Draft cache system from draft_cache_system.R
# - Integrated with corpus context
```

## Phase 2: Deprecation Plan

When workflow functions are added:

1. Current `draft_*()` functions will be deprecated with warnings
2. They will call protoscribe internally: `protoscribe::draft_*()`
3. `assess()`, `transcribe()` etc. will be reindeer functions
4. Old code will still work, just with deprecation messages

## New Workflow (Future)

```r
library(reindeer)     # Corpus + workflows
library(protoscribe)  # Suggestion generators

# 1. Get corpus and files (reindeer)
corp <- corpus("path/to/db_emuDB")
files <- signal_files(corp)

# 2. Generate suggestions (protoscribe)
suggestions <- protoscribe::draft_periods(
  audio_files = files$full_path,
  session_names = files$session,
  bundle_names = files$bundle
)

# 3. Apply to corpus (reindeer - to be added)
assess(corp, suggestions)
prepare(corp, suggestions)
log <- transcribe(corp, suggestions)
```

## Benefits of This Architecture

1. **Separation of concerns**: Generation vs. database management
2. **Cleaner codebase**: Each package has clear responsibility
3. **Easier testing**: Can test generation without database
4. **Better extensibility**: Easy to add new draft functions
5. **Follows patterns**: Like superassp (producer) → reindeer (consumer)

## No Action Required

This is a documentation commit only. No changes to reindeer functionality yet.
The existing draft annotation functions continue to work as before.

## Timeline

- ✅ Phase 1: protoscribe package created (DONE)
- ⏳ Phase 2: reindeer workflow functions (future PR)
- ⏳ Phase 3: Deprecation and cleanup (future PR)

## References

- protoscribe repository: (location TBD)
- Migration planning docs: See protoscribe package
- Discussion: (link to issue/discussion TBD)

---
**Note**: This is a planning document. Actual migration of assess/transcribe 
functions to reindeer will happen in Phase 2.
