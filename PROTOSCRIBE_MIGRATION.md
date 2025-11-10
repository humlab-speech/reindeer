# Draft Annotation Migration Notice

**Date**: 2025-10-23  
**Status**: ✅ Migration Complete in protoscribe

## What Happened

Draft annotation functionality (suggestion system, workflow, and caching) has been **migrated to the protoscribe package**.

## Migrated Functionality

The following functionality has moved from reindeer to protoscribe:

### 1. Suggestion Classes
- `Suggestion`, `EventSuggestion`, `SegmentSuggestion`, `ItemSuggestion`
- Now in: `protoscribe::Suggestion` etc.

### 2. Workflow Functions
- `assess()` - Validate suggestions
- `prepare()` - Create database structures
- `transcribe()` - Apply to database
- `reverse()` - Rollback operations
- `TranscriptionLog` - Operation tracking
- Now in: `protoscribe::assess()`, `protoscribe::transcribe()`, etc.

### 3. Draft Generation Functions
- `draft_periods()` - Glottal period detection
- `draft_momel_intsint()` - Intonation annotation
- Now in: `protoscribe::draft_periods()`, `protoscribe::draft_momel_intsint()`

### 4. Caching System
- Complete SQLite-based draft caching
- Cache management utilities
- Now in: `protoscribe::draft_cache_*()` functions

### 5. Python Integration
- Parselmouth-based annotation tools
- Now in: `protoscribe` inst/python/

## Files That Can Be Removed from reindeer

Once protoscribe is installed and working, these files can be removed:

1. **R/reindeer_transcription_system_optimized.R** (~488 lines)
2. **R/draft_cache_system.R** (~515 lines)
3. **R/cache_size_management.R** (~500 lines)
4. **R/reindeeR_annotate_python.R** (if still exists)
5. **R/reindeer_annotate_momel.R** (if still exists)

## Updated Usage

### New (reindeer + protoscribe)
```r
library(reindeer)     # For corpus management
library(protoscribe)  # For draft annotations

corp <- corpus("path/to/db")                    # reindeer
suggestions <- draft_periods(corp, bundles)     # protoscribe
assess(suggestions)                             # protoscribe
transcribe(suggestions)                         # protoscribe
```

## Next Steps for reindeer

1. Add protoscribe to DESCRIPTION Imports
2. Remove migrated files (after testing)
3. Update documentation to reference protoscribe
4. Optional: Add compatibility wrappers

See full details in this file.

---
**Migration Date**: 2025-10-23  
**protoscribe Version**: 0.0.0.9000
