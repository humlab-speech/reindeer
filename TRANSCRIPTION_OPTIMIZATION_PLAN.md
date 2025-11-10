# Transcription System Optimization Plan

## Current State
- Base transcription system in `reindeer_transcription_system.R` (1156 lines)
- Partial optimization in `reindeer_transcription_system_optimized.R` (443 lines)
- Python/Parselmouth implementations in `reindeeR_annotate_python.R`
- Legacy Praat calls still in `reindeeR_annotate.R`

## Goals
1. **Remove all Praat system calls** - Replace with Python/Parselmouth
2. **Apply data.table throughout** - Use for all data manipulations
3. **Implement lazy evaluation** - Query optimization patterns
4. **Batch operations** - Reduce database round-trips
5. **Parallel safety** - Ensure thread-safe operations

## Implementation Steps

### Phase 1: Complete data.table Migration ✓ (Partially Done)
- [x] Base Suggestion class uses data.table
- [x] assess() optimized with batch queries
- [ ] transcribe() needs batch operations
- [ ] prepare() needs parallelization
- [ ] All helper functions need data.table

### Phase 2: Remove Praat Dependencies
- [ ] Deprecate Praat-calling functions in reindeeR_annotate.R
- [ ] Ensure all functionality available in Python versions
- [ ] Update documentation to point to new functions
- [ ] Add migration guide

### Phase 3: Performance Enhancements
- [ ] Batch database writes in transcribe()
- [ ] Parallel bundle processing where safe
- [ ] Connection pooling for database operations
- [ ] Cache metadata lookups

### Phase 4: Integration with Query System
- [ ] Suggestion objects work with segment_list
- [ ] Lazy evaluation for chained operations
- [ ] Integration with ask_for() results

## Performance Targets
- assess(): <100ms for 1000 suggestions
- transcribe(): <500ms for 1000 items
- prepare(): <200ms for level creation
- 10x speedup over emuR for batch operations

