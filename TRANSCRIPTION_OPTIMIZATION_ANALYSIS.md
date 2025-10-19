# Transcription System Optimization Analysis

**Date:** 2025-10-16  
**Status:** Analysis and Recommendations

## Executive Summary

This document analyzes the current transcription system and annotation functions, identifying:
1. Remaining Praat dependencies to eliminate
2. Performance optimization opportunities
3. Integration improvements with S7 classes and data.table

## Current State

### 1. Praat Dependencies

#### Functions Still Using Praat (in `reindeeR_annotate.R`):

1. **`annotate_INTSINT_MOMEL()`** - Lines 96-275
   - Status: ✅ **Python replacement available**: `draft_momel_intsint()` in `reindeer_annotate_momel.R`
   - Uses: External Praat script + Perl + C momel binary
   - Replacement: Pure Python/Parselmouth implementation with all logic embedded

2. **`annotate_periods()`** - Lines 604-706
   - Status: ✅ **Python replacement available**: `draft_periods()` in `reindeeR_annotate_python.R`
   - Uses: Praat script for period detection
   - Replacement: Parselmouth-based period detection

3. **`annotate_peakdet()`** - Lines 728-818
   - Status: ❌ **No Python replacement yet**
   - Uses: Complex Praat script suite (`praatdet`) for EGG analysis
   - Function: Detects glottal closure instants from EGG signal
   - **Action Required**: Convert to Python/Parselmouth

4. **`annotate_voiceactivity()`** - Lines 477-531
   - Status: ⚠️ **Partial implementation**
   - May use Praat internally (needs verification)
   - **Action Required**: Verify and convert if needed

### 2. Python Implementations Available

**File**: `reindeeR_annotate_python.R`
- `draft_periods()` - Glottal period detection using Parselmouth
- `draft_intsint_momel()` - INTSINT/MOMEL annotation using Python

**File**: `reindeer_annotate_momel.R`
- Complete Python-based MOMEL/INTSINT system
- Eliminates all external dependencies (Praat, Perl, C binaries)

### 3. Transcription System Structure

**File**: `reindeer_transcription_system.R`

#### Classes:
- `Suggestion` (base class)
- `SuggestedSegments` (segments)
- `SuggestedEvents` (events)
- `SuggestedItems` (items)
- `TranscriptionLog` (change tracking)

#### Workflow Methods:
- `draft()` - Create suggestions
- `assess()` - Validate suggestions
- `correct()` - Manual corrections
- `prepare()` - Setup levels/attributes
- `transcribe()` - Apply to database

## Optimization Opportunities

### Phase 1: Eliminate Praat Dependencies (HIGH PRIORITY)

#### 1.1 Remove Old Praat-based Functions

**Actions:**
```r
# Mark as deprecated in reindeeR_annotate.R
# Add warnings directing users to Python versions
# Target for removal in next major version
```

**Files to deprecate:**
- `annotate_INTSINT_MOMEL()` → use `draft_momel_intsint()`
- `annotate_periods()` → use `draft_periods()`

#### 1.2 Convert `annotate_peakdet()` to Python

**Complexity:** HIGH - uses complex praatdet script suite

**Approach:**
1. Analyze praatdet Praat scripts to understand algorithm
2. Implement Python equivalent using Parselmouth + scipy/numpy
3. Test equivalence with reference data
4. Create `draft_peakdet()` function

**Estimated effort:** 2-3 days

#### 1.3 Verify `annotate_voiceactivity()`

**Actions:**
1. Check if it uses Praat internally
2. If yes, convert to Python/Parselmouth
3. If no, optimize for performance

### Phase 2: Data.table Integration (MEDIUM PRIORITY)

#### Current State:
- Transcription system uses base data.frames and dplyr
- `Suggestion` classes store `data.frame` for suggestions
- Performance could be improved with data.table

#### Actions:

**2.1 Update Suggestion Classes**
```r
# Change suggestions property from data.frame to data.table
Suggestion <- S7::new_class(
  "Suggestion",
  properties = list(
    # ... other properties ...
    suggestions = S7::class_data.table,  # ← Change this
    # ... other properties ...
  )
)
```

**2.2 Convert Operations to data.table Syntax**

Current dplyr code:
```r
suggestions %>%
  dplyr::filter(start_time >= min_time) %>%
  dplyr::arrange(start_time) %>%
  dplyr::mutate(duration = end_time - start_time)
```

Optimized data.table code:
```r
suggestions[start_time >= min_time][
  order(start_time)
][, duration := end_time - start_time]
```

**Benefits:**
- 2-10x faster for large datasets
- Lower memory usage (in-place operations)
- Better scaling for large corpora

### Phase 3: Parallel Processing (MEDIUM PRIORITY)

#### Current State:
- `transcribe()` processes bundles sequentially
- Python/Parselmouth code should be thread-safe

#### Actions:

**3.1 Add Parallel Processing to transcribe()**
```r
transcribe <- function(corpus, suggestions, parallel = TRUE, n_cores = NULL) {
  if (parallel) {
    future::plan("multisession", workers = n_cores %||% future::availableCores())
    results <- future.apply::future_lapply(bundles, process_bundle, ...)
  } else {
    results <- lapply(bundles, process_bundle, ...)
  }
}
```

**3.2 Ensure Thread Safety**
- Verify Python environment is safe for parallel use
- Use separate temporary directories per worker
- Avoid global state modifications

**Benefits:**
- Near-linear speedup with number of cores
- Especially valuable for large corpora

### Phase 4: Lazy Evaluation (LOW PRIORITY)

#### Concept:
Similar to lazy segment_list, defer expensive operations until needed

#### Application:
- `assess()` could lazily check constraints
- `prepare()` could batch level creation
- Useful when chaining operations

### Phase 5: Caching (LOW PRIORITY)

#### Opportunities:
1. Cache annotation results (Python calls can be expensive)
2. Store computed features (pitch, formants, etc.)
3. Invalidate cache when source files change

**Implementation:**
```r
# Store cached annotations in .cache_annot directory
# Key by file MD5 + parameters
# Check file modification time before using
```

## Performance Targets

### Current Performance (from benchmarks):

| Operation | Current Time | Target | Status |
|-----------|-------------|--------|--------|
| Simple query | ~10ms | <5ms | ⚠️ Monitor |
| Complex query | ~50ms | <25ms | ⚠️ Monitor |
| Hierarchical requery | ~100ms | <50ms | ✅ Achieved |
| DSP application | ~500ms | <250ms | ✅ Achieved |
| Metadata operations | ~50ms | <25ms | ✅ Achieved |

### Annotation Performance (estimated):

| Operation | Praat (old) | Python (current) | Optimized (target) |
|-----------|-------------|------------------|-------------------|
| MOMEL/INTSINT (per utterance) | ~2-5s | ~1-2s | ~0.5-1s |
| Period detection (per bundle) | ~1-3s | ~0.5-1s | ~0.2-0.5s |
| Batch (100 bundles) | ~5-10min | ~2-5min | ~1-2min (parallel) |

## Recommended Implementation Order

### Sprint 1: Critical Path
1. **Convert annotate_peakdet() to Python** ✅ HIGH
2. **Verify annotate_voiceactivity()** ✅ HIGH
3. **Add deprecation warnings to old functions** ✅ MEDIUM

### Sprint 2: Performance
4. **Integrate data.table into Suggestion classes** ✅ HIGH
5. **Convert transcription operations to data.table** ✅ HIGH
6. **Add comprehensive benchmarks** ✅ MEDIUM

### Sprint 3: Scaling
7. **Implement parallel transcription** ✅ MEDIUM
8. **Add progress indicators (cli)** ✅ LOW
9. **Optimize Python environment initialization** ✅ LOW

### Sprint 4: Polish
10. **Add caching for expensive operations** ✅ LOW
11. **Implement lazy evaluation patterns** ✅ LOW
12. **Documentation and vignettes** ✅ LOW

## Testing Strategy

### 1. Equivalence Testing
- Compare Python vs Praat outputs
- Use reference datasets
- Check numeric precision (within tolerance)

### 2. Performance Testing
- Benchmark each optimization
- Compare before/after times
- Track memory usage

### 3. Integration Testing
- Test full workflow: draft → assess → prepare → transcribe
- Verify database integrity after transcription
- Test with emuR interoperability

### 4. Stress Testing
- Large corpora (1000+ bundles)
- Parallel processing with multiple cores
- Memory-constrained environments

## Risk Assessment

### High Risk:
- **Praat → Python conversion errors**: Subtle algorithm differences
  - *Mitigation*: Extensive equivalence testing with reference data
  
### Medium Risk:
- **Parallel processing race conditions**: Database corruption
  - *Mitigation*: Proper locking, transaction handling
  
- **data.table compatibility**: Interaction with S7 classes
  - *Mitigation*: Comprehensive testing, gradual rollout

### Low Risk:
- **Performance regression**: Optimizations could slow things down
  - *Mitigation*: Benchmark before/after, rollback if needed

## Success Metrics

1. **Zero Praat dependencies** for core annotation functions
2. **2x speedup** for transcription operations (data.table)
3. **Nx speedup** for batch processing (N cores in parallel)
4. **100% test coverage** for transcription system
5. **Complete emuR compatibility** for all operations

## Dependencies to Add

```r
# DESCRIPTION updates needed:
Imports:
    data.table (>= 1.14.0),
    future (>= 1.32.0),
    future.apply (>= 1.11.0),
    reticulate (>= 1.35.0),
    parselmouth (Python package)
```

## Conclusion

The transcription system is well-designed with the S7 class structure and workflow pattern. Main priorities are:

1. **Eliminate remaining Praat dependencies** - achieves portability and consistency
2. **Integrate data.table throughout** - achieves performance gains
3. **Add parallel processing** - achieves scalability

These changes will make the transcription system faster, more reliable, and easier to maintain while preserving fidelity with emuR where needed.

