# Performance Analysis: tidy_trackdata.R

## Executive Summary

Analysis of `tidy_trackdata.R` reveals several significant performance optimization opportunities, particularly in database operations, metadata handling, and parallel processing.

## Critical Performance Issues

### 1. Repeated Database Loading (HIGH IMPACT)
**Location**: Lines 1030-1046 in `quantify.segmentlist()`

**Problem**: The database handle is loaded from `basePath` attribute multiple times per function call:
```r
utils::capture.output(
  .inside_of <- emuR::load_emuDB(.inside_of,verbose = FALSE)
) -> dbload.info
```

**Impact**: Unnecessary I/O and connection overhead for every quantify() call

**Solution**: 
- Cache database handle in corpus object
- Reuse existing connection instead of reloading
- Add `@connection` property to corpus S7 class

**Estimated Speedup**: 10-20% for functions with many database operations

---

### 2. Metadata Retrieval Not Cached (HIGH IMPACT)
**Location**: Line 1268

**Problem**: `get_metadata()` queries database every time quantify() is called:
```r
meta <- reindeer:::get_metadata(.inside_of,manditory=names(.metadata_defaults))
```

**Impact**: Redundant database queries when processing multiple segment lists

**Solution**:
- Add metadata cache to corpus object
- Use `memoise` package to cache get_metadata() results
- Invalidate cache only when `.meta_json` files change
- Store in `@metadata_cache` property

**Estimated Speedup**: 15-30% for metadata-dependent operations

---

### 3. Signal Files List Reconstructed Repeatedly (MEDIUM IMPACT)
**Location**: Line 1238

**Problem**: `list_files()` called every time instead of using optimized `peek_signals()`:
```r
signalFiles <- emuR::list_files(.inside_of,inputSignalsExtension)
```

**Impact**: Slow emuR function called when faster alternative exists

**Solution**:
- Replace with `peek_signals()` which uses direct filesystem scan
- Cache result in corpus object as `@signal_files_cache`
- Update cache only when files added/removed

**Estimated Speedup**: 20-40% for this operation

---

### 4. Row-wise Processing (HIGH IMPACT)
**Location**: Line 1401-1403

**Problem**: Using `rowwise()` which is notoriously slow:
```r
appliedDFResultInList <- segmentDSPDF |>
  dplyr::rowwise() |>
  purrr::pmap(.f=processAndStore,.progress = pb)
```

**Impact**: Sequential processing when operations are independent

**Solution**:
- Remove `rowwise()` - it's unnecessary here
- Use `furrr::future_pmap()` for parallel processing
- Add `.parallel` parameter (default TRUE)
```r
if (.parallel) {
  appliedDFResultInList <- furrr::future_pmap(
    segmentDSPDF,
    processAndStore,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = pb
  )
} else {
  appliedDFResultInList <- purrr::pmap(segmentDSPDF, processAndStore, .progress = pb)
}
```

**Estimated Speedup**: 2-4x on multi-core systems

---

### 5. Multiple Sequential Joins (MEDIUM IMPACT)
**Location**: Lines 1269-1299

**Problem**: Multiple `left_join()` operations performed sequentially:
```r
meta |> dplyr::left_join(dsp,by = .dsp_settings_by)
completedStoredDSPSettings <- meta |> dplyr::left_join(dsp,by = .dsp_settings_by)
sessionBundleDSPSettingsDF <- completedStoredDSPSettings |> dplyr::left_join(signalFiles,by=c("session","bundle"))
```

**Impact**: Multiple passes through data

**Solution**:
- Use `data.table` for faster joins
- Combine joins where possible
- Pre-index on join keys

**Estimated Speedup**: 10-20% for large datasets

---

### 6. quantify2() Double Nesting (MEDIUM IMPACT)
**Location**: Lines 2013-2044

**Problem**: Nested data structures created twice independently:
```r
df1 <- df1_base |>
  tidyr::nest(.by = dplyr::all_of(group_by_vars), .key = param1_name)
df2 <- df2_base |>
  tidyr::nest(.by = dplyr::all_of(group_by_vars), .key = param2_name)
```

**Impact**: Redundant nesting operations

**Solution**:
- Add file paths before nesting (already done)
- Consider single combined data structure if patterns allow

**Estimated Speedup**: 5-10%

---

## Optimization Recommendations (Prioritized)

### Tier 1: Immediate High-Impact Changes

1. **Remove `rowwise()` and add parallel processing**
   - Implementation time: 30 minutes
   - Expected speedup: 2-4x on multi-core systems
   - No breaking changes

2. **Cache metadata in corpus object**
   - Implementation time: 2 hours
   - Expected speedup: 15-30%
   - Requires corpus S7 class modification

3. **Replace `list_files()` with cached `peek_signals()`**
   - Implementation time: 1 hour
   - Expected speedup: 20-40% for this operation
   - No breaking changes

### Tier 2: Medium-Impact Changes

4. **Cache database handle in corpus**
   - Implementation time: 1 hour
   - Expected speedup: 10-20%
   - Requires corpus modification

5. **Optimize joins with data.table**
   - Implementation time: 3 hours
   - Expected speedup: 10-20% for large datasets
   - Adds dependency on data.table

6. **Memoize expensive database queries**
   - Implementation time: 2 hours
   - Expected speedup: 15-25%
   - Adds dependency on memoise

### Tier 3: Nice-to-Have Optimizations

7. **Vectorize string operations**
   - File path construction
   - Extension matching
   - Implementation time: 2 hours
   - Expected speedup: 5-10%

8. **Implement connection pooling**
   - Implementation time: 3 hours
   - Expected speedup: 5-10%
   - More complex, lower benefit

9. **Parallel directory scanning in peek_signals()**
   - Only beneficial for databases with 1000+ bundles
   - Implementation time: 1 hour
   - Expected speedup: Minimal for most use cases

## Implementation Strategy

### Phase 1: Quick Wins (1 day)
- Remove rowwise()
- Add parallel processing option
- Replace list_files() with peek_signals()

### Phase 2: Caching Infrastructure (2-3 days)
- Add metadata cache to corpus
- Add signal files cache to corpus
- Cache database handle
- Implement cache invalidation logic

### Phase 3: Advanced Optimizations (3-4 days)
- Implement memoization for queries
- Optimize joins with data.table
- Add benchmarking tests

## Estimated Overall Performance Improvement

With all Tier 1 and Tier 2 optimizations:
- **Sequential processing**: 40-60% faster
- **Parallel processing**: 150-300% faster (2.5-4x speedup)
- **Metadata-heavy operations**: 50-80% faster
- **Large dataset operations**: 60-100% faster (2x speedup)

## Testing Requirements

1. Benchmark current performance on:
   - Small corpus (< 10 bundles)
   - Medium corpus (10-100 bundles)
   - Large corpus (100-1000 bundles)

2. Verify correctness:
   - Results identical to emuR functions
   - Cache invalidation works correctly
   - Parallel processing produces same results

3. Memory profiling:
   - Ensure caching doesn't cause memory issues
   - Monitor cache size growth

## Notes

- Most optimizations are backward compatible
- Parallel processing should be optional (default on)
- Caching requires careful invalidation logic
- Consider adding `.use_cache` parameter for debugging
