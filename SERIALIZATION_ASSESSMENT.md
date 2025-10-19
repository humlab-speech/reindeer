# Serialization Assessment for Cache Systems in Reindeer

**Date:** October 19, 2025  
**Focus:** Cache serialization in `enrich()` and `quantify()` methods

---

## Executive Summary

The reindeer package currently uses R's native `serialize()`/`unserialize()` functions for caching intermediate DSP results in the `quantify()` method. This assessment evaluates alternative serialization schemes to optimize for:

1. **Space efficiency** - Reduce cache storage requirements
2. **Processing performance** - Faster serialization/deserialization
3. **Data integrity** - Preserve data types and structures accurately

---

## Current Implementation

### Location
- **File:** `R/tidy_trackdata_helpers.R`
- **Functions:** `.set_persistent_cache()` (line 307), `.get_persistent_cache()` (line 287)

### Current Method: R's `serialize()`

```r
# Serialization (line 309)
result_blob <- serialize(result, NULL)

# Deserialization (line 299)
unserialize(result$result_blob[[1]])
```

### Storage Backend
- **Database:** SQLite
- **Table:** `cache`
- **Column:** `result_blob` (BLOB type)
- **Location:** `tempdir()/reindeer_cache/quantify_cache.sqlite` (default)

### Current Cache Architecture
```
quantify() → .process_segments_vectorized()
    ↓
.get_persistent_cache() ← Check cache
    ↓ (if miss)
Process DSP data
    ↓
.set_persistent_cache() → serialize() → SQLite BLOB
```

---

## Evaluated Alternatives

### 1. **qs Package** (Quick Serialization)

#### Pros
- **Speed:** 2-4x faster serialization, 3-5x faster deserialization than base R
- **Compression:** Better compression ratios (typically 20-40% smaller)
- **Algorithms:** Multiple compression options (lz4, zstd, uncompressed)
- **Maintained:** Active development, CRAN package
- **R-native:** Preserves all R object types perfectly

#### Cons
- **Dependency:** Adds external package dependency
- **Breaking change:** Cache files not backward compatible with serialize()
- **Documentation:** Less widely known than base R

#### Performance Estimates
- Serialization: 2-3x faster
- Deserialization: 3-5x faster  
- Storage: 60-80% of serialize() size (with default compression)
- Storage: 80-90% of serialize() size (with lz4 - fastest)

#### Implementation
```r
# Requires: install.packages("qs")
result_blob <- qs::qserialize(result, preset = "fast")  # or "balanced", "high"
result <- qs::qdeserialize(result_blob)
```

---

### 2. **fst Package** (Fast Serialization of Data Frames)

#### Pros
- **Speed:** Extremely fast for data frames (10-20x faster than RDS)
- **Compression:** Excellent compression with minimal CPU overhead
- **Column access:** Can read specific columns without loading entire object
- **Multi-threaded:** Uses multiple cores automatically

#### Cons
- **Limited types:** Only works with data frames/tibbles
- **Column types:** Limited to common types (numeric, character, factor, Date, POSIXct)
- **Not universal:** Cannot serialize arbitrary R objects (lists, S7 objects)
- **Use case:** Only applicable if quantify results are always data frames

#### Performance Estimates (for data frames only)
- Serialization: 10-20x faster than serialize()
- Deserialization: 5-10x faster
- Storage: 50-70% of serialize() size

#### Implementation Notes
- Only viable if we can guarantee quantify() results are always data frames
- Current implementation returns data frames, but flexibility may be needed
- Would require restructuring cache to use file-based storage instead of BLOB

---

### 3. **Apache Arrow/Feather**

#### Pros
- **Interoperability:** Language-agnostic format (Python, R, C++, Java)
- **Zero-copy:** Extremely fast memory-mapped access
- **Columnar:** Efficient for analytical operations
- **Types:** Rich type system including nested data

#### Cons
- **Heavy dependency:** Large package with C++ dependencies
- **Overkill:** More than needed for simple caching
- **Complexity:** Steeper learning curve
- **File-based:** Works with files, not BLOBs (would require architecture change)

#### Performance Estimates
- Serialization: 5-8x faster for data frames
- Deserialization: 3-5x faster with memory mapping
- Storage: 40-60% of serialize() size (Parquet format)

---

### 4. **RDS with Compression Options**

#### Pros
- **Native:** No dependencies
- **Stable:** Well-tested and reliable
- **Compatible:** Standard R format

#### Cons
- **Slower:** Similar or slightly slower than serialize()
- **Compression:** Limited to gzip, bzip2, xz
- **No improvement:** Minimal benefit over current approach

#### Performance
- Similar to current serialize() approach
- Not a significant improvement

---

### 5. **Custom Binary Format**

#### Pros
- **Optimized:** Tailored to specific data structures
- **Compact:** Can be very space-efficient for known structures

#### Cons
- **Maintenance:** Requires ongoing maintenance
- **Complexity:** Higher development cost
- **Fragility:** Can break with data structure changes
- **Not recommended:** Overkill for this use case

---

## Benchmark Methodology

### Test Data Structures
1. **Small result:** 100 rows, 5 columns (formant data)
2. **Medium result:** 1000 rows, 10 columns (f0 + formants)
3. **Large result:** 10000 rows, 20 columns (full spectral data)

### Metrics
1. Serialization time (ms)
2. Deserialization time (ms)
3. Size in bytes
4. Compression ratio vs. serialize()
5. Round-trip fidelity (verify data integrity)

### Test Script
See `benchmarking/benchmark_serialization.R`

---

## Recommendations

### Primary Recommendation: **qs Package**

#### Rationale
1. **Best balance:** Significant performance gains without major architecture changes
2. **Drop-in replacement:** Minimal code changes required
3. **Proven:** Used by many R packages for caching
4. **R-native:** Handles all R object types perfectly
5. **Compression:** Good space savings with fast algorithms

#### Implementation Plan

**Phase 1: Add qs support with fallback (1-2 hours)**
```r
.set_persistent_cache <- function(cache_key, result, conn, 
                                  max_cache_size_mb = 1000,
                                  use_qs = TRUE) {
  # Try qs if available and requested
  if (use_qs && requireNamespace("qs", quietly = TRUE)) {
    result_blob <- qs::qserialize(result, preset = "fast")
    format <- "qs"
  } else {
    result_blob <- serialize(result, NULL)
    format <- "rds"
  }
  
  size_bytes <- length(result_blob)
  # ... rest of function with format column in cache table
}
```

**Phase 2: Update cache schema**
- Add `format` column to cache table ("qs" or "rds")
- Allows gradual migration and backward compatibility
- Old cache entries still work

**Phase 3: Benchmarking**
- Compare performance with serialize()
- Verify data integrity
- Measure cache size reduction

**Phase 4: Documentation**
- Update user documentation
- Add `.use_qs` parameter to quantify()
- Document performance characteristics

#### Expected Benefits
- **Speed:** 2-4x faster cache operations
- **Storage:** 20-40% smaller cache files
- **Backward compatible:** Optional, falls back to serialize()
- **Minimal risk:** Well-tested package

---

### Alternative Recommendation: **fst (Conditional)**

Only if we can guarantee quantify() always returns data frames and we want maximum speed.

#### Requirements
1. Refactor cache to use file-based storage (not SQLite BLOBs)
2. Ensure all quantify results are coercible to data frames
3. Handle metadata separately

#### Benefits (if applicable)
- **Speed:** 10-20x faster for large data frames
- **Efficiency:** Excellent for columnar data
- **Multi-threaded:** Automatic parallelization

#### Implementation Complexity
- Medium to high
- Requires cache architecture redesign
- Benefits may not justify complexity for typical use cases

---

### NOT Recommended

1. **Arrow/Feather:** Too heavy for this use case
2. **Custom format:** Unnecessary complexity
3. **RDS variants:** No significant improvement
4. **No change:** Missing optimization opportunity

---

## Implementation Priority

### High Priority (Recommended)
- ✅ **qs package integration** - Best cost/benefit ratio

### Medium Priority (Nice to have)
- Consider fst for specific large-scale scenarios
- Add benchmarking infrastructure for cache performance
- Allow user-configurable cache location

### Low Priority
- Investigate cache compression strategies
- Implement cache analytics (hit rate, size trends)

---

## Testing Requirements

### Functional Tests
1. Serialization round-trip fidelity
2. Backward compatibility with existing caches
3. Error handling for corrupted cache entries
4. Cache size limits working correctly

### Performance Tests
1. Benchmark serialization speed
2. Benchmark deserialization speed
3. Measure cache size reduction
4. Compare memory usage

### Integration Tests
1. quantify() with cache enabled/disabled
2. Large corpus processing
3. Parallel processing with caching
4. Cache invalidation logic

---

## Migration Strategy

### For Existing Users
1. **Default behavior:** Continue using serialize() for now
2. **Opt-in:** Add `.cache_format = "qs"` parameter
3. **Future:** Switch to qs as default in next major version
4. **Migration tool:** Provide function to convert cache formats

### Cache Format Handling
```r
# Detect format and deserialize accordingly
.get_persistent_cache <- function(cache_key, conn) {
  result <- DBI::dbGetQuery(conn, 
    "SELECT result_blob, format FROM cache WHERE cache_key = ?",
    params = list(cache_key))
  
  if (nrow(result) > 0) {
    format <- result$format[1] %||% "rds"  # Default to rds for old entries
    
    blob <- result$result_blob[[1]]
    data <- if (format == "qs" && requireNamespace("qs", quietly = TRUE)) {
      qs::qdeserialize(blob)
    } else {
      unserialize(blob)
    }
    
    # Update access time
    DBI::dbExecute(conn, 
      "UPDATE cache SET accessed_at = ? WHERE cache_key = ?",
      params = list(as.integer(Sys.time()), cache_key))
    
    return(data)
  }
  NULL
}
```

---

## Cost-Benefit Analysis

### qs Integration

| Aspect | Cost | Benefit |
|--------|------|---------|
| Development | 2-3 hours | One-time |
| Testing | 2-3 hours | One-time |
| Documentation | 1 hour | One-time |
| New dependency | Minimal (0.5 MB) | Well-maintained CRAN package |
| Performance gain | - | 2-4x faster cache operations |
| Storage savings | - | 20-40% smaller cache |
| User impact | None (optional) | Faster workflows |

**Total effort:** ~6 hours  
**Expected improvement:** 2-4x cache performance, 20-40% storage savings

### ROI Calculation
For a typical workflow with 1000 cached results:
- Time saved per operation: ~10-50ms
- Total time saved: 10-50 seconds per workflow
- Cache size reduction: 50-200 MB typical

For large-scale analysis (10,000+ segments):
- Time saved: 2-5 minutes per analysis
- Cache size reduction: 500 MB - 2 GB

---

## Conclusion

The **qs package** provides the best balance of performance improvement, ease of implementation, and minimal risk. It offers 2-4x faster cache operations and 20-40% storage savings with only a small additional dependency.

The implementation can be done in a backward-compatible, opt-in manner, allowing gradual migration and minimal disruption to existing users.

**Recommended action:** Implement qs support as an optional feature with fallback to serialize(), then benchmark real-world performance gains before making it the default.

---

## References

1. qs package: https://cran.r-project.org/package=qs
2. fst package: https://www.fstpackage.org/
3. Apache Arrow R: https://arrow.apache.org/docs/r/
4. R serialize documentation: https://stat.ethz.ch/R-manual/R-devel/library/base/html/serialize.html

---

## Appendix: Code Examples

### Example 1: Current Implementation
```r
# Current cache set (tidy_trackdata_helpers.R:307)
.set_persistent_cache <- function(cache_key, result, conn, max_cache_size_mb = 1000) {
  result_blob <- serialize(result, NULL)
  size_bytes <- length(result_blob)
  current_time <- as.integer(Sys.time())
  
  DBI::dbExecute(conn, "
    INSERT OR REPLACE INTO cache (cache_key, result_blob, created_at, accessed_at, size_bytes)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(cache_key, list(result_blob), current_time, current_time, size_bytes))
}
```

### Example 2: Proposed qs Implementation
```r
.set_persistent_cache <- function(cache_key, result, conn, 
                                  max_cache_size_mb = 1000,
                                  format = c("auto", "qs", "rds")) {
  format <- match.arg(format)
  
  # Determine format
  if (format == "auto") {
    format <- if (requireNamespace("qs", quietly = TRUE)) "qs" else "rds"
  }
  
  # Serialize
  result_blob <- if (format == "qs") {
    qs::qserialize(result, preset = "fast")
  } else {
    serialize(result, NULL)
  }
  
  size_bytes <- length(result_blob)
  current_time <- as.integer(Sys.time())
  
  # Store with format marker
  DBI::dbExecute(conn, "
    INSERT OR REPLACE INTO cache 
    (cache_key, result_blob, format, created_at, accessed_at, size_bytes)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(cache_key, list(result_blob), format, 
                   current_time, current_time, size_bytes))
}
```

### Example 3: Migration Helper
```r
#' Convert cache format from RDS to qs
#' @param cache_dir Cache directory path
#' @export
convert_cache_format <- function(cache_dir = NULL) {
  if (!requireNamespace("qs", quietly = TRUE)) {
    cli::cli_abort("qs package required for cache conversion")
  }
  
  conn <- .get_persistent_cache_connection(cache_dir)
  on.exit(DBI::dbDisconnect(conn))
  
  # Get all RDS entries
  entries <- DBI::dbGetQuery(conn, "
    SELECT cache_key, result_blob FROM cache 
    WHERE format = 'rds' OR format IS NULL
  ")
  
  if (nrow(entries) == 0) {
    cli::cli_alert_info("No entries to convert")
    return(invisible())
  }
  
  cli::cli_alert_info("Converting {nrow(entries)} cache entries to qs format")
  pb <- cli::cli_progress_bar(total = nrow(entries))
  
  for (i in seq_len(nrow(entries))) {
    # Deserialize with RDS
    obj <- unserialize(entries$result_blob[[i]])
    
    # Reserialize with qs
    new_blob <- qs::qserialize(obj, preset = "fast")
    
    # Update in database
    DBI::dbExecute(conn, "
      UPDATE cache 
      SET result_blob = ?, format = 'qs', size_bytes = ?
      WHERE cache_key = ?
    ", params = list(list(new_blob), length(new_blob), entries$cache_key[i]))
    
    cli::cli_progress_update(id = pb)
  }
  
  cli::cli_progress_done(id = pb)
  cli::cli_alert_success("Cache conversion complete")
}
```
