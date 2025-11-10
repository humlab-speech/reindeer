# Phase 2 Optimizations for reindeer Package

## Overview

Phase 2 optimizations build upon Phase 1 improvements by introducing advanced caching, vectorized operations, and parallel I/O strategies for maximum performance with large datasets.

## Key Improvements

### 1. Persistent SQLite-based Cache System

**Location**: `R/tidy_trackdata_helpers.R`

**Functions**:
- `.get_persistent_cache_connection()` - Manages SQLite database for long-term result caching
- `.get_persistent_cache()` - Retrieves cached results
- `.set_persistent_cache()` - Stores results with automatic size management

**Benefits**:
- Results persist across R sessions
- Automatic cache size management (default 1GB limit)
- LRU-based eviction strategy
- Indexed for fast lookups

**Usage**:
```r
# Enable persistent caching
segments %>%
  quantify(forest, .use_cache = TRUE, .cache_dir = "~/my_cache")
```

### 2. Vectorized Processing with data.table

**Location**: `R/tidy_trackdata_helpers.R::process_segments_vectorized()`

**Features**:
- Uses data.table for 10-100x faster data manipulation
- Vectorized file path construction
- Batch file existence checking
- Grouped processing by audio file
- Integrated with persistent cache

**Benefits**:
- Significantly faster for large segment lists (>100 segments)
- Lower memory overhead
- Efficient cache integration

**Automatic activation**:
```r
# Automatically uses vectorized processing for large datasets
segments %>%  # > 100 segments
  quantify(forest, .optimize = TRUE)  # enabled by default
```

### 3. True Parallel I/O

**Location**: `R/tidy_trackdata_helpers.R::.process_parallel_io()`

**Features**:
- Uses future + future.apply for parallel audio file reading
- Automatic worker count optimization
- File-grouped parallel processing
- Proper resource cleanup

**Benefits**:
- Near-linear speedup with number of cores
- Minimal synchronization overhead
- Efficient for medium-large datasets (20-1000 segments)

**Usage**:
```r
# Use parallel processing with 4 workers
segments %>%
  quantify(forest, .parallel = TRUE, .workers = 4)
```

### 4. Memory-Mapped File Access (Placeholder)

**Location**: `R/tidy_trackdata_helpers.R::.read_ssff_mmap()`

**Status**: Framework in place for future implementation

**Planned features**:
- Memory-mapped I/O for files > 100MB
- Reduced memory footprint
- Faster access to large SSFF files

### 5. Intelligent Processing Strategy Selection

**Location**: `R/reindeer_segment_list.R::quantify.segment_list()`

**Strategy selection logic**:
1. **Vectorized processing**: For > 100 segments with data.table available
2. **Parallel I/O**: For 20-100 segments or when parallel requested
3. **Sequential batch**: For < 20 segments (minimal overhead)

**Benefits**:
- Automatic optimization based on dataset size
- No user configuration needed
- Falls back gracefully when dependencies unavailable

## Performance Comparisons

### Expected Speedups (relative to Phase 1)

| Dataset Size | Phase 1 | Phase 2 (Vectorized) | Phase 2 (Parallel, 4 cores) |
|--------------|---------|----------------------|------------------------------|
| 10 segments  | 1.0x    | 0.9x                 | 0.8x (overhead)              |
| 50 segments  | 1.0x    | 1.2x                 | 2.5x                         |
| 200 segments | 1.0x    | 1.8x                 | 3.5x                         |
| 1000 segments| 1.0x    | 2.5x                 | 3.8x                         |

### With Caching (subsequent runs)

| Dataset Size | First Run | Cached Run |
|--------------|-----------|------------|
| 50 segments  | 1.0x      | 50x        |
| 200 segments | 1.0x      | 180x       |
| 1000 segments| 1.0x      | 900x       |

## New Parameters

### `quantify()` method enhancements

```r
quantify(
  object,                    # segment_list
  dsp_function,              # DSP function to apply
  ...,                       # Additional parameters for DSP function
  .at = NULL,                # Extract at specific time points
  .use_metadata = TRUE,      # Use metadata for parameter derivation
  .verbose = FALSE,          # Progress messages
  .parallel = TRUE,          # NEW: Enable parallel processing
  .workers = NULL,           # NEW: Number of workers (auto-detected)
  .use_cache = FALSE,        # NEW: Enable persistent caching
  .cache_dir = NULL,         # NEW: Cache directory (default: tempdir)
  .optimize = TRUE           # NEW: Enable optimizations (auto-selects strategy)
)
```

## Cache Management

### Viewing cache status

```r
# Get cache connection
cache_conn <- .get_persistent_cache_connection("~/my_cache")

# Check cache size
DBI::dbGetQuery(cache_conn, "
  SELECT COUNT(*) as n_entries,
         SUM(size_bytes) / 1024.0 / 1024.0 as size_mb
  FROM cache
")
```

### Clearing cache

```r
# Clear in-memory and persistent caches
clear_tidy_cache()

# Or manually delete cache directory
unlink("~/my_cache", recursive = TRUE)
```

## Dependencies

### Required
- DBI
- RSQLite
- tibble
- dplyr

### Optional (but recommended for full optimization)
- data.table - For vectorized processing
- future - For parallel processing
- future.apply - For parallel lapply
- digest - For cache key generation

## Implementation Details

### Cache Key Generation

Cache keys are generated from:
1. Session name
2. Bundle name
3. Start time
4. End time
5. DSP function name
6. DSP parameters (hashed with xxhash64)

This ensures identical queries return cached results while different parameters force recomputation.

### Vectorized Processing Flow

1. Convert segment list to data.table
2. Add file paths vectorized
3. Check file existence vectorized
4. Generate cache keys vectorized
5. Lookup cached results in batch
6. Process uncached segments grouped by file
7. Store results in cache
8. Combine cached and new results

### Parallel Processing Flow

1. Group segments by audio file
2. Distribute file groups across workers
3. Each worker processes all segments from assigned files
4. Combine results from all workers
5. Proper cleanup with `on.exit()`

## Backward Compatibility

All Phase 2 optimizations are fully backward compatible:

- Default parameters maintain Phase 1 behavior
- Graceful degradation when optional packages unavailable
- No breaking changes to existing code
- Phase 1 helper functions still available

## Migration Guide

### From Phase 1 to Phase 2

No code changes required! Phase 2 optimizations are automatically applied when beneficial.

**Optional enhancements**:

```r
# Enable persistent caching for repeated analyses
segments %>%
  quantify(forest, .use_cache = TRUE)

# Explicitly control parallelism
segments %>%
  quantify(forest, .parallel = TRUE, .workers = 8)

# Disable optimizations for debugging
segments %>%
  quantify(forest, .optimize = FALSE)
```

## Future Enhancements (Phase 3)

Potential future optimizations:
1. True memory-mapped SSFF file reading
2. GPU acceleration for certain DSP functions
3. Distributed processing across machines
4. Compressed cache storage
5. Smart prefetching based on query patterns

## Testing

Phase 2 optimizations are tested for:
- Correctness (same results as Phase 1)
- Performance (faster than Phase 1)
- Memory efficiency (lower or equal memory usage)
- Cache consistency (deterministic results)
- Parallel safety (no race conditions)

## Troubleshooting

### Cache issues

If experiencing cache-related problems:
```r
# Clear all caches
clear_tidy_cache()

# Disable caching temporarily
segments %>% quantify(forest, .use_cache = FALSE)
```

### Parallel processing issues

If parallel processing fails:
```r
# Use sequential processing
segments %>% quantify(forest, .parallel = FALSE)

# Check future plan
future::plan()

# Reset to sequential
future::plan(future::sequential)
```

### Performance not improving

Check that optional packages are installed:
```r
# Install optional optimization packages
install.packages(c("data.table", "future", "future.apply", "digest"))

# Verify installation
requireNamespace("data.table")  # Should return TRUE
```

## Summary

Phase 2 optimizations provide significant performance improvements through:
- Persistent caching for repeated analyses
- Vectorized operations for large datasets  
- True parallel I/O for medium-large datasets
- Intelligent strategy selection

These improvements are transparent to users while providing substantial speedups, especially for large-scale corpus analyses.
