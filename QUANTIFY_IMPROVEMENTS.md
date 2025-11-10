# Quantify Function Improvements

## Summary

The `quantify` method for `segment_list` objects has been significantly improved to work seamlessly with the new S7 class system and the `corpus` class. The updated implementation provides better integration, cleaner code, and enhanced functionality.

## Key Improvements

### 1. Proper Integration with S7 Classes

- **Corpus Access**: Now properly loads the corpus object from the segment_list's `db_path` property
- **Type Safety**: Uses S7 generics and methods for type-safe dispatch
- **Clean Validation**: Validates inputs and provides clear error messages

### 2. Enhanced Metadata Integration

```r
# Metadata is now fetched efficiently
- Only fetches metadata for bundles in the segment list (not all bundles)
- Properly pivots metadata from key-value pairs to columns
- Joins metadata with segments before processing for efficiency
```

### 3. Signal File Path Resolution

```r
# Now properly derives signal file paths from corpus config
media_ext <- corpus_obj@config$mediafileExtension %||% "wav"
signal_file <- file.path(
  corpus_obj@basePath,
  paste0(seg$session, "_ses"),
  paste0(seg$bundle, "_bndl"),
  paste0(seg$bundle, ".", media_ext)
)
```

### 4. Time Point Extraction (New Feature)

The `.at` parameter allows extraction of specific time points from SSFF tracks:

```r
# Extract at midpoint only
formants_mid <- quantify(segs, superassp::forest, .at = 0.5)

# Extract at three points
formants_three <- quantify(segs, superassp::forest, .at = c(0.2, 0.5, 0.8))
```

This is particularly useful when you don't need the full time series but just specific measurement points.

### 5. Better SSFF Track Handling

```r
# Handles AsspDataObj results properly
if (inherits(result, "AsspDataObj")) {
  track_data <- wrassp::as.data.frame.AsspDataObj(result)
  
  # Extract at specific time points if requested
  if (!is.null(.at)) {
    n_frames <- nrow(track_data)
    frame_indices <- pmax(1, pmin(n_frames, round(.at * n_frames)))
    track_data <- track_data[frame_indices, , drop = FALSE]
    track_data$.time_point <- .at
  }
}
```

### 6. Row Replication for Multiple Time Points

When extracting multiple time points, each segment's metadata is properly replicated:

```r
# Replicate segment info for each extracted point
n_result_rows <- nrow(result_df)
seg_replicated <- seg[rep(1, n_result_rows), , drop = FALSE]
combined <- dplyr::bind_cols(
  tibble::as_tibble(seg_replicated),
  tibble::as_tibble(result_df)
)
```

### 7. Improved Error Handling

- Provides clear messages when corpus cannot be loaded
- Warns when signal files are missing
- Reports errors during DSP processing (in verbose mode)
- Gracefully handles NULL results

### 8. Better Progress Reporting

```r
if (.verbose) {
  n_segs <- length(unique(combined$start_item_id))
  n_rows <- nrow(combined)
  cli::cli_alert_success("Processed {n_segs} segment{?s} ({n_rows} row{?s} total)")
}
```

## Usage Examples

### Basic Usage

```r
# Create corpus and query
corpus_obj <- corpus("/path/to/ae_emuDB")
segs <- ask_for(corpus_obj, "Phonetic == t")

# Apply DSP function
formants <- quantify(segs, superassp::forest)
```

### With Metadata-Driven Parameters

```r
# Metadata (Age, Gender) automatically adjusts DSP parameters
formants <- quantify(segs, superassp::forest, .use_metadata = TRUE)
```

### Extract Specific Time Points

```r
# Midpoint only
mid_formants <- quantify(segs, superassp::forest, .at = 0.5)

# Three points (20%, 50%, 80% through segment)
three_points <- quantify(segs, superassp::forest, .at = c(0.2, 0.5, 0.8))
```

### Disable Parallel Processing

```r
# Sequential processing (useful for debugging)
formants <- quantify(segs, superassp::forest, .parallel = FALSE, .verbose = TRUE)
```

### Override DSP Parameters

```r
# Explicit parameter override
formants <- quantify(segs, superassp::forest, 
                    nominalF1 = 600,  # Override automatic parameter
                    .use_metadata = TRUE)
```

## Comparison with emuR::get_trackdata()

The `quantify` method provides similar functionality to `emuR::get_trackdata()` but with:

1. **Metadata Integration**: Automatic parameter derivation from speaker metadata
2. **Flexible DSP Functions**: Works with any superassp function, not just wrassp
3. **Time Point Extraction**: Built-in support for extracting specific time points
4. **Parallel Processing**: Efficient parallel execution with progress tracking
5. **Better Error Handling**: More informative error messages and warnings
6. **Type Safety**: S7 classes ensure type correctness

## Performance Characteristics

- **Parallel Processing**: Default uses `future::multisession` with N-1 cores
- **Metadata Caching**: Fetches metadata once and joins efficiently
- **Lazy Evaluation**: Only processes segments with valid signal files
- **Memory Efficient**: Processes segments individually, not loading entire database

## Future Enhancements

Potential improvements for future versions:

1. **Caching**: Cache DSP results to disk for reuse
2. **Batch Processing**: Process multiple segment lists simultaneously
3. **Streaming**: Support for very large segment lists with streaming
4. **Custom Extractors**: Plugin system for custom time point extraction logic
5. **GPU Acceleration**: Support for GPU-accelerated DSP functions

## Technical Notes

### Dependencies

- `S7`: For generic methods and classes
- `dplyr`, `tidyr`: For data manipulation
- `future`, `furrr`: For parallel processing
- `cli`: For progress reporting and messages
- `wrassp`: For SSFF track handling (when applicable)
- `DBI`: For database access

### Thread Safety

The parallel processing implementation is thread-safe:
- Each worker has its own database connection
- No shared mutable state between workers
- Results are combined after all workers complete

### Memory Management

- Database connections are properly closed with `on.exit()`
- Large result objects are not duplicated unnecessarily
- NULL results are filtered out before combination

