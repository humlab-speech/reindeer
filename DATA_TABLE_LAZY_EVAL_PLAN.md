# Data.table Integration and Lazy Evaluation Implementation Plan

## Overview
This document outlines the comprehensive integration of data.table for performance improvements and implementation of lazy evaluation for query operations.

## Current State Assessment

### Files requiring data.table integration:
1. **reindeer_query_optimized.r** - SQLite query execution and result handling
2. **reindeer_segment_list.R** - Segment list operations (scout, retreat, ascend_to, descend_to)
3. **tidy_trackdata.R** - Track data operations (quantify, quantify2, peek functions)
4. **reindeeR_metadata_optimized.R** - Metadata operations
5. **reindeeR_signalextensions_dt.R** - Already uses data.table but needs verification

### Data.table Benefits:
- Fast aggregation and joins
- Memory efficient in-place operations
- Excellent performance with large datasets
- Native support for rolling joins and overlaps

## Phase 1: Add data.table as Core Dependency

### 1.1 Update DESCRIPTION
```r
Imports: 
    data.table (>= 1.14.0),
    dtplyr,
    ...existing packages...
```

### 1.2 Update NAMESPACE
```r
import(data.table)
```

## Phase 2: Implement Lazy Evaluation for Query Operations

### 2.1 Create lazy_segment_list S7 Class

```r
lazy_segment_list <- S7::new_class(
  "lazy_segment_list",
  properties = list(
    corpus = class_any,  # reindeer::corpus object
    query_parts = class_list,  # List of query components to build SQL
    db_path = class_character,
    db_uuid = class_character,
    materialized = class_logical,  # FALSE until collect() is called
    cache = class_any  # Stores materialized result
  )
)
```

### 2.2 Lazy Operations

Operations that should return lazy_segment_list:
- `ask_for()` - Returns lazy_segment_list
- `scout(n)` - Appends sequence query to query_parts
- `retreat(n)` - Appends sequence query in reverse
- `ascend_to(level)` - Appends dominance query
- `descend_to(level)` - Appends reverse dominance query

### 2.3 Materialization

Operations that force materialization:
- `collect()` - Explicit materialization
- `quantify()` - Needs actual data
- `print()` / `summary()` - Show preview
- Conversion to data.frame/tibble
- File operations

### 2.4 SQL Query Building

```r
build_sql_query <- function(lazy_sl) {
  # Start with base query from ask_for
  base_sql <- lazy_sl@query_parts$base
  
  # Apply transformations
  for (transform in lazy_sl@query_parts$transforms) {
    base_sql <- apply_transform(base_sql, transform)
  }
  
  return(base_sql)
}
```

## Phase 3: Data.table Integration in Core Functions

### 3.1 ask_for() - Query Result Handling

Replace tibble operations with data.table:

```r
ask_for <- function(emuDB, query, ...) {
  # ... existing setup code ...
  
  # Execute SQL query
  result_dt <- data.table::setDT(
    DBI::dbGetQuery(conn, sql_query)
  )
  
  # Set keys for fast operations
  data.table::setkey(result_dt, session, bundle, start_item_id)
  
  # Return as lazy_segment_list
  return(lazy_segment_list(
    corpus = corpus_obj,
    query_parts = list(base = sql_query, transforms = list()),
    db_path = db_path,
    db_uuid = db_uuid,
    materialized = FALSE,
    cache = result_dt
  ))
}
```

### 3.2 scout() / retreat() - Sequence Operations

```r
scout <- function(seg_list, n = 1) {
  if (inherits(seg_list, "lazy_segment_list")) {
    # Append to query_parts
    seg_list@query_parts$transforms <- c(
      seg_list@query_parts$transforms,
      list(list(type = "scout", n = n))
    )
    return(seg_list)
  }
  
  # For materialized segment_list, use data.table operations
  dt <- data.table::as.data.table(seg_list)
  data.table::setkey(dt, session, bundle, end_item_seq_idx)
  
  # Fast rolling join for sequence operations
  result_dt <- dt[, {
    # Find next n items
    # ... data.table sequence logic ...
  }, by = .(session, bundle)]
  
  return(segment_list(result_dt, ...))
}
```

### 3.3 ascend_to() / descend_to() - Hierarchical Operations

```r
ascend_to <- function(seg_list, level) {
  if (inherits(seg_list, "lazy_segment_list")) {
    seg_list@query_parts$transforms <- c(
      seg_list@query_parts$transforms,
      list(list(type = "ascend", level = level))
    )
    return(seg_list)
  }
  
  # For materialized, use data.table with links table
  dt <- data.table::as.data.table(seg_list)
  
  # Read links from database
  conn <- DBI::dbConnect(RSQLite::SQLite(), seg_list@db_path)
  links_dt <- data.table::setDT(
    DBI::dbGetQuery(conn, "SELECT * FROM links")
  )
  data.table::setkey(links_dt, from_id)
  
  # Fast join operation
  result_dt <- merge(dt, links_dt, by.x = "start_item_id", by.y = "from_id")
  # ... rest of hierarchical logic ...
  
  DBI::dbDisconnect(conn)
  return(segment_list(result_dt, ...))
}
```

### 3.4 quantify() - Track Data Extraction

```r
quantify <- function(seg_list, dsp_fun, ...) {
  # Force materialization if lazy
  if (inherits(seg_list, "lazy_segment_list")) {
    seg_list <- collect(seg_list)
  }
  
  # Convert to data.table for fast operations
  dt <- data.table::as.data.table(seg_list)
  data.table::setkey(dt, session, bundle)
  
  # Group by session/bundle for efficient processing
  result_list <- dt[, {
    # Process all segments in this group
    .quantify_group(.SD, dsp_fun, ...)
  }, by = .(session, bundle)]
  
  # Return as extended_segment_list
  return(extended_segment_list(result_list, ...))
}

.quantify_group <- function(group_dt, dsp_fun, ...) {
  # Get unique signal file for this group
  signal_file <- unique(group_dt$signal_file_path)
  
  # Process all segments from this file at once
  results <- lapply(1:nrow(group_dt), function(i) {
    row <- group_dt[i]
    # ... DSP processing ...
  })
  
  # Use data.table::rbindlist for fast combining
  data.table::rbindlist(results, fill = TRUE)
}
```

### 3.5 quantify2() - Multi-segment Operations

```r
quantify2 <- function(seg_list1, seg_list2, dsp_fun, by = "session", ...) {
  # Force materialization
  if (inherits(seg_list1, "lazy_segment_list")) seg_list1 <- collect(seg_list1)
  if (inherits(seg_list2, "lazy_segment_list")) seg_list2 <- collect(seg_list2)
  
  # Convert to data.table
  dt1 <- data.table::as.data.table(seg_list1)
  dt2 <- data.table::as.data.table(seg_list2)
  
  # Add source identifiers
  dt1[, `:=`(source = "list1", row_id = .I)]
  dt2[, `:=`(source = "list2", row_id = .I)]
  
  # Combine using data.table
  combined_dt <- data.table::rbindlist(list(dt1, dt2), fill = TRUE)
  
  # Set keys for grouping
  setkeyv(combined_dt, by)
  
  # Process by groups
  result_dt <- combined_dt[, {
    .quantify2_group(.SD, dsp_fun, ...)
  }, by = c(by)]
  
  return(extended_segment_list(result_dt, ...))
}
```

### 3.6 Metadata Operations

```r
dspp_metadataParameters <- function(corpus, seg_list, dsp_fun) {
  # Get metadata as data.table
  conn <- DBI::dbConnect(RSQLite::SQLite(), corpus@cache_path)
  metadata_dt <- data.table::setDT(
    DBI::dbGetQuery(conn, "SELECT * FROM metadata")
  )
  DBI::dbDisconnect(conn)
  
  # Set keys for fast joins
  data.table::setkey(metadata_dt, session, bundle)
  
  seg_dt <- data.table::as.data.table(seg_list)
  data.table::setkey(seg_dt, session, bundle)
  
  # Fast join with metadata
  merged_dt <- metadata_dt[seg_dt]
  
  # Get DSP function parameters
  formal_args <- names(formals(dsp_fun))
  
  # Build parameter lists using data.table operations
  params_dt <- merged_dt[, {
    .resolve_parameters(.SD, dsp_fun, formal_args)
  }, by = .(session, bundle)]
  
  return(params_dt)
}
```

## Phase 4: Peek Functions Optimization

### 4.1 peek_signals()

```r
peek_signals <- function(corpus) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), corpus@cache_path)
  
  # Get bundles
  bundles_dt <- data.table::setDT(
    DBI::dbGetQuery(conn, "SELECT * FROM bundle")
  )
  
  # Build file paths using data.table
  bundles_dt[, signal_files := {
    file.path(
      corpus@basePath,
      paste0(session, "_ses"),
      paste0(name, "_bndl"),
      paste0(name, ".", corpus@config$mediafileExtension)
    )
  }]
  
  # Check existence
  bundles_dt[, exists := file.exists(signal_files)]
  
  # Select and rename columns
  result_dt <- bundles_dt[exists == TRUE, 
    .(session, bundle = name, name = basename(signal_files), 
      extension = corpus@config$mediafileExtension, 
      full_path = signal_files)]
  
  DBI::dbDisconnect(conn)
  return(result_dt)
}
```

## Phase 5: Testing and Validation

### 5.1 Test Lazy Evaluation

```r
test_that("Lazy evaluation delays execution", {
  corpus <- corpus(test_db_path)
  
  # These should not execute SQL queries
  lazy1 <- ask_for(corpus, "Phonetic == t")
  lazy2 <- scout(lazy1, 1)
  lazy3 <- ascend_to(lazy2, "Word")
  
  # Only this should execute
  result <- collect(lazy3)
  
  # Result should be equivalent to direct query
  direct <- ask_for(corpus, "[Word ^ [Phonetic == t -> #Phonetic]]")
  expect_equal(result, direct)
})
```

### 5.2 Benchmark Improvements

```r
# Compare old vs new implementations
bench::mark(
  old_way = {
    seg <- query_old(corpus, "Phonetic == t")
    seg <- scout(seg, 1)
    seg <- ascend_to(seg, "Word")
  },
  new_lazy = {
    seg <- ask_for(corpus, "Phonetic == t") %>%
      scout(1) %>%
      ascend_to("Word") %>%
      collect()
  },
  new_direct = {
    ask_for(corpus, "[Word ^ [Phonetic == t -> #Phonetic]]") %>%
      collect()
  },
  check = FALSE
)
```

## Phase 6: Documentation Updates

### 6.1 User-Facing Documentation

- Add vignette on lazy evaluation
- Document when materialization occurs
- Provide best practices for query chains
- Show performance comparisons

### 6.2 Developer Documentation

- Document SQL query building
- Explain data.table usage patterns
- Provide guidelines for adding new operations

## Implementation Priority

1. **High Priority** (Immediate)
   - Add data.table to dependencies
   - Implement lazy_segment_list class
   - Convert ask_for() to use data.table
   - Implement collect() method

2. **Medium Priority** (Next)
   - Implement lazy versions of scout/retreat
   - Implement lazy versions of ascend_to/descend_to
   - Convert quantify() to use data.table
   - Convert metadata operations to data.table

3. **Low Priority** (Later)
   - Optimize peek functions
   - Add comprehensive benchmarks
   - Update all documentation
   - Add user guides

## Performance Targets

- 50%+ improvement in query chaining operations
- 30%+ improvement in quantify() on large segment lists
- 70%+ improvement in metadata parameter resolution
- Minimal memory overhead from lazy evaluation

## Migration Strategy

1. Keep old functions as `*_legacy` for backward compatibility
2. Add new functions with same names
3. Deprecation warnings in next release
4. Remove legacy functions in release after that

## Notes

- All data.table operations should use `:=` for in-place modification
- Set keys whenever possible for fast joins
- Use `rbindlist()` instead of `rbind()` or `bind_rows()`
- Use `fread()` / `fwrite()` for any file I/O
- Consider using `data.table::shift()` for lag/lead operations
