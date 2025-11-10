# Data.table and Lazy Evaluation Implementation Status

## Completed

### 1. Infrastructure
- ✅ Added data.table to DESCRIPTION as core dependency  
- ✅ Created lazy_segment_list S7 class (reindeer_lazy_segment_list.R)
- ✅ Implemented `collect()` method for materialization
- ✅ Created SQL query building infrastructure
- ✅ Added lazy parameter to `ask_for()` function

### 2. Lazy Segment List Features
- ✅ Query part storage (base + transforms)
- ✅ Deferred execution until collect() called
- ✅ Print method showing query structure without execution
- ✅ Summary method with row count estimation
- ✅ as.data.frame method forcing materialization
- ✅ Helper functions: is_lazy(), needs_collect()

### 3. Transform Types Defined
- ✅ scout (forward sequence)
- ✅ retreat (backward sequence)  
- ✅ ascend (upward dominance)
- ✅ descend (downward dominance)

## In Progress

### 1. Query SQL Building
- ⏳ build_simple_query_sql() - DONE
- ⏳ build_sequence_query_sql() - Placeholder
- ⏳ build_dominance_query_sql() - Placeholder
- ⏳ build_function_query_sql() - Placeholder
- ⏳ build_conjunction_query_sql() - Placeholder
- ⏳ build_disjunction_query_sql() - Placeholder

## TODO

### High Priority

1. **Complete SQL Building Functions**
   - Implement all build_*_query_sql() functions
   - Test that they generate correct SQL
   - Verify output matches non-lazy execution

2. **Update segment_list Methods to Support Lazy**
   - scout() - check if lazy, append transform or execute
   - retreat() - check if lazy, append transform or execute
   - ascend_to() - check if lazy, append transform or execute  
   - descend_to() - check if lazy, append transform or execute

3. **Data.table Integration in Key Functions**
   - Convert ask_for() result handling to data.table
   - Update quantify() to use data.table operations
   - Update quantify2() to use data.table operations
   - Optimize metadata parameter resolution with data.table

### Medium Priority

4. **Peek Functions Optimization**
   - Convert peek_signals() to use data.table
   - Convert peek_tracks() to use data.table
   - Convert peek_annotations() to use data.table

5. **Extended Segment List Operations**
   - Update all track data operations to use data.table
   - Optimize grouping and aggregation operations
   - Implement efficient caching with data.table

6. **Testing**
   - Create comprehensive test suite for lazy evaluation
   - Test query chain equivalence
   - Benchmark lazy vs immediate execution
   - Test data.table performance improvements

### Low Priority

7. **Documentation**
   - Add vignette on lazy evaluation
   - Document when materialization occurs
   - Provide performance comparison examples
   - Add developer guide for adding new operations

8. **Additional Optimizations**
   - Consider dtplyr for dplyr-like syntax
   - Implement query caching
   - Add query plan visualization
   - Profile and optimize hot paths

## Performance Targets

### Lazy Evaluation
- Query chain building: < 1ms overhead per operation
- Materialization: No slower than immediate execution
- Memory: Minimal overhead (<1MB for query representation)

### Data.table Integration  
- Simple queries: 50%+ faster
- Complex joins: 70%+ faster
- Large result sets: 80%+ faster
- Metadata operations: 60%+ faster

## API Design Decisions

### Lazy by Default
- `ask_for()` returns lazy_segment_list by default
- Set `lazy = FALSE` for immediate execution
- Maintains backward compatibility

### Automatic Materialization
- Print/summary show preview without full materialization
- quantify() forces materialization
- File operations force materialization
- Conversion to data.frame forces materialization

### Query Chain Syntax
```r
# All lazy until collect()
result <- corpus %>%
  ask_for("Phonetic == t") %>%
  scout(1) %>%
  ascend_to("Word") %>%
  collect()

# Or force immediate execution
result <- ask_for(corpus, "Phonetic == t", lazy = FALSE)
```

## Implementation Notes

### SQL Query Building
- Use WITH clauses (CTEs) for chaining operations
- Each transform wraps previous query
- Final SQL can be quite complex but efficient
- Database optimizer handles the complexity

### Data.table Best Practices
- Always use `:=` for in-place modification
- Set keys early for fast joins
- Use `rbindlist()` not `rbind()` or `bind_rows()`
- Profile with `data.table::getDTthreads()` settings

### Backward Compatibility
- Old API still works (lazy = FALSE)
- segment_list class unchanged
- emuR interoperability maintained
- Gradual migration path

## Next Steps

1. Complete SQL building functions (highest priority)
2. Update segment_list methods for lazy support
3. Add comprehensive tests
4. Run benchmarks
5. Document new features
6. Create migration guide

## Known Limitations

### Current
- Complex query types not yet lazy (sequences, dominance)
- Fallback to immediate execution for unsupported queries
- No query optimization beyond what SQLite does

### Future Improvements
- Query plan caching
- Parallel query execution
- Incremental materialization
- Smarter transform ordering
