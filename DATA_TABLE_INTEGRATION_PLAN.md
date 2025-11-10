# Data.table Integration and Optimization Plan

## Objective
Integrate data.table throughout the reindeer package for maximum performance while maintaining 100% fidelity with emuR behavior and enabling lazy evaluation where beneficial.

## Key Priorities (High to Low)

### 1. Core Query Functions (HIGH PRIORITY - IN PROGRESS)
**Status**: Implementing now

#### ask_for() - OPTIMIZE
- [x] Already returns data.table-compatible results
- [ ] Ensure internal operations use data.table for joining
- [ ] Add lazy evaluation support (return lazy_segment_list by default)
- [ ] Add `collect()` option to materialize immediately

#### scout() / retreat() - NEEDS OPTIMIZATION  
- [ ] Replace dplyr joins with data.table syntax
- [ ] Use data.table's rolling joins for sequence operations
- [ ] Integrate with lazy_segment_list (append SQL transforms)

#### ascend_to() / descend_to() - NEEDS OPTIMIZATION
- [ ] Use data.table for link traversal
- [ ] Optimize hierarchical joins with data.table
- [ ] Integrate with lazy evaluation

### 2. DSP and Track Data Functions (HIGH PRIORITY)
**Status**: Partially complete

#### dspp_metadataParameters() - COMPLETED ✓
- [x] Optimized with data.table
- [x] 40-60% performance improvement
- [x] Benchmarked and documented

#### quantify() - NEEDS OPTIMIZATION
- [ ] Replace dplyr with data.table for segment processing
- [ ] Use data.table's by-group operations for bundle-wise processing
- [ ] Optimize result assembly with data.table::rbindlist()

#### quantify2() - NEEDS OPTIMIZATION  
- [ ] Use data.table merge for combining two segment lists
- [ ] Optimize session-level grouping
- [ ] Fast assembly of results

### 3. Metadata Functions (MEDIUM PRIORITY)
**Status**: Needs work

#### get_metadata() - NEEDS OPTIMIZATION
- [ ] Use data.table::fread() for JSON reading if beneficial
- [ ] data.table for hierarchical override logic
- [ ] Cache with data.table in SQLite

#### add_metadata() / set_metadata() - NEEDS OPTIMIZATION
- [ ] data.table for validation and type checking
- [ ] Batch updates with data.table

### 4. Supporting Functions (MEDIUM PRIORITY)

#### peek_* functions - NEEDS OPTIMIZATION
- [ ] Use data.table for file listing and filtering
- [ ] Fast path traversal

#### signal_files() - NEEDS OPTIMIZATION
- [ ] data.table for building file inventory
- [ ] Efficient filtering

### 5. Segment List Class Methods (HIGH PRIORITY)

#### Print/Summary - NEEDS OPTIMIZATION
- [ ] Use data.table's print methods where appropriate
- [ ] Fast summarization with data.table

#### Subsetting/Filtering - NEEDS OPTIMIZATION
- [ ] Implement `[.segment_list` with data.table indexing
- [ ] Support fast filtering

## Implementation Strategy

### Phase 1: Core Query Chain (CURRENT)
1. ✓ Implement lazy_segment_list class
2. [ ] Integrate ask_for() with lazy evaluation
3. [ ] Optimize scout/retreat with data.table
4. [ ] Optimize ascend/descend with data.table
5. [ ] Add comprehensive benchmarks

### Phase 2: DSP Functions
1. ✓ Optimize dspp_metadataParameters()
2. [ ] Optimize quantify()
3. [ ] Optimize quantify2()
4. [ ] Benchmark against emuR

### Phase 3: Metadata and Support
1. [ ] Optimize metadata functions
2. [ ] Optimize peek_* functions
3. [ ] Complete integration testing

## Data.table Best Practices

### Key Operations to Use
```r
# Fast filtering
DT[condition, ]

# Fast grouping  
DT[, .(result), by = group]

# Fast joins
merge(DT1, DT2, by = "key")
DT1[DT2, on = "key"]  # right join

# Fast updates by reference
DT[, new_col := value]
set(DT, i, j, value)

# Fast binding
rbindlist(list_of_DTs, use.names = TRUE, fill = TRUE)

# Setting keys for fast operations
setkey(DT, col1, col2)
setindex(DT, col)
```

### When to Materialize vs Stay Lazy
- **Stay Lazy**: Query chains (ask_for -> scout -> ascend)
- **Materialize**: When DSP processing needed (quantify)
- **Materialize**: When interactive inspection requested (print with data)

## Performance Targets

### Query Operations
- Simple queries: < 100ms for 10k items
- Complex chains: < 500ms for 10k items
- 5-10x faster than emuR::query

### DSP Operations
- quantify(): 2-3x faster than emuR::get_trackdata()
- dspp_metadataParameters(): 40-60% faster (achieved ✓)

### Metadata Operations
- get_metadata(): 3-5x faster than current
- Bulk operations: 10x faster with caching

## Testing Requirements

### Fidelity Tests (MANDATORY)
- [ ] All results must match emuR output exactly
- [ ] Test with ae database and larger corpuses
- [ ] Edge cases: empty results, single items, large batches

### Performance Tests
- [ ] Benchmark against emuR for all major operations
- [ ] Memory profiling for large datasets
- [ ] Parallel processing benchmarks

### Integration Tests
- [ ] Query chains work end-to-end
- [ ] DSP pipelines work correctly
- [ ] Metadata propagates correctly

## Status Tracking

- ✓ = Completed
- 🚧 = In Progress  
- ⏳ = Planned
- ❌ = Blocked/Issues

| Component | Status | Performance Gain | Fidelity | Notes |
|-----------|--------|------------------|----------|-------|
| dspp_metadataParameters | ✓ | 40-60% | 100% | Benchmarked |
| lazy_segment_list | ✓ | N/A | N/A | Framework ready |
| ask_for | 🚧 | TBD | 100% | Needs lazy integration |
| scout/retreat | ⏳ | TBD | 100% | Planned |
| ascend/descend | ⏳ | TBD | 100% | Planned |
| quantify | ⏳ | TBD | 100% | Planned |
| quantify2 | ⏳ | TBD | 100% | Planned |

## Next Actions

1. **Integrate ask_for with lazy evaluation**
   - Return lazy_segment_list by default
   - Add `materialize = TRUE` option for immediate results
   - Ensure SQL building is efficient

2. **Optimize scout/retreat**
   - Implement data.table versions
   - Add lazy SQL transforms
   - Benchmark improvements

3. **Optimize ascend/descend**
   - data.table for link following
   - Lazy SQL for dominance queries
   - Test hierarchical chains

4. **Update all tests and benchmarks**
   - Ensure fidelity maintained
   - Document performance gains
   - Update vignette

## Decision Log

### 2025-10-16: Lazy by Default
**Decision**: make_for() returns lazy_segment_list by default
**Rationale**: Most query chains benefit from SQL optimization
**Impact**: Need collect() or automatic materialization for DSP

### 2025-10-16: data.table Throughout
**Decision**: Use data.table for all tabular operations
**Rationale**: Significant performance gains (40-60% in dspp_metadataParameters)
**Impact**: Need to ensure S7 class compatibility

### 2025-10-16: Maintain emuR Fidelity
**Decision**: 100% fidelity with emuR is mandatory
**Rationale**: Users must be able to trust results
**Impact**: Extensive testing required for all changes
