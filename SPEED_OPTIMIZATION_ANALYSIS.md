# Speed Optimization Analysis

## Analysis Date
2025-10-15

## Recent Changes Analyzed
- Commit 479074d: Harmonize query system and implement enrich/quantify for segment_list
- Commit 7b1a6cb: Update benchmark results with query optimization and MOMEL/INTSINT
- Commit 42c062d: Harmonize query system: rename query_opt to ask_for with query alias

## Identified Performance Bottlenecks

### 1. **reindeer_query_optimized.r**

#### Issue 1.1: Repeated Database Connections
**Location**: `ask_for()` function (lines 50-91)
**Problem**: Opens and validates database path on every query
**Impact**: Moderate - adds overhead for repeated queries
**Solution**: Cache database connections

#### Issue 1.2: String Operations in Parser
**Location**: `parse_eql_query()` and related functions (lines 130-285)
**Problem**: Uses character-by-character iteration in `split_on_operator()`
**Impact**: Low-Moderate for complex nested queries
**Solution**: Pre-compile regex patterns, use vectorized operations

#### Issue 1.3: Result Formatting
**Location**: `format_as_emuRsegs()` (called in execute_query)
**Problem**: Potentially repeated type conversions
**Impact**: Low for small results, Moderate for large results
**Solution**: Direct construction with correct types

### 2. **reindeer_enrich.R**

#### Issue 2.1: Sequential Processing
**Location**: Lines 103-140
**Problem**: Loops through bundles sequentially (for loop)
**Impact**: **HIGH** - No parallelization
**Solution**: Use future/furrr for parallel processing

#### Issue 2.2: Repeated Metadata Lookups
**Location**: Lines 107-111
**Problem**: Filters bundle_metadata for each bundle individually
**Impact**: Moderate - O(n*m) filtering operations
**Solution**: Join operations or hash table lookup

#### Issue 2.3: Parameter Derivation Overhead
**Location**: `derive_dsp_parameters()` (lines 156-213)
**Problem**: Called for every bundle, recalculates same values
**Impact**: Low-Moderate
**Solution**: Cache parameter derivations by metadata profile

### 3. **reindeer_segment_list.R**

#### Issue 3.1: Sequential Segment Processing  
**Location**: Lines 228-276 in `quantify()` method
**Problem**: Processes segments one by one in a for loop
**Impact**: **HIGH** - DSP operations are computationally expensive
**Solution**: Batch processing and parallel execution

#### Issue 3.2: Repeated File Path Construction
**Location**: Lines 248-252
**Problem**: Constructs file paths individually for each segment
**Impact**: Low
**Solution**: Pre-compute all paths

#### Issue 3.3: Metadata Lookups in Loop
**Location**: Lines 234-245
**Problem**: Filters metadata for each segment
**Impact**: Moderate
**Solution**: Pre-join metadata before loop

## Proposed Optimizations

### High Priority (Large Performance Gains)

1. **Parallelize enrich() function**
   - Use `furrr::future_map()` for bundle processing
   - Expected speedup: 2-8x (depending on cores)

2. **Parallelize quantify() method**
   - Use `furrr::future_map()` for segment processing  
   - Expected speedup: 2-8x (depending on cores)

3. **Batch DSP Operations**
   - Group segments by bundle/session for batch processing
   - Expected speedup: 1.5-2x

### Medium Priority (Moderate Gains)

4. **Database Connection Pooling**
   - Cache connections in ask_for()
   - Expected speedup: 1.1-1.3x for repeated queries

5. **Optimize Metadata Lookups**
   - Use hash tables or pre-joins
   - Expected speedup: 1.2-1.5x for many bundles

6. **Pre-compile Regex Patterns**
   - Store compiled patterns as package data
   - Expected speedup: 1.1-1.2x for complex queries

### Low Priority (Small Gains)

7. **Vectorize String Operations**
   - Replace character-by-character loops
   - Expected speedup: 1.05-1.1x

8. **Optimize Type Conversions**
   - Construct results with correct types from start
   - Expected speedup: 1.05-1.1x

## Implementation Plan

### Phase 1: Critical Parallelization (Immediate)
- Implement parallel processing in enrich()
- Implement parallel processing in quantify()
- Add progress bars for user feedback
- Test thread safety with reticulate/Python calls

### Phase 2: Data Structure Optimization
- Implement connection pooling
- Optimize metadata lookups with joins/hash tables
- Pre-compute file paths and metadata

### Phase 3: Micro-optimizations
- Compile regex patterns
- Vectorize string operations
- Optimize type conversions

## Benchmarking Strategy

For each optimization:
1. Create micro-benchmark comparing old vs new
2. Test on small (7 bundles), medium (70 bundles), large (700 bundles) datasets
3. Measure memory usage alongside speed
4. Verify correctness of results

## Thread Safety Considerations

**Critical**: When implementing parallelization:
- Test reticulate Python calls in parallel (for MOMEL/INTSINT)
- Test superassp DSP functions in parallel
- Ensure SQLite connections are thread-safe (use separate connections per worker)
- Check for race conditions in file I/O

## Expected Overall Improvements

Conservative estimates for typical workflows:

| Operation | Current | After Phase 1 | After Phase 2 | After Phase 3 |
|-----------|---------|---------------|---------------|---------------|
| enrich() 100 bundles | 100s | 20-30s | 15-25s | 14-23s |
| quantify() 1000 segs | 200s | 40-60s | 30-50s | 28-47s |
| ask_for() repeated | 1.0s | 1.0s | 0.75s | 0.70s |

**Total expected improvement: 3-5x for typical workflows**
