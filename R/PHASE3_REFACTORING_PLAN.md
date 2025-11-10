# Phase 3: Large File Refactoring Plan

**Date**: 2025-11-09
**Status**: Planning
**Goal**: Break files >1000 lines into logical, maintainable modules

## Target Files

| File | Lines | Target | Modules |
|------|-------|--------|---------|
| `reindeer-corpus.R` | 1,910 | 3 files | Class, Methods, Database |
| `reindeer_simulation.R` | 1,778 | 3 files | Core, Preprocessing, Cache |
| `reindeer_query_optimized.R` | 1,296 | 2 files | Parser, Executor |
| `reindeer_metadata_optimized.R` | 1,247 | 2 files | Core, Import/Export |

## File 1: reindeer-corpus.R (1,910 lines)

### Current Structure

Lines 1-138: S7 class definitions
- `corpus` class
- `bundle_list` class

Lines 140-255: Subsetting operators
- `[.corpus`
- `[<-.corpus`

Lines 256-581: Print/Display methods
- `print.corpus`
- `summary.corpus`
- `glimpse()`
- `print.bundle_list`

Lines 582-649: Utility functions
- `get_handle()`
- Connection management

Lines 650-1140: Metadata & Import functions
- `get_metadata_for_patterns()`
- `corpus_assign_metadata()`
- `corpus_import_media()`

Lines 1141-1262: Database cache building
- `build_emuDB_cache()`

Lines 1263-1777: Database schema & processing
- `initialize_database_schema()`
- `discover_sessions_bundles()`
- `process_bundles_batch()`
- `parse_annot_json()`

Lines 1778-1910: Metadata internals
- `gather_metadata_internal()`
- Serialization functions

### Proposed Split

**File 1: `R/corpus_class.R`** (~400 lines)
```r
# S7 Class Definitions
# - corpus class definition + constructor
# - bundle_list class definition
# - Validators
```

**File 2: `R/corpus_methods.R`** (~500 lines)
```r
# S7 Methods for corpus class
# - Subsetting: [, [<-
# - Display: print, summary, glimpse
# - Utilities: get_handle, get_connection
```

**File 3: `R/corpus_database.R`** (~600 lines)
```r
# Database Operations
# - build_emuDB_cache()
# - initialize_database_schema()
# - discover_sessions_bundles()
# - process_bundles_batch()
# - parse_annot_json()
# - insert_batch_results()
```

**File 4: `R/corpus_metadata_io.R`** (~400 lines)
```r
# Metadata I/O
# - get_metadata_for_patterns()
# - corpus_assign_metadata()
# - gather_metadata_internal()
# - Serialization functions
```

## File 2: reindeer_simulation.R (1,778 lines)

### Current Structure

Lines 1-700: Core simulation functions
- `quantify_simulate()`
- `enrich_simulate()`

Lines 700-1200: Preprocessing integration
- Wrapper functions
- Parameter expansion

Lines 1200-1778: Cache management
- `reminisce()`
- `list_simulations()`
- Cache size monitoring

### Proposed Split

**File 1: `R/simulation_core.R`** (~600 lines)
```r
# Core Simulation Logic
# - quantify_simulate()
# - enrich_simulate()
# - Parameter grid expansion
```

**File 2: `R/simulation_preprocessing.R`** (~600 lines)
```r
# Preprocessing Integration
# - Wrapper generation
# - Media transformation
# - Parameter coupling
```

**File 3: `R/simulation_cache.R`** (~600 lines)
```r
# Simulation Cache Management
# - reminisce()
# - list_simulations()
# - Cache operations
# - Size monitoring
```

## File 3: reindeer_query_optimized.R (1,296 lines)

### Proposed Split

**File 1: `R/query_parser.R`** (~650 lines)
```r
# EQL Query Parsing
# - parse_eql_query()
# - Token parsing
# - Syntax tree building
```

**File 2: `R/query_executor.R`** (~650 lines)
```r
# Query Execution
# - SQL generation
# - Result formatting
# - ask_for() wrapper
```

## File 4: reindeer_metadata_optimized.R (1,247 lines)

### Proposed Split

**File 1: `R/metadata_core.R`** (~600 lines)
```r
# Core Metadata Operations
# - get_metadata()
# - add_metadata()
# - Inheritance resolution
```

**File 2: `R/metadata_import_export.R`** (~650 lines)
```r
# Import/Export
# - import_metadata()
# - export_metadata()
# - Excel I/O
# - biographize()
```

## Implementation Strategy

### Phase 3A: reindeer-corpus.R (Week 1)

**Day 1-2: Preparation**
1. Create backup branch: `git checkout -b refactor-corpus-file`
2. Run full test suite: baseline
3. Document all internal dependencies

**Day 3: Extract class definitions**
```bash
# Create corpus_class.R
# Move S7 class definitions
# Test: devtools::load_all(), ensure classes work
```

**Day 4: Extract methods**
```bash
# Create corpus_methods.R
# Move S7 methods
# Test: Subsetting, printing work correctly
```

**Day 5: Extract database & metadata**
```bash
# Create corpus_database.R and corpus_metadata_io.R
# Move remaining functions
# Delete original reindeer-corpus.R
# Update DESCRIPTION Collate order
# Full test suite
```

### Phase 3B: reindeer_simulation.R (Week 2)

Similar process, 5 days

### Phase 3C: Query & Metadata (Week 3)

Split remaining large files

### Testing Strategy

**After each split:**
```r
# 1. Load package
devtools::load_all()

# 2. Run specific tests
devtools::test()

# 3. Check NAMESPACE
devtools::document()

# 4. Build package
devtools::build()

# 5. R CMD check
devtools::check()
```

**Integration tests:**
```r
test_that("corpus workflow after refactoring", {
  corp <- corpus("test_db_emuDB")
  bundles <- corp[".*", ".*"]
  expect_s3_class(bundles, "bundle_list")
  
  # Test all major operations
  handle <- get_handle(corp)
  expect_true(inherits(handle, "emuDBhandle"))
})
```

## Benefits

### Code Organization
- ✅ Each file has single, clear purpose
- ✅ Easier to navigate (files ~400-600 lines vs 1,900)
- ✅ Logical grouping of related functions
- ✅ Clearer dependencies

### Maintainability
- ✅ Easier to find specific functionality
- ✅ Smaller files = less merge conflicts
- ✅ Isolated changes = easier testing
- ✅ Better for onboarding new contributors

### Performance
- ⚠️ No change (all functions still loaded)
- ✅ Faster IDE operations (smaller files to parse)

## Risks & Mitigation

### Risk 1: Breaking Dependencies
**Mitigation**: 
- Thorough testing after each file split
- No changes to function signatures or exports
- Keep internal functions internal

### Risk 2: Collate Order Issues
**Mitigation**:
- Let roxygen2 manage Collate order
- Use `@include` directives if needed
- Test `devtools::load_all()` frequently

### Risk 3: Lost Git History
**Mitigation**:
- Use feature branch
- Commit frequently
- Document what came from where

## Success Criteria

✅ All files <1000 lines
✅ All tests passing
✅ Package builds without errors
✅ NAMESPACE unchanged
✅ Documentation complete
✅ Git history readable

## Rollback Plan

If issues discovered:
```bash
# Option 1: Revert commits
git revert <commit-range>

# Option 2: Abandon branch
git checkout main
git branch -D refactor-corpus-file
```

## After Refactoring

Update documentation:
- CLAUDE.md - Update file structure section
- README.md - Update if file structure mentioned
- Add note about refactoring in NEWS.md

## Timeline

- Week 1: reindeer-corpus.R → 4 files
- Week 2: reindeer_simulation.R → 3 files  
- Week 3: Query & Metadata → 4 files
- Week 4: Testing, documentation, review

**Total**: 11 large files → ~15 focused modules
**Impact**: Better code organization, easier maintenance
**Risk**: Low (no API changes, comprehensive testing)
