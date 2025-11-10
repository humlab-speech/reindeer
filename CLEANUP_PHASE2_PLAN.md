# Phase 2: Naming Convention Standardization Plan

**Date**: 2025-11-09
**Status**: Planning
**Depends on**: Phase 1 (Completed)

## Overview

Standardize all naming conventions to snake_case for improved consistency and readability.

## Current Inconsistencies

### File Names

**Mixed Conventions**:
```
reindeeR_metadata_optimized.R      # camelCase with capital R
reindeer_segment_list.R            # snake_case (correct)
reindeeR_signalextensions_dt.R     # camelCase + abbreviation
reindeeR_database.R                # camelCase
```

### Function Names

**Mostly Consistent** (snake_case):
- ✅ `ask_for()`
- ✅ `quantify()`
- ✅ `get_metadata()`
- ✅ `add_metadata()`

**Some Exceptions**:
- `dspp_metadataParameters_dt()` - mixed case in middle

### Parameter Names

**Inconsistent**:
```r
# Function 1
function(listOfFiles, beginTime, endTime, ...)  # camelCase

# Function 2  
function(audio_files, session_names, ...)      # snake_case

# Function 3
function(path, verbose, ...)                   # lowercase
```

## Proposed Standard: snake_case

**Rationale**:
1. Matches tidyverse style guide
2. Most of codebase already uses it
3. Easier to read: `get_metadata` vs `getMetadata`
4. Consistent with R community best practices

## Refactoring Plan

### Phase 2A: File Names (Week 1)

**Rename files** (9 files):

| Old Name | New Name | Risk |
|----------|----------|------|
| `reindeeR_metadata_optimized.R` | `reindeer_metadata_optimized.R` | Low |
| `reindeeR_database.R` | `reindeer_database.R` | Low |
| `reindeeR_annotate_python.R` | `reindeer_annotate_python.R` | Low |
| `reindeeR_signalextensions_dt.R` | `reindeer_signal_extensions_dt.R` | Low |
| `reindeeR_cmdi.R` | `reindeer_cmdi.R` | Low |
| `reindeeR_autosync.R` | `reindeer_autosync.R` | Low |
| `reindeeR_autosync_wrappers.R` | `reindeer_autosync_wrappers.R` | Low |
| `reindeer_query_optimized.r` | `reindeer_query_optimized.R` | Low (capitalize) |
| `emuR_develoment_utils.R` | `emur_development_utils.R` | Low (fix typo) |

**Git commands**:
```bash
git mv reindeeR_metadata_optimized.R reindeer_metadata_optimized.R
git mv reindeeR_database.R reindeer_database.R
# ... etc
```

### Phase 2B: Function Parameters (Week 2)

**Standardize parameter names** across all functions:

#### High-Impact Changes (Many Functions)

| Current | Standard | Occurrences |
|---------|----------|-------------|
| `listOfFiles` | `audio_files` or `file_paths` | ~15 functions |
| `beginTime` | `begin_time` | ~20 functions |
| `endTime` | `end_time` | ~20 functions |
| `explicitExt` | `file_extension` | ~10 functions |
| `outputDirectory` | `output_dir` | ~15 functions |
| `toFile` | `write_to_file` | ~10 functions |

#### Implementation Strategy

**Option 1: Gradual Deprecation** (Recommended)
```r
# Keep old parameter names but deprecated
function(audio_files = NULL, 
         listOfFiles = deprecated(),  # Old name
         begin_time = 0.0,
         beginTime = deprecated(),    # Old name
         ...) {
  
  # Handle deprecation
  if (lifecycle::is_present(listOfFiles)) {
    lifecycle::deprecate_warn(
      "0.8.0", 
      "function(listOfFiles)", 
      "function(audio_files)"
    )
    audio_files <- listOfFiles
  }
  
  if (lifecycle::is_present(beginTime)) {
    lifecycle::deprecate_warn(
      "0.8.0",
      "function(beginTime)",
      "function(begin_time)"
    )
    begin_time <- beginTime
  }
  
  # Function body uses new names
  # ...
}
```

**Option 2: Breaking Change with Version Bump**
```r
# Simply rename parameters
# Document in NEWS.md as breaking change
# Bump version to 0.8.0 (minor version)
```

### Phase 2C: Internal Function Names (Week 2-3)

**Focus on**:
- Helper functions that mix conventions
- Example: `dspp_metadataParameters_dt()` → `dspp_metadata_parameters_dt()`

**Low Priority** (these are internal, non-exported):
- Don't need immediate changes
- Can be addressed opportunistically

## Testing Strategy

### Automated Tests

1. **Name consistency checker**:
```r
test_that("file names follow snake_case", {
  r_files <- list.files("R", pattern = "\\.R$", full.names = FALSE)
  
  # Check for camelCase
  has_camel <- grepl("[a-z][A-Z]", r_files)
  
  expect_false(
    any(has_camel),
    info = paste("Files with camelCase:", paste(r_files[has_camel], collapse = ", "))
  )
})

test_that("exported functions use snake_case", {
  exports <- getNamespaceExports("reindeer")
  
  # Allow certain exceptions like S7 classes
  exceptions <- c("corpus", "segment_list", "bundle_list")
  exports <- setdiff(exports, exceptions)
  
  has_camel <- grepl("[a-z][A-Z]", exports)
  
  expect_false(
    any(has_camel),
    info = paste("Functions with camelCase:", paste(exports[has_camel], collapse = ", "))
  )
})
```

2. **Backward compatibility tests**:
```r
test_that("deprecated parameters still work with warnings", {
  expect_warning(
    result <- some_function(listOfFiles = "test.wav"),
    "listOfFiles.*deprecated.*audio_files"
  )
  
  # Result should be correct despite using old name
  expect_equal(result, expected_result)
})
```

### Manual Testing

Test on real workflows:
```r
# Load test database
corp <- corpus("test_db_emuDB")

# Test old parameter names (should warn)
segments <- ask_for(corp, "Phonetic == t")
result <- quantify(segments, corp, tracks = "f0")

# Verify everything still works
expect_s3_class(result, "extended_segment_list")
```

## Documentation Updates

### Files to Update

1. **CLAUDE.md**:
   - Update all example code to use new names
   - Add note about naming convention standard

2. **Function documentation**:
   - Update `@param` tags with new names
   - Add `@param oldName [Deprecated]` for old parameters

3. **NEWS.md**:
```markdown
# reindeer 0.8.0

## Breaking Changes (if using Option 2)

* Function parameters standardized to snake_case (#XX)
  - `listOfFiles` → `audio_files`
  - `beginTime` → `begin_time`
  - `endTime` → `end_time`
  - See migration guide below

## Deprecations (if using Option 1)

* Old parameter names deprecated but still functional with warnings
* Will be removed in v0.9.0
* Update your code to use new names

## Migration Guide

**Old code**:
```r
result <- my_function(
  listOfFiles = files,
  beginTime = 0.0,
  endTime = 5.0
)
```

**New code**:
```r
result <- my_function(
  audio_files = files,
  begin_time = 0.0,
  end_time = 5.0
)
```
```

4. **README.md**:
   - Update all example code

5. **Vignettes**:
   - Update all tutorial code

## Risk Assessment

| Change Type | Risk Level | Mitigation |
|-------------|------------|------------|
| File renaming | **Low** | Git tracks renames; no API change |
| Exported function params | **Medium-High** | Use deprecation warnings (Option 1) |
| Internal function changes | **Low** | Not user-facing |
| Documentation updates | **Low** | Verify with spell checker |

## Implementation Timeline

**Week 1**: File renaming
- Day 1-2: Rename files, update imports
- Day 3: Run full test suite
- Day 4: Update documentation references
- Day 5: Review and commit

**Week 2**: Parameter standardization (high-impact)
- Day 1-2: Implement deprecation warnings
- Day 3-4: Update all function signatures
- Day 5: Test backward compatibility

**Week 3**: Documentation and final testing
- Day 1-2: Update all documentation
- Day 3-4: Update vignettes and examples
- Day 5: Final review and merge

## Success Criteria

✅ All file names use snake_case (or legitimate exceptions like RcppExports.R)
✅ All exported function parameters use snake_case
✅ All tests pass
✅ Backward compatibility maintained (if Option 1)
✅ Documentation updated
✅ No new warnings in R CMD check

## Rollback Plan

If critical issues discovered:
```bash
git revert <commit-hash>
# Or revert entire branch
git checkout main
git branch -D standardize-naming
```

## Next Steps After Phase 2

- Phase 3: Refactor large files
- Phase 4: Improve documentation
- Phase 5: Enhance testing

## References

- Tidyverse Style Guide: https://style.tidyverse.org/
- Advanced R: https://adv-r.hadley.nz/names-values.html
- lifecycle package: https://lifecycle.r-lib.org/
