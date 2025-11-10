# Query and DSP System Harmonization Plan

## Overview
Major refactoring to harmonize the query system, eliminate wrassp dependency, 
and create a consistent DSP/metadata integration framework.

## Tasks

### 1. Query System Harmonization
- [x] Rename `query_opt()` to `ask_for()` 
- [ ] Create `query()` as alias to `ask_for()`
- [ ] Update all tests to use new naming
- [ ] Update benchmarks to use new naming
- [ ] Create `segment_list` S7 class (subclass of tibble)
- [ ] Implement `quantify()` method for `segment_list`

### 2. Media Import Modernization  
- [ ] Replace wrassp with av package in `import_recordings()`
- [ ] Rename to `import_media_to_bundle()`
- [ ] Use mediafileExtension from config
- [ ] Implement channel_map argument
- [ ] Add conversion warnings

### 3. Corpus Inspection Functions
- [ ] Create `peek_signals()` function
- [ ] Create `peek_at(corpus, "signals")` function  
- [ ] Make independent of emuR
- [ ] Optimize for performance

### 4. DSP Function Harmonization
- [ ] Rename `furnish()` to `enrich()`
- [ ] Implement `.using` parameter
- [ ] Add metadata-driven parameter resolution
- [ ] Support Gender/Age specific defaults
- [ ] Support bundle-level metadata override

### 5. Class Structure
- [ ] segment_list S7 class
- [ ] bundle_list S7 class (from corpus subsetting)
- [ ] Proper inheritance from tibble
- [ ] Required columns validation

## Dependencies to Remove
- wrassp (replace with av + seewave/tuneR where needed)

## Dependencies to Add  
- av (for media file handling)
- seewave (for audio signal processing if needed)

## Testing Strategy
- Ensure emuR::query equivalence
- Validate media import/export
- Check metadata inheritance
- Verify DSP parameter resolution
