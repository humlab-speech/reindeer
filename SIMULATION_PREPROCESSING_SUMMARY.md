# Simulation Preprocessing Implementation Summary

**Date**: 2025-10-20
**Feature**: Media preprocessing support in simulation infrastructure

## Overview

The simulation system (`R/reindeer_simulation.R`) has been extended to support preprocessing of media files before DSP analysis. This enables systematic exploration of how media transformations (sample rate, codec, format, etc.) affect DSP results.

## Key Changes

### 1. New Parameters

Both `quantify_simulate()` and `enrich_simulate()` now accept:

- **`.prep_function`**: A preprocessing function to apply to media files before DSP
  Example: `superassp::prep_recode()`

- **`.prep_simulate`**: Named list specifying preprocessing parameter grid
  Example: `list(sample_rate = c(16000, 22050, 44100), format = "wav")`

### 2. Parameter Grid Expansion

The `create_parameter_grid()` function now:
- Accepts both DSP and prep parameter specifications
- Computes full outer product: `n_dsp_combinations × n_prep_combinations`
- Stores metadata distinguishing DSP vs prep parameters
- Generates unique hash for each DSP+prep combination

### 3. Database Schema Updates

#### simulation_metadata table
```sql
prep_function TEXT,              -- Preprocessing function name (optional)
prep_parameter_names TEXT        -- JSON array of prep parameter names
```

#### parameter_combinations table
```sql
prep_params_json TEXT            -- JSON of prep parameters (optional)
```

### 4. Execution Flow

#### quantify_simulate():
1. Create parameter grid from `.simulate` and `.prep_simulate`
2. For each combination:
   - If `.prep_function` provided:
     - Create wrapper that applies prep → DSP
     - Pass to `quantify()`
   - Else: Call `quantify()` with DSP function directly
3. Cache results with both DSP and prep params

#### enrich_simulate():
1. Create parameter grid from `.simulate` and `.prep_simulate`
2. For each file and parameter combination:
   - If `.prep_function` provided:
     - Apply prep function with prep params
     - Pass result to DSP function
   - Else: Pass file path to DSP function directly
3. Store track results with both param sets

### 5. Display Updates

Print methods for `simulation_results` and `simulation_tracks` now:
- Show prep function name if used
- Display DSP and prep parameters separately when both exist
- Calculate and show total combination count

## Usage Examples

### Example 1: Formant Analysis with Sample Rate Variation

```r
library(reindeer)
library(superassp)

corp <- corpus("path/to/db_emuDB")
segments <- ask_for(corp, "Phonetic == a")

results <- quantify_simulate(
  segments,
  .using = superassp::forest,
  .simulate = list(
    nominalF1 = seq(500, 900, 100)  # 5 values
  ),
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(
    sample_rate = c(16000, 22050, 44100),  # 3 values
    format = "wav"
  ),
  .simulation_store = "simulations/formants"
)

# Total combinations: 5 DSP × 3 prep = 15 per segment
print(results)
```

### Example 2: Pitch Tracking with Codec Comparison

```r
track_sim <- enrich_simulate(
  corp,
  .using = superassp::ksvF0,
  .simulate = list(
    minF = c(50, 75, 100),      # 3 values
    maxF = c(300, 400, 500)     # 3 values
  ),
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(
    format = c("wav", "flac", "mp3"),  # 3 values
    bit_rate = c(128000, 320000)       # 2 values
  ),
  .simulation_store = "simulations/pitch"
)

# Total combinations: 3 × 3 × 3 × 2 = 54 per file
```

### Example 3: Preprocessing Only

```r
# Test sample rate effects with fixed DSP params
results <- quantify_simulate(
  segments,
  .using = superassp::forest,
  nominalF1 = 700,  # Fixed
  maxFormantHz = 5500,  # Fixed
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(
    sample_rate = c(8000, 16000, 22050, 32000, 44100)
  ),
  .simulation_store = "simulations/samplerate_test"
)

# 5 combinations (prep params only)
```

### Example 4: DSP Parameters Only (Traditional)

```r
# No preprocessing, just DSP parameter exploration
results <- quantify_simulate(
  segments,
  .using = superassp::forest,
  .simulate = list(
    nominalF1 = seq(400, 1000, 50),  # 13 values
    windowShift = c(5, 10)            # 2 values
  ),
  .simulation_store = "simulations/traditional"
)

# 13 × 2 = 26 combinations (DSP only)
```

## Preprocessing Function Requirements

A preprocessing function for use with `.prep_function` must:

1. **Accept `listOfFiles` parameter**: Path(s) to media file(s)
2. **Accept preprocessing parameters**: As named arguments
3. **Return audio data**: Compatible with DSP function input

Example compatible functions:
- `superassp::prep_recode()` - Sample rate, format, codec conversion
- Custom functions following the same pattern

### superassp::prep_recode() parameters

```r
prep_recode(
  listOfFiles,
  format,              # "wav", "mp3", "flac", etc.
  sample_rate = NULL,  # Target sample rate (Hz)
  bit_rate = NULL,     # For lossy codecs (bits/sec)
  start_time = NULL,   # Window start (seconds)
  end_time = NULL,     # Window end (seconds)
  channels = NULL,     # 1 (mono), 2 (stereo), NULL (keep)
  ...
)
```

Returns: Integer vector with audio samples (s32le format) with attributes:
- `channels`: Number of channels
- `sample_rate`: Sample rate in Hz

## Cache Structure

Simulation caches now store:

**Metadata**:
- DSP function name
- Prep function name (if used)
- DSP parameter names (JSON array)
- Prep parameter names (JSON array)
- Number of combinations
- Computation time

**Parameter Combinations**:
- Unique hash (MD5 of DSP + prep params)
- DSP params (JSON)
- Prep params (JSON)

**Results**:
- Linked to parameter combination ID
- Serialized result objects
- Signal file hashes for reproducibility

## Retrieval

Existing retrieval functions work with preprocessing:

```r
# Retrieve specific parameter combination
result <- reminisce(
  segments,
  parameters = list(
    nominalF1 = 700,         # DSP param
    sample_rate = 22050      # Prep param
  ),
  cache_path = "simulations/formants_20251020_143022.sqlite"
)
```

The hash-based lookup handles both DSP and prep parameters automatically.

## Testing

To test the preprocessing functionality:

1. Install superassp with prep_recode support:
   ```r
   remotes::install_github("humlab-speech/superassp", ref = "cpp_optimization")
   ```

2. Run a simple test:
   ```r
   library(reindeer)
   library(superassp)

   corp <- corpus("path/to/test_db_emuDB")
   segs <- ask_for(corp, "[#Phonetic -> Phonetic]") %>% head(5)

   test_sim <- quantify_simulate(
     segs,
     .using = forest,
     .simulate = list(nominalF1 = c(600, 700)),
     .prep_function = prep_recode,
     .prep_simulate = list(sample_rate = c(16000, 22050)),
     .simulation_store = tempdir(),
     .verbose = TRUE
   )

   print(test_sim)  # Should show 2 × 2 = 4 combinations
   ```

## Backward Compatibility

All changes are backward compatible:
- `.prep_function` and `.prep_simulate` are optional
- Omitting them results in traditional DSP-only simulation
- Existing simulation code continues to work unchanged

## Performance Considerations

Each parameter combination requires:
1. Media file read
2. Preprocessing (if `.prep_function` provided)
3. DSP analysis
4. Result serialization and storage

For `n_segments × n_dsp_params × n_prep_params` combinations, consider:
- Using parallel processing (`.parallel = TRUE`)
- Starting with small parameter grids
- Monitoring disk space for cache files
- Using `.verbose = TRUE` to track progress

## Future Enhancements

Potential additions:
1. **Chained preprocessing**: Multiple prep functions in sequence
2. **Conditional preprocessing**: Apply prep based on metadata/file properties
3. **Prep result caching**: Cache preprocessed audio to avoid recomputation
4. **Comparison utilities**: Built-in functions to compare prep parameter effects
5. **Visualization**: Plot DSP results across prep parameter dimensions

## Documentation

Updated files:
- `R/reindeer_simulation.R`: Implementation with comprehensive examples
- `CLAUDE.md`: Quick reference for future development
- `SIMULATION_PREPROCESSING_SUMMARY.md`: This document

## See Also

- Simulation infrastructure: `R/reindeer_simulation.R`
- superassp prep_recode: https://github.com/humlab-speech/superassp
- Original simulation system: `SIMULATION_INFRASTRUCTURE_SUMMARY.md`
