# reindeer <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/humlab-speech/reindeer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/humlab-speech/reindeer/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/humlab-speech/reindeer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/humlab-speech/reindeer?branch=main)
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

An R package for working with speech data in a nordic climate. **reindeer** extends the capabilities of [emuR](https://github.com/IPS-LMU/emuR) with optimized workflows for metadata management, signal processing, and advanced querying.

## Features

- 🗄️ **Optimized Metadata Management**: SQLite-backed caching with 150x speedup over standard emuR
- 🔍 **Advanced Query System**: Direct SQLite queries for faster segment retrieval
- 📊 **Signal Processing**: Age/gender-specific DSP parameters with data.table optimization
- 🧪 **Simulation Infrastructure**: Systematic parameter space exploration with preprocessing support
- 🔗 **S7 Object System**: Modern, type-safe corpus and segment_list classes
- ⚡ **Performance**: 3-4x faster operations through qs serialization and vectorization

## Installation

```r
# Install from GitHub
remotes::install_github("humlab-speech/reindeer")

# With superassp for signal processing
remotes::install_github("humlab-speech/superassp", ref = "cpp_optimization")
```

## Quick Start

```r
library(reindeer)

# Load corpus with automatic caching
corp <- corpus("path/to/database_emuDB")

# Query segments with optimized SQLite backend
segments <- ask_for(corp, "Phonetic == t")

# Quantify with metadata-driven DSP parameters
results <- quantify(segments, superassp::forest)

# Add metadata to results
results_meta <- biographize(results, corp)
```

## Core Workflows

### 1. Metadata Management

```r
# Gather metadata from .meta_json files into SQLite cache
gather_metadata(corp)

# Retrieve with inheritance (bundle → session → database)
metadata <- get_metadata(corp)

# Set metadata programmatically
add_metadata(corp, list(Age = 25, Gender = "Male"),
             session = "S1", bundle = "B1")

# Export/import via Excel for batch editing
export_metadata(corp, "metadata.xlsx")
import_metadata(corp, "metadata.xlsx")
```

### 2. Signal Processing Simulation

```r
# Simulate with both DSP and preprocessing parameters
results <- quantify_simulate(
  segments,
  .using = superassp::forest,
  .simulate = list(
    nominalF1 = seq(500, 900, 100)  # DSP parameters
  ),
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(
    sample_rate = c(16000, 22050, 44100),  # Preprocessing
    format = "wav"
  ),
  .simulation_store = "simulations/formants"
)

# Creates 5 × 3 = 15 combinations per segment
print(results)
```

### 3. Track Enrichment

```r
# Add formant tracks to corpus
corp %>% enrich(.using = superassp::forest)

# Simulate track generation with parameter grids
track_sim <- enrich_simulate(
  corp,
  .using = superassp::ksvF0,
  .simulate = list(minF = c(50, 75, 100)),
  .simulation_store = "simulations/pitch"
)
```

## Performance Benchmarks

Compared to standard emuR workflows:

| Operation | emuR | reindeer | Speedup |
|-----------|------|----------|---------|
| Metadata retrieval (1000 bundles) | 12s | 0.08s | **150x** |
| Query execution | Standard | Optimized | **2-5x** |
| Cache serialization | base | qs | **3-4x** |
| DSP parameter computation | dplyr | data.table | **3-5x** |

## Documentation

- [Package website](https://humlab-speech.github.io/reindeer/)
- [Tidy Speech Processing](https://humlab-speech.github.io/reindeer/articles/Tidy_speech_processing.html)
- [Metadata Management](https://humlab-speech.github.io/reindeer/articles/metadata_management.html)
- [Simulation Infrastructure](https://humlab-speech.github.io/reindeer/articles/simulation-infrastructure.html)

## Key Components

### S7 Classes

- **`corpus`**: Main class with persistent SQLite connection
- **`segment_list`**: Query results with time-aligned segments
- **`extended_segment_list`**: Segments enriched with metadata/tracks
- **`lazy_segment_list`**: Delayed evaluation (planned optimization)

### Optimized Functions

- `ask_for()` / `query()`: Direct SQLite EQL queries
- `quantify()`: DSP analysis with metadata-driven parameters
- `enrich()`: Corpus-wide track generation
- `gather_metadata()`: Efficient metadata caching
- `biographize()`: Add metadata to query results

### Simulation System

- `quantify_simulate()`: Parameter grid exploration on segments
- `enrich_simulate()`: Track generation parameter exploration
- `prep_passthrough()`: Test utility for preprocessing (no transformation)
- `reminisce()` / `reminisce_tracks()`: Retrieve cached simulations

## Testing

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test_simulation_preprocessing.R")

# Check package
devtools::check()
```

## Development

Built with modern R best practices:
- S7 object system for type safety
- data.table for performance
- SQLite for caching
- GitHub Actions CI/CD
- pkgdown documentation
- Test coverage with codecov

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure R CMD check passes
5. Submit a pull request

## Citation

If you use reindeer in your research, please cite:

```
Karlsson, F. (2025). reindeer: Enhanced EMU-SDMS for Speech Research.
R package version 0.1.17. https://github.com/humlab-speech/reindeer
```

## License

GPL (>= 2)

## See Also

- [emuR](https://github.com/IPS-LMU/emuR) - EMU Speech Database Management System
- [superassp](https://github.com/humlab-speech/superassp) - Advanced Signal Processing
- [EMU-SDMS Manual](https://ips-lmu.github.io/The-EMU-SDMS-Manual/)
