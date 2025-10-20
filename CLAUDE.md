# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**reindeer** is an R package that extends [emuR](https://github.com/IPS-LMU/emuR) for working with speech data in a nordic climate. It provides optimized workflows for:

- Metadata management with SQLite caching
- Speech signal processing with age/gender-appropriate parameters
- Annotation and transcription systems
- Query optimization using direct SQLite access
- Track data quantification and enrichment

The package uses modern S7 classes and data.table for performance, with extensive caching to handle computationally intensive operations on large speech corpora.

## Development Commands

### Building and Testing

```bash
# Install package with dependencies
Rscript -e "devtools::install(dependencies = TRUE)"

# Load for development
Rscript -e "devtools::load_all()"

# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test_query_optimized.R')"

# Check package (R CMD check)
Rscript -e "devtools::check()"

# Build documentation
Rscript -e "devtools::document()"
```

### Benchmarking

```bash
# Run benchmarks from benchmarking/ directory
Rscript benchmarking/benchmark_dspp.R
Rscript benchmarking/benchmark_serialization.R

# Verify implementation correctness
Rscript verify_implementation.R
```

### Building Vignettes

```bash
# Render a specific vignette
Rscript render_vignette.R

# Build all vignettes
Rscript -e "devtools::build_vignettes()"
```

## Architecture Overview

### Core S7 Classes

The package uses S7 object system for type safety and performance:

- **`corpus`** (`R/reindeer-corpus.R`): Main class representing an Emu database with persistent SQLite connection
  - Properties: `dbName`, `basePath`, `config`, `.uuid`, `.connection`
  - Constructor handles both paths and emuDBhandle objects
  - Automatically builds/updates SQLite cache on creation
  - Supports bracket notation for metadata access: `corpus["Session","Bundle"]`

- **`segment_list`** (`R/reindeer_segment_list.R`): Query results containing time-aligned segments
  - Subclass of tibble with required columns: session, bundle, start, end, label
  - Created by `ask_for()` and `query()` functions

- **`extended_segment_list`**: segment_list enriched with metadata and/or signal tracks

- **`lazy_segment_list`** (`R/reindeer_lazy_segment_list.R`): Delayed evaluation wrapper
  - Not yet fully integrated into main workflow
  - Future optimization for query chains

### Data Flow Architecture

```
User Query (EQL)
    ↓
ask_for() / query() [R/reindeer_query_optimized.r]
    ↓
SQLite Cache (_emuDBcache.sqlite) ← built by build_emuDB_cache()
    ↓
segment_list (S7 object)
    ↓
quantify() / enrich() [R/reindeeR_signalextensions_dt.R]
    ↓
Signal Processing + Persistent Cache
    ↓
extended_segment_list with tracks/metadata
```

### Metadata System

Three-level hierarchy with proper inheritance (R/reindeeR_metadata_optimized.R):

1. **Database level**: Defaults for all bundles (stored in `<dbname>_DBconfig.json`)
2. **Session level**: Overrides for session (stored in `<session>.meta_json`)
3. **Bundle level**: Bundle-specific values (stored in `<bundle>.meta_json`)

Key functions:
- `gather_metadata(corpus)`: Scan .meta_json files and populate SQLite cache
- `get_metadata(corpus)`: Retrieve with inheritance resolved
- `add_metadata(corpus, list(...), session, bundle)`: Set metadata programmatically
- `export_metadata(corpus, "file.xlsx")` / `import_metadata(corpus, "file.xlsx")`: Batch editing
- `biographize(segments, corpus)`: Enrich query results with metadata

**Ground truth**: Always the `.meta_json` files. SQLite cache is for performance only.

### Query System

Two implementations:
1. **Standard emuR**: Via `emuR::query()` (slower, more compatible)
2. **Optimized**: Via `ask_for()` / `query()` - direct SQLite queries (`R/reindeer_query_optimized.r`)

The optimized system:
- Parses EQL (EMU Query Language) directly
- Queries `_emuDBcache.sqlite` without emuR overhead
- Supports: ==, !=, =~, !~, sequences [A -> B], dominance [A ^ B], projection #Level
- Returns segment_list objects

Example:
```r
corpus("path/to/db_emuDB") -> corp
ask_for(corp, "Phonetic == t") -> segments
```

### Signal Processing & Caching

**Age/Gender-Specific Parameters** (`R/reindeeR_signalextensions_dt.R`):
- `dspp_metadataParameters_dt()`: Returns processing params based on Age/Gender
- Uses LOESS smoothing over empirical defaults from literature
- Optimized with data.table (3-4x faster than original dplyr version)

**Track Quantification**:
- `quantify(segments, corpus, tracks, ...)`: Extract signal measurements
- Uses persistent cache in SQLite to avoid recomputation
- Cache serialization currently uses base R `serialize()`
- **Optimization available**: qs package provides 3-4x speedup (see SERIALIZATION_QUICK_REF.md)

**Cache Management** (`R/cache_utils.R`):
- `.set_persistent_cache()` / `.get_persistent_cache()`: Internal cache functions
- Cache invalidation based on signal file modification time + processing parameters
- Uses digest/hash to identify unique parameter combinations

### Python Integration

The package uses Python (via reticulate) for some annotation tasks:

- **MOMEL/INTSINT** (`R/reindeer_annotate_momel.R`, `R/reindeeR_annotate_python.R`):
  - Pitch stylization and tonal annotation
  - Uses Parselmouth (Python Praat wrapper)
  - `draft_intsint_momel()`: Main implementation

### C++ Components

Two C++ files in `src/`:
- `corpus_config_helpers.cpp`: Fast database configuration operations
- `RcppExports.cpp`: Auto-generated Rcpp bindings

Functions are called from `R/RcppExports.R` and used in corpus configuration utilities.

## Key Design Patterns

### 1. Ground Truth vs Cache

- **Ground truth**: JSON files (`.meta_json`, `_DBconfig.json`, `_annot.json`)
- **Cache**: SQLite database (`_emuDBcache.sqlite`)
- Always trust JSON files; rebuild cache when in doubt: `gather_metadata(corpus)`

### 2. Optimized vs Legacy

Many functions have optimized versions:
- `reindeeR_metadata_optimized.R` vs `reindeeR_metadata.R` (deprecated)
- `reindeer_query_optimized.r` (`ask_for`) vs `emuR::query()`
- `reindeeR_signalextensions_dt.R` (data.table) vs `reindeeR_signalextensions.R` (dplyr, deprecated)
- `reindeer_transcription_system_optimized.R` vs `reindeer_transcription_system.R` (deprecated)

See DEPRECATED_FUNCTIONS.md for complete list of files marked for deletion.

### 3. Lazy Evaluation (Future)

`R/reindeer_lazy_segment_list.R` implements lazy evaluation for query chains but is not yet integrated into main workflow. This is a planned optimization.

### 4. Simulation System

`R/reindeer_simulation.R` provides infrastructure for systematic parameter space exploration:
- **`quantify_simulate()`**: Run DSP analysis with parameter grids on segments
- **`enrich_simulate()`**: Generate tracks with parameter grids across corpus
- **Preprocessing support**: Apply transformations to media before DSP (NEW!)
  - `.prep_function`: Function to transform media (e.g., `superassp::prep_recode`)
  - `.prep_simulate`: Parameter grid for preprocessing (e.g., sample rates, codecs)
  - Full outer product of DSP × prep parameters
- **Caching**: Results stored in SQLite for retrieval and analysis
- `list_simulations()`: Show available simulation caches
- `reminisce()` / `reminisce_tracks()`: Retrieve cached simulation results

**Example with preprocessing**:
```r
# Simulate formant analysis with different sample rates
quantify_simulate(
  segments,
  .using = superassp::forest,
  .simulate = list(nominalF1 = seq(500, 900, 100)),  # DSP params
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(sample_rate = c(16000, 22050, 44100)),  # Prep params
  .simulation_store = "simulations/formants"
)
# Creates 3 sample rates × 5 nominalF1 = 15 combinations per segment
```

## Testing

Test files in `tests/testthat/`:
- `test_query_optimized.R`: Query system tests
- `test_metadata_optimized.R`: Metadata system tests
- `test_quantify_segment_list.R`: Signal quantification tests
- `test-annotation-fidelity.R`: Annotation accuracy tests
- `test_reindeeR-metadata.R`: Legacy metadata tests

Tests use the `ae` demo database from emuR package.

## Important Conventions

### Function Naming
- `ask_for()` / `query()`: Query the database (returns segment_list)
- `quantify()`: Extract measurements from signal tracks
- `enrich()`: Add signal data to segments
- `biographize()`: Add metadata to segments
- `gather_*()`: Collect data from JSON files into cache
- `get_*()`: Retrieve data (usually from cache)
- `add_*()`: Set/update data (writes to JSON + updates cache)

### File Naming
- `reindeeR_*.R`: Core reindeer functionality
- `reindeer_*.R`: Extended/specialized functionality
- `*_optimized.R`: Performance-optimized implementations
- `*_dt.R`: data.table-based implementations
- `emuR_*.R`: Extensions/wrappers for emuR functions

### Object Properties
- Use `@` for S7 property access: `corpus@basePath`
- Use `$` for emuDBhandle attributes: `handle$basePath`

## Performance Considerations

### Current Optimizations
1. **SQLite caching**: Metadata, query results, signal processing cache
2. **data.table**: Used for large data operations (3-5x speedup vs dplyr)
3. **Persistent cache**: Avoid recomputing identical signal processing operations
4. **Lazy evaluation**: Prepared but not yet fully integrated

### Known Optimization Opportunities
1. **Cache serialization**: Switch from `serialize()` to `qs` package (3-4x faster, see SERIALIZATION_QUICK_REF.md)
2. **Lazy segment_list**: Integrate `lazy_segment_list` into `ask_for()` workflow
3. **Parallel processing**: `transcribe_parallel()` exists but not widely used

## Common Development Tasks

### Adding a new signal track

1. Define track in database config: `add_ssffTrackDefinition()`
2. Compute track: Use `superassp` or custom DSP function
3. Add track to corpus: Store SSFF files in bundle directories
4. Access in queries: `quantify(segments, corp, tracks = "newtrack")`

### Adding metadata fields

```r
# Set database-wide default
add_metadata(corp, list(Project = "MyStudy"))

# Set session-level
add_metadata(corp, list(Speaker = "P01"), session = "Session1")

# Set bundle-level
add_metadata(corp, list(Quality = "Good"), session = "Session1", bundle = "Bundle1")

# Always call gather_metadata() after manual .meta_json edits
gather_metadata(corp)
```

### Debugging cache issues

```r
# Rebuild metadata cache
gather_metadata(corp, verbose = TRUE)

# Check cache contents
con <- get_connection(corp)
DBI::dbGetQuery(con, "SELECT * FROM metadata_fields")
DBI::dbDisconnect(con)

# Clear all caches
clear_all_caches(corp)  # if defined in corpus_config.R
```

### Running specific benchmarks

```bash
# Compare implementations
Rscript benchmarking/benchmark_dspp.R

# Test serialization methods
Rscript benchmarking/benchmark_serialization.R
```

## Documentation Files

The repository contains extensive markdown documentation (these are implementation notes, not user-facing docs):

- **METADATA_SYSTEM.md**: Detailed metadata architecture
- **SERIALIZATION_QUICK_REF.md**: Cache optimization guide
- **DEPRECATED_FUNCTIONS.md**: Files/functions marked for deletion
- **CACHE_OPTIMIZATION_SUMMARY.md**: Cache implementation details
- Multiple `*_IMPLEMENTATION_SUMMARY.md` files documenting major features

These are developer notes, not user documentation. User docs are in:
- `vignettes/*.Rmd`: User-facing tutorials
- `man/*.Rd`: Function reference (auto-generated by roxygen2)

## Dependencies

Key dependencies:
- **emuR** (>= 2.0.2): Base EMU-SDMS functionality
- **S7**: Modern OOP system
- **data.table** (>= 1.14.0): High-performance data manipulation
- **reticulate**: Python integration (for Parselmouth/MOMEL)
- **superassp**: Advanced signal processing (from GitHub: humlab-speech/superassp)
- **qs** (>= 0.25.0): Fast serialization (in Imports as of recent optimization)
- **DBI, RSQLite**: SQLite database access
- **digest**: Hash computation for cache keys

## Working with Git

Current branch: `S7speedy` (feature branch for S7 optimization)
Main branch: `main`

Recent focus areas (based on commit history):
- Cache serialization optimization (qs package)
- Deprecating legacy code
- Workflow documentation
- Performance benchmarking

When committing, follow existing commit message style:
- `feat:` for new features
- `refactor:` for code restructuring
- `fix:` for bug fixes
- Be specific about the change and its impact

## Notes for Claude Code

1. **Always prefer optimized versions**: Use `*_optimized.R` and `*_dt.R` implementations
2. **Don't edit deprecated files**: See DEPRECATED_FUNCTIONS.md for files to avoid
3. **Maintain ground truth**: Always update JSON files, not just SQLite cache
4. **Test with ae database**: Use `reindeer:::create_ae_db()` for testing
5. **Check benchmarks**: Run relevant benchmarks after performance-related changes
6. **Update docs**: Roxygen comments for functions, vignettes for workflows
7. **S7 class properties**: Use `@` not `$` for S7 objects
8. **Cache invalidation**: Consider digest/hash when modifying cache-related code
