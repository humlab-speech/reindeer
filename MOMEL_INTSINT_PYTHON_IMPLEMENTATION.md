# MOMEL-INTSINT Python/Parselmouth Implementation

## Overview

This document describes the complete reimplementation of the MOMEL-INTSINT intonation transcription system in Python using Parselmouth, replacing the previous implementation that relied on external Praat scripts, Perl scripts, and compiled C binaries.

## Background

The original implementation of MOMEL-INTSINT in reindeer involved:

1. **Praat scripts** (`processINTSINTMOMEL.praat`) - For F0 extraction and orchestration
2. **C binary** (`momel` executable) - For target point detection using quadratic spline modeling
3. **Perl script** (`intsint.pl`) - For INTSINT tone optimization
4. **File I/O** - Extensive writing and reading of intermediate files

This approach had several limitations:
- **Platform dependencies**: Required platform-specific compiled binaries
- **Installation complexity**: Users needed Praat, Perl, and compiled C executables
- **Maintenance burden**: Multiple languages and external dependencies
- **Performance**: Extensive file I/O operations
- **Debugging difficulty**: Errors could occur in any of the components

## New Implementation

### Architecture

The new implementation is a pure Python module (`inst/python/momel_intsint.py`) that:

1. **Uses Parselmouth** for all Praat functionality (F0 extraction)
2. **Implements MOMEL in Python** - Complete port of the C algorithm
3. **Implements INTSINT in Python** - Complete port of the Perl algorithm
4. **Operates in memory** - No intermediate file I/O
5. **Integrates with reticulate** - Seamless R-Python interoperability

### Key Components

#### momel_intsint.py

**Core Functions:**

- `extract_f0_parselmouth()` - Extract F0 using Parselmouth
- `eliminate_glitches()` - Remove F0 measurement artifacts
- `calc_regression()` - Quadratic regression for target detection
- `cible()` - Find pitch target candidates
- `reduc()` - Reduce and cluster targets
- `borne()` - Add boundary targets
- `momel()` - Complete MOMEL algorithm
- `optimize_intsint()` - Optimize INTSINT labels for given parameters
- `intsint()` - Complete INTSINT algorithm
- `process_momel_intsint()` - End-to-end processing pipeline

**Data Structures:**

- `Target` - Represents a MOMEL target point (time, frequency)
- `IntsintTarget` - Represents an INTSINT target (time, tone, observed, estimated)

#### R Interface (reindeer_annotate_momel.R)

**New Functions:**

- `draft_momel_intsint()` - Create INTSINT suggestions using Python implementation
- `annotate_momel_intsint()` - Complete annotation workflow with assessment

**Integration:**

- Returns `EventSuggestion` objects compatible with transcription system
- Supports all standard transcription workflow functions:
  - `assess()` - Validate suggestions
  - `prepare()` - Create required levels
  - `transcribe()` - Apply to database
  - `reverse()` - Roll back changes via TranscriptionLog

### Algorithm Fidelity

The Python implementation faithfully reproduces the original algorithms:

#### MOMEL (Target Detection)

1. **Windowed quadratic regression** - Find local F0 extrema
2. **Iterative outlier removal** - Remove points exceeding error threshold
3. **Parabola extremum detection** - Locate target points
4. **Clustering and reduction** - Group and filter targets
5. **Boundary extension** - Add edge targets

Original C parameters preserved:
- Window lengths
- Error thresholds  
- Frequency bounds
- Filtering parameters

#### INTSINT (Tone Assignment)

1. **Octave-scale conversion** - Work in log-frequency space
2. **Range optimization** - Find optimal pitch span (MIN_RANGE to MAX_RANGE)
3. **Key optimization** - Find optimal reference pitch (mid ± MEAN_SHIFT)
4. **Tone assignment** - Choose from {T, M, B, H, U, S, D, L}
5. **Error minimization** - Minimize sum-squared prediction error

Original Perl parameters preserved:
- Pause detection threshold
- Range bounds
- Optimization steps
- Tone estimation formulae

### Testing

#### Unit Tests (test-momel_intsint_python.R)

- **Basic functionality**: Octave conversion, glitch elimination
- **Edge cases**: Empty contours, extreme values
- **Data structures**: Target and IntsintTarget objects
- **Module availability**: Parselmouth installation checks

#### Fidelity Tests (test-momel_fidelity.R)

- **Praat comparison**: Compare with original implementation (if available)
- **Result validation**: Check tone labels, range, key
- **Parallel safety**: Verify thread-safe for parallel processing
- **Signal types**: Test on various audio signals

### Performance Characteristics

**Advantages:**

- **No file I/O**: All processing in memory
- **Single language**: Pure Python implementation
- **Parallelizable**: Thread-safe for bundle-level parallelization
- **Debuggable**: Python stack traces and inspection

**Considerations:**

- **First import**: Initial reticulate/Python startup overhead
- **Memory**: F0 contours kept in memory (minimal for typical use)
- **Python dependency**: Requires Parselmouth package

### Usage

#### Basic Usage

```r
# Load corpus
corp <- corpus("path/to/database_emuDB")

# Get bundles to process
bundles <- corp[".*", ".*"]

# Draft suggestions
suggestions <- draft_momel_intsint(
  corp, 
  bundles,
  windowSize = 30,
  minF = 60,
  maxF = 750,
  pitchSpan = 1.5
)

# Assess quality
assessment <- assess(suggestions)

# Prepare database (create level if needed)
prepare(corp, suggestions)

# Transcribe
log <- transcribe(corp, suggestions)

# Or use convenience function
log <- annotate_momel_intsint(corp, bundles)
```

#### Parameters

All original MOMEL-INTSINT parameters are supported:

- `windowSize` - Analysis window (default: 30 ms)
- `minF`, `maxF` - F0 search range (default: 60-750 Hz)
- `pitchSpan` - Pitch span in octaves (default: 1.5)
- `maximumError` - MOMEL error threshold (default: 1.04)
- `reducWindowSize` - Reduction window (default: 20 ms)
- `minimalDistance` - Target spacing (default: 20 ms)
- `minimalFrequencyRatio` - Frequency ratio threshold (default: 0.05)
- `time_step` - F0 extraction step (default: 0.01 s)

### Integration with Transcription System

The Python implementation fully integrates with the reindeer transcription system:

#### Suggestion Creation

- Returns `EventSuggestion` S7 objects
- Includes metadata (method, parameters, additional data)
- Supports label groups (Absolute_tones, Relative_tones)

#### Assessment

- Validates level existence and type
- Checks for timing conflicts
- Verifies label validity

#### Preparation

- Creates EVENT level if needed
- Sets legal labels and label groups
- Updates database configuration

#### Transcription

- Inserts EVENT items via annotation files
- Updates SQLite cache
- Creates TranscriptionLog for rollback

### Migration from Praat Implementation

For users of the old `annotate_INTSINT_MOMEL()` function:

**Old:**
```r
annotate_INTSINT_MOMEL(emuDBhandle, seglist)
```

**New:**
```r
corp <- corpus(emuDBhandle$basePath)
annotate_momel_intsint(corp, seglist)
```

The new implementation:
- Uses corpus object instead of emuDB handle
- Returns TranscriptionLog instead of tibble
- Integrates with transcription suggestion system
- Provides rollback capability

### Future Enhancements

Potential improvements:

1. **Caching**: Store F0 contours to avoid recomputation
2. **Batch processing**: Vectorized operations for multiple bundles
3. **Alternative pitch trackers**: Support for YIN, RAPT, etc.
4. **GPU acceleration**: NumPy operations on GPU
5. **Interactive refinement**: Manual target adjustment tools

### Dependencies

**Python:**
- `numpy` - Numerical operations
- `parselmouth` - Praat functionality
- `math` - Mathematical functions

**R:**
- `reticulate` - Python interface
- `cli` - Progress indicators
- `dplyr` - Data manipulation

### References

- Hirst, D., & Espesser, R. (1993). Automatic modelling of fundamental frequency using a quadratic spline function. *Travaux de l'Institut de Phonétique d'Aix*, 15, 75-85.

- Hirst, D. (2005). Form and function in the representation of speech prosody. *Speech Communication*, 46(3-4), 334-347.

- Hirst, D., De Looze, C., & Rilliard, A. (2017). Coding intonation in different languages: The Momel-Intsint approach. In *Chinese Lexical Semantics* (pp. 369-379). Springer.

### Maintenance Notes

**Code locations:**
- Python implementation: `inst/python/momel_intsint.py`
- R interface: `R/reindeer_annotate_momel.R`
- Tests: `tests/testthat/test-momel_intsint_python.R`, `test-momel_fidelity.R`

**Testing:**
```r
devtools::test(filter = "momel")
```

**Known issues:**
- None currently identified

**Version history:**
- v1.0 (2025-10-15): Initial Python implementation
