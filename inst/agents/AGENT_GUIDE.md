# Agent Guide: Reimplementing Praat Scripts with reindeer

This guide instructs coding agents on how to use the `reindeer` R package to
efficiently reimplement Praat-based speech analysis pipelines in R, producing
output faithful to the original Praat scripts.

## Package Architecture

reindeer extends [emuR](https://github.com/IPS-LMU/emuR) for speech corpus
management. The package handles corpus loading, metadata, querying, and signal
processing. Draft annotation generation (MOMEL/INTSINT, periods, VOT, VAD) lives
in the companion package [protoscribe](https://github.com/humlab-speech/protoscribe).

### Core Classes (S7)

| Class | File | Purpose |
|---|---|---|
| `corpus` | `R/corpus_class.R` | Database handle with SQLite connection, metadata, config |
| `segment_list` | `R/segment_list_classes.R` | Query results: time-aligned annotation segments |
| `extended_segment_list` | `R/segment_list_classes.R` | Segments enriched with DSP measurements |
| `lazy_segment_list` | `R/reindeer_lazy_segment_list.R` | Deferred query execution |
| `bundle_list` | `R/corpus_class.R` | Subset of corpus bundles |

**S7 property access uses `@`**, not `$`:
```r
corpus@basePath
corpus@dbName
corpus@config
```

**S7 class checks**: Use `S7::S7_inherits(x, segment_list)`, NOT
`inherits(x, "segment_list")`. S7 uses namespaced class names at runtime.
Helper predicates `is_segment_list(x)` and `is_extended_segment_list(x)` are
also exported.

### Key User-Facing Functions

```r
# Load corpus
corp <- corpus("path/to/db_emuDB")
corp <- corpus("path/to/db_emuDB", quick = TRUE)  # skip cache rebuild

# Query annotations (EQL -- EMU Query Language)
segs <- ask_for(corp, "Phonetic == t")
segs <- ask_for(corp, "[Phonetic == t -> Phonetic == s]")      # sequence
segs <- ask_for(corp, "[Phonetic == t ^ Syllable == stressed]") # dominance

# Apply DSP to segments (returns extended_segment_list)
formants <- quantify(segs, dsp_function = superassp::forest)
formants <- quantify(segs, dsp_function = superassp::forest, .at = 0.5)        # midpoint
formants <- quantify(segs, dsp_function = superassp::forest, .use_cache = TRUE) # cached

# Apply DSP to whole corpus (writes SSFF track files)
enrich(corp, .using = superassp::forest)
enrich(corp, .using = superassp::ksvF0, minF = 75, maxF = 500)

# Metadata
get_metadata(corp)
add_metadata(corp, list(Age = 25, Gender = "Male"), session = "S1", bundle = "B1")
biographize(segs, corp)   # enrich segment_list with metadata columns

# Sequence navigation (move forward/backward relative to query results)
next_phone <- scout(segs, steps_forward = 1)        # next item in tier
prev_phone <- scout(segs, steps_forward = -1)       # previous item
context     <- scout(segs, steps_forward = 1, capture = 2)  # capture 2 items
retreat(segs, steps_backward = 1)                   # alias: move backward
ascend_to(segs, level = "Syllable")                 # navigate up in hierarchy
descend_to(segs, level = "Phonetic")                # navigate down in hierarchy

# Corpus inspection
peek_signals(corp)                     # list available SSFF signal tracks
peek_signals(corp, extension = "fms")  # filter by extension

# Cache management
manage_cache(corp, action = "check")   # check cache health
manage_cache(corp, action = "clean")   # remove old cache files

# Serve interactive annotation UI
serve(corp)
```

### Signal Processing Pipeline

```
corpus --> enrich(.using = dsp_func)     --> SSFF files in bundle dirs
segment_list --> quantify(dsp_function)  --> extended_segment_list with measurements
```

Both `enrich()` and `quantify()` support:
- **Metadata-driven parameters**: Age/Gender metadata automatically maps to
  DSP function parameters (formant settings, pitch range, etc.) via
  `derive_dsp_parameters()` in `R/reindeer_enrich.R`
- **Persistent caching**: `.use_cache = TRUE` stores results in SQLite
- **Parallel processing**: `.parallel = TRUE` (default) via `future`/`furrr`
- **Cache format**: `.cache_format = "auto"` uses `qs` if available (faster,
  smaller), falls back to base R serialize

### Parameter Derivation from Metadata

The `DSPP` dataset (`data/DSPP.rda`) contains empirically-derived default DSP
parameters by age and gender. `dspp_metadataParameters_dt()` in
`R/reindeer_signal_extensions_dt.R` computes these using LOESS smoothing over
literature values:

```r
# What parameters does a 25-year-old male speaker get for formant analysis?
dspp_metadataParameters_dt()  # returns full table
# Columns: Age, Gender, nominalF1, maxFormantHz, ...
```

### Simulation System

For systematic parameter exploration:
```r
quantify_simulate(
  segments,
  .using = superassp::forest,
  .simulate = list(nominalF1 = seq(500, 900, 100)),
  .prep_function = superassp::prep_recode,
  .prep_simulate = list(sample_rate = c(16000, 22050, 44100)),
  .simulation_store = "simulations/formants"
)
# Creates outer product: 3 sample_rates x 5 nominalF1 = 15 combos per segment

list_simulations("simulations/formants")
reminisce("simulations/formants", simulation_id = "...")
reminisce_tracks(corp, simulation_store = "simulations/formants")
```

## Bundled Praat Scripts

The package bundles several Praat scripts in `inst/praat/`. These are NOT called
by reindeer R code directly -- they serve as reference implementations and are
called by the protoscribe companion package or used standalone.

### Script Inventory

| Script | Path | Author | Purpose |
|---|---|---|---|
| praat_periods | `inst/praat/praat_periods.praat` | F. Karlsson (original) | Glottal cycle detection + intensity |
| DDK segmentation | `inst/praat/DDK/ddk_segment.praat` | F. Karlsson (original) | Diadochokinetic syllable segmentation |
| processINTSINTMOMEL | `inst/praat/Momel-Intsint/processINTSINTMOMEL.praat` | F. Karlsson (original wrapper) | Batch MOMEL/INTSINT processing |
| Momel-Intsint plugin | `inst/praat/Momel-Intsint/plugin_momel-intsint/` | D. Hirst (unmodified) | MOMEL target detection + INTSINT coding |
| praatdet | `inst/praat/praatdet/` | J. Kirby (unmodified, git submodule) | EGG open quotient analysis |
| Prosogram | `inst/praat/prosogram_v300f/` | P. Mertens (unmodified) | Prosodic analysis + visualization |

### Python Reimplementations

`inst/pymomelintsint/` contains Python/Parselmouth reimplementations of the
MOMEL/INTSINT pipeline:

| File | Purpose |
|---|---|
| `momelintsint.py` | Full reimplementation: `automatic_min_max_fo()`, `momel()`, `code_with_intsint()`, `spectral_tilt()`, `prosody_index()`, plus Iseli-Alwan and Hawks-Miller helpers |
| `python_only_momelintsint.py` | Pure Python INTSINT optimizer (no Perl dependency); Swedish-language comments; development/exploratory |
| `intsint.pl` | Modified v2.12 of Hirst's Perl INTSINT (STDIN/STDOUT I/O instead of file-based) |
| `orig_intsint.pl` | Preserved original v2.11 for reference |
| `scriptPharyFullV3.praat` | Pharyngealization analysis reference (unknown provenance; Windows paths; included for reference only) |

**Note:** `momelintsint.py` ends with live executable code (lines 400-408)
with hardcoded corpus paths. These lines run when the module is imported and
will fail in any environment other than the original author's machine. Wrap
imports in `if __name__ == "__main__":` or refactor before using as a module.

See `PRAAT_MODIFICATIONS.md` for detailed provenance and modification history.

## Reimplementation Strategy

When reimplementing a Praat script as an R function for use with reindeer:

### 1. Understand the Praat Script's I/O

Read the Praat script's `form` block to identify inputs and the output format.
Every Praat script in `inst/praat/` follows a pattern:

```
form <Title>
    sentence SoundFile ...     # input audio
    real Parameter1 ...        # numeric parameters
    sentence OutputFile ...    # output path
endform
```

Map these to R function arguments.

### 2. Choose the Right Integration Point

| If reimplementing... | Use this pattern |
|---|---|
| A per-file DSP function (formants, F0, etc.) | Write a function compatible with `quantify()` / `enrich()` |
| A batch processing pipeline | Write a function that takes a `corpus` object |
| An annotation generator | Contribute to protoscribe, not reindeer |
| A measurement extractor | Write as `quantify()` method or standalone function |

### 3. Match Praat's DSP Exactly

For faithful reimplementation, use `wrassp` (bundled with emuR) or
`superassp` which wraps Praat algorithms via `parselmouth`:

| Praat Operation | R Equivalent |
|---|---|
| `To Pitch` | `wrassp::ksvF0()` or `superassp::praat_pitch()` |
| `To Formant (burg)` | `wrassp::forest()` or `superassp::praat_formant()` |
| `To Intensity` | `wrassp::rmsana()` or `superassp::praat_intensity()` |
| `To PointProcess (periodic, peaks)` | No direct equivalent -- reimplement in R |
| `To Spectrum` / `To Ltas` | `stats::spectrum()` or use `superassp` |
| `To MFCC` | `tuneR::melfcc()` or `superassp::praat_mfcc()` |
| MOMEL target detection | `inst/pymomelintsint/momelintsint.py::momel()` or use protoscribe |
| INTSINT coding | `inst/pymomelintsint/python_only_momelintsint.py::code_with_intsint()` |
| EGG Oq (praatdet) | No R equivalent yet -- candidate for reimplementation |
| DDK segmentation | No R equivalent yet -- candidate for reimplementation |

### 4. Write the R Function

The `quantify()` S7 generic dispatches on a `segment_list` first argument.
The `dsp_function` argument must follow the `wrassp`/`superassp` calling
convention: `f(listOfFiles, beginTime, endTime, ...)`.

```r
#' My reimplemented DSP function
#'
#' @param listOfFiles Character vector of audio file paths
#' @param beginTime Start time (seconds)
#' @param endTime End time (seconds)
#' @param ... Additional parameters
#' @return An SSFF (Simple Signal File Format) object, or a data.frame
my_dsp_function <- function(listOfFiles, beginTime = 0, endTime = 0, ...) {
  results <- lapply(listOfFiles, function(f) {
    # Read audio
    snd <- wrassp::read.AsspDataObj(f)
    sr <- attr(snd, "sampleRate")

    # Extract segment if times specified
    if (beginTime > 0 || endTime > 0) {
      start_sample <- max(1, floor(beginTime * sr))
      end_sample <- min(length(snd$audio), ceiling(endTime * sr))
      # ... extract segment
    }

    # Apply DSP (match Praat algorithm faithfully)
    # ...

    # Return SSFF-compatible object or data.frame
  })
  results
}
```

### 5. Register with quantify/enrich

Once your function follows the `listOfFiles` calling convention:

```r
# Use with quantify (per-segment) -- note: dsp_function is the second argument
segs <- ask_for(corp, "Phonetic == t")
results <- quantify(segs, dsp_function = my_dsp_function, .use_cache = TRUE)

# Use with enrich (whole corpus)
enrich(corp, .using = my_dsp_function, .force = TRUE)
```

### 6. Validate Output Fidelity

Compare R output against Praat output for the same input files:

```r
# Run Praat script on test file
system2("praat", c("--run", "inst/praat/my_script.praat", args))

# Run R reimplementation
r_result <- my_dsp_function("test.wav")

# Compare
all.equal(praat_result, r_result, tolerance = 1e-6)
```

For spectral measures (formants, MFCC, spectral tilt), expect small numerical
differences due to windowing and interpolation. Correlation > 0.99 is the target.

## Specific Reimplementation Guides

### praat_periods.praat --> R

The script detects periodic peaks and measures intensity at each peak.

**Praat algorithm:**
1. `To Intensity: minimum_f0, 0.0, 1` -- intensity contour
2. `To PointProcess (periodic, peaks): min_f0, max_f0, 1, 0` -- peak detection
3. For each peak: query intensity at that time

**R reimplementation approach:**
```r
# wrassp doesn't have PointProcess (periodic, peaks)
# Use superassp if available, or reimplement:
# 1. Compute pitch with wrassp::ksvF0() to get voiced frames
# 2. Find pitch period peaks from the autocorrelation
# 3. Query intensity (wrassp::rmsana()) at peak times

# Or use parselmouth via reticulate:
library(reticulate)
praat <- import("parselmouth")
snd <- praat$Sound("file.wav")
pp <- praat$praat$call(snd, "To PointProcess (periodic, peaks)",
                       min_f0, max_f0, TRUE, FALSE)
```

### DDK ddk_segment.praat --> R

**Praat algorithm:**
1. `To Intensity: minimum_pitch, 0, "yes"` -- intensity contour
2. `To TextGrid (silences): threshold, min_silent, min_sounding, C, V` -- segment
3. Identify DDK sequence boundaries from coarser silence detection (-25 dB)
4. Output timing table

**R reimplementation approach:**
```r
# 1. Compute intensity
int_obj <- wrassp::rmsana("file.wav", windowShift = 5)

# 2. Threshold-based segmentation
intensity <- int_obj$rms[, 1]
is_silent <- intensity < threshold  # in dB

# 3. Find run-length encoding of silent/sounding
rle_result <- rle(is_silent)

# 4. Filter by minimum durations, label C/V
# 5. Create segment table
```

### MOMEL/INTSINT --> R (via protoscribe)

The MOMEL/INTSINT pipeline is already reimplemented in Python
(`inst/pymomelintsint/momelintsint.py`). For R integration, use protoscribe:

```r
library(protoscribe)
suggestions <- protoscribe::draft_momel_intsint(audio_files, sessions, bundles)
protoscribe::assess(suggestions)
protoscribe::transcribe(suggestions)
```

If reimplementing in pure R (no Python):
1. Use the algorithm from `python_only_momelintsint.py::code_with_intsint()`
2. The INTSINT optimizer is a grid search over pitch range and key parameters
3. The momel binary (`inst/pymomelintsint/momel_osx_intel`) can be called via
   `system2()` -- it reads F0 values from stdin, writes target points to stdout

### Spectral Tilt Measures --> R

The spectral tilt implementation in `momelintsint.py::spectral_tilt()` ports
algorithms from OpenSauce/praatsauce:

**Measures computed:**
- H1-H2 (raw `L2L1` and Iseli-Alwan corrected `L2cL1c`)
- H1*-A3* (corrected `L1cLF3c`, uncorrected `L1LF3`)
- Spectral balance (0-500 Hz vs 500-1000 Hz energy ratio)
- SLF (spectral linear fit 100-5000 Hz, logarithmic)
- C1 (first MFCC coefficient -- spectral tilt proxy)

**Key helper functions (port these to R):**

`correction_iseli_i()` (lines 163-200) -- Iseli-Alwan harmonic amplitude correction:
```r
correction_iseli_i <- function(f, F_i, B_i, fs) {
  r_i <- exp(-pi * B_i / fs)
  omega_i <- 2 * pi * F_i / fs
  omega <- 2 * pi * f / fs
  numerator_sqrt <- r_i^2 + 1 - 2 * r_i * cos(omega_i)
  denom_factor1 <- r_i^2 + 1 - 2 * r_i * cos(omega_i + omega)
  denom_factor2 <- r_i^2 + 1 - 2 * r_i * cos(omega_i - omega)
  20 * log10(numerator_sqrt) - 10 * log10(denom_factor1) - 10 * log10(denom_factor2)
}
```

`bandwidth_hawks_miller()` (lines 202-258) -- Hawks-Miller bandwidth estimation:
```r
bandwidth_hawks_miller <- function(F_i, F0) {
  S <- 1 + 0.25 * (F0 - 132) / 88
  C1 <- c(165.327516, -6.73636734e-1, 1.80874446e-3, -4.52201682e-6, 7.49514000e-9, -4.70219241e-12)
  C2 <- c(15.8146139, 8.10159009e-2, -9.79728215e-5, 5.28725064e-8, -1.07099364e-11, 7.91528509e-16)
  # Evaluate 5th-order polynomial, choose C1 or C2 based on F_i < 500
  coef <- ifelse(F_i < 500, C1, C2)  # simplified; full version uses matrix ops
  S * sum(coef * F_i^(0:5))
}
```

Note also `correct_iseli_z()` (lines 137-146), an older scalar version of the
same correction that is NOT vectorized. Use `correction_iseli_i()` (vectorized)
for production code.

## Psychoacoustic Scales

reindeer provides scale conversion functions in `R/reindeer_psychoacoustics.R`:

```r
st(x, ref = 16.35160)  # Hz to semitones (default ref = C0)
erb(f)                   # Hz to ERB-rate (Moore & Glasberg 1983)
# For Bark scale, use emuR::bark()
```

## Testing

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test
Rscript -e "testthat::test_file('tests/testthat/test_query_optimized.R')"

# Load for interactive development
Rscript -e "devtools::load_all()"

# Check package
Rscript -e "devtools::check()"
```

Tests use the `ae` demo database from emuR: `reindeer:::create_ae_db()` returns
a path to a temporary database.

Current test status (v0.4.9): 0 failures, 1407 passing, 50 skips (all intentional).

## Dependencies

**Required (Imports):** S7, data.table, DBI, RSQLite, wrassp, cli, digest,
jsonlite, RcppSimdJson, assertthat, future, furrr, httpuv, xml2, Rcpp, tibble,
Rdpack, imputeTS, future.apply

**Optional (Suggests):** emuR, superassp (GitHub: humlab-speech/superassp), qs,
protoscribe, knitr, rmarkdown, yardstick, bigstatsr, openxlsx, av, readr

superassp provides the bridge between Praat's DSP algorithms and R. Install with:
```r
remotes::install_github("humlab-speech/superassp")
```

## Common Pitfalls

1. **S7 class checks**: Use `S7::S7_inherits(x, segment_list)`, NOT
   `inherits(x, "segment_list")`. S7 uses namespaced class names at runtime.
   Use `is_segment_list()` / `is_extended_segment_list()` for convenience.

2. **corpus constructor**: `corpus("path")` expects a path ending in `_emuDB`.
   It auto-appends the suffix if missing.

3. **Metadata inheritance**: Bundle metadata overrides session, which overrides
   database defaults. Always use `get_metadata()` which resolves inheritance.

4. **Cache invalidation**: The quantify cache keys on signal file mtime +
   parameter hash. If you modify audio files, caches auto-invalidate.

5. **data.table ALTREP**: When constructing parameter lists for cache hashing,
   rebuild lists element-by-element to avoid ALTREP hash poisoning:
   ```r
   # BAD: as.list(dt[i, ..cols]) -- ALTREP may produce different hashes
   # GOOD: setNames(lapply(col_names, function(nm) row[[nm]]), col_names)
   ```

6. **SQL security**: All queries use parameterized SQL. Never concatenate user
   input into SQL strings.

7. **quantify() argument names**: `dsp_function` is the second argument name.
   Using positional form `quantify(segs, superassp::forest)` works but naming
   it explicitly (`dsp_function = superassp::forest`) is safer against future
   signature changes.

8. **momelintsint.py module import**: The file ends with live executable code
   (lines 400-408) including a `glob.glob()` call to hardcoded paths. This code
   runs on import. Do not `import momelintsint` directly; extract only the
   needed functions, or add a `if __name__ == "__main__":` guard first.

9. **momel binary naming**: In `momelintsint.py` the Linux binary is referenced
   as `momel_linux_intel` (line 79), but the actual file in
   `plugin_momel-intsint/analysis/` is named `momel_linux`. Ensure the binary
   path is resolved correctly when deploying on Linux.
