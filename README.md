# reindeer <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/humlab-speech/reindeer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/humlab-speech/reindeer/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/humlab-speech/reindeer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/humlab-speech/reindeer?branch=main)
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

**reindeer** is an R package for reproducible analysis of speech
corpora. It extends [emuR](https://github.com/IPS-LMU/emuR) with a
tidyverse-friendly query and measurement workflow, three-level speaker
metadata that drives age- and gender-aware DSP, and automatic
generation of standards-compliant archival metadata (CMDI, DataCite).
It is designed for phonetic and clinical-phonetic research where the
provenance of every measurement matters.

## What reindeer is for

- Analysing annotated speech databases (EMU-SDMS format) at the
  segment, word, and utterance levels.
- Extracting acoustic measurements (formants, pitch, intensity, …)
  with DSP parameters chosen from each speaker's metadata.
- Treating segment lists as tidy tables so dplyr, ggplot2, and the rest
  of the tidyverse work without translation.
- Auditing every step of a pipeline through a provenance trail that
  survives serialisation.
- Producing FAIR archival metadata (CMDI, DataCite, README) without
  manual XML editing.

## Installation

```r
# Reindeer + signal-processing companion
remotes::install_github("humlab-speech/reindeer")
remotes::install_github("humlab-speech/superassp")
```

## Five-minute workflow

```r
library(reindeer)
library(dplyr)

corp <- corpus("path/to/your_emuDB")

# 1. Query vowels via EMU Query Language
vowels <- query(corp, "Phonetic =~ [aeiou]")

# 2. Extract formants at the midpoint
formants <- quantify(vowels, superassp::forest, .at = 0.5)

# 3. Join speaker metadata
data <- enrich(formants, corp)

# 4. Summarise with dplyr
data |>
  group_by(label, Gender) |>
  summarise(mean_F1 = mean(F1, na.rm = TRUE),
            mean_F2 = mean(F2, na.rm = TRUE),
            .groups = "drop")
```

See `vignette("getting_started")` for the expanded walkthrough.

## Capability tour

### Speaker-aware DSP

Set Age / Gender once; every `quantify()` and `enrich()` call picks
appropriate formant ranges, window lengths, and pitch limits.

```r
set_metadata(corp,
             list(Speaker = "P001", Age = 25, Gender = "Female"),
             session = "Session1")

dsp_parameters(corpus_obj = corp)   # inspect what each bundle will use
```

See `vignette("metadata_management")`.

### Lazy pipelines and provenance

`query()` builds a plan; nothing runs until you collect, print, or pipe
into a dplyr verb. Every step is recorded, so silent row loss is
visible.

```r
plan <- query(corp, "Phonetic =~ [aeiou]") |>
  filter(label != "@") |>
  scout(steps_forward = 1) |>
  ascend_to("Word")

result <- collect(plan)
provenance(result)     # per-step row counts
dropped_rows(result)   # which steps lost rows
```

A navigation step that drops more than 25 % of its input warns by
default. See `vignette("lazy_and_provenance")`.

### Persistent measurement cache

```r
formants <- quantify(vowels, superassp::forest, .use_cache = TRUE)
table(formants$.cache_status)   # "hit" / "miss"
inspect_cache(corp)
```

The cache key includes the DSP parameters and the bundle's
Age / Gender, so external metadata edits invalidate the right rows.
See `vignette("cache_management")`.

### FAIR archival metadata

`describe_corpus()` writes a README, CMDI XML, and DataCite JSON next
to the corpus. With `options(reindeer.auto_cmdi = TRUE)` it runs
automatically after any metadata change.

```r
describe_corpus(corp)
```

### Interactive annotation

```r
serve_app(corp)                                  # all bundles
serve_app(corp, seglist = query(corp, "..."))    # a query result
```

`serve_app()` launches a local instance of the EMU-webApp so you can
inspect, correct, or extend annotations from R.

## Companion packages

- **[superassp](https://github.com/humlab-speech/superassp)** —
  formant, pitch, voice-quality, and other DSP functions used inside
  `quantify()`.
- **[protoscribe](https://github.com/humlab-speech/protoscribe)** —
  draft annotation generation (VAD, VOT, MOMEL/INTSINT, …).
- **[erodex](https://github.com/humlab-speech/erodex)** — parameter-
  grid simulation and result inspection.
- **[eggstract](https://github.com/humlab-speech/eggstract)** —
  electroglottography measurements.

## Documentation

- [Package website](https://humlab-speech.github.io/reindeer/)
- `vignette("getting_started")` — five-minute pipeline.
- `vignette("metadata_management")` — three-level inheritance,
  Excel round-trip, FAIR export.
- `vignette("cache_management")` — inspecting and pruning the quantify
  cache.
- `vignette("lazy_and_provenance")` — lazy plans, provenance, and
  pipe-loss debugging.

## Citation

If you use reindeer in published work, please cite:

```
Nylén, F. (2026). reindeer: Reproducible Analysis of Speech Corpora
in R. R package version 0.9.0.
https://github.com/humlab-speech/reindeer
```

## License

GPL (>= 2).

## See also

- [emuR](https://github.com/IPS-LMU/emuR) — the EMU Speech Database
  Management System this package builds on.
- [EMU-SDMS Manual](https://ips-lmu.github.io/The-EMU-SDMS-Manual/) —
  reference for the corpus format and EMU Query Language.
