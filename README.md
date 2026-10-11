# reindeer

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
remotes::install_github("humlab-speech/superassp")   # for quantify()
```

## Five-minute workflow

```r
library(reindeer)
library(dplyr)

corp <- corpus("path/to/your_emuDB")
# (or `corp <- demo_corpus()` to follow along with the bundled sample.)

# 0. Record speaker metadata so DSP picks age/gender-aware parameters
set_metadata(corp, list(Speaker = "P001", Age = 25, Gender = "Female"),
             session = "Session1")

# 1. Query vowels via EMU Query Language
vowels <- query(corp, "Phonetic =~ [aeiou]")

# 2. Extract formants at the midpoint
formants <- quantify(vowels, superassp::trk_formant_forest, .at = 0.5)

# 3. Join speaker metadata
data <- biographize(formants, corp)

# 4. Summarise with dplyr
data |>
  group_by(labels, Gender) |>
  summarise(mean_F1_Hz = mean(F1_Hz, na.rm = TRUE),
            mean_F2_Hz = mean(F2_Hz, na.rm = TRUE),
            .groups = "drop")
```

See `vignette("getting_started")` for the expanded walkthrough.

## Capability tour

### Speaker-aware DSP

Set Age / Gender once; every `quantify()` call picks
appropriate formant ranges, window lengths, and pitch limits.

```r
set_metadata(corp,
             list(Speaker = "P001", Age = 25, Gender = "Female"),
             session = "Session1")

dsp_parameters(corpus_obj = corp)   # inspect what each bundle will use
```

See `vignette("speaker_aware")`.

### Lazy pipelines and provenance

`query()` builds a plan; nothing runs until you collect, print, or pipe
into a dplyr verb. Every step is recorded, so silent row loss is
visible.

```r
plan <- query(corp, "Phonetic =~ [aeiou]") |>
  filter(labels != "@") |>
  scout(steps_forward = 1) |>
  ascend_to("Word")

result <- collect(plan)
provenance(result)     # per-step row counts
dropped(result)        # rows lost per step
```

A navigation step that drops more than 25 % of its input warns by
default. See `vignette("querying")`.

### Persistent measurement cache

```r
formants <- quantify(vowels, superassp::trk_formant_forest, .use_cache = TRUE)
table(formants$.cache_status)   # "hit" / "miss"
summary(corp)               # cache size and health
```

The cache key includes the DSP parameters and the bundle's
Age / Gender, so external metadata edits invalidate the right rows.
Enabling the cache turns iterative re-runs of an analysis on a large
corpus from minutes back to seconds, at the cost of some disk space
in `.quantify_cache/` next to the database. See
`vignette("speaker_aware")`.

### FAIR archival metadata

`describe_corpus()` writes a README, CMDI XML, and DataCite JSON next
to the corpus. The files are
regenerated automatically after metadata changes (switch off with
`options(reindeer.auto_cmdi = FALSE)`).

```r
describe_corpus(corp)
```

### Interactive annotation

```r
annotate(corp)                                   # edit the whole corpus
annotate(query(corp, "Phonetic == t"))           # step through a segment list
```

`annotate()` opens the corpus in [Artic](https://github.com/humlab-speech/artic).
Given a segment list (a `query()` or `quantify()` result, sorted as you like)
it opens a playlist of time anchors, so you review just those portions.

## Companion packages

- **[superassp](https://github.com/humlab-speech/superassp)** — formant, pitch,
  voice-quality and other DSP functions used inside `quantify()`.

## Documentation

- [Package website](https://humlab-speech.github.io/reindeer/)
- `vignette("getting_started")` — query, measure, plot in ten minutes.
- `vignette("querying")` — EQL, navigation, lazy plans, provenance.
- `vignette("speaker_aware")` — metadata-driven parameters, caching.
- `vignette("metadata")` — the three metadata levels, spreadsheets.
- `vignette("building_a_corpus")` — import recordings.
- `vignette("annotating")` — Artic.
- `vignette("interoperability")` — ELAN, Praat, and a map from emuR.
- `vignette("archiving")` — README, CITATION.cff, DataCite, CMDI.

## Citation

If you use reindeer in published work, please cite:

```r
citation("reindeer")
```

## License

GPL (>= 2).

## See also

- [emuR](https://github.com/IPS-LMU/emuR) — the EMU Speech Database
  Management System this package builds on.
- [EMU-SDMS Manual](https://ips-lmu.github.io/The-EMU-SDMS-Manual/) —
  reference for the corpus format and EMU Query Language.
