# Praat Code Provenance and Modifications

This document tracks the origin, version, and modification status of all Praat
scripts and related code bundled with the reindeer package.

Last updated: 2026-03-09 (reindeer v0.4.8)

## Overview

reindeer bundles Praat scripts in `inst/praat/` as reference implementations for
speech analysis algorithms. As of v0.3.0, these scripts are NOT called by
reindeer R code directly. They are used by the companion package
[protoscribe](https://github.com/humlab-speech/protoscribe) or run standalone.

Python reimplementations of the MOMEL/INTSINT pipeline are in
`inst/pymomelintsint/`.

## Component Inventory

| Component | Path | Origin | Modified? |
|---|---|---|---|
| praatdet | `inst/praat/praatdet/` | James Kirby | No (git submodule) |
| Momel-Intsint plugin | `inst/praat/Momel-Intsint/plugin_momel-intsint/` | Daniel Hirst | No |
| processINTSINTMOMEL | `inst/praat/Momel-Intsint/processINTSINTMOMEL.praat` | F. Karlsson | N/A (original) |
| Prosogram | `inst/praat/prosogram_v300f/` | Piet Mertens | No |
| DDK segmentation | `inst/praat/DDK/ddk_segment.praat` | F. Karlsson | N/A (original) |
| praat_periods | `inst/praat/praat_periods.praat` | F. Karlsson | N/A (original) |
| momelintsint.py | `inst/pymomelintsint/momelintsint.py` | F. Karlsson | N/A (original) |
| python_only_momelintsint.py | `inst/pymomelintsint/python_only_momelintsint.py` | F. Karlsson | N/A (original) |
| intsint.pl (modified) | `inst/pymomelintsint/intsint.pl` | D. Hirst, modified by F. Karlsson | Yes |

---

## 1. praatdet -- EGG Open Quotient Analysis

- **Path:** `inst/praat/praatdet/`
- **Author:** James Kirby, Nanyang Technological University
- **Upstream:** https://github.com/kirbyj/praatdet
- **Version:** v0.3 (2020-08-23)
- **License:** GPL-3.0
- **Inclusion:** Git submodule, pinned at commit `5c56b97`
- **Modified from upstream:** No
- **Upstream status:** 1 commit behind (trivial TODO.md update only)

**Purpose:** Determine open quotient (Oq) and fundamental frequency from the
EGG (electroglottography) waveform using dEGG peak detection and Howard's
threshold method.

**Key scripts:**
- `praatdet.praat` -- main entry point (batch processing wrapper)
- `getoq.praat` -- core Oq extraction
- `howard.praat` -- Howard's threshold method implementation
- `peakdet.praat` -- dEGG peak detection
- `smooth.praat` -- linearly weighted symmetric moving average
- `degg.praat` -- derivative EGG computation
- `shelldet.praat` -- command-line interface variant

**Output format:** Comma-separated text with columns:
`filename, var1, var2, ..., label, period, start, end, egg_f0, degg_oq, howard_oq`

**Citation:** Kirby, James. 2020. Praatdet: Praat-based tools for EGG analysis
(v0.3). https://github.com/kirbyj/praatdet

---

## 2. Momel-Intsint Plugin

- **Path:** `inst/praat/Momel-Intsint/plugin_momel-intsint/`
- **Author:** Daniel Hirst, Laboratoire Parole et Langage, Aix-en-Provence
- **Upstream:** Distributed as Praat plugin from http://www.lpl-aix.fr/~hirst/
- **Version:** setup.praat dated 2021-05-06; individual scripts have own dates
- **License:** No explicit license file in distribution
- **Inclusion:** Direct copy of plugin distribution
- **Modified from upstream:** No

**Purpose:** Automatic pitch target detection (MOMEL) and intonation coding
(INTSINT) for prosodic analysis.

**Key scripts:**

| Script | Date | Purpose |
|---|---|---|
| `analysis/automatic_min_max_f0.praat` | 2016-11-21 | Two-pass auto pitch range detection |
| `analysis/momel_single_file.praat` | 2011-09-23 | MOMEL target point detection (calls momel binary) |
| `analysis/code_with_intsint.praat` | 2013-03-21 | INTSINT labeling of MOMEL targets (calls intsint.pl) |
| `analysis/calculate_intsint_labels.praat` | -- | Batch INTSINT calculation |
| `analysis/calculate_momel_targets_extracts.praat` | -- | Batch MOMEL targets |
| `analysis/correct_momel_targets.praat` | -- | Interactive MOMEL correction |
| `analysis/detect_f0.praat` | -- | F0 detection |
| `analysis/intsint.pl` | v2.11, 2006-11-29 | Original Perl INTSINT optimizer (file-based I/O) |

**Bundled binaries:**

| Binary | Platform | Architecture |
|---|---|---|
| `analysis/momel_osx_intel` | macOS | x86_64 |
| `analysis/momel_osx_ppc` | macOS | PPC |
| `analysis/momel_linux` | Linux | x86_64 |
| `analysis/momel_win.exe` | Windows | x86 |

**C sources:** `momel sources/momel.c`, `momel.h` (for recompilation)

**Algorithm:** MOMEL (MOdelling MELody) detects pitch targets as local
extrema of a smoothed F0 contour. INTSINT (INternational Transcription System
for INTonation) assigns categorical tonal labels (T, M, B, H, L, U, D, S)
by grid-search optimization over pitch range and key parameters.

---

## 3. processINTSINTMOMEL.praat (Local Wrapper)

- **Path:** `inst/praat/Momel-Intsint/processINTSINTMOMEL.praat`
- **Author:** Fredrik Karlsson (reindeer author)
- **Origin:** Original script, not from upstream

**Purpose:** Batch-processes `.wav` files through the Momel-Intsint plugin
pipeline. For each file:
1. Calls `automatic_min_max_f0.praat` with configurable pitch span
2. Saves computed Pitch object as binary `.Pitch` file
3. Calls `momel_single_file.praat` with 7 MOMEL parameters
4. Calls `code_with_intsint.praat` for INTSINT labeling
5. Combines results into semicolon-separated output CSV

**Parameters (from `form` block):**

| Parameter | Default | Description |
|---|---|---|
| `Input_Directory` | -- | Directory containing `.wav` files |
| `Window_length` | 30 ms | MOMEL target window length |
| `Minimum_f0` | 60 Hz | Pitch floor |
| `Maximum_f0` | 750 Hz | Pitch ceiling |
| `Pitch_span` | 1.5 | Pitch range in octaves (2.5 for expressive speech) |
| `Maximum_error` | 1.04 | MOMEL max error threshold |
| `Reduced_window_length` | 20 ms | MOMEL reduced window |
| `Minimal_distance` | 20 ms | Min distance between targets |
| `Minimal_frequency_ratio` | 0.05 | Min frequency ratio for targets |
| `Output_file` | -- | Semicolon-separated output CSV path |

**Note:** Contains hardcoded default paths (`/Users/frkkan96/Desktop/INT/`)
that are overridden when called programmatically.

---

## 4. Prosogram v3.00f

- **Path:** `inst/praat/prosogram_v300f/`
- **Author:** Piet Mertens, KU Leuven
- **Upstream:** http://sites.google.com/site/prosogram/
- **Version:** 3.00f (July 15, 2020)
- **License:** Copyright 2003-2020 Piet Mertens (no separate license file)
- **Inclusion:** Direct copy
- **Modified from upstream:** No

**Purpose:** Automatic prosodic analysis and visualization. Generates prosograms
(F0 stylization plots with annotation). Includes polytonia detection, pitch
range analysis, and corpus-level processing.

**Key scripts:**

| Script | Lines | Purpose |
|---|---|---|
| `prosogram.praat` | 102 | Main entry point / form dialog |
| `prosomain.praat` | ~3,800 | Core processing engine |
| `prosoplot.praat` | -- | Plotting routines |
| `segment.praat` | -- | Segmentation routines |
| `stylize.praat` | -- | F0 stylization |
| `polytonia.praat` | -- | Polytonia analysis |
| `util.praat` | -- | Utility functions |

---

## 5. DDK Segmentation Script

- **Path:** `inst/praat/DDK/ddk_segment.praat`
- **Author:** Fredrik Karlsson (reindeer author)
- **Origin:** Original script

**Purpose:** Automatic segmentation of diadochokinetic (DDK) speech sequences.
DDK tasks involve rapid repetition of syllables (e.g., /pa-ta-ka/) and are used
in clinical speech assessment.

**Algorithm:**
1. Compute intensity contour (`To Intensity: minimum_pitch, 0, "yes"`)
2. Silence detection (`To TextGrid (silences)`) with user thresholds
3. Create two-tier TextGrid: DDK Syllables (coarse) + DDK Segments (fine)
4. Identify DDK sequence boundaries from coarser silence detection (-25 dB)
5. Insert boundaries and generate timing table

**Parameters:**

| Parameter | Default | Description |
|---|---|---|
| `SoundDirectory` | -- | Input directory with `.wav` files |
| `Silence_threshold` | -9.0 dB | Segment-level silence threshold |
| `Minimum_silent_interval_duration` | 0.05 s | Min silence to split segments |
| `Minimum_sounding_interval_duration` | 0.025 s | Min sounding interval |
| `Consonant_label` | "C" | Label for consonant intervals |
| `Vowel_label` | "V" | Label for vowel intervals |
| `Sequence_silence_threshold` | -25.0 dB | Coarse DDK sequence detection |
| `Sequence_minimum_duration` | 0.100 s | Min DDK sequence duration |

**Output:** Semicolon-separated table with columns from TextGrid: tier, tmin,
tmax, text, plus `segment` (filename stem).

---

## 6. praat_periods.praat

- **Path:** `inst/praat/praat_periods.praat`
- **Author:** Fredrik Karlsson (reindeer author)
- **Origin:** Original script

**Purpose:** Detect glottal cycle peaks (periodic peaks) in a sound file and
measure intensity at each peak time. Used for voice quality analysis.

**Algorithm:**
1. Read sound file (or extract segment if begin/end times given)
2. Compute intensity contour (`To Intensity: minimum_f0, 0.0, 1`)
3. Detect periodic peaks (`To PointProcess (periodic, peaks): min_f0, max_f0, 1, 0`)
4. For each detected peak, query intensity value
5. Output CSV with `Time` and `Intensity` columns

**Parameters:**

| Parameter | Default | Description |
|---|---|---|
| `SoundFile` | -- | Input audio file path |
| `BeginTime` | 0.0 | Segment start (0 = file start) |
| `EndTime` | 0.0 | Segment end (0 = file end) |
| `Time_step` | 0.005 s | Analysis time step |
| `Minimum_f0` | 75.0 Hz | Pitch floor for peak detection |
| `Maximum_f0` | 600.0 Hz | Pitch ceiling for peak detection |
| `WindowShape` | Gaussian1 | Window for part extraction |
| `Interpolation` | cubic | Intensity interpolation method |
| `RelativeWidth` | 1.0 | Window relative width |
| `TrackOut` | -- | Output CSV file path |

---

## 7. Python MOMEL/INTSINT Reimplementation

- **Path:** `inst/pymomelintsint/`
- **Author:** Fredrik Karlsson (reindeer author)
- **Origin:** Original reimplementation incorporating third-party algorithms

### momelintsint.py (408 lines)

Full Python/Parselmouth reimplementation of the MOMEL/INTSINT pipeline plus
spectral tilt measures.

**Functions:**

| Function | Lines | Reimplements |
|---|---|---|
| `automatic_min_max_fo()` | 61-68 | Hirst's `automatic_min_max_f0.praat` |
| `momel()` | 72-96 | Wraps momel binary via subprocess/stdin |
| `code_with_intsint()` | 98-132 | Wraps modified `intsint.pl` via subprocess/stdin |
| `spectral_tilt()` | 263-331 | Ported from praatsauce/OpenSauce |
| `prosody_index()` | 346-381 | Orchestration: all of the above combined |
| `correction_iseli_i()` | 163-200 | Iseli-Alwan harmonic amplitude correction |
| `bandwidth_hawks_miller()` | 202-258 | Hawks-Miller bandwidth estimation |

**Dependencies:** parselmouth, numpy, scipy, pandas

**Academic references cited in code:**
- Tsiakoulis et al. 2010 (MFCC spectral moments)
- Sluijter & Heuven 1996 (spectral balance)
- Schweitzer 2019 (spectral tilt / SLF)
- Kakouros et al. 2018 (spectral tilt comparison)
- Campbell & Beckman 1997 (H1-H2)
- Okobi 2006 (H1*-A3*)
- Hawks & Miller 1995 (formant bandwidth estimation)
- Iseli & Alwan (harmonic magnitude correction)

**OpenSauce attribution:** `correction_iseli_i()` and `bandwidth_hawks_miller()`
explicitly cite their port from
[OpenSauce](https://github.com/voicesauce/opensauce-python) (lines 181, 219).

### python_only_momelintsint.py (215 lines)

Earlier/alternative version with pure Python INTSINT optimization (no Perl
dependency). Contains Swedish-language comments. Development/exploratory code.

**Key difference from `momelintsint.py`:** `code_with_intsint()` is implemented
as a pure Python grid search rather than calling the Perl script.

### intsint.pl -- Modified v2.12

- **Original:** Daniel Hirst, v2.11 (2006-11-29)
- **Modified by:** Fredrik Karlsson, v2.12 (2024-04-28)
- **Original preserved as:** `orig_intsint.pl`

**Modifications (v2.11 --> v2.12):**

| Change | v2.11 (original) | v2.12 (modified) |
|---|---|---|
| Input source | File-based (`open(IN, ...)`) | STDIN (`while (<STDIN>)`) |
| Output target | File-based (`print OUT ...`) | STDOUT (`print STDOUT ...`) |
| Argument handling | `$ARGV[0]` file extension parsing | Removed (no args needed) |
| Header output | References input/output filenames | Generic `"; INTSINT labels created on..."` |
| Verbose output | Conditional (`$verbose and print`) | Always prints to STDOUT |
| Version history | Up to v2.11 | Added v2.12 entry |

**Rationale:** The modifications convert `intsint.pl` from a standalone
file-based script to a pipe-friendly filter, enabling it to be called from
Python via `subprocess.Popen()` with stdin/stdout communication. This avoids
temporary file I/O overhead.

### momel_osx_intel binary

- **Path:** `inst/pymomelintsint/momel_osx_intel`
- **Origin:** Daniel Hirst (Momel-Intsint plugin distribution)
- **Modified:** No (byte-identical to `plugin_momel-intsint/analysis/momel_osx_intel`)
- **Architecture:** Mach-O 64-bit x86_64

### scriptPharyFullV3.praat (unknown provenance)

- **Path:** `inst/pymomelintsint/scriptPharyFullV3.praat`
- **Author:** Unknown
- **Lines:** ~1,100
- **Purpose:** Pharyngealization analysis (Iseli-Alwan normalization, H1-H2,
  H1-A1, H1-A2, H1-A3 computation)
- **Note:** Contains Windows-specific paths. Likely included as reference
  material for the spectral tilt implementation. Provenance unclear.

---

## Git History

Chronological commit history for Praat-related files:

| Date | Commit | Description |
|---|---|---|
| 2022-01-01 | `96c14f1` | Initial structuring of inst/ (Momel-Intsint, Prosogram) |
| 2022-01-01 | `9a505a7` | Removed verbose code from Momel scripts |
| 2022-01-02 | `d668c7f` | processINTSINTMOMEL now saves Pitch files to output dir |
| 2022-01-10 | `62bf415` | Moved SLAM into python subdirectory |
| 2022-02-26 | `bff51ae` | Initial DDK segmentation script |
| 2022-02-27 | `07cac55` | DDK: directory-level batch processing |
| 2022-02-27 | `88db6db` | DDK: output table location revision |
| 2022-02-28 | `43ef09b` | DDK: removed unnecessary arguments |
| 2022-08-08 | `297a3e0` | Stashed code fixes |
| 2022-08-08 | `26486cb` | Mac-specific fixes |
| 2022-10-17 | `e4ee68a` | Added praat_periods.praat; praatdet submodule |
| 2025-10-15 | `1206870` | Python/Parselmouth annotation system (pymomelintsint) |

---

## Notes on Non-Portable Files

R CMD check warns about these files. They are inherent to the bundled
third-party assets and cannot be renamed without breaking compatibility:

**Executable files** (momel binaries -- required at runtime):
- `plugin_momel-intsint/analysis/momel_linux`
- `plugin_momel-intsint/analysis/momel_osx_intel`
- `plugin_momel-intsint/analysis/momel_osx_ppc`
- `plugin_momel-intsint/analysis/momel_win.exe`
- `inst/pymomelintsint/momel_osx_intel`

**Non-portable filenames** (spaces, non-ASCII characters):
- `Momel-Intsint/Read_me - Praat plugins.pdf` (space in filename)
- `praatdet/examples/dh\u00e2lem_iso_1_mis.wav` (circumflex a)
- `praatdet/examples/d\u00e2lem_iso_1_mis.wav` (circumflex a)
- `plugin_momel-intsint/momel sources/` (space in directory name)

**Path exceeding 100 bytes:**
- `plugin_momel-intsint/analysis/calculate_momel_targets_extracts.praat`
