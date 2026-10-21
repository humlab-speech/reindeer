# reindeer way forward, 3.1.0 → 3.2.0

Date: 2026-10-21. Base: `main` @ `ec1aa02` (3.1.0 plus CI fixes).
Assesses `REINDEER_AMENDMENT_PLAN_2.2.0.md` and its critique
`REINDEER_FORWARD_PLAN_3.1.0.md` against the tree, together with
`REINDEER_WAY_FORWARD_3.1.md`, which replaced the forward plan's work list and
was executed. This document supersedes the latter two as the active plan.

## 0. How this was checked

- The headline defects of the 2.2.0 assessment were re-run with a script that
  is independent of the test suite. The suite was written by the same hands as
  the fixes, so it cannot be the only witness.
- Full suite, `R CMD check --as-cran`, per-file `covr`, and `lintr` with
  `object_usage_linter`. R 4.6.1, macOS arm64, superassp 3.2.0 (installed),
  emuR 2.6.0.
- `quantify()` was compared with `emuR::get_trackdata()` on the `ae` demo
  corpus. `ae` ships SSFF formant tracks (`.fms`) made by the same libassp
  forest routine that `superassp::trk_formant_forest` wraps, so it is an emuR
  ground truth for measurements. None of the three earlier documents used it.

## 1. Lineage

| Document | Written at | Outcome |
|---|---|---|
| Amendment plan 2.2.0 | `b15b50a` | Complete, released 3.0.0 |
| Forward plan 3.1.0 | `1b7d78f` (mid Phase 1) | Critique kept; work list never executed |
| Way forward 3.1 | `3c6825a` (3.0.0) | F0–F4 done, released 3.1.0 |

## 2. What holds (verified at `ec1aa02`)

| Claim | Evidence |
|---|---|
| A1 lazy plans no longer share state | `nrow()` then `quantify()` keeps tracks; input plan unchanged; lazy and eager `scout()` both 246 rows |
| A2/A3 speaker norms reach DSP | `Age = 6, Gender = "Female"` at **session** level moves median F1 by 52 Hz (forest); output shape stable |
| A5 cache key | renamed function hits; swapped function, different captured constant (median F1 353 vs 410 Hz) and replaced `.wav` all miss |
| A8 metadata | `Date` survives, mixed types kept as `3 / 3.7 / n/a`, bracket assignment anchored |
| EQL parity | 80/80 golden cases, no parity skips |
| `R CMD check --as-cran` | 0 errors, 0 warnings, 0 notes (local) |
| Tests | 63 files, 512 `test_that` blocks, 0 failures, 2 skips, 107 s |
| Coverage | 71.3 % (CI floor 69 %) |
| Surface | 31 exports, 19 Imports |

The query side (EQL, navigation, metadata) is in good shape. The measurement
side is not.

## 3. Findings

Severity: **high** = a user gets a wrong or empty measurement without an error.

**R1. `.at` does not take the nearest frame (high).**
`R/tidy_trackdata_helpers.R:247` selects `round(.at * n_frames)`, an index
into the slice, in both modes. Against `get_trackdata(ae, …, "fm", cut = 0.5)`
the DSP values are identical wherever both pick the same frame (131 of 131),
but in 122 of 253 midpoints reindeer takes the frame 5 ms earlier, always the
one farther from the cut time. F1 |Δ|: median 0, p90 27.6 Hz, max 1106 Hz.
D3 ("keep nearest, emuR parity") was the right decision; the code never
implemented it, and no test compared with emuR.

**R2. Reading registered tracks returns nothing (high).**
`quantify(segs, "fm", .at = 0.5)` on `ae` returns 253 rows with no track
columns and no warning, after 4.4 s. `superassp::read_track()` now aborts for
a single time point that is not on a frame boundary unless `snap = "nearest"`;
`R/segment_list_quantify.R:426-429` swallows the error, and `:502-505` returns
the input unchanged (the warning is only shown with `.verbose = TRUE`). This is
how an emuR user reaches the tracks already in the corpus. `read_track()` also
defaults to `zero_to_na = TRUE`, while emuR and function mode keep 0.

**R3. Tracks registered by reindeer are recomputed, not read (high).**
Same swallowed error, but these tracks carry a `generator`, so `:441-479`
re-runs the routine on the whole file once per segment and time point, with
the generator's stored arguments instead of each speaker's parameters.
Way forward 3.1 dropped N1.1 because "character-track mode reads registered
tracks from disk". It does not.

**R4. Corpus mode registers tracks it never wrote (high with released superassp).**
superassp 3.2.0 ignores `toFile = TRUE` in its libassp wrappers. superassp
3.2.3 (installed 2026-10-21) writes the file and returns a count, so with a
`>= 3.2.3` pin only the missing output check below remains. `quantify(corp, .using = trk_rms)` and
`trk_pitch_ksv` registered tracks with 0 files on disk. Forest appeared to
work only because `ae` already ships `.fms` files written by emuR.
`R/quantify_corpus.R:252-266` counts a call as success without checking the
output file, caches it as done, and registers the track with a warning. A
DBconfig that points at missing SSFF files breaks the corpus for emuR. CI
installs superassp HEAD, which writes files, so CI cannot see this.

**R5. A whole-file DSP error silently switches that bundle to window mode
(medium–high).** `.dsp_whole_table()` (`:189-193`) turns an error into `NULL`,
and the bundle then goes through the window path. Repro: a routine that fails
on its second call. msajc010 comes back with windowed values that differ from
the bundle-mode run. There is no warning and no marker in the rows. Per
`WHOLE_FILE_FIDELITY.md`, the modes differ by 17 Hz F0, 118 Hz COG and 6 dB.
This is A4's failure class, back in through WP4.3.

**R6. Behind R2–R5 is one idiom.** There are 21 `error = function(e) NULL`
handlers in `R/`, five of them on measurement paths
(`tidy_trackdata_helpers.R:193`, `segment_list_quantify.R:428,448,458`,
`quantify_corpus.R:138`). The plans fixed instances, never the pattern.

**R7. The superassp contract is unguarded (high, process).** DESCRIPTION says
`superassp (>= 3.0.0)`, and CI installs GitHub HEAD. The only contract test
checks argument names (WP2.5). Since 3.0, superassp has changed `read_track()`
and `toFile` semantics. Eight superassp commits landed on 2026-10-20, and
reindeer CI has not run since 2026-10-11. The forward plan (C5) warned about
emuR drift; the drift that bit was superassp.

**R8. Marked done, not done (medium).**
- `dplyr::bind_rows(s, s)` errors ("Columns 1, 2, … must be named"). The test
  is skipped with "fixed by the class merge", and WP5.1 is marked done.
- `quantify_egg` was unexported in 3.0 with "use `quantify()`", but
  `quantify()` has no EGG path: the segment-list method only analyses the
  media file. WP5.2 had deferred the absorption. `companion_eggstract.R` is
  orphaned (34 % covered). The private `eggstract` Suggests is why CI needs
  `dependencies: '"hard"'`, `install-pandoc: false` and
  `_R_CHECK_FORCE_SUGGESTS_=false`, and it is a CRAN blocker.
- WP5.4 conventions are not met:
  - First arguments are `corpus_obj`, `corpus`, `.segments`, `segs_tbl` and
    `object`.
  - `quantify()` takes `dsp_function` on segment lists but `.using` on
    corpora.
  - Arguments deprecated before 3.0 survived the 3.0 removal release, against
    the NEWS policy: `.parallel`, `.workers` and `.optimize` (ignored since
    2.3), `quick` (2.5), and the camelCase aliases (2.6). The suite still
    calls `quantify(fileExtension =)`.
- F3.2 promised `object_usage_linter`, but `.lintr` enables 7 linters without
  it. Enabling it gives 56 unused-variable hits and no undefined globals.

**R9. Bogus deprecation on every documented Artic call (low–medium).**
`annotate(port =)` warns "`port` is deprecated; use `port`". The identity
pairs in `.artic_arg_map` (`host`, `port`, `browser`, `debug`;
`R/reindeer_annotate.R:126-132`) go through `.rename_args()`.

**R10. `CLAUDE.md` is wrong in about 20 places (medium; development here is
agent-driven, and this file is read first).**
- Three files it points to no longer exist: `R/R/query_executor.R`,
  `R/reindeer-corpus.R`, `benchmarking/benchmark_dspp.R`.
- It gives removed verbs as guidance: `gather_metadata`, `add_metadata`,
  `import_metadata`, `enrich`, `extended_segment_list`, `serve`,
  `check_*_cache_size`.
- It describes the old cache design: `serialize()`, `_signal_cache.sqlite`,
  qs in Imports.
- It lists a test file that no longer exists.

**R11. Dead or unreachable code (low).**
- The DSPP regeneration code (~170 lines, `R/reindeer_signal_extensions_dt.R`,
  3 % covered) runs only from the source tree on
  `data-raw/default_parameters.xlsx`. It is the only user of `imputeTS`.
- `import_media(annotations =)` always aborts.
- `R/annotation_io.R` is 0 % covered.

**R12. CI.** superassp 3.2.2 fixed `munmap`, so the Windows
`continue-on-error` added on 2026-10-11 can go.

## 4. Why the plans missed these

1. **Parity was built for queries only.** Fidelity was defined as emuR parity,
   but only the query side got a parity harness. DSP fidelity was argued by
   comparing reindeer with itself: bundle vs window (assessment P2, forward
   plan C3/C12, way forward F1.3). With `ae`'s `.fms` as reference, R1 and R2
   each show up in one call.
2. **"Done" meant "the WP's new test is green".** DSP tests use fakes:
   `test_quantify_corpus.R` writes `"fake-ssff-data"`, and no test reads a
   real SSFF file. The status tables never checked for leftover skips or
   dropped sub-items. The 2.2.0 lesson "why the suite misses all of this" was
   applied to EQL and not to `quantify()`.
3. **Instances were fixed, patterns left.** A4 was fixed by safe assignment;
   error-to-`NULL` survived as an idiom, and the new executor brought the
   failure class back.
4. **Drift was watched in the wrong place.** The emuR fixtures are pinned;
   superassp, which moves weekly, was not.
5. **Way forward 3.1 was mostly right about what to drop.** It correctly
   dropped energy instrumentation, the CI benchmark job, EXPLAIN goldens,
   multi-version pkgdown, the invented 80 % floor and the false provenance
   invariant. One drop (N1.1) rested on a false premise (R3).
6. **The forward plan's critique was right in kind but executed too
   narrowly.** Its infrastructure items are moot. C3, C5 and C12 pointed
   at the right area.

## 5. Way forward

Principle: the package exists to measure speech, so the order is:

1. Measurements, checked against emuR's own output.
2. The superassp contract that measurements depend on.
3. Finish or delete what was marked done.
4. Documentation.

One WP, one commit, test first. A WP's proof is either a comparison with emuR
or a test that fails on `ec1aa02`. A WP is not done while any `skip()` names
it.

### Phase G0: measurement fidelity (release 3.1.1)

**G0.0 `CLAUDE.md` (S).** Replace it with about one page of statements that
hold: verbs, files, cache layout, test commands, and how to add a parity
case. This comes first because every later WP is executed from it.

**G0.1 DSP parity harness (S–M).**
- Add `tests/testthat/test_dsp_parity.R` and
  `fixtures/trackdata_golden.rds`. The goldens come from
  `tools/make-trackdata-golden.R`; emuR stays test-only, as for the EQL
  goldens.
- Reference: `get_trackdata(ae, "Phonetic =~ .*", "fm")` with
  `cut` ∈ {0.25, 0.5, 0.75}, plus full trajectories.
- Compared:
  - function mode with `superassp::trk_formant_forest`,
    `.use_metadata = FALSE`;
  - character mode `"fm"`.
- Exact equality on values and frame times. Failing cases skip with the WP
  that fixes them.

**G0.2 `.at` by time (S).**
- Select the frame nearest to `start + .at * (end - start)` in both modes.
  Window-mode frame times are window-relative, so add the window start.
- Caches hold tables and raw results from before selection, so `.cache_schema`
  should not need a bump. Verify that before committing.
- NEWS: about half of all midpoint values change, toward emuR.
- Proof: the G0.1 function-mode `cut` cases pass.

**G0.3 Character mode reads each file once (S–M).**
- Per bundle: read the track file once with `zero_to_na = FALSE`, then slice
  with the function-mode code (`.slice_table()` plus the G0.2 selection). That
  also allows `.at = NULL` (full trajectories, as in emuR).
- A read failure is a classed error naming the file, never a recompute.
- Delete the per-segment loop and the generator-recompute branch. A missing
  file gives one classed error that points to
  `quantify(corpus, .using = …)`.
- Proof: G0.1 character-mode cases pass; 253 segments take < 0.2 s (now 4.4 s).

**G0.4 Errors are not fallbacks (S).**
- `.dsp_whole_table()` separates two outcomes:
  - The routine failed: rows are `NA`, plus one classed `reindeer_dsp_warning`
    that lists the bundles.
  - The routine returned no track: take the window path, as now.
- Corpus mode counts a bundle as done only when its output file exists. Zero
  files written → abort, with DBconfig untouched.
- One test greps the three quantify files for `error = function(e) NULL`.
- Proof:
  - The R5 repro, as a test.
  - A routine that writes nothing → abort, DBconfig unchanged.

**G0.5 superassp contract (S).**
- `test_superassp_contract.R` runs against the real package for forest, ksv,
  rms and one spectral routine. It checks that:
  - `toFile = FALSE` returns an `AsspDataObj` with `sampleRate` and
    `startTime`;
  - `toFile = TRUE` writes `<bundle>.<ext>` (return value ignored);
  - the `read_track()` call that G0.3 uses returns the expected frame;
  - the existing formals-map check still passes.
- DESCRIPTION: `superassp (>= 3.2.3)` (D12).
- CI: add a weekly `schedule:` to R-CMD-check so upstream drift fails without
  a reindeer push, and drop the Windows `continue-on-error`.

Gate G0: zero DSP-parity skips, the R4 and R5 repros pass, and the contract
test passes on superassp HEAD. **Release 3.1.1.**

### Phase G1: finish or delete (release 3.2.0)

- **G1.1** `bind_rows(s, s)`: add `vec_ptype2`/`vec_cast` for `segment_list`
  and lift the skip. S.
- **G1.2** `.rename_args()` skips pairs where old == new. Proof:
  `annotate(port = 1)` raises no warning. S.
- **G1.3** EGG (D9: delete). Delete `companion_eggstract.R`, `eggstract`
  from Suggests, and the eggstract workarounds and comments in CI. S.
- **G1.4** Remove the arguments deprecated before 3.0 (`.parallel`,
  `.workers`, `.optimize`, `quick`, the 2.6 camelCase aliases) per the NEWS
  policy (D10), plus `import_media(annotations =)`. Tests switch to the
  current names. S.
- **G1.5** Move the DSPP regeneration to `data-raw/DSPP.R` and drop `imputeTS`.
  Proof: shipped `DSPP` unchanged; `R CMD check` unchanged. S.
- **G1.6** Add `object_usage_linter` and fix its 56 hits. S.

### Phase G2: documentation (with 3.2.0)

- **G2.1** Interoperability vignette: `get_trackdata(db, s, "fms")` maps to
  `quantify(s, "fm")` (after G0.3), and `.at` is emuR's `cut`.
- **G2.2** Mark the forward plan and way forward 3.1 superseded; move older
  `dev/` assessments to `dev/archive/`. Release checklist:
  `grep -rn 'skip("WP' tests/` lists no WP that is marked done.

### Decisions

| # | Question | Decided (2026-10-21) |
|---|---|---|
| D9 | EGG: finish or delete? | **Delete.** Also removes CRAN blocker 2 and the CI workarounds |
| D10 | Remove pre-3.0 deprecated arguments in 3.2.0 or at 4.0? | **3.2.0**, as NEWS policy says |
| D11 | Ship G0.2 as a bug fix although numbers change? | **Yes, 3.1.1**, with a NEWS entry |
| D12 | superassp release with the `toFile` fix | **superassp 3.2.3** (installed): `toFile = TRUE` writes the file and returns a count. Pin `>= 3.2.3` |

### Not doing

- Energy instrumentation, CI benchmark gate, EXPLAIN goldens, multi-version
  pkgdown: the way forward 3.1 reasons stand.
- Renaming first arguments, or unifying `dsp_function`/`.using`: a breaking
  change for cosmetics. Document both in `?quantify`.
- The 26-export target: 31 is fine.
- First open of 700 bundles: 2.2 CPU-s, once per corpus.
- CRAN: `CRAN_DECISION.md` stands. G1.3 removes one blocker.
- Automatic substitution of a registered track for a function call: a
  stale-norms risk of the A5 class. G0.3 delivers the read-from-disk benefit
  when the user asks for the track by name.

## 6. Releases

| Release | WPs | Character |
|---|---|---|
| 3.1.1 | G0.0–G0.5 | Measurements agree with emuR; no silent fallbacks; superassp contract tested |
| 3.2.0 | G1, G2 | Finished or deleted leftovers; docs follow |

## 7. Status

| WP | State |
|---|---|
| G0.0–G0.5 | open |
| G1.1–G1.6 | open |
| G2.1–G2.2 | open |

## Appendix: one-call repros (on `ec1aa02`)

```r
corp <- demo_corpus()
segs <- collect(query(corp, "Phonetic =~ .*"))

# R2: no track columns, no warning
names(quantify(segs, "fm", .at = 0.5))

# R1: frame choice vs emuR (122 of 253 differ)
a  <- quantify(segs, superassp::trk_formant_forest, .at = 0.5, .use_metadata = FALSE)
db <- emuR::load_emuDB(corp@basePath)
e  <- emuR::get_trackdata(db, emuR::query(db, "Phonetic =~ .*"), "fm",
                          cut = 0.5, resultType = "tibble")
# compare a$frame_time with e$times_orig / 1000 after matching on bundle + start

# R5: a routine failing on its 2nd call -> that bundle silently windowed
i <- 0
flaky <- function(listOfFiles, ...) {
  i <<- i + 1
  if (i == 2) stop("boom")
  superassp::trk_formant_forest(listOfFiles, ...)
}
quantify(segs, flaky, .at = 0.5, .use_cache = FALSE)   # no warning; msajc010 differs
```
