# reindeer 2.2.0 assessment

Date: 2026-10-11. Target: `main` @ `b15b50a` (2.2.0), 58 R files / 20.8k lines,
68 exports, 77 Rd pages, 6 vignettes, 46 test files.

Scope, in the order requested: (1) runtime power, (2) performance,
(3) standards and maintainability, (4) vignettes, (5) reference docs,
(6) superseded functionality, (7) pkgdown, plus (8) a minimal,
verb-centred user interface.

Supersedes `dev/REINDEER_PACKAGE_ASSESSMENT.md` (1.1.1) and the open items of
`dev/REINDEER_REMEDIATION_PLAN.md`; the plan's status is reconciled in §11.

## How this was produced

Six read-only reviewers in parallel (query engine; quantify/DSP/cache;
metadata/corpus/FAIR; segment_list/lazy/provenance; docs/vignettes/pkgdown;
Artic/interop/legacy + dead-code and import sweep), plus direct measurement
and re-verification of every headline claim. Test corpora: the bundled `ae`
demo (7 bundles, 253 Phonetic segments) and synthetic copies of it with 504 and
700 bundles. R 4.6.1, M1 Pro 10 cores, superassp 3.2.0, emuR 2.6.0.

- `[V]` = verified by reading the code and/or running it. `[I]` = inference.
- Some timings were taken while the machine was loaded (load avg ~26); ratios
  held across repeats, absolute numbers are noisy.
- Repro scripts: the session scratchpad (`wf*.R`, `q/`, `dsp/`, `meta/`, `seg/`).

Baseline: `devtools::test()` 0 failures, 1 skip. `R CMD check --as-cran` on the
2.2.0 tarball: **1 NOTE** (see §3.1). Test run costs 280 CPU-s.

---

## Executive summary

The package builds clean and its tests pass, but the user-facing core does not
yet meet its own specification, and the test suite cannot see it. The
defects that matter most are not slow paths; they are **silently wrong
results**. Three examples, each reproduced:

1. `v <- query(corp, ...); nrow(v); quantify(v, ...)` returns **no
   measurements**, and `collect(quantify(v, ...)); collect(v)` returns `v`
   **with** measurements. Lazy objects share one materialisation cache.
2. Age/Gender set at **session** level (the README's own example) never reach
   DSP: `seg_params` is `NULL`, superassp runs on defaults, no warning. Even at
   bundle level, `trk_formant_forest` ignores the passed norms because reindeer
   never maps `Gender` to the routine's `gender` argument. Median ΔF1 between
   a 6-year-old girl's norms and defaults: **0 Hz**.
3. EQL parity: every SEGMENT start/end is off by half a sample vs `emuR::query()`;
   `Phonetic = t` returns 253 rows (emuR 17); `Phonetic == 'm'|'n'` returns 0
   (emuR 19); `Start(Word,Phonetic)==1` returns 0 (emuR 54);
   `ascend_to()`/`descend_to()` return 0 rows for any multi-level move.

On power, one design decision dominates: **the package starts and tears down
its own `future::multisession` worker pools inside calls.** For the README's
24-vowel `quantify()`, 93% of wall time is worker boot/teardown (2.2–3.4 s vs
~0.3 s of DSP). The second lever is **running the DSP once per segment instead
of once per bundle** (14–31× more DSP work than needed). The third is the
**annotation cache build** (per-item `tibble()`: 153 s for 700 bundles where
emuR takes 9 s and a vectorised prototype 1.3 s).

The interface can shrink from **68 exports to ~26** without dropping any
capability (§8): bookkeeping (cache sync, metadata gathering, FAIR
regeneration, cache size checks, materialisation, worker plans) becomes
automatic, and inspection moves into `print()`/`summary()` methods.

### Ranked action list

| # | Action | Area | Effort |
|---|---|---|---|
| 1 | Give each lazy plan step its own `.state`; keep one ordered step list | correctness | S |
| 2 | Fix result misalignment on DSP error (`raw_results[i] <- list(...)`) | correctness | S |
| 3 | Cache key: add signal mtime+size and real function identity (name+package+version) | correctness | S |
| 4 | DSP params from inherited metadata; per-routine argument map incl. `gender`; warn when a norm is not applied | correctness, core promise | M |
| 5 | Never call `future::plan()` inside the package; run under the user's plan (default sequential) | power | S |
| 6 | Register `collect`/`glimpse` on `dplyr::collect`/`pillar::glimpse`; fix print header | correctness, UX | S |
| 7 | EQL: half-sample times, `=`, quoted alternatives, ORDER, reject unknown args; then a tokenizer + recursive-descent parser and one EQL→SQL compiler | fidelity | S then M–L |
| 8 | Multi-hop hierarchy navigation (recursive CTE over the config path) | fidelity | M |
| 9 | DSP once per bundle (or reuse the on-disk track), slice segments | power | M |
| 10 | Vectorised, incremental (md5/mtime-gated) annotation cache build | power | M |
| 11 | Delete dead code (~1,870 lines, C++ layer, autosync trio) and dead assets | maintainability | M |
| 12 | Consolidate API (§8) behind one deprecation cycle | interface | M–L |
| 13 | Vignettes/reference/pkgdown rebuilt on the consolidated API, examples executed | docs | M–L |

Do 1–8 before the power work: several power fixes (per-bundle DSP, cached
results) would otherwise faithfully reproduce wrong output faster.

---

## A. Silently wrong results (fix first)

These are fidelity defects in the sense of the brief: output differs from what
the function promises, with no error or warning. None is covered by a test.

**A1. Lazy plans share one materialisation cache.** `[V]`
`.state` is an environment (`R/reindeer_lazy_segment_list.R:58-63`); S7
copy-on-modify copies the object when `quantify()`/`biographize()`/`scout()`
append to `@query_parts` (`R/segment_list_quantify.R:411`,
`R/metadata_import_export.R:404`, `R/reindeer_sequence_ops_optimized.R:139,435,626`)
but keeps the pointer. `collect_lazy_impl()` returns and writes the cache through
it (`:104-106`, `:174-175`).
- `nrow(v); collect(quantify(v, F, .at=.5))` → no F1 columns.
- `collect(quantify(v, ...)); collect(v)` → `v` comes back with F1 columns.
- `collect(q1); scout(q1, 1, collect=FALSE)` → 253 rows (correct: 246).
- README order (`nrow` → `quantify` → `biographize` → `group_by(Gender)`) errors
  "Column `Gender` is not found".
Fix: a `.fork_lazy()` helper that every plan-append uses. Also keep transforms
and post-transforms in one ordered list (today `biographize() |> scout()` runs
SQL first, then biographize, so `quantify() |> scout()` would measure the
scouted segments). S.

**A2. Session- and database-level speaker metadata never reach DSP.** `[V]`
`R/segment_list_quantify.R:190-196` reads `metadata_bundle` only. Session-level
`Age=6, Gender="Female"` → `seg_params` `NULL` for every segment; bundle-level
→ `nominalF1=714, numFormants=4, windowSize=25`. The README sets speaker data
at session level. Fix: reuse `get_metadata()`'s COALESCE query filtered to the
needed bundles. S.

**A3. Norms that do reach superassp mostly have no effect.** `[V]`
- `trk_formant_forest` ignores `nominalF1`/`windowSize` while `gender = "m"`
  (its default); reindeer never passes `gender` (DSPP has no lowercase `gender`
  column; `metadata_fields` only match a formal literally named `Gender`,
  `R/dsp_parameter_derivation.R:49,69-73`). Median |ΔF1| metadata vs defaults:
  0 Hz in every bundle. With `gender = "u"` the norms work (F1 256 → 298 Hz).
- 9 of 29 pitch/formant `trk_*` routines get no norms (argument names
  `minimum_f0`, `maxHzFormant`, `number_of_formants`, …) and no warning: the
  check at `R/segment_list_quantify.R:247-259` only fires when *all* parameters
  are missing.
- `numFormants` changes the output shape (F5/B5 only for some speakers).
Fix: a per-routine argument map (aliases + gender coding) owned in one table;
warn per routine when a derived parameter has no target; a test that a derived
parameter changes output. M. This is the package's headline promise.

**A4. A DSP error shifts results onto the wrong segments and into the cache.**
`[V]` `R/tidy_trackdata_helpers.R:596`: `raw_results[[i]] <- tryCatch(..., error = function(e) NULL)`
deletes the list slot. With a routine failing on row 4, `msajc010 674.50`
received `msajc003 340.20`'s result and the shifted results were cached under
the wrong keys. When the last segment fails the call aborts ("Supplied 252
items to be assigned to 253"; ksv on all 253 segments). Fix:
`raw_results[i] <- list(tryCatch(...))` + regression test. S.

**A5. The measurement cache serves stale or foreign results.** `[V]` Key
(`R/tidy_trackdata_helpers.R:546-550`) = session, bundle, start, end, caller's
expression text, parameter digest, `.at`.
- Replacing `msajc003.wav` (new mtime) → all rows `hit` with old values.
- `f <- trk_formant_forest; quantify(s, f)` then `f <- trk_pitch_ksv; quantify(s, f)`
  → `hit`, returns formants.
- `superassp::trk_formant_forest` vs `trk_formant_forest` never share entries;
  on the lazy path the "name" is a ~3 KB deparse of the S7 generic.
Fix: identity = `fn@name` + package + package version; add file mtime + size.
S. Once fixed, the cache can default to on (§1, P8).

**A6. EQL results diverge from `emuR::query()`.** `[V]` (~95 queries compared
on `ae`; four re-verified independently.)
- Half-sample offset on every SEGMENT time: emuR `start = (ss-0.5)/sr`,
  `end = (se+0.5)/sr`; reindeer `ss/sr`, `se/sr` (`R/query_parser.R:1695-1709`).
  `566.925` vs `566.95`. Navigation verbs set `sample_end` one short (`-1L` at
  `R/reindeer_sequence_ops_optimized.R:233,499,680`). S.
- Lazy path: `=` applies no filter (`R/query_executor.R:323` handles `==`/`!=`
  only): 253 rows vs 17. S.
- Quoted alternatives `'m'|'n'`: 0 vs 19. S.
- Unbracketed conjunction (valid EQL): `Text == always & Accent == S` 0 vs 1;
  three-way `[A & B & C]` parse error vs 4 rows. M.
- `Start/Medial/End/Num` follow one link only (`R/query_parser.R:1394,1439`):
  `Start(Word,Phonetic)==1` 0 vs 54; `Medial(Syllable,Phonetic)` 0 vs 99;
  `Num(Intonational,Word)<3` 7 vs 0. M.
- Nested sequences match on the start item only: `[[A -> B] -> C]` 17 vs 15;
  sequence-in-dominance 16 vs 8. Projection in compound/nested queries returns
  the wrong level (`[#Phoneme==n & Start(..)]` 4 vs 12). M–L.
- Lazy results have no ORDER BY (`R/query_executor.R:368,380,409`). S.
- `sessionPattern`, `bundlePattern`, `calcTimes`, `timeRefSegmentLevel`,
  `resultType` unsupported; the lazy path silently swallows them
  (`sessionPattern = "nomatch"` still returns 17 rows). S to reject, M to implement.
- Non-EQL input accepted silently (double-quoted labels, unquoted `^` regex
  that the docs teach, `[A -> B -> C]` → 0 rows instead of an error). S–M.
Why the suite misses all of this: the parity helper sorts rows and compares
with `tol = 1e-3`; only the lazy path is compared with emuR; there are no
emuR comparisons for conjunction, position functions, nesting, projection or
`requery_*`; `test_navigation_contracts.R:62` asserts `expect_gte(x, 0)`.

**A7. Hierarchy navigation is single-hop.** `[V]` `ascend_dt`/`descend_dt`
join `links` once (`R/reindeer_sequence_ops_optimized.R:446-525, 637-704`):
Phonetic→Syllable, Phonetic→Word, Phoneme→Word, Word→Phoneme all return 0
rows (emuR `requery_hier` 12/20/…). `descend_to` returns one row per child
where emuR returns one span per parent; ITEM times come back `NA`. On the lazy
path `apply_ascend`/`apply_descend` are swapped
(`R/reindeer_lazy_segment_list.R:324,359`) and `scout` ignores
`capture`/`count_from` (durations 83 ms vs 226 ms for `capture = 3`).
`ignore_bundle_boundaries = TRUE` labels items from another bundle with the
input bundle. The `getting_started` vignette's showcase pipeline returns 0 rows
because of this. Fix: recursive CTE over the configured level path, one
implementation for eager and lazy. M.

**A8. Metadata corruption paths.** `[V]` (spec: bundle > session > db; legacy
`<name>.meta_json` used only when no `METADATA.json` at that level.) Read-side
precedence and legacy fallback are **correct**. Write-side and type handling
are not:
- First write to a legacy-only bundle/session writes only the new key; legacy
  fields are lost (`R/corpus_metadata_io.R:70-74,123,182`). S.
- A `Date` value breaks `get_metadata()` and everything downstream
  (`R/metadata_core.R:709-724`). S.
- Mixed types per field take the first type seen (`:676-680`): `3L/3.7/"n/a"`
  → `3/3/NA`. S.
- Reopen does not see external edits (quick gate `R/corpus_class.R:188-197`). S.
- A malformed session file empties bundle rows before parsing
  (`R/metadata_core.R:251-258`), the mtime gate then reports "no change", and
  `corpus()` refuses to open; malformed bundle files are read as empty. M.
- `reset.before.add = TRUE` leaves the cache stale (`R/metadata_import_export.R:341-372`). S.
- `corp["0000","msajc01"] <- list(...)` writes to msajc010, 012 and 015
  (unanchored regex, `R/corpus_methods.R:67-80`); `corp["S", ] <- list()` errors. S.
- Excel round trip flattens the hierarchy (bundle sheet holds resolved values),
  writes numbers back as strings, and can fail after partial writes
  (`R/metadata_import_export.R:193`). M.

**A9. Other silent or broken paths.** `[V]` unless marked.
- `quantify(corpus, .using = fn)` with defaults ships a SQLite connection to
  multisession workers → `bad_weak_ptr`; missing output files are not
  regenerated on cache hit (`R/quantify_corpus.R:250-299,265-277`).
- Cached and computed rows come back in different order; size bands change
  row order, failure handling and leaked columns
  (`R/tidy_trackdata_helpers.R:629-631`).
- Internal columns leak into results: `seg_params, signal_file, file_exists,
  cache_key, cached_result, file_group_id, seg_params_digest, result` (the last
  a list column holding the whole `AsspDataObj` per row).
- `.at` silently ignored for `lst_*` outputs; vector fields recycle into extra
  rows; `.at` takes the nearest frame without interpolation.
- `demo_corpus(format = "flac")` errors (unpacks `aeflac_emuDB`, opens `ae_emuDB`;
  `R/reindeer_demodata.R:20-35`).
- Artic: media requests call `shiny::parseQueryString` (Suggests, ungated,
  `R/reindeer_serve.R:87`); `review(bundleListName =)` always aborts;
  SAVEBUNDLE has no rollback; `.parse_annot_json` reads non-existent
  `seqIdx`/`labelIdx` (`R/annotation_io.R:148,164`) so re-inserting a bundle
  fails on a UNIQUE constraint [V parser/insert, I for the serve path].
- Provenance: `left_join` row growth recorded as `rows_lost = -506` with no
  warning; `right/full_join` yield `NA` session/start still classed as
  `segment_list`; `rename()` keeps the class without validation;
  `group_by()`/`summarise()` drop provenance; `bind_rows(s, s)` errors (no
  `vec_ptype2`/`vec_cast`); SQL-side navigation records no step and no loss
  warning (`scout(99, collect=FALSE)` → 0 rows, silent).

---

## 1. Runtime power consumption

Ranked by energy saved in typical use.

**P1. The package manages its own worker pools.** `[V]` Five sites call
`future::plan(multisession)` inside a call and restore the old plan on exit:
`R/parallel_utils.R:70-79` (via quantify), `R/corpus_database.R:89`,
`R/metadata_core.R:333`, `R/quantify_corpus.R:245`. Each call boots fresh R
processes that load reindeer + superassp, then kills them.
- README `quantify()` of 24 vowels: 2.2–3.4 s wall; wall-clock profile: 93% in
  `.with_worker_plan` (`socketSelect` 2.0 s, `makeCluster` 0.8 s, teardown
  0.16 s). Whole-file DSP on all 7 files directly: 0.31 s.
- n = 100: workers 8.5 CPU-s vs 1.2 CPU-s sequential (7× energy) and slower
  under load.
- Metadata gather at 504 bundles: parallel 5.13 s vs sequential 1.14 s.
- Cache build at 700 bundles: 33 s wall but ~150 CPU-s across 10 workers.
- `options(reindeer.workers = 1)` is ignored by quantify:
  `R/segment_list_quantify.R:292-294` sets `.workers <- detectCores() - 1`
  before the policy helper sees it.
Fix: follow the `future` convention — the package declares *what* can run in
parallel (`future.apply::future_lapply`), the user's `future::plan()` decides
*whether*. Default sequential. Remove `.parallel/.workers/.optimize`,
`.with_worker_plan`, and furrr. S, and the largest per-call saving available.

**P2. DSP runs once per segment instead of once per bundle.** `[V]`
`R/tidy_trackdata_helpers.R:185-203, 586-609, 714-721`. Raw superassp over the
253 segments: per-segment loop 3.64 s; one call with begin/end vectors 2.92 s;
whole file ×7 0.26 s; whole file, one vectorised call 0.12 s (14–31×). ksv:
0.11 s vs 3.35 s. Measured fidelity of the whole-file approach at segment
midpoints: median |ΔF1| 0 Hz, max 5 Hz (n = 34); windowed ksv additionally
fails with "Attempt to access data before Begin-Of-File" on ~110 of 253
segments, which whole-file analysis avoids. Each superassp call also runs
`detectCores()` → `system("sysctl")` (~20% of a call; upstream fix in
superassp). Fix: compute (or read the registered SSFF/JSTF track from disk)
once per bundle, slice all segments from it; keep windowed analysis as
`.mode = "window"`. M.

**P3. Annotation cache build is per-item tibble construction.** `[V]`
`R/corpus_database.R:426-487`: 91% of build time inside `tibble::tibble()`.
700 bundles: 153 s serial; emuR `load_emuDB` 9.0 s; a vectorised prototype
(read bytes once, parse once, build per-level vectors, `rbindlist`, md5 from
the same bytes) 1.3 s with identical row counts. Each `_annot.json` is read
twice (`:309-310`). First open of `ae`: 2.8–4.1 s. M.

**P4. Staleness is either never checked or fully rebuilt.** `[V]` With the
default `quick = TRUE` nothing is checked (stale results after external edits,
A8); otherwise the cache is unlinked and rebuilt (`R/corpus_database.R:56-57`).
emuR re-validates 700 bundle md5s in 0.36 s. The metadata mtime gate never arms
when no bundle-level metadata exists (early returns skip
`.metadata_record_mtime`, `R/metadata_core.R:295-298,317-323`): 2,311
`file.exists()` per open at 504 bundles vs 3 when armed. Any one changed file
triggers a full metadata rebuild (`:251-258`), and metadata writes don't
record their own mtimes, so the next open rescans. Fix: one incremental,
mtime-then-md5-gated sync for annotations and metadata, upserting per bundle
(this is WP2.5). M.

**P5. FAIR regeneration rewrites everything on every metadata write.** `[V]`
With auto-CMDI on, writing an unchanged value costs 0.64 s at 504 bundles:
`get_metadata()` ×6, `load_DBconfig()` ×8, 1,024 `file.exists()`, 9 files
rewritten. A `.cmdi_dirty` sentinel forces `force = TRUE` and bypasses the
state-hash guard (`R/corpus_describe.R:491-499`); the sentinel is written even
when auto-CMDI is off. Fix: hash-gate only, compute metadata once and pass it
down, drop the sentinel. S.

**P6. `scout()` loops per segment.** `[V]`
`R/reindeer_sequence_ops_optimized.R:252-334`: 25.3k segments 39.2 s vs
`emuR::requery_seq` 0.36 s (109×); 2k segments 2.2 s vs 49 ms. One non-equi
join + grouped aggregate. S–M.

**P7. Connection and lookup churn per query.** `[V]` Navigation verbs build a
new `corpus()` per call when `.from` is absent (~60 ms + an open SQLite handle
each, `:207,463,654`); a lazy query + collect opens 2–3 connections and never
uses `corpus@.connection`; the `labels(db_uuid,name,label)` index is never
used because no query constrains `db_uuid` (`EXPLAIN` → `SCAN l`, ~11 ms per
leaf at 84k labels, linear); attribute→level probes full-scan (3.6 ms ×2–4 per
query) though the mapping is in DBconfig; `get_db_uuid()` re-reads DBconfig
though `corpus@.uuid` exists. S each.

**P8. The measurement cache is opt-in and its hits are expensive.** `[V]` Warm
hits store raw `AsspDataObj` and re-flatten per row: `as.data.frame.AsspDataObj`
40–54% of warm time. n = 253: cold 4.9 s → warm 0.57–0.77 s; n = 20: 0.40 → 0.07 s.
Fix: cache flattened per-bundle tables, assemble with one `rbindlist`; after A5
make `.use_cache = TRUE` the default so iterative analyses stop recomputing. S–M.

**P9. Package load attaches Suggests.** `[V]` `library(reindeer)` 0.68 s,
30 namespaces including ggplot2 (0.45 s alone), dplyr, scales, gtable,
RColorBrewer: `.onLoad` calls `requireNamespace("ggplot2"/"dplyr")` to register
methods (`R/zzz.R:66-121`). Fix: delayed registration (`vctrs::s3_register()`
pattern, or `rlang::on_package_load()`), which hooks the target package's own
load. S.

**P10. Compiled code nobody calls.** `[V]` `src/corpus_config_helpers.cpp`
(8 Rcpp exports) is reachable only from dead R functions in
`R/reindeer_corpus_config.R`; every source install still compiles it. Together
with RcppSimdJson (which parses DBconfig then discards it for a jsonlite
re-parse, `R/json_utils.R:53-58`), wrassp (one WAV-header read) and
jsonvalidate (pulls V8 for one soft check), four compiled dependencies sit on
the hard path. Delete/move to Suggests → pure-R package. S.

**P11. Artic server.** `[V]` No busy-poll (good). But media and static assets
are `readBin()`-ed whole into R on the main thread per request
(`R/reindeer_serve.R:635-640`), and the server is never stopped (disconnect only
closes the websocket; no `.onUnload`). Fix: httpuv `staticPaths` for the dist,
file bodies for media, stop on disconnect and unload. S.

**P12. CI.** `[V]` Tests take 280 CPU-s per run × 5 OS/R combinations per
push, and print tibbles/messages into the log. Share one fixture corpus per
file, run DSP-heavy files on one OS only. S.

## 2. Performance (latency only)

- Query latency on 700 bundles is mostly better than emuR `[V]`
  (median ms, emuR / lazy+collect / eager): simple 119/17/15; sequence
  177/73/70; dominance 324/88/61; `Start` 532/268/197; `Num` 475/167/161.
  Exceptions: deep dominance `[Word==F ^ Phonetic==t]` 268/338/481 (every hop
  re-joins `items`); nested sub-queries are executed at plan time and inlined as
  a 1.43 MB literal `IN (...)` list (`R/query_parser.R:552`) — compile them as
  CTEs. `deduce_item_times` walks every descendant per ITEM row (184 ms for
  5.4k Words). Eager conjunction/disjunction still merge in R (WP3.3).
- `benchmarking/benchmark_queries.R:37` times `query()` without `collect()`,
  i.e. plan construction only; its speedups are overstated.
  `benchmark_serialization.R` calls `qs::`, which is no longer used.
- Lazy SQL fusion works when used: `query(Phoneme) |> scout(1) |> ascend_to("Syllable")`
  74 ms / 12.8 KB with `collect = FALSE` vs 303 ms / 56 MB eager — but navigation
  defaults to `collect = TRUE`, so laziness ends at the first navigation verb,
  and every dplyr verb on a lazy list collects. `[V]`
- dplyr verbs on `segment_list` cost ~2× a tibble (0.5–1 ms fixed): S7
  construct + validate on every reconstruct and restore, plus `deparse()` for
  provenance. `print()` copies the whole table (12 MB at 100k rows) — use a
  `pillar::tbl_sum()` method and let tibble print. `[V]`
- `corp[i, j]` is 14× slower than `get_metadata()`; `import_metadata()` is
  O(N²) (6.5 s at 504 bundles). `[V]`
- Quantify assembly and pivots use per-row `lapply`/`cbind`; `dsp_parameters`
  reloads `data(DSPP)` per call (cheap, 1 ms). `[V]`

## 3. Standards, code quality, maintainability

**3.1 R CMD check `--as-cran`: 1 NOTE.** `[V]` `Remotes` field; Suggests not in
mainstream repos (superassp, eggstract); six `http://` URLs that 301 (DSPP,
semitones Rd); three 404s to private companion repos (README, end-to-end
vignette); **tarball 10.9 MB** vs CRAN's 5 MB — two copies of the same demo
data (`ae.tar.xz` 4.5 MB + `aeflac.tar.xz` 4.5 MB) plus the Artic dist (3.4 MB).

**3.2 One engine per concern.**
- EQL: the parser is regex templates plus a character-loop operator splitter
  with binary-only splitting and precedence by try-order
  (`R/query_parser.R:149,199,369`), and two executors (eager
  `execute_*_corrected`, lazy `build_*_sql`) that disagree (A6). Replace with a
  tokenizer + recursive-descent parser over the EBNF (~400 lines) producing an
  AST, and one AST→SQL compiler used by both paths. M–L.
- Quantify: three copy-pasted executors (`.process_by_file_batch`,
  `.process_parallel_io`, `.process_segments_vectorized`) selected by row-count
  band, plus `quantify.corpus` and the character-track mode each with their own
  metadata fetch and assembly. One per-bundle executor (P2). M.
- Annotation JSON: three parsers with different semantics
  (`R/corpus_database.R`, `R/annotation_io.R`, inline in `R/corpus_creation.R`). S.
- Metadata: three read paths and three write paths
  (`metadata_core`, `metadata_import_export`, `metadata_unified`,
  `corpus_metadata_io`); `gather_metadata` ≡ `load_metadata(source="files")`. M.
- Interop: `read_eaf` returns ms, `read_textgrid` seconds, column names differ
  from what the writers expect; both readers grow lists (O(n²)). M.

**3.3 Class design.** `[V]` Six related classes (`segment_list`,
`extended_segment_list`, `lazy_segment_list`, `track_long`, nest output,
`bundle_list`). `extended_segment_list` is a `segment_list` with extra columns
(`biographize()` returns one with `dsp_function = ""`) and duplicates
constructor, vctrs/dplyr hooks and ~200 lines of print/summary. The lazy class
needs ~30 hand-registered auto-collect methods and still fails in
`pivot_tracks_longer()`, `autoplot()` and `is_segment_list()`. S7 class names
are namespaced (`reindeer::segment_list`), which is why every S3 method is
registered by hand in `.onLoad`. `[I]` `new_class(..., package = NULL)` would
allow ordinary roxygen `S3method()` registration.

**3.4 Generic conflicts.** `[V]` reindeer defines its own `collect()` and
`glimpse()` generics. With dplyr attached after reindeer, `collect(lazy)` errors
"no applicable method"; with reindeer attached after, `collect(tibble)` and
`glimpse(tibble)` error. `annotate()` masks `ggplot2::annotate()` (forwarding
works, but `conflicted` users must choose).

**3.5 Conditions.** `[V]` Metadata/corpus files: 39 plain `cli_abort`,
2 classed, 15 `assert_that` (unclassed), 28 `error = function(e) NULL`
swallows, 12 `cat()`. A broken METADATA.json surfaces as an unclassed jsonlite
error that does not name the file. Goal (5) needs classed
`reindeer_metadata_error`/`reindeer_io_error` and warnings in place of the
swallows.

**3.6 Smaller items.** `[V]` `%||%` is defined twice with different semantics
(`R/reindeer.R:43` null-or-empty, `R/segment_list_pivot.R:280` null-only);
Collate makes the second win package-wide. SQL built with `sprintf()` at
`R/corpus_creation.R:197`. Artic save/media take client-supplied path parts
without traversal checks (bounded by `127.0.0.1`). Several functions close the
corpus's shared connection (`R/schema_validation.R:155`,
`R/corpus_creation.R:139,202,252`); the finalizer is registered after work that
can fail (`R/corpus_class.R:221`); `get_corpus_cached()` pins corpora in a
global env forever. `metadata.schema.json` declares draft-07 but is validated
with the draft-04 `imjv` engine and constrains `participant.age` while the API
uses flat `Age`. `parallel::detectCores()` is used but `parallel` is not
declared.

**3.7 Dependencies: Imports 25 → 15.** `[V]` Drop: assertthat (→ `cli_abort`),
furrr (→ future.apply), base64enc (→ `jsonlite::base64_enc`), RcppSimdJson,
Rcpp. Move to Suggests: imputeTS (only the `recompute = TRUE` table rebuild),
wrassp (one header read), jsonvalidate (V8), httpuv (Artic only, gate it).

**3.8 Naming.** `[V]` First argument for a corpus: `corpus_obj` (13 exports),
`corpus` (8), `emuDB` (`query`), `geom` (`annotate`). For segments: `seg` (10),
`.segments` (4), `segs_tbl`, `object`, `lazy_sl`, `x`, `data`. Patterns:
`session_pattern` vs `sessionPattern`. Pick `corpus` and one segment name, and
snake_case throughout, in the same release as §8.

## 4. Vignettes

All six render (getting_started 9.1 s, lazy_and_provenance 1.3 s,
metadata_management 14.7 s, cache_management 12.0 s, end_to_end 3.7 s,
interactive_annotation 1.1 s with global `eval = FALSE`). Gating is now
per-chunk. But what they show is often wrong or empty: `[V]`
- getting_started's lazy showcase: `ascend_to("Syllable")` from Phonetic loses
  23/23 rows (A7) and the tutorial prints a 0-row result.
- Prose says "returns a lazy_segment_list"; the output shows `segment_list`
  (the dplyr verb collected).
- lazy_and_provenance: Syllable times are all `NA`, so the filter demo yields 0
  rows; the "25 % warning" demo loses 9.4 % and is wrapped in
  `suppressWarnings()`.
- Speaker-aware DSP — the package's distinguishing feature — is never shown
  working: getting_started measures before setting metadata; its "three levels"
  chunk sets only the database level; end_to_end measures formants on the
  following phone ("w"); `metadata_management.Rmd:168` passes `corp` as `age`.
  (After A2/A3 it would also show no effect.)
- Superseded API in user vignettes: `add_metadata`, `gather_metadata`,
  `?enrich`, `emuR::add_ssffTrackDefinition`, unexported `get_emuDBhandle`;
  links into `dev/` files that don't ship.
- Internal columns, a literal `{nrow(x)} row{?s}` header, "7 bundle",
  "Total duration: 00:00:00" (the WAV duration read fails silently,
  `R/reindeer_cmdi.R:212-224`) and coercion warnings all appear on the site.
- Overlap: FAIR in 2 vignettes, cache in 3, lazy/provenance in 3, metadata in 3.
- Missing: EQL cookbook, speaker-aware measurement, building a corpus from
  recordings (`import_bundle`/`import_session` have no example anywhere), FAIR
  archiving + validation, Praat/ELAN round trip, plotting, emuR → reindeer
  migration table.

**Target set (8, after §8 lands):**
1. *Getting started* — `demo_corpus()` → query → quantify → biographize → plot,
   all output real, ends on a figure.
2. *Querying with EQL* — every operator, projection, position functions,
   `scout`/hierarchy navigation, laziness and provenance as a section.
3. *Speaker-aware measurement* — set Age/Gender at each level, preview with
   `dsp_parameters()`, measure, show the norm effect, cache behaviour, pivots.
4. *Managing metadata* — the three levels, legacy files, Excel round trip.
5. *Building a corpus* — create, import recordings (+ existing TextGrid/EAF),
   register tracks.
6. *Archiving (FAIR)* — what `describe_corpus()` writes, validation, automatic
   regeneration (absorbs `CMDI_VALIDATION.md`).
7. *Interoperability and migrating from emuR* — Praat/ELAN, function map.
8. *Reviewing and annotating in Artic* — ~150 lines, not 592.

Retire `cache_management` (into 3), `lazy_and_provenance` (into 2) and the
roadmap-laden `end_to_end_pipeline`.

## 5. Reference documentation (90/10 rule)

- **Examples don't run.** `[V]` 46 blocks use `@examplesIf interactive()`, 4 use
  `\dontrun`; only `demo_corpus`, `erb` and `semitones` execute, so the site
  shows no example output. 30 pages have no example. Run on `demo_corpus()`;
  gate only DSP on `requireNamespace("superassp")`.
- **Worst pages by share of usage/output text** `[V]`: `create_cmdi_metadata`
  ~35% (says "internal… use describe_corpus", wrong default profile,
  undocumented args, "planned additions"); `set_metadata` ~55% (the Rd is named
  `add_metadata`, duplicated description and return); `manage_cache` ~55%
  (mentions simulation/protoscribe caches; examples pass the `corpus` function);
  `scout` ~60% (the `collect =` caveat twice); `biographize` ~65% (no added
  columns listed, `segs_tbl`/`corpus_obj` args).
- **`@return` rarely says what comes back.** `biographize`, `ascend_to`,
  `descend_to`, `pivot_tracks_longer` name no columns; `quantify` promises
  `F1…B3` but returns `F1_Hz` with units; `describe_corpus` doesn't list the
  five files it writes.
- **Implementation leaks:** `query`'s "Deferral is partial: plan-time work…",
  `corpus`'s `@.uuid/@.connection`, `describe_corpus`'s internal function names,
  leaked link definitions at `man/query.Rd:83-85`.
- Examples reference undefined `corp` or `path/to/...`; typos on the package
  page ("managemnet", "compatability", "provides provides", "continnous").
- Rule of thumb for the rewrite: Description = what it's for (2–3 lines);
  Arguments; Value = class + columns; one runnable example that shows the
  output; a "Details" paragraph only where a user-visible distinction needs it.

## 6. Superseded functionality to remove

`[V]` unless marked. Roughly 78 unreachable functions / ~1,870 lines:
- Autosync trio (~1,030 lines): all of `R/reindeer_autosync.R` (16 functions),
  `R/reindeer_autosync_wrappers.R` (7; one writes legacy `.meta_json`),
  `R/interop_elan_autosync.R`; `corpus(sync_eaf=, sync_cmdi=)` and the `.sync`
  property (`sync_cmdi = TRUE` produces nothing). The working mechanism is
  `.auto_describe()`.
- `R/reindeer_corpus_config.R` dead helpers (~440 lines, two pairs of duplicate
  definitions, one calling a non-existent `load_DBConfig`, one infinite
  recursion fallback) + `src/` + `R/RcppExports.R`.
- `R/emuR_perspectives.R` (dead, and buggy), dead `json_utils` helpers,
  `get_metadata_field`, `set_metadata_validated` (contains `readline`),
  `retreat` + its lazy transform, `emu_ae`/`ae` aliases,
  `.cmdi_dirty` sentinel.
- Stubs: `enrich_egg` (since 1.3), `enrich` (2.0), `serve`/`serve_app` (2.2),
  `st` (soft-deprecated since 0.8.12). Write the policy down: one minor release
  as a classed stub, then delete.
- Assets: `tests/signalfiles` (~57 MB tracked, unreferenced), `inst/praat`
  (submodule + prosogram/Momel/DDK, unreferenced, already Rbuildignored) and
  `.gitmodules`, `inst/pymomelintsint`, `inst/examples` (calls removed
  `serve()` and a deleted file), `inst/templates`, one of the two demo
  archives, `inst/default_parameters.xlsx` → `data-raw/`; `inst/agents`,
  `inst/scripts/validate-cmdi.sh` → `tools/`. `R/reindeer_cmdi.R:673` looks
  for a non-existent `inst/cmdi`.
- Repo hygiene: tracked `.DS_Store`/`build.log`; untracked clutter (two
  tarballs 19.5 MB, `reindeer.Rcheck/` 31 MB, stale `docs/` 6.2 MB, `doc/`,
  `Meta/`, `.RData`, `.Rhistory`, `Rplots.pdf`, `bench.log`); `specs/` vs
  `dev/specs/`; ~25 dead `.Rbuildignore` patterns; `VIGNETTES_SUMMARY.md`
  (self-declared stale); NEWS before 1.0 → `dev/NEWS-archive.md` (NEWS is
  1,415 lines, 41 headings, a "future work (v1.1)" section).
- `CLAUDE.md` points at seven files that no longer exist, calls
  `lazy_segment_list` "not yet integrated" (query is lazy by default),
  presents unexported cache functions as API and recommends
  `add_metadata`/`gather_metadata`.

## 7. pkgdown

`[V]`
- `pkgdown::check_pkgdown()` fails: `import_bundle`, `import_session` missing
  from the index.
- Deprecated `enrich` sits in the public "Measurement" group; internal-keyword
  pages are listed publicly (`create_cmdi_metadata`, `derive_dsp_parameters`,
  `get_corpus_cached`).
- Root `CLAUDE.md`, `CMDI_VALIDATION.md`, `VIGNETTES_SUMMARY.md` are published as
  site pages (pkgdown picks up root `.md`); move them to `dev/`.
- Reference groups are by object type; the navbar article menu is maintained
  by hand and duplicates `articles:`; no og:image.
- README (home page): citation says 1.0.1, `inst/CITATION` says 0.4.10 with the
  old title (generate from `meta$Version`); mentions simulation and EMU-webApp;
  its `collect(plan)` breaks with dplyr attached (3.4); DESCRIPTION Title
  ("…Nordic Climate") differs from the site title.
- Target reference index, grouped by workflow once §8 lands:
  Open & build · Speakers · Query & navigate · Measure · Reshape & plot ·
  Audit · Archive · Interoperability · Review in Artic · Classes ·
  Deprecated. No internal pages.

---

## 8. A minimal, verb-centred interface: 68 → ~26 exports

Principle: an export earns its place only if it is a step in an analysis —
open, build, describe speakers, query, navigate, measure, join speaker data,
reshape/plot, audit, review, archive. Bookkeeping runs automatically.
Inspection belongs in `print()`/`summary()` of the classes. Nothing below
removes a capability; it removes a *name*.

### 8.1 The verbs

| Stage | Export | Replaces / absorbs |
|---|---|---|
| Open | `corpus(path, create = FALSE)` + `print`/`summary` | `peek_signals`, `glimpse(corp)`, `validate_corpus` (status line), cache size from `inspect_cache`/`check_cache_size`, `get_corpus_cached` |
| | `demo_corpus()` | `format =` argument, second archive |
| Build | `import_media(corpus, files, session, metadata = NULL, annotations = NULL)` (vectorised; optional TextGrid/EAF per file) | `import_bundle`, `import_session`, media branch of `[<-.corpus` |
| Speakers | `get_metadata(corpus, ...)` | `corp[i, j]` getter (or keep as sugar delegating here) |
| | `set_metadata(corpus, x, session = NULL, bundle = NULL)`; `x` = list, data.frame, or path to an exported workbook | `add_metadata`, `import_metadata`, `load_metadata`, `[<-.corpus` metadata branch |
| | `export_metadata(corpus, file)` (per-level raw values, round-trippable) | — |
| | `biographize(segs)` — corpus taken from `segs` | `metadata_cols` |
| Query | `query(corpus, eql, ...)` — lazy and transparent | `collect` export (register on `dplyr::collect`), `lazy_segment_list` constructor |
| Navigate | `scout(segs, n)` (negative `n` = backwards) | `retreat` |
| | `move_to(segs, level)` — direction inferred from the hierarchy, as `emuR::requery_hier` does | `ascend_to`, `descend_to` |
| Measure | `quantify(x, .using, .at = NULL, ...)` on segments or a corpus; eggstract routines dispatch like any other | `quantify_egg`, `enrich`, `enrich_egg`, `.parallel/.workers/.optimize/.cache_format` |
| | `dsp_parameters(x, .using)` — preview what `quantify()` will pass, per bundle | `derive_dsp_parameters` (internal) |
| Reshape | `pivot_tracks_longer()`, `pivot_tracks_wider()` | `nest_by_session/bundle/speaker` (→ `tidyr::nest(.by =)` with a provenance-preserving method) |
| | `segment_cols()`, `track_cols()` (tidyselect helpers) | `signal_cols`, `metadata_cols` |
| Plot | `autoplot()` methods (registered, not exported) with `type =` | `geom_formant_trajectory`, `geom_pitch_track`, `geom_label_tier` |
| Audit | `provenance(x)` (with `pct_lost`), `dropped(x)` | `dropped_rows` |
| Cache | `clear_cache(corpus, older_than = NULL, dry_run = FALSE)` | `manage_cache`, `inspect_cache` (→ `summary(corp)`), `check_cache_size` (automatic) |
| Interop | `read_annotations(path)`, `write_annotations(segs, path)` — format from extension, one schema (ms, `labels`, `level`) | `read_eaf`, `read_textgrid`, `write_eaf`, `write_textgrid` |
| Archive | `describe_corpus(corpus, output_dir = NULL)` — validates by default | `create_cmdi_metadata`, `validate_cmdi`, `validate_corpus` |
| Review | `annotate(x)` — a corpus opens for editing; a segment list opens as a playlist (corpus from `x`) | `review` (added 2.2.0, merge before it spreads), `install_artic`/`artic_info` (internal: the dist is vendored; a no-argument `install_artic()` aborts today because `pin.json` has no url/sha256) |
| Classes | `segment_list`, `as_segment_list()`, `is_segment_list()` (TRUE for lazy plans too) | `extended_segment_list` (merged), `lazy_segment_list` (internal) |
| Units | `semitones()`, `erb()` — or move to superassp | `st` |

Count: 26 (+2 if the unit helpers stay). The two RStudio gadgets stay
exported only if RStudio needs that for addin bindings `[I]`, with
`@keywords internal` so they leave the index.

### 8.2 What becomes automatic

| Today the user calls | Becomes |
|---|---|
| `gather_metadata()` / `load_metadata()` | mtime-gated sync inside `corpus()` and every metadata read |
| `corpus(quick = FALSE)` to see external edits | incremental md5/mtime sync on open (P3, P4) |
| `describe_corpus()` after edits / `options(reindeer.auto_cmdi = TRUE)` | default on, hash-gated, run after write verbs only, never on reads; never overwrites a hand-written README (own a dedicated filename or marker block) |
| `check_cache_size()` | warning on cache write when over threshold |
| `.use_cache = TRUE` | default on once keys are correct (A5) |
| `collect()` | materialisation on first data access (exists; fix A1) |
| `future::plan()` set by reindeer | the user's plan, default sequential (P1) |
| `validate_corpus()` | schema validation on every write; status in `summary(corp)` |
| `derive_dsp_parameters()` | inside `quantify()`, with a per-routine warning when a norm cannot be applied |

### 8.3 Conventions for the consolidated surface

- First argument: `corpus` for corpus verbs, `segs` (or `x`) for segment verbs.
  The corpus a segment list came from is recoverable from it, so verbs that
  need both (`biographize`, `annotate`, `quantify`) take only the segments.
- snake_case arguments throughout (`session_pattern`, not `sessionPattern`);
  dot-prefixed arguments only on verbs that also take `...`.
- Every verb returns the same class family and records one provenance step.
- Unknown arguments are an error (`rlang::check_dots_empty()` / `check_dots_used()`),
  not silently ignored (A6).
- One deprecation cycle: old names become classed stubs pointing at the new
  verb for one minor release, then go.

---

## 9. Proposed order of work

1. **Delete** (no behaviour change, shrinks everything below): §6 dead code,
   C++, autosync trio, assets, clutter; Imports 25 → 15. Gate: tests green,
   `R CMD check` unchanged.
2. **Silent-wrong results** A1–A5, A8, A9 with one regression test each
   (A1 lazy copy, A2 session-level norms, A3 norm changes output, A4 error
   mid-batch, A5 mtime + aliased function), plus 3.4 (`collect`/`glimpse`).
3. **EQL and navigation parity** A6, A7: first the S items (half-sample,
   `=`, quoted alternatives, ORDER BY, reject unsupported args), then the
   parser/compiler rewrite with an emuR parity suite that compares unsorted
   rows at exact tolerance and covers `requery_seq`/`requery_hier`.
4. **Power**: P1 → P9 → P5 → P2 → P3/P4 → P6/P7 → P8; record before/after in
   `benchmarking/` (fix the benchmark to collect first).
5. **Interface** §8 with deprecation stubs and NEWS.
6. **Docs** §4, §5, §7 on the new interface (doing this before step 5 means
   writing it twice).

## 10. Expected effect (measured baselines → targets)

| Workflow | Now | After | Basis |
|---|---|---|---|
| `library(reindeer)` | 0.68 s, 30 namespaces | ~0.2 s `[I]` | P9 |
| First open, `ae` | 2.8–4.1 s | < 0.5 s `[I]` | P3 prototype |
| First open, 700 bundles | 153 s serial / ~150 CPU-s parallel | ~1.3 s | P3 prototype `[V]` |
| Re-open after one external edit, 700 bundles | stale (quick) or full rebuild | ~0.4 s `[I]` (emuR revalidates in 0.36 s) | P4 |
| `quantify()` 24 vowels, cold | 2.2–3.4 s | ~0.3 s | P1 + P2 `[V]` components |
| `quantify()` 253 segments, cold | 3.4–5.4 s | ~0.3–0.5 s `[I]` | P2 |
| same, re-run | full recompute (cache off) | ~0.1 s `[I]` | P8 |
| `scout()` 25k segments | 39 s | < 1 s `[I]` | P6 (emuR 0.36 s) |
| Metadata write with FAIR regeneration, 504 bundles | 0.64 s even when unchanged | ~0 when unchanged | P5 |
| Exports | 68 | ~26 | §8 |
| Imports / compiled hard deps | 25 / 4 | 15 / 0 | 3.7, P10 |
| Tarball | 10.9 MB | ~6.4 MB (one demo archive) | 3.1 |

## 11. Status of the previous remediation plan

| Item | Status |
|---|---|
| WP1.2 autosync watches METADATA.json | done, but the code is unreachable (§6) |
| WP1.3 mtime-gated hashing | done for annot/metadata; DBconfig still md5'd per call |
| WP1.4 fast gatherer in `corpus()` | done; gate doesn't arm on early returns, reopen is stale (P4, A8) |
| WP1.7/B1 nested metadata serializer | done; new regressions: Date crash, type widening (A8) |
| WP1.8/B2 connection finalizer | done; undermined by shared-connection closes and global pinning (3.6) |
| WP2.1 worker policy | partial: clamped, but still spawns per call, option ignored (P1) |
| Cache band 21–100 | done; underlying executor has A4/A5/order bugs |
| WP2.2 enrich batching | partial (writes batched in `quantify.corpus`; reads per bundle; default path crashes, A9) |
| WP2.4 DSPP memo / needed-rows fetch | memo open (cheap); needed-rows done |
| WP2.5 incremental cache rebuild | open (P3, P4) |
| WP3.1 navigation read scoping | partial: `ascend_to` labels read unscoped; per-segment loop remains (P6) |
| WP3.2 one connection | partial (P7) |
| WP3.3 eager conjunction in SQL | open |
| WP3.4 assembly vectorisation | open |
| D6 superassp norms | formally closed (>= 3.0.0 pinned), substantively open (A2, A3) |
| Phase 5 docs | none of the five work packages finished (§4, §5, §7) |
| Phase 6 deletions | only the PSOCK helper removed (§6) |
