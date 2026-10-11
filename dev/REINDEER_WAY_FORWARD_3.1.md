# reindeer way forward, 3.0.0 → 3.1.0

Date: 2026-10-11. Base: `main` @ `3c6825a` (3.0.0).
Assesses `dev/REINDEER_FORWARD_PLAN_3.1.0.md` (the "forward plan") against the
tree as shipped and replaces its work list. The amendment programme
(`REINDEER_AMENDMENT_PLAN_2.2.0.md`) is complete; this is what is left.

## 1. Verdict on the forward plan

Its critique of the 2.2.0 pair is sound (CPU-s as proxy, fidelity proof for one
routine, schema gap). Its work list was written at `1b7d78f`, mid-Phase 1, and
**about a third of it describes a package that no longer exists**. The rest
mixes real gaps with speculative infrastructure.

### 1.1 Stale against 3.0.0 (verified in tree)

| Forward plan says | Tree says |
|---|---|
| 68 exports, 24 Imports (§4) | 31 exports, 18 Imports |
| `future::plan()` at 4 sites (§4) | 0 sites; `parallel_utils.R:20` tells users to set the plan |
| P8: closure cache key ignores environment | `.fn_identity()` (`R/tidy_trackdata_helpers.R:103`) digests captured variables. Residual hole is narrower, see N-1.3 |
| P5/N1.5: eviction needed | LRU-by-`accessed_at` eviction (oldest 25 % over a 1000 MB cap) exists in single and batch writers; `accessed_at` touched on hit; `clear_cache(older_than=)` exists |
| N1.2: per-call `sysctl` in superassp | `detectCores()` now runs once in superassp `.onLoad`, guarded by an option. Re-profile before spending anything |
| N2.3: `import_metadata`, `corp[i,j]` perf | `import_metadata` was removed in 3.0.0; the path is `set_metadata(workbook)` |
| N2.4: navigation defaults `collect = TRUE` | still true for `move_to`/`scout` but that is the documented contract; lazy fusion is a perf option, not a defect |
| N3.2: add R-devel to matrix | `ubuntu-latest devel` is already in the check matrix |
| N4.3: migration vignette table | there is no migration vignette (8 vignettes: annotating, archiving, building_a_corpus, getting_started, interoperability, metadata, querying, speaker_aware) |
| N6.2: untrack `end_to_end_pipeline.html`, benchmark `.rds/.csv/.png` | html gone (`vignettes/*.html` ignored); `benchmarking/*.rds` already ignored. Tracked: 2 CSV + 2 simulation PNGs only |
| N2.5: `library(reindeer)` ≤ 0.2 s | unreachable by design: dplyr/pillar/vctrs are Imports (D1). RESULTS_2.5.0 records 0.28 s, 16 namespaces |
| N5.1/N5.2 doc lint | `tests/testthat/test_doc_contracts.R` already asserts `pkg::fun` refs exist and documented columns match returned columns |

### 1.2 Real, still open

- **Schema mismatch (C9).** `inst/schemas/metadata.schema.json` is draft-07;
  `R/schema_validation.R:38` validates with the draft-04 `imjv` engine. The
  schema only constrains nested `participant.age`/`gender`, while the API
  stores flat `Age`/`Gender`, and `additionalProperties: true` everywhere, so
  the fields that drive DSP norms are **never validated**. Worse,
  `.get_schema_validator()` returns `NULL` when `jsonvalidate` is absent
  (Suggests since WP1.6), so "validate on every write" silently turns off.
  This is the one silent-wrong class left in the metadata path.
- **Benchmark scripts measure deleted code.** `benchmarking/README.md` and
  `run_benchmarks.R:100-101` still run `benchmark_json.R` (RcppSimdJson,
  removed) and `benchmark_simulation.R` (moved to erodex; the file no longer
  exists). `QUERY_BENCHMARKS.md` (23 KB) and the other scripts predate the
  rewrite. There is no single reproducible harness; `BASELINE_2.2.0.md` /
  `RESULTS_2.5.0.md` numbers have no script that regenerates them.
- **Measured misses from 2.5.0, unexplained follow-up:** first open of 700
  bundles 3.6 s vs 1.3 s target (1.8 s JSON parse, ~1 s inserts); vowel-subset
  `quantify` 0.73 s vs 0.3 s.
- **Cache-key residual.** `.fn_identity()` captures only *non-function*
  variables mentioned by name in the body of a closure. Two closures that
  differ only in a captured *function*, or global-env functions that call
  other global helpers, still collide.
- **Fidelity of whole-file DSP (C3/C12).** Equivalence is evidenced for forest
  only (median |ΔF1| ≤ 5 Hz). Pitch and spectral routines run whole-file by
  default with no recorded evidence, and the default-mode departure from
  emuR has no user-facing decision note beyond NEWS.
- **Coverage and lint are not gated** (`test-coverage.yaml` has no threshold;
  no `.lintr`). Current coverage is unknown, so an "80 %" floor is invented.
- **Fuzzing emuR parity** is a good idea: the A6/A7 defects were exactly
  parity bugs, and the golden set is 80 hand-picked queries.
- **CRAN** has a settled answer that the plan treats as open: `Remotes:
  superassp` and Suggests on a private repo (`eggstract`) make CRAN
  impossible until superassp is on CRAN. That is a one-paragraph decision, not
  an M-sized WP with a spike.

### 1.3 Wrong or not worth building

- **N0.2 energy via `powermetrics`/RAPL.** Needs sudo, not CI-able, platform
  specific, and the audience is phoneticians. CPU-s + peak RSS (already in the
  plan's N0.1) is the right proxy; say so and stop.
- **N0.3 CI benchmark job at 1.2×.** Shared-runner noise is far above 20 %;
  it would flake or be ignored. Run `tools/bench.R` locally at release against
  the committed baseline instead.
- **N1.1 auto-reuse of registered SSFF tracks for `.using = fn`.** SSFF files
  carry no slot for reindeer's parameter digest, so "name + extension +
  digest equals" needs a sidecar, and a stale track silently reused with the
  wrong speaker norms is the A5 failure class re-introduced. The benefit is
  already delivered: character-track mode reads registered tracks from disk,
  and the per-bundle cache re-runs 253 segments in 0.07 s. Drop.
- **N2.1 EXPLAIN-golden tests, prepared-statement cache.** Query latency
  targets are met and 80/80 parity holds. EXPLAIN text changes across SQLite
  versions, so the goldens would be brittle. Index audit only if a benchmark
  shows a miss.
- **N2.2 dplyr-verb overhead (≤ 1.2×).** No measurement shows a problem. Time
  it first.
- **N3.4 provenance invariant `rows_kept + rows_lost == input`.** False for
  joins and `bind_rows` (rows are added; `rows_added` exists since WP2.7).
  Restate as `kept + lost == input` for filter/navigation verbs only.
- **N3.5 and N6.1** are the same dead-code sweep; merge.
- **N7.2 multi-version pkgdown, dark theme.** The plan itself flags the risk.
  Keep single-version; NEWS is the archive.
- **N4.1 "numbers match `benchmarking/`" as a CI check.** Hardware-dependent
  numbers in a vignette rot. State orders of magnitude and link to RESULTS.

## 2. Way forward

Principle: spend effort where a user can still get a wrong answer or where a
number is claimed without a script behind it; skip infrastructure whose cost
exceeds the defect rate. One WP, one commit, test first, as before.

### Phase F0: reproducible measurement (S–M)

**F0.1 One bench harness.** `tools/bench.R` regenerates the
`BASELINE_2.2.0.md`/`RESULTS_2.5.0.md` tables (open ae / 700, reopen after edit,
quantify cold/warm 253, scout 25k, metadata write 504, library load): wall,
CPU-s (`proc.time`), peak RSS (`/usr/bin/time -l`), git sha, load average.
Writes `benchmarking/BASELINE_3.0.0.md`. Run on an idle machine (load < 4);
two runs within ±10 %.
**F0.2 Prune `benchmarking/`.** Delete scripts for removed code
(`benchmark_json.R`, simulation references, `QUERY_BENCHMARKS.md` if
superseded, `dspp` analysis), the 4 tracked CSV/PNG artifacts, and rewrite
`benchmarking/README.md` to list only what `tools/bench.R` calls.

### Phase F1: correctness leftovers (release 3.0.1)

**F1.1 Metadata validation that validates.** Make the schema match the API:
flat `Age` (number), `Gender` (string), plus the other canonical flat keys;
drop or keep nested `participant` only if something writes it (it does not).
Declare the draft the engine supports (`imjv` = draft-04; re-declare, or move
to a draft-07 engine). When `jsonvalidate` is missing, emit one classed
`reindeer_validation_skipped` warning per session instead of silently
returning `NULL`; decide whether to move `jsonvalidate` back to Imports (small
dependency; the 18 → 19 cost is cheaper than a silent no-op).
Proof: `Age = "six"` written at db/session/bundle level aborts with
`reindeer_metadata_error` naming the file; nested-only schema regression gone.
**F1.2 Cache-key residual.** Extend `.fn_identity()` to digest captured
functions' bodies one level deep and refuse (warn once, bypass cache) for
closures whose free variables cannot be resolved. Test: two closures
differing only in a captured function miss; same closure twice hits.
**F1.3 Fidelity matrix for whole-file DSP.** For one pitch routine, one
spectral routine and ksv on `ae`: whole-file vs `.mode = "window"` at segment
midpoints, tolerances recorded in `dev/WHOLE_FILE_FIDELITY.md`. Routines that
exceed tolerance go on an allow-list that defaults to window mode. Add a
short "why whole-file is the default and when to use window" section to the
`quantify()` reference (the user-facing decision note, C12).
Gate: tests green; `R CMD check --as-cran` unchanged from 3.0.0.

### Phase F2: measured performance (release 3.1.0)

Each item starts from F0.1 numbers; do it only if the number misses.

- **F2.1 First open, 700 bundles (3.6 s).** Profile the 1.8 s parse and 1 s
  insert split; candidates: parse only changed bytes in vectorised form,
  single multi-row insert, `PRAGMA` journal/synchronous during bulk sync.
  Target 2 s or record why not.
- **F2.2 Peak RSS.** `profmem` the three headline workflows; fix the top two
  allocations. Target ≥ 30 % peak-RSS reduction on the worst one, else close
  with the profile attached.
- **F2.3 Re-profile superassp threading** after F0; update the required
  superassp version in NEWS if upstream still costs > 10 % per call.
- **F2.4 `set_metadata(workbook)` scaling.** Time 100/250/504 bundles; if
  super-linear, single-pass fix. 504 bundles < 1 s.
- **F2.5 Idle teardown test.** `httpuv::listServers()` empty after `annotate()`
  closes and after unload; 5-minute idle CPU note. Test only.

### Phase F3: standards, bounded (release 3.1.0)

- **F3.1 Coverage baseline, then floor.** Run `covr` once, commit the number,
  set the CI threshold at (baseline − 2 pts) and per-file minimum only for
  `query_parser`, `query_executor`, `tidy_trackdata_helpers`, `metadata_core`.
- **F3.2 lintr, minimal.** `.lintr` with the repo's existing style
  (snake_case, 100-col off); fix `object_usage_linter` hits; no style churn.
- **F3.3 EQL fuzzer (local, not CI).** `tools/fuzz-eql.R`: grammar-driven
  generator → reindeer vs emuR on `ae`; failures minimised and added to
  `eql_golden.rds` via `tools/make-eql-golden.R`; a triage list of emuR
  quirks lives next to the parity helper. Document the add-a-case workflow in
  the `helper-parity.R` header (P12).
- **F3.4 Metadata precedence property test.** Random db/session/bundle writes
  assert bundle > session > db; one test file, base R generators.
- **F3.5 Dead-code sweep.** After F1–F2: orphaned helpers from the executor and
  parser rewrites, `companion_eggstract.R` if unused, leftover
  `benchmarking/` items. Exports unchanged; line count in the commit.
- **F3.6 CRAN decision note.** `dev/CRAN_DECISION.md`: non-goal while
  `Remotes: superassp` and private Suggests exist; revisit when superassp is
  on CRAN. Also `http://` URLs in `R/reindeer_cmdi.R`/`interop_elan.R` are XML
  namespace identifiers and must stay `http`.

### Phase F4: docs and site (release 3.1.0, only what is missing)

- **F4.1 Migration page.** One short vignette or article `migrating.Rmd`:
  emuR → reindeer and 2.x → 3.0 old-name → new-name map. Generate the 3.0 half
  from the "Removed in 3.0.0" block in NEWS.md so it cannot drift.
- **F4.2 Performance note.** A section in `getting_started` (not a new
  vignette): sequential default, user `future::plan()`, cache behaviour and
  `clear_cache()`, orders of magnitude with a link to `RESULTS_*.md`.
- **F4.3 `check_pkgdown()` in the pkgdown workflow** and ecosystem links
  (protoscribe, erodex, eggstract, emuR) in the navbar. Nothing else.

### Dropped (with reason in §1.3)

N0.2, N0.3 CI bench job, N1.1, N1.2 as a WP, N1.5, N2.1, N2.2, N2.5, N3.5/N6.1
duplicate, N4.1 CI numbers check, N4.2, N5.1/N5.2, N6.2 beyond F0.2, N7.2.

## 3. Releases

| Release | Phases | Character |
|---|---|---|
| 3.0.1 | F0, F1 | Last silent-no-op/wrong paths closed; measurement reproducible |
| 3.1.0 | F2, F3, F4 | Measured perf only where a target missed; fuzz, coverage, lint; migration page |

Gate F1: F1.1–F1.3 tests green, parity 80/80 unchanged.
Gate F2/F3: every perf claim has a before/after from `tools/bench.R`; coverage
at or above the committed baseline; `R CMD check --as-cran` clean except the
Remotes NOTE.

## 4. Status

| WP | State |
|---|---|
| F0.1–F0.2 | open |
| F1.1–F1.3 | open |
| F2.1–F2.5 | open (each conditional on its F0.1 number) |
| F3.1–F3.6 | open |
| F4.1–F4.3 | open |
