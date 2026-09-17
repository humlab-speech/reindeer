# reindeer package assessment

Target: `reindeer` 1.1.1 (`main`, commit `ba063ef`), 53 R files / 19,142 lines,
72 exports, 79 man pages, 6 vignettes, 34 test files / 5,697 lines.

Scope, in the order requested: (1) runtime power, (2) performance, (3) standards
and maintainability, (4) vignettes, (5) reference documentation, (6) superseded
functionality, (7) pkgdown.

## How this was produced

> **Update (same day):** running the test suite and exercising the documented
> workflows during planning turned up five further defects that this document did
> not list including one that breaks the README workflow outright. They are
> recorded as B1–B5 in `dev/REINDEER_REMEDIATION_PLAN.md` §2, which is the
> authoritative findings list from here on. Nothing below has been retracted:
> every finding in this file was re-verified and still holds.

Read-only recon by six parallel scouts over the query engine, DSP/cache path,
metadata/IO layer, legacy surface, documentation, and energy hotspots, followed
by line-level verification of every claim included here. Verification was done
against the working tree with exact `file:line` reads; anything I could not
confirm is either dropped or marked `[INFERENCE]`.

One micro-benchmark was run on this machine (R 4.6.1, M1 Pro, 10 cores,
`future` worker spawn cost). No package build, install, or test run was
performed, so nothing here depends on a warm dependency tree.

Appendix A lists claims that verification **refuted**. Read it before acting on
second-hand notes elsewhere in the repo; several plausible-sounding problems are
not real.

## Executive summary

Ranked by benefit per unit of work. IDs refer to the detailed findings.

| # | Action | Area | Effort | Evidence |
|---|--------|------|--------|----------|
| 1 | Stop spawning `future::multisession` workers inside calls; clamp to work units and prefer fork | power | S | `R/segment_list_quantify.R:222`, `R/tidy_trackdata_helpers.R:629`, `R/reindeer_enrich.R:298`, `R/metadata_core.R:331`, `R/corpus_database.R:88`, `R/reindeer_corpus_config.R:625` |
| 2 | Route the 21–100 segment band through the caching executor | power | S | `R/segment_list_quantify.R:222-236`, `R/tidy_trackdata_helpers.R:619-620` |
| 3 | Make `corpus()` use the parallel, mtime-gated metadata gatherer | power | S | `R/corpus_class.R:194,199` → `R/corpus_database.R:553` vs `R/metadata_core.R:222` |
| 4 | Gate autosync hashing on mtime; stop rewriting state on no-op; fix the dead `.meta_json` scan | power + bug | S | `R/reindeer_autosync.R:150-200, 226` |
| 5 | Let `serve()`/`serve_app()` accept a `lazy_segment_list` (collect it) | docs truth | S | `R/reindeer_serve.R:74-75`, `R/reindeer_serve.R:42`; broken examples at 4 doc sites |
| 6 | Make examples actually run (drop blanket `@examplesIf interactive()`) | standards | M | 44/47 example blocks never execute; `man/ascend_to.Rd` codoc mismatch is the visible symptom |
| 7 | Execute vignette chunks that only need `emuR`, keep DSP chunks gated | docs | M | all 6 vignettes gated on `REINDEER_EVAL_VIGNETTES`; CI never sets it |
| 8 | Replace the `scout_dt` per-segment R loop with one join | performance | M | `R/reindeer_sequence_ops_optimized.R:213-306` |
| 9 | Push `scout`/`ascend_to`/`descend_to` label and link reads into SQL | performance | M | `R/reindeer_sequence_ops_optimized.R:188-203, 425-440, 604-625` |
| 10 | Batch `enrich(corpus)` cache reads/writes with the helpers that already exist | power | S | `R/reindeer_enrich.R:338,356`; batched versions at `R/tidy_trackdata_helpers.R:372-460` |
| 11 | Delete unreachable autosync wrappers (and the one that writes legacy `.meta_json`) | cleanliness | S | `R/reindeer_autosync_wrappers.R:87-150` |
| 12 | Fix the stale doc set: `CLAUDE.md` pointers, README/CITATION versions, leaked link definitions | docs | S | Appendix B |

---

## 1. Runtime power consumption

The dominant energy cost is not arithmetic. It is process lifecycle and repeated
full-corpus I/O. Query correctness is solid, so all changes below can be made
without touching output.

### E1. Per-call R worker process spawn (largest single win)

Six places create a `multisession` future plan inside a function and restore the
old plan via `on.exit`. Each worker is a fresh R interpreter that must re-load
reindeer and its dependencies before doing any work. Measured cost on this
machine:

```
workers=2  spawn+first-task=0.77s  stop=0.05s
workers=9  spawn+first-task=1.13s  stop=0.10s
```

Sites, with the default they use:

| Site | Default workers | Trigger |
|------|-----------------|---------|
| `R/segment_list_quantify.R:222-236` | `detectCores() - 1` | >20 segments |
| `R/tidy_trackdata_helpers.R:629-650` | `availableCores() - 1` | called from the above |
| `R/reindeer_enrich.R:298-300` | `detectCores() - 1` | `.parallel = TRUE`, any corpus size |
| `R/metadata_core.R:330-332` | `min(4, detectCores() - 1)` | metadata scan |
| `R/corpus_database.R:17,88` | `availableCores() - 1` | cache build, >10 bundles |
| `R/reindeer_corpus_config.R:620-633` | `detectCores() - 1` | PSOCK `makeCluster` + `clusterExport` of the corpus object |

Two aggravating factors:

- The threshold for `quantify()` is 20 rows (`R/segment_list_quantify.R:222`), so
  a 21-segment query launches nine R processes. A `future_lapply` over 21 trivial
  items costs 0.10 s even with warm workers, against 0.0003 s for `lapply`.
- Workers are sized to cores, not to work. `.process_parallel_io()` splits by
  signal file (`R/tidy_trackdata_helpers.R:641`), so a three-bundle query still
  pays for nine workers and leaves six idle.

Fix, in order of preference: clamp `.cores` to `min(.cores, length(file_groups))`;
do not enter the parallel branch when the work unit count is 1 or the estimated
segment-seconds are small; set the plan once at package load on Unix with
`future::multicore` (fork, no package reload) instead of per call; expose a
`options(reindeer.workers = ...)` so a laptop can pin to efficiency cores.
Expected effect: ~1 s of CPU per call removed, proportional to core count.

### E2. The 21–100 segment band silently drops the cache request

`quantify(..., .use_cache = TRUE)` routes 21–100 rows to `.process_parallel_io()`,
whose signature takes no cache arguments (`R/tidy_trackdata_helpers.R:619-620`).
The documented promise (`vignettes/getting_started.Rmd`, README) is that the
cache skips work already on disk; in this band it never does. Repeated iteration
over a medium result set recomputes every time. Fix: send cache-enabled calls
through `.process_segments_vectorized()` (which has batched cache support at
`:454-457`), or thread `use_cache`/`cache_conn`/`cache_format` into the parallel
executor.

### E3. Autosync re-hashes the corpus on every call and writes state unconditionally

`detect_annot_changes()` walks the tree, MD5s every `_annot.json`
(`R/reindeer_autosync.R:150-176`), rewrites the state file at `:197`, and only
then checks whether anything changed (`:199-200`). MD5 streams the whole file, so
cost is O(total annotation bytes) per call, plus a pretty-printed JSON rewrite
and fsync even when nothing moved.

Same function family has a functional bug worth fixing in the same pass:
`detect_metadata_changes()` scans for `pattern = "^\\.meta_json$"`
(`R/reindeer_autosync.R:226`), the legacy name. Editing `METADATA.json` never
triggers a metadata sync.

Fix: gate hashing on `file.info()$mtime` against the last scan; move
`save_sync_state()` below the early return; change the pattern to
`METADATA\\.json$` (keep the legacy match if back-compat is required).

### E4. `corpus()` calls the slow metadata gatherer

Two implementations of the same job exist:

- `gather_metadata()` (`R/metadata_core.R:222`): parallel, mtime fast-path,
  bulk insert inside `dbWithTransaction` (`:251`, `:358`, `:441`), RcppSimdJson.
- `gather_metadata_internal()` (`R/corpus_database.R:553`): sequential, jsonlite,
  per-level delete-all plus reinsert, no gate.

The public verbs (`load_metadata()`, `gather_metadata()`) use the fast one. The
constructor uses the slow one (`R/corpus_class.R:194,199`). Every
`corpus(path, quick = FALSE)`, and every `quick = TRUE` open of a corpus whose
metadata cache is empty, pays the slow path. Fix: call `gather_metadata()` from
the constructor, or delete the internal variant and keep one implementation.

### E5. Cache rebuild is destructive and double-reads every annotation file

`build_emuDB_cache()` unlinks the cache (`R/corpus_database.R:56-57`) and, per
bundle, parses the annotation JSON and separately MD5s the same file
(`:308-309`). The `bundle` table already stores `md5_annot_json`, so an
incremental upsert keyed on it is available without schema changes. Hash the raw
bytes already read for parsing (`digest::digest(raw, algo = "md5",
serialize = FALSE)`) to remove the second read entirely.

### E6. `serve()` buffers whole media files

For requests without a `Range` header, or with the open-ended `bytes=0-` that
browsers send on first load, the handler reads the entire WAV into RAM
(`R/reindeer_serve.R:236`, helper at `:849`). The partial-content path that
streams via `file()` and `seek()` sits two lines below and is bypassed by
`bytes=0-`. Treat an open-ended range as `0..size-1` and go through the streaming
branch. Byte-identical response, bounded memory, no repeated full-file reads on
seek.

### E7. `enrich(corpus)` cache I/O is per bundle

`R/reindeer_enrich.R:338` reads with `.get_persistent_cache()` and `:356` writes
with `.set_persistent_cache()`, which is two statements per hit
(`R/tidy_trackdata_helpers.R:270-277`) and, per miss, an untransacted
`INSERT OR REPLACE` plus a `SELECT SUM(size_bytes)` full-table scan (`:315-333`).
Batched alternatives already exist and are tested: `.get_persistent_cache_batch()`
(`:372`) and `.set_persistent_cache_batch()` (single transaction, one eviction
pass, `:413-460`). Route the bundle loop through them.

### E8. Small fixed costs worth collecting

| Location | Cost | Fix |
|---|---|---|
| `R/interop_textgrid.R:19` | reads the whole TextGrid to sniff a 2-byte BOM | `readBin(path, "raw", n = 4)` |
| `R/query_parser.R:1417`, `R/corpus_describe.R:25` | re-parses `_DBconfig.json` although `corpus@config` is already in memory | pass the config through |
| `R/reindeer_serve.R:614` | re-reads and MD5s the annotation file just written | hash the in-memory string |
| `R/tidy_trackdata_helpers.R:221` → `R/cache_size_management.R:103-112` | recursive cache-dir walk + `file.info` per entry on every cache open | keep a running byte total (the `cache.size_bytes` column exists) |
| `R/segment_list_quantify.R:151-158` | fetches the whole `metadata_bundle` table then filters in R | reuse the parameterised `IN` query at `R/reindeer_enrich.R:252-271` |
| `R/dsp_parameters_public.R:38`, `R/reindeer_enrich.R:453` | `data(DSPP)` lookup + tibble copy per bundle | memoise the tibble in a package environment |

### E9. What is already efficient (do not touch)

No `Sys.sleep`, polling, busy-wait, timer, or task-callback anywhere in `R/`
(`serve()` is pure httpuv callbacks; `.onLoad` only registers methods). No
subprocess launches. Cache keys hash parameters and mtimes, never audio content.
`.process_segments_vectorized()` writes misses in one transaction. `qs2` is
preferred with an `rds` fallback and cross-format recovery.

---

## 2. Performance

The query engine is a parse-to-SQL engine: EQL compiles to parameterised SQL and
results are identical to `emuR::query()`, with measured speedups of 2.5–3.8x and
large memory reductions (`benchmarking/benchmark_summary.csv`: dominance query
57.9 ms → 15.3 ms, 3.48 MB → 31 KB). The remaining wins are in the R-side
post-processing and in read scope.

### P1. `scout_dt()` loops per segment

`R/reindeer_sequence_ops_optimized.R:213` iterates `seq_len(nrow(dt))`, subsets a
row, and builds a fresh `data.table` per segment into a preallocated list
(`:215-306`). This is the single hottest R-level construct in the navigation
path. One non-equi keyed join (`all_items_dt[dt, on = .(db_uuid, session, bundle,
level, seq_idx >= start_seq, seq_idx <= end_seq)]`) replaces the loop and the
per-row allocation.

### P2. Whole-table reads before filtering in R

`scout` selects every label row for the database (`:200-203`), `ascend` a
link-join filtered only by `db_uuid` (`:425-440`), `descend` the entire `links`
table (`:604`), plus all items at the target level (`:188-192`, `:438-440`,
`:609-611`). Cost scales with corpus size rather than result size. Restrict each
read to the item ids in hand, or express the whole operation as one SQL join.

### P3. Three connections per materialised query

`query()` opens one to build SQL (`R/query_executor.R:146`),
`collect_lazy_impl()` another (`R/reindeer_lazy_segment_list.R:119`), and
`deduce_item_times()` a third (`R/query_parser.R:1499`). Each is a `dbConnect`
plus `RSQLite::initRegExp`. Thread the existing connection through, and cache the
built `list(sql, params)` from plan time instead of rebuilding it in
`build_sql_from_parts()`.

### P4. Unmemoised level/attribute probes

`.resolve_level_attribute()` (`R/query_parser.R:394-407`) issues up to two
`SELECT`s per level reference and is called for every branch of a compound query.
A six-level query can fire eight probes. Memoise a level/attribute map per
connection environment, or build it once with `SELECT DISTINCT level FROM items`.

### P5. One DSP call per segment, when superassp accepts a vector

Every executor calls `do.call(dsp_function, list(listOfFiles = <path>, ...,
beginTime = seg$start/1000, endTime = seg$end/1000))` once per segment:
`R/tidy_trackdata_helpers.R:138-141`, `:535-538`, `:669-672`,
`R/reindeer_enrich.R:348-351`. superassp's wrappers accept `beginTime`/`endTime`
vectors matched to `listOfFiles`, and only fan out internally when
`n_files > 1`. Issuing one call per file with recycled file paths removes the
per-segment closure dispatch, argument recycling, and file-open bookkeeping
while producing identical per-window results. This is the highest-leverage
change for EGG-free DSP workloads and should be validated against
`test_quantify_segment_list.R` for bit-identical output.

### P6. Row-wise grouping and per-row tibble construction

`R/tidy_trackdata_helpers.R:580-612` groups by `seq_len(nrow(dt_all))` and builds
two tibbles per group; `:143-170` and `:684-701` do per-segment row slicing plus
`as_tibble()`. Replace with vectorised expansion (`rep()` the segment columns by
result-row count, `rbindlist()` the result frames, one `as_tibble()` at the end).

### P7. Eager conjunction/disjunction abandon SQL

`execute_conjunction_query()` uses base `merge()` plus a column subset
(`R/query_parser.R:876-886`); `execute_disjunction_query()` uses
`unique(rbind(...))` plus a data-frame `order()` (`:899-903`). The lazy path
already compiles both to a single SQL statement
(`build_conjunction_query_sql`, `build_disjunction_query_sql`). Having eager mode
reuse the lazy builders removes the divergence and the copies.

### P8. Copy-heavy driver code in `quantify()`

`as.data.frame(object)` at `R/segment_list_quantify.R:100`, `:147`, `:170`,
`:262`; `merge()` at `:174`; a per-row `lapply` over key strings at `:193`; a
per-row `vapply` digest of identical parameters at
`R/tidy_trackdata_helpers.R:483-485` (a 500-segment bundle hashes the same list
500 times). Index-based joins and one digest per distinct parameter list fix all
of these.

### P9. Literal `IN` lists for pre-executed sub-queries

`R/query_parser.R:482-487` splices sub-query identities into the outer SQL as
quoted literals, so SQLite re-parses a statement whose length grows with the
sub-result. Bind them as parameters or keep the sub-result in a temp table joined
by key.

### P10. Result-shape conversions

`as.data.frame()` conversions at the end of each sequence op
(`R/reindeer_sequence_ops_optimized.R:306`, `:506`, `:674`;
`R/reindeer_lazy_segment_list.R:135`) and `rbindlist(list(dt_cached,
dt_uncached), fill = TRUE)` at `R/tidy_trackdata_helpers.R:574` each copy the
full result. Worth revisiting only after P1/P6; the S7 `segment_list` wraps
`tbl_df`, so the downcast exists for downstream compatibility and should be
checked before removal.

---

## 3. Standards, code quality, maintainability

### S1. `R CMD check` state

From `reindeer.Rcheck/00check.log` (R 4.6.1, `--no-manual --no-vignettes`, run
2026-09-06): 7 WARNINGs, 1 NOTE, examples and tests OK.

| Check | Result | Detail |
|---|---|---|
| portable file names | WARNING | non-ASCII names under `inst/praat/praatdet/examples/`, `tests/signalfiles/EGG/Session 1,2` |
| non-ASCII in R code | WARNING | `R/segment_list_pivot.R` |
| dependencies in R code | WARNING | `loadNamespace`/`requireNamespace` for `eggstract`, `protoscribe` not declared in Suggests |
| Rd cross-references | WARNING | `man/inspect_cache.Rd` links to `list_cache_files`, which has no page |
| code/documentation mismatch | WARNING | `man/ascend_to.Rd` usage vs code (`...` vs `level/.from/.quiet/collect`) |
| Rd usage sections | WARNING | undocumented `...` in `ascend_to`; usage without alias in four `enrich.*`/`quantify.lazy_segment_list` pages; undocumented `preview` in `print.lazy_segment_list` |
| unstated deps in examples | WARNING | same `eggstract`/`protoscribe` gap |
| R code possible problems | NOTE | partial argument match `session`→`session_pattern` in `browse_corpus_gadget`; undefined globals `.data`, `median`, `seg_params`, `seg_params_digest`, `.cache_status` |

Two structural notes. The check was run with vignettes and manual skipped, so
`checking running R code from vignettes` never happened. And `options(warn=2)`
style cleanliness is not the goal here; the codoc mismatch is interesting only
because it is the *one* place where a doc error was caught, and it was caught by
luck. Appendix B has the fixes.

### S2. Examples almost never execute

Of 79 man pages: 47 have examples, 32 have none. Of the 47, 40 are wrapped in
`@examplesIf interactive()`, 4 in `\dontrun{}`, and **3 actually execute** during
`R CMD check` (`demo_corpus`, `erb`, `semitones`). The gate is deliberate (no
heavy companions in CI) but the consequence is that reference examples cannot
drift-check the API. Two visible symptoms already exist: the obsolete
`quantify(segs, corpus, tracks = "fm")` example in `man/segment_list.Rd`, and the
`ascend_to` codoc mismatch.

Recommended compromise: gate on capability, not on interactivity. Use
`@examplesIf requireNamespace("emuR", quietly = TRUE)` for corpus and query
examples (emuR is cheap and already in Suggests), keep `@examplesIf
requireNamespace("superassp", quietly = TRUE) && interactive()` only for the DSP
chunks, and let structured types (`segment_list`, `lazy_segment_list`,
`extended_segment_list`, `collect`, `print`/`summary` methods) carry examples
built from `as_segment_list()` on a literal tibble, which needs no corpus at all.
That converts a large block of the 32 example-free pages into executable
documentation at zero CI cost.

### S3. Vignettes are never executed either

All six vignettes set `eval = identical(Sys.getenv("REINDEER_EVAL_VIGNETTES"),
"true")` in their setup chunk, and no CI workflow sets that variable. Outputs
shown in the vignettes are hand-written comments. Consequence: four documented
`serve_app(corp, seglist = <query result>)` calls cannot work (see S4), and
specific claimed output such as the provenance table in
`vignettes/lazy_and_provenance.Rmd:72-90` has never been compared to a real run.

### S4. Documented contract vs implementation: `serve(seglist = )`

`serve()` is an S7 generic on `corpus` (`R/reindeer_serve.R:42`). Its only method
aborts unless `seglist` is a `segment_list` or `data.frame` (`:74-75`).
`lazy_segment_list` is a separate S7 class with no parent
(`R/reindeer_lazy_segment_list.R:33-41`), and `lazy_segment_list` is what
`query()` returns by default. So:

- the package's own example, `R/reindeer_serve.R:38`
  (`serve(corp, seglist = query(corp, "Duration > 500"))`),
- `vignettes/getting_started.Rmd:196`,
- `vignettes/interactive_annotation.Rmd:114`, `:133`, `:439`,

all abort as written. Fix at the source: in the `serve()` method, collect a
`lazy_segment_list` before validation (one line, no behaviour change for other
inputs).

### S5. Silent failure paths conflict with the stated reporting goal

Thirteen catch-all `error = function(e) NULL` sites swallow failures, including
in the FAIR pipeline: `R/corpus_describe.R:56`, `:595`,
`R/metadata_import_export.R:284`, `:334`, `R/reindeer_cmdi.R:117`, `:255`,
`:493`. Given the package's goal of robust reporting, these should at minimum
emit a classed warning or record a provenance entry. Related:
`suppressWarnings()` appears 16 times in `R/` (5 of them in `R/interop_textgrid.R`).

### S6. Deprecation handling is inconsistent

No roxygen `@deprecated` tag exists anywhere in the package. `st()` calls
`.Deprecated()` (`R/reindeer_psychoacoustics.R:27`), while `add_metadata()` and
`gather_metadata()` are documented in prose as "deprecated alias" in
`vignettes/metadata_management.Rmd:85,128` but emit nothing and carry
first-class roxygen blocks. Meanwhile the ten genuinely removed functions
(`R/deprecated_stubs.R`) abort loudly and correctly. Pick one policy: either add
lifecycle badges and `.Deprecated()` calls, or stop calling them deprecated in
prose.

### S7. Package weight

Installed size is 13.1 MB, of which `inst/extdata` 8.6 MB (two tarballs of demo
corpora) and `inst/praat` 3.0 MB (vendored Praat scripts). No R code references
`inst/praat`, `inst/pymomelintsint`, or `inst/praat/praatdet` (grep over `R/`
returns nothing), so 3 MB of third-party scripts ship on every install for no
runtime purpose. The demo corpora are used by tests and vignettes; consider
downloading on demand or splitting into a companion data package if install size
matters.

### S8. Imports audit

All declared Imports are genuinely used except the ones noted below. `Rdpack`
looks unused at first glance but is required: three Rd pages use `\insertCite`
with `inst/REFERENCES.bib`, and `NAMESPACE` imports `Rdpack::reprompt`. `wrassp`
is used once (`R/reindeer_cmdi.R:219`) and `imputeTS` twice
(`R/reindeer_signal_extensions_dt.R:177,180`); both are heavy for their footprint
but function correctly. `future`, `furrr`, and `future.apply` are all loaded;
`future.apply` could fold into `furrr::future_map`, and consolidating the
parallel API on one layer would simplify E1. `assertthat` (44 call sites across
11 files) is a superseded idiom in a codebase that already depends on `cli` and
`rlang`; migrating it during normal edits costs nothing at runtime but improves
consistency. DESCRIPTION lacks `RoxygenNote`, which suggests hand edits.

### S9. Repository and CI hygiene

`git status` is clean apart from `build.log`, and build outputs (`src/*.o`,
`*.so`, `docs/`, `Meta/`, `doc/`, `*.tar.gz`, `*.Rcheck/`) are properly ignored.
No action needed. Remaining nits:

- `.Rbuildignore` misses `CMDI_VALIDATION.md` and `tests/*.md`, so dev notes
  (`tests/AUTOSYNC_TEST_SUMMARY.md`, `TESTING_AND_BENCHMARKS.md`,
  `TEST_DOCUMENTATION.md`, `TRANSCRIPTION_FIDELITY_TESTS.md`) ship inside the
  source tarball.
- Several `.Rbuildignore` patterns match files that no longer exist
  (`^SERVE_.*\.md$`, `^SERIALIZATION_.*\.md$`, `^CI_CD_.*\.md$`, …).
- `CLAUDE.md` points at eight files that were deleted
  (`METADATA_SYSTEM.md`, `SERVE_FUNCTION_SUMMARY.md`,
  `CACHE_SIZE_MANAGEMENT_SUMMARY.md`, `SERIALIZATION_QUICK_REF.md`,
  `DRAFT_CACHE_SYSTEM_SUMMARY.md`, `SIMULATION_PREPROCESSING_SUMMARY.md`,
  `CACHE_OPTIMIZATION_SUMMARY.md`, `CI_CD_SETUP_SUMMARY.md`). Anything that
  relies on this file, human or agent, is being sent to missing documentation.
- `codecov.yml` ignores `R/deprecated` and `R/*DELETE*`, neither of which exists.

### S10. Test suite

34 files, 5,697 lines, edition 3, with a shared fixture layer
(`tests/testthat/helper-corpus.R`) that distinguishes read-only shared corpora
from isolated copies for mutating tests. Tests are behaviour-oriented
(`test_provenance.R` asserts row counts, warnings, and error classes), with 72
`skip_if_not_installed()` guards keeping the suite usable without companions.
This is a strength; no action.

---

## 4. Vignettes

Six vignettes, 1,419 lines, all registered in `_pkgdown.yml` under "Getting
started" and "Workflows". Coverage of the headline capabilities is good and the
prose is clear, task-first, and free of the usual AI tells (I scanned for them:
two uses of "simply", ten of "just", one of "robust" across vignettes, man pages,
README, and roxygen). The problem is not voice, it is evidence.

### V1. Nothing runs, and it shows

Every chunk is gated off (S3). Readers therefore never see a single real output
value in 1,419 lines. Under the requested 90/10 standard (usage and expected
output first), the vignettes are currently 100 % usage and 0 % output: they
describe results instead of showing them. Compare the current
`vignettes/lazy_and_provenance.Rmd:72-90`, which shows a plausible provenance
table, with the same table produced by a real run.

### V2. Broken examples reach the website

Four `serve_app(..., seglist = <lazy query>)` calls abort (S4). This is the most
visible failure for a new user, because `getting_started` is the fifth section
of the first vignette.

### V3. Stale claims

- `vignettes/metadata_management.Rmd:85,128` call `add_metadata()` and
  `gather_metadata()` deprecated; neither emits a deprecation warning (S6).
- `vignettes/metadata_management.Rmd:148-149` says `describe_corpus()` writes
  three artifacts; the code emits five (README, CMDI, DataCite, CITATION.cff,
  JSON-LD), as `vignettes/end_to_end_pipeline.Rmd` correctly states.
- `vignettes/end_to_end_pipeline.Rmd:201-205` promises `explain()`, filter
  pushdown, and DOI minting "slated for v1.1" while the package is at 1.1.1. There
  is no `man/explain.Rd`. Either cut the roadmap or move it to a NEWS entry.
- `vignettes/cache_management.Rmd:78` reaches into the internal
  `corp@.cache_dir` field instead of using a supported accessor.

### V4. Recommended restructuring

Keep six vignettes but re-cut them around workflows rather than subsystems:

1. **getting_started** (keep, tighten). End with a real printed result and a
   `sessionInfo()`-style trace of what happened, so the five-minute promise is
   verifiable.
2. **speaker-aware measurement** (new, merge the DSP halves of
   `getting_started` and `metadata_management`). This is the package's strongest
   differentiator and deserves one narrative: set metadata, inspect
   `dsp_parameters()`, quantify with `.at`, show the cache hit/miss column.
3. **metadata_management** (keep, fix V3, add the Excel round-trip with a visible
   before/after table).
4. **fairest_artifacts** (new). `describe_corpus()` currently gets one section; it
   is the FAIR/archival selling point and the hardest thing to get right. Show
   the five emitted files and how to validate the CMDI.
5. **lazy_and_provenance** (keep, real output, keep the 25 % warning story).
6. **interactive_annotation** (cut hard). At 563 lines and 28 chunks it is the
   largest vignette and documents the most peripheral feature; 150 lines with one
   end-to-end serve-review-save example is enough.

Delete `end_to_end_pipeline` or demote it to a README "capability tour" if the
companion packages cannot be installed in CI; a walkthrough that cannot run is
the most expensive kind of documentation debt.

### V5. Practical execution plan

- Gate chunks by capability, not by a global env var:
  `eval = requireNamespace("emuR", quietly = TRUE)` for corpus/query/metadata
  chunks, plus `superassp`/`eggstract`/`protoscribe` for DSP chunks. Most
  vignettes become executable in CI today; only the DSP chunks fall back to
  prose.
- Until superassp installs cleanly on R ≥ 4.6 (the interim CI note), render the
  site with a demo corpus (`demo_corpus()`, 8.6 MB already bundled) and
  `REINDEER_EVAL_VIGNETTES=true` in a job that pins R 4.5.
- Add one `knitr` chunk per vignette that prints the object the section is
  talking about (`print(head(x))` or `glimpse(x)`), so every claim about shape
  and columns is machine-checked.

---

## 5. Reference documentation

Structure is better than average: `man/query.Rd` leads with usage, lists the
returned columns and their meaning, separates supported syntax from pitfalls, and
cross-links related verbs. `man/quantify.Rd` and `man/inspect_cache.Rd` document
the output columns (`.cache_status`, `.time_point`), which is exactly the
expected-output orientation asked for.

### D1. Missing required pieces

- 32 pages have no examples, including core verbs: `collect`, `glimpse`,
  `enrich.corpus`, `quantify.lazy_segment_list`, `nest_by_*`, all `print.` and
  `summary.` methods, and `derive_dsp_parameters`.
- `man/enrich.Rd` does not document `with =` or `corpus_obj =`; both are the
  arguments that decide what the function does.
- `man/inspect_cache.Rd` links to `list_cache_files`, which is not documented or
  exported.
- `man/segment_list.Rd` carries an obsolete `quantify(segs, corpus, tracks = "fm")`
  example (removed API) inside `\dontrun`, so it can drift forever.

### D2. Nine man pages describe internals before use

Pages in the "internal" pkgdown bucket and several `enrich.*`/`quantify.*` method
pages lead with implementation notes (S7 dispatch, cache plumbing) rather than
what the caller gets. Apply the 90/10 rule when these are next touched: first
paragraph states what the call does and what comes back, one short example,
then at most a sentence of implementation detail.

### D3. Rendering defect on the website

`man/query.Rd:83-85` contains leaked markdown link-reference definitions:

```
[]: R:\%20
[A -> B]: R:A\%20-\%3E\%20B
[A ^ B]: R:A\%20\%5E\%20B
```

They come from `R/query_executor.R:57`, where `` `[A -> B]`, `[A ^ B]` `` inside
backticks is still treated as an autolink target by roxygen2 markdown. The
rendered page shows this garbage at the end of "Common pitfalls". Escape the
brackets (`\[A -> B\]`) or use `\verb{}`. Only `man/query.Rd` is affected.

### D4. Consistency of voice

The docs are plain and direct. Two things to keep an eye on when editing: the
em-dash rate in prose is 11–17 per 1,000 words in the vignettes and README
(getting_started 12.8, metadata_management 16.9), which reads heavier than the
man pages (0–9 per 1,000) and is the main remaining stylistic difference between
the two; and the second-person "you" register in the vignettes is fine, but the
man pages occasionally slide into third-person "the user". Pick one per surface.

### D5. Citation and version drift

`README.md` cites "R package version 1.0.1"; DESCRIPTION says 1.1.1.
`inst/CITATION` says "R package version 0.4.10" twice and gives a title
("Extend emuR for Speech Data in a Nordic Climate") that differs from the README
citation title. Make `inst/CITATION` read `meta$Version` so it can never drift,
and generate the README citation from the same source.

---

## 6. Superseded functionality

### X1. Ten redirect stubs (keep, retarget the messaging)

`R/deprecated_stubs.R` exports `draft_vad`, `draft_vot`, `draft_periods`,
`draft_momel_intsint`, `quantify_simulate`, `enrich_simulate`, `reminisce`,
`reminisce_tracks`, `list_simulations` as `cli_abort()` redirects. They are
deliberate and documented in NEWS. Keep until a major release, but they currently
occupy the pkgdown "internal" bucket, which is where the two *live* deprecated
aliases also hide. Give the removed-function stubs their own index section named
"Moved to companion packages" so a user searching the reference index finds them.

### X2. Unreachable autosync wrappers (delete)

`R/reindeer_autosync_wrappers.R` is 411 lines of `@noRd` code. Seven functions;
only `write_bundle_metadata` and `write_session_metadata` are called, and only
from `tests/testthat/test_autosync.R`. Worse, the session writer hardcodes the
legacy filename (`:106`), contradicting the single-source-of-truth rule stated in
`R/metadata_core.R:12` and documented everywhere else. Delete the five
unreachable wrappers and the two test-only ones, or move them into the test
fixture if they are genuinely test scaffolding.

### X3. `detect_metadata_changes` legacy scan (fix or delete)

Beyond the energy cost (E3), the function currently only detects legacy
`.meta_json` files, so its stated purpose (notice metadata edits and re-emit FAIR
artifacts) does not work for the format the package mandates. If nothing else
uses the legacy path, delete the branch and the related state keys.

### X4. Demo-corpus aliases (delete)

`ae()` and `emu_ae()` in `R/reindeer_demodata.R:40-41` are internal, unexported,
and have zero callers.

### X5. `tempdir()` cache fallback (delete or document)

`R/tidy_trackdata_helpers.R:197-199` falls back to
`file.path(tempdir(), "reindeer_cache")`. Both callers pass a resolved directory
from the corpus (`corpus@.cache_dir` defaults to `<db>/.quantify_cache`), so the
fallback is unreachable in practice and, if it ever ran, would silently discard
cached work at session end. Delete it and abort with a clear message instead.

### X6. Vendored Praat assets (delete from `inst/`)

`inst/praat` (3.0 MB), `inst/praat/praatdet`, and `inst/pymomelintsint` are not
referenced by any R code. They are the residue of draft-annotation work that now
lives in protoscribe. Removing them cuts installed size by roughly a quarter and
removes the `R CMD check` non-ASCII filename warning for
`inst/praat/praatdet/examples/`.

### X7. Two cache-key identity gaps (fix)

Both are latent correctness issues, not cleanup:

- `.precompute_cache_key_parts()` records
  `dsp_name = deparse(substitute(dsp_function))[1]`
  (`R/tidy_trackdata_helpers.R:81-85`), but its only call site
  (`:482`) passes the local variable `dsp_function`, so `substitute()` returns
  the symbol `dsp_function` and the captured string is always the literal
  `"dsp_function"`. Two routines whose parameters happen to have the same names
  and values collide. Note the contrast: the *display* name captured at the
  user-facing boundary (`R/segment_list_quantify.R:301-313`) is correct, because
  there `dsp_function` is the method's formal argument.
- `enrich`'s bundle key is `digest(list(full_path, mtime, dsp_params))`
  (`R/reindeer_enrich.R:332-336`), with no function identity at all, so a
  bundle-level "already processed" marker can suppress a later run with a
  different `.using`. The correct name is already in scope as `dsp_fun_name`
  (`R/reindeer_enrich.R:88`, threaded into `.enrich_corpus_impl` at `:92`), which
  makes both fixes mechanical.

### X8. Duplicate `gather_*` implementations (see E4)

Two implementations of "scan metadata into the cache". Keep one.

### X9. Autosync vs `describe_corpus` overlap (decision needed)

`R/reindeer_autosync.R` (698 lines) plus `R/reindeer_autosync_wrappers.R` (411)
plus `R/interop_elan_autosync.R` (243) implement an EAF/CMDI sync state machine
driven from the corpus constructor (`R/corpus_class.R:155-175`, `:184`).
Separately, `describe_corpus()` + `create_cmdi_metadata()` + `write_eaf()`
produce the same two artifact families on demand. They are not duplicates today
(the sync path maintains a state file and pushes per-save), but they are two
answers to "keep exports in step with the database". Before adding more to either,
write down which one is the supported path; my read of the docs and tests is that
`describe_corpus()` is the user-facing one and the sync machine is used mainly by
its own tests.

---

## 7. pkgdown

The site configuration is in good shape: `_pkgdown.yml` covers all 72 exports
across 15 topic sections plus an internal bucket, articles are registered, the
navbar is complete, and the CI workflow (build on push to `main`, deploy via
Pages) is valid YAML with a correct `with: path: docs` upload step.

### PD1. Internal notes are published

pkgdown renders every `*.md` file in the package root that is not `README`,
`LICENSE`, `LICENCE`, or `NEWS` (pkgdown `R/build-home-md.R::package_mds()`;
there is no `exclude:` option and `.Rbuildignore` is not consulted). The local
`docs/` tree is proof it has already happened here: `docs/CLAUDE.html` and
`docs/VIGNETTES_SUMMARY.html` are both present, both listed in
`docs/sitemap.xml`, and both indexed in `docs/search.json`. Anyone landing from
a search engine can read the agent instructions and the dev notes.

Fix, in order of preference:

- Move `VIGNETTES_SUMMARY.md` and `CMDI_VALIDATION.md` into `dev/` (pkgdown does
  not render subdirectories), and point `CLAUDE.md` at them.
- `CLAUDE.md` must stay at the root for tooling to pick it up, so delete its
  generated page in the workflow instead, right after the build step:

  ```yaml
  - name: Drop non-documentation pages
    run: rm -f docs/CLAUDE.html
  ```

  Add any future root-level dev note to the same line.

### PD2. Articles show code without results

Because the vignette gate is never set in CI and the pkgdown job installs only
hard dependencies (`dependencies: '"hard"'`), every article on the site renders
as unevaluated code. This is the site's biggest quality gap; see V5 for the fix.

### PD3. Reference index gaps

The deprecated aliases and the moved-function stubs (`add_metadata`,
`gather_metadata`, `import_metadata`, `serve`, `st`,
`deprecated-moved-functions`) sit in the internal bucket, so the index shows no
hint that a rename happened. Add a short "Deprecated and moved" section (X1).

### PD4. Landing page

`home.description` and the `home.title` are accurate. Recommend adding the three
capability pillars as a bulleted block on the index (speaker-aware DSP driven by
metadata, native EQL parity with a cache, FAIR artifacts), because the current
index leaves the package's differentiators to the README body far down the page.

### PD5. OpenGraph

`opengraph.twitter.creator` is set but there is no `og:image`, so shared links
render without a card image. A static PNG in `man/figures/` referenced from
`_pkgdown.yml` fixes previews.

### PD6. `development: mode: auto`

Fine for a released package, but note that `main` is only 1.1.1 with unreleased
NEWS entries; if the intent is to surface "development" badges, the version and
NEWS top section need to agree.

---

## 8. Suggested sequencing

**Batch 1 (a few hours, no behaviour change):** E1 clamp/gate worker spawn; E2
route cached calls through the caching executor; E3 autosync mtime gate + save
order + metadata pattern; E4 point the constructor at the fast gatherer; X4, X5,
X7; S4 collect lazy seglists in `serve()`; Appendix B doc fixes.

**Batch 2 (a day):** E5 incremental cache rebuild; E6 streaming media; E7 batched
enrich cache; E8 fixed costs (TextGrid BOM, config reuse, SAVEBUNDLE hash, cache
size); S1 `R CMD check` warnings to zero; S2 convert the cheap half of the
`interactive()` examples to `requireNamespace()` gates.

**Batch 3 (planning required):** V4 vignette re-cut with executable chunks; P1/P2
navigation joins; P5 per-file DSP calls (validate against
`test_quantify_segment_list.R`); X2/X3/X6/X9 cleanup decisions.

---

## Appendix A. Claims checked and refuted

Do not act on these; they came up during review and did not survive verification.

| Claim | Verdict |
|---|---|
| `.github/workflows/pkgdown.yaml` is malformed YAML (missing `with:`) | **False.** Parses cleanly; the upload step has `with: path: docs` and the deploy step is well-formed. |
| `Rdpack` is an unused Import | **False.** `NAMESPACE` imports `Rdpack::reprompt`, and three man pages use `\insertCite` against `inst/REFERENCES.bib`. |
| The quantify/enrich cache defaults to `tempdir()`, so caches are lost every session | **True only as dead code.** Both call sites pass `corpus_obj@.cache_dir`, which the constructor sets to `<db>/.quantify_cache`. The fallback is unreachable (see X5). |
| The repo tracks build artifacts (`src/*.o`, `docs/`, tarball, check dir) | **False.** `git status` is clean apart from `build.log`; all of those are gitignored. |
| `gather_metadata()` is gone / metadata is always slow | **Half true.** The fast implementation exists and is what the public verbs call; the constructor uses the slower duplicate (E4). |
| Vignettes are unreadable / AI-written | **False.** Prose is clear and near-free of AI tells; the defect is unexecuted and unverified output. |

## Appendix B. Ready-to-apply patches

Small, mechanical, output-preserving. Each was verified against the working tree;
apply with your usual flow and re-run `devtools::document()` where noted.

1. **`R/reindeer_serve.R`**, accept a lazy segment list (fixes S4 and four doc
   sites). Immediately after `emuDBhandle <- get_emuDBhandle(corpus)` (line 64,
   before the `if (is.null(seglist))` block at line 71) insert
   `if (S7::S7_inherits(seglist, lazy_segment_list)) seglist <- collect(seglist)`.
   `collect()` is already defined for this class (`R/reindeer_lazy_segment_list.R:82`).
2. **`R/reindeer_autosync.R:224-229`**, make the scan see the mandated filename.
   `list.files(pattern=)` matches basenames, so the minimal fix is a second,
   const-built call and a concatenation:
   `c(list.files(basePath, pattern = paste0("^", metadata.filename, "$"), recursive = TRUE, full.names = TRUE), <existing legacy call>)`.
   Keep the legacy pattern only if `.meta_json` files must still be watched.
3. **`R/reindeer_autosync.R:196-200`**, move `save_sync_state(db_handle, state)`
   below the `if (length(changed) == 0) return(NULL)` block.
4. **`R/reindeer_serve.R:236`**, treat `bytes=0-` as `0..size-1` so it takes the
   `206` streaming branch, and apply the same in the helper at `:849`.
5. **`R/interop_textgrid.R:19`**, `readBin(path, "raw", n = 4)` for the BOM sniff.
6. **`R/query_executor.R:57`**, escape the brackets:
   `` `\[A -> B\]`, `\[A ^ B\]` `` then `devtools::document()`; this clears
   `man/query.Rd:83-85`.
7. **`inst/CITATION`**, replace the two hardcoded `0.4.10` strings with
   `meta$Version` (standard `bibentry` pattern) and align the title with
   DESCRIPTION.
8. **`README.md`**, citation version `1.0.1` → current version.
9. **pkgdown workflow**, append `rm -f docs/CLAUDE.html` after the build step
   (PD1), and move `VIGNETTES_SUMMARY.md` / `CMDI_VALIDATION.md` under `dev/`.
10. **DESCRIPTION**, add `eggstract` and `protoscribe` to Suggests and to
    `Remotes:` (both are GitHub-only, like `superassp`), and add `RoxygenNote`.
    This clears the two "undeclared namespace" WARNINGs; adding the packages to
    the CI `extra-packages:` list is what actually makes the guards testable.

## Appendix C. Reproduction notes

- Inventory and line citations were produced with direct file reads; where a
  tool display compressed output, the exact line was re-read before being quoted.
- Worker-spawn measurement:

  ```r
  library(future); library(future.apply)
  t <- function(expr) { s <- Sys.time(); force(expr)
                        as.numeric(Sys.time() - s, units = "secs") }
  t({ plan(multisession, workers = 9); value(future(1L)) })   # 1.13 s
  t(plan(sequential))                                          # 0.10 s
  plan(multisession, workers = 9)
  t(invisible(future_lapply(1:21, identity)))                  # 0.10 s
  ```

- `R CMD check` numbers come from `reindeer.Rcheck/00check.log` (R 4.6.1,
  `--no-manual --no-vignettes`). Re-run without those flags before treating the
  vignette checks as verified.
- Benchmark figures in section 2 come from the committed
  `benchmarking/QUERY_BENCHMARKS.md` materials and the local
  `benchmark_summary.csv`; regenerate on the current engine before publishing
  them.
