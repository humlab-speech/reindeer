# reindeer remediation plan

Companion to `dev/REINDEER_PACKAGE_ASSESSMENT.md`. That document says what is
wrong and where; this one says what to change, in what order, and what proves
each change is safe.

Target release: 1.2.0. No public API breaks in this program except the decisions
listed in §3, each of which needs an explicit call before its work package runs.

## 0. How to use this plan

Every work package has the same shape:

- **Evidence** — the finding being fixed, with a `file:line`.
- **Change** — what to edit, specific enough to execute without re-deriving it.
- **Proof** — the test or command that must pass, written *before* the change
  where the behaviour is user-visible.
- **Risk** — what can break, and the mitigation.

Work items are ordered by dependency, not by size, inside each phase. Phases are
gated: do not start the next one with a red gate.

Non-goals for this program: no new features, no interface redesign beyond the
items below, no emuR parity regressions, no changes to the behaviour of
protoscribe, eggstract, erodex, or superassp.

## 0a. Status

| Phase | State | Commit |
|---|---|---|
| 0 Baseline and safety net | done | `09592e3` |
| 1 Correctness | done | `83b1f34` |
| 2 Energy | done | `3a7936e` |
| 3 Performance | in progress (WP3.1 read scoping done; the join rewrite, connection reuse, eager SQL and assembly vectorisation remain) | see the commit that follows |
| 4 Standards | one warning left: `R CMD check` is down to a single `\usage`/`\arguments` warning; codoc, examples, tests and the NOTEs are clean | `14c0467` … `a2a073f` |
| 5 Documentation, vignettes, site | not started | |
| 6 Deletions and closure | partially done (dead PSOCK helper removed in phase 2) | |

### Findings that changed the plan

Several assessment claims did not survive inspection. Recorded here so nobody
re-litigates them.

- **quantify() was in worse shape than the assessment said.** The failure
  covered everything above 20 segments: 21-100 aborted in `rbindlist()`, and
  above 100 the executor flattened `AsspDataObj` tracks into matrices and then
  returned nothing, so `quantify()` silently handed back the input segments
  with no measurements. Both are fixed in phase 1.
- **superassp API generation (B6).** The installed 2.9.5 build exposes `trk_*`
  wrappers whose only formals are `(listOfFiles, ...)`, so no age/gender norms
  reach the DSP; the 3.0.0 source checkout exposes the wrapped routine's
  formals. reindeer now warns once when a routine exposes no parameters.
  Decision D6 stands: pin superassp >= 3.0.0.
- **WP2.4: four of six items do not hold.** The TextGrid BOM path reads the
  whole file *because the raw buffer feeds the encoding conversion*;
  `load_DBconfig()` is memoised by path + mtime, so the query path is not
  re-parsing JSON; `check_cache_size()` costs one `file.info()` for a file path
  (the directory walk only happens in `inspect_cache()`);
  hashing the in-memory JSON in the SAVEBUNDLE handler cannot reproduce the
  bytes on disk (`writeLines()` appends a newline), which would desynchronise
  the stored md5 from autosync's checksums. Memoising the DSPP table would hand
  callers a shared mutable data.table. Only the unfiltered metadata fetch in
  `quantify()` was a real cost, and it is fixed.
- **WP2.3 as specified was not justified.** A response body has to materialise
  the bytes, so the `bytes=0-` branch is not more expensive than the range
  branch. What *was* real: the inline webapp handler mis-parsed open-ended
  ranges (`bytes=100-` produced an `NA` inside `if`) and duplicated the
  range-aware helper. It now delegates to the helper, and a test pins the range
  behaviour.
- **WP2.5 (incremental cache rebuild) is deferred.** It needs `build_emuDB_cache()`
  restructured plus eviction of rows for bundles deleted from disk, and a
  stale-cache bug there costs far more than the energy it saves. Prerequisite:
  the rebuild-equivalence test (build twice → identical tables; delete a bundle
  → its rows are pruned) must exist first. The double read of each annotation
  file (`fromJSON()` + `md5sum()`) goes with it.
- **WP3.1 is half done, deliberately.** The navigation verbs no longer read
  whole tables: `scout`, `ascend_to` and `descend_to` scope their `items`,
  `links` and `labels` queries to the (session, bundle) pairs in the input, so
  the read is proportional to the query rather than to the corpus. Verified by
  comparing old and new results across eight navigation cases on the demo
  corpus - byte-identical, including the pre-existing 0-row `ascend_to("Word")`
  and `descend_to("Syllable")` outcomes on that corpus.
  The per-segment loop itself is untouched: replacing it with a non-equi join is
  a bigger change and needs the same equivalence harness, which now exists as a
  script (`/tmp/nav_equiv.R` pattern) and should be promoted into
  `tests/testthat/` before the rewrite.
- **WP3.2-WP3.4 remain.** Threading one connection through the query path,
  moving eager conjunction/disjunction onto the SQL builders, and vectorising
  result assembly are all still open, each with its existing gate
  (`test_query_optimized.R` parity for the first two, the quantify/pivot tests
  for the third).
- **The last check warning is a roxygen move, not a code problem.**
  `\arguments` must match `\usage` on every page. The S7 generics now declare
  their own signature (`scout(.segments, ...)`, `quantify(object, ...)`,
  `ascend_to`, `descend_to`, `enrich`), so the method-only `@param` entries
  (`steps_forward`, `level`, `.from`, `dsp_function`, `.at`, `.use_cache`, ...)
  have to move from the generic's block to the family's first method block.
  Left undone: a script that tried to do it automatically over-reached on
  `ascend_to` and `descend_to` (it took the whole preceding comment run as one
  `@param` entry) and was reverted from a pre-run copy. Do it by hand, four to
  ten lines per family, and re-run the check; an automatic roxygen rewrite needs
  a parse+diff gate before it writes.
- **Two deletions remain the maintainer's call**: `inst/praat/praatdet` (a
  nested git checkout) and `tests/signalfiles` (124 MB, referenced only from an
  obsolete worktree). Both are excluded from the build meanwhile, so neither
  ships and neither trips the non-portable-filename check.
- **A navigation dead end aborts instead of returning nothing.**
  `scout(steps_forward = 99)` — far enough past the end that no item
  matches — fails inside the `segment_list` validator
  (`R/segment_list_classes.R:116`) rather than returning an empty result,
  so the chain cannot report "100% of rows lost". Fixing it means letting
  the validator accept a zero-row segment list with the required columns
  present, then checking that `dropped_rows()` and the loss warning handle
  that shape. `vignettes/lazy_and_provenance.Rmd` documents the limit
  meanwhile.
- **Collate matters.** Adding `R/parallel_utils.R` broke `R CMD INSTALL`
  ("files missing from Collate field") while `devtools::test()` stayed green,
  because `load_all()` ignores Collate. Run `devtools::document()` whenever a
  file is added.
- **The first full `R CMD check` caught two defects the suite could not.**
  (a) The phase-2 batched cache write passed `cache_format` where the scope has
  `.cache_format`, so a corpus-level `enrich()` with a cache miss would have
  failed at that line - no unit test reached it. (b) The doc-contract tests read
  `man/*.Rd` from the source tree, which does not exist under check, so they
  errored instead of skipping. Both fixed in `86b9f86`. Check status after the
  fixes: 2 WARNINGs (the `ascend_to` Rd usage/codoc pair), 0 NOTEs, examples OK.
  Run the check with `_R_CHECK_FORCE_SUGGESTS_=false` as CI does, and build with
  vignettes: `--no-build-vignettes` fabricates a "no files in inst/doc" warning.

## 1. Invariants and baseline

These hold after every commit, not just at phase gates:

1. `devtools::test()` — no failures, no errors.
2. `R CMD check` — never worse than baseline (7 WARNING, 1 NOTE). Target is zero
   by the end of phase 4.
3. `query()` output identical to `emuR::query()` on the ae corpus
   (`tests/testthat/test_query_optimized.R` is the gate).
4. No measurement value changes unless a work item says it should.
5. No new exported symbol without documentation and an index entry.
6. Every `pkg::fun` reference in `R/` roxygen, `vignettes/` and `README.md`
   resolves against the installed package, and every column named in a
   `@return` block exists in the returned object. Both are asserted by tests
   from WP0.2 onward.

**Baseline, measured 2026-09-17 on this machine** (R 4.6.1, M1 Pro, 10 cores):

| What | Result |
|---|---|
| `devtools::test()` | **1799 passed, 0 failed, 0 errors, 3 warnings**, 247.6 s |
| `devtools::load_all()` | 4.2 s, 72 exports |
| `R CMD check` (last run, 2026-09-06) | 7 WARNING, 1 NOTE (`--no-manual --no-vignettes`) |
| Worker spawn (`future::multisession`) | 0.77 s for 2 workers, 1.13 s for 9, +0.10 s teardown |
| Local toolchain | devtools, testthat, emuR, superassp, qs2, pkgdown, knitr all installed |

The three baseline test warnings are a defect, not noise: see B1 below.

## 2. Defects found while preparing this plan

These were not in the assessment; they come from the baseline run and from
tracing its output.

### B1. Nested metadata values are silently truncated in the cache

`serialize_metadata_value()` (`R/metadata_core.R:483-499`) has branches for
NULL, logical, integer, numeric, Date and POSIXt; everything else falls into
`list(value = as.character(value), type = "character")`. A nested list such as
`project = list(name = "TestProj", description = "A test corpus.")` therefore
becomes a length-2 character vector, and the assignment in
`process_metadata_list()` (`R/metadata_core.R:435-438`) recycles it into a
length-1 slot. Reproduced today:

```
add_metadata(corp, list(project = list(name = "TestProj", description = "A test corpus."),
                        funding = list(funder = "TestFunder", grantNumber = "G-001")))
# Warning: number of items to replace is not a multiple of replacement length
get_metadata(corp)$project    # "TestProj"   <- description and startDate gone
get_metadata(corp)$funding    # "TestFunder" <- grantNumber gone
```

Scope, verified: `METADATA.json` is written first and correctly
(`R/corpus_metadata_io.R:84`, `auto_unbox = TRUE`), so ground truth is intact and
the loss is confined to the SQLite projection. The FAIR emitters are unaffected
because `collect_corpus_summary()` reads the nested values straight from
`METADATA.json` (`R/corpus_describe.R:33-49`) and then falls back to the flat
cache fields (`:52-56`). What *is* affected: `get_metadata()` (the documented
read API), everything built on it (`enrich(with = "metadata")`, `biographize()`),
and users' trust, since the warning is unexplainable from the docs.

### B2. Corpus connections are never closed

The baseline run ends with RSQLite's finalizer warning, `call dbDisconnect()
when finished working with a connection`. `close_connection()` exists
(`R/corpus_methods.R:444`) but nothing calls it automatically and there is no
`reg.finalizer` anywhere in the package. Each `corpus()` in a long session leaks
a SQLite handle until GC.

### B3. Three test warnings are load-bearing

`tests/testthat/test_describe.R:63-72` and `:129-136` pass while emitting the
B1 recycling warning. The suite currently encodes the truncation as acceptable
behaviour; those tests must be tightened to `expect_silent()` plus a full
round-trip assertion.

### B4. The documented segment column is `label`; the actual column is `labels`

`collect(query(corp, "Phonetic == n"))` returns columns `labels, start, end,
db_uuid, session, bundle, …` — byte-identical to `emuR::query()`, which is
correct for parity. The documents disagree with reality:

| Surface | Says | Reality |
|---|---|---|
| `README.md` (2×), `vignettes/getting_started.Rmd` (5×), `vignettes/interactive_annotation.Rmd` (6×) | `label` | `labels` |
| `man/query.Rd` `@return` (3×) | `label` | `labels` |
| `man/segment_list.Rd`, `man/geom_label_tier.Rd`, class docs | `labels` | correct |

Verified failure: `count(label)` on a query result errors with
`Column 'label' is not found`. The README's five-minute workflow does not run.
Nothing caught it because no example executes and the tests use whichever name
they choose internally.

### B5. Every documented DSP call names a function that does not exist

reindeer's docs reference `superassp::forest` 38 times, `superassp::ksvF0` 7,
`superassp::rmsana` 3, `superassp::dftSpectrum` 2 (README, all six vignettes,
and 5 roxygen example blocks). The installed superassp (2.9.5) and the current
source checkout (3.0.0) export none of those names; the real entry points are
`trk_formant_forest`, `trk_formant_burg`, `trk_rms`, `trk_intensity`, and
`lst_*` analysis functions.

```
> quantify(vowels, superassp::forest, .at = 0.5)
Error: 'forest' is not an exported object from 'namespace:superassp'
```

The suite cannot catch this: every test passes a locally defined `fake_dsp`
(`tests/testthat/test_quantify_segment_list.R:72`, `:131`), so no real DSP
function is ever resolved. This is the most severe documentation defect in the
package, and it is invisible for exactly the reason the assessment identified:
examples and vignettes never execute.

## 3. Decisions required before execution

| ID | Decision | Recommendation | Blocks |
|----|----------|----------------|--------|
| D1 | Keep or retire the EAF/CMDI sync state machine (`R/reindeer_autosync.R` 698 l, `R/reindeer_autosync_wrappers.R` 411 l, `R/interop_elan_autosync.R` 243 l, 633-line test) now that `describe_corpus()` + `write_eaf()` cover the user-facing need | Keep through 1.2 (fix the bugs, delete the 5 unreachable wrappers), schedule full retirement for 2.0 | X2, X9 |
| D2 | Keep the ten moved-function stubs through 1.2 | Keep; move them to their own reference-index section | PD3 |
| D3 | Keep the 8.6 MB demo corpora in `inst/extdata` | Keep (they enable offline examples and vignettes), delete the unused 3 MB `inst/praat` tree | X6 |
| D4 | Raise the parallel threshold from 20 segments and clamp workers to work units | Yes; output is identical, and provide `options(reindeer.workers = n)` as the escape hatch | WP2.1 |
| D5 | Vignette execution policy: global env-var gate vs per-chunk capability gate | Capability gate (`requireNamespace`), so most vignette code runs in CI today | WP5.1 |
| D6 | Which superassp API generation the docs target, and whether reindeer should expose its own stable aliases | Pin a minimum version (superassp ≥ 3.0.0, `trk_*` API) and record it in `DESCRIPTION`; defer an alias layer (`reindeer::dsp_formants()`) unless the upstream API is still moving | WP0.2 T1, WP1.9, WP4.2 |

Until a decision is made, the dependent work packages stay out of scope.

## 4. Work packages

### Phase 0 — Baseline and safety net (est. 1 day)

**WP0.1 Record the verification baselines.**
Write `benchmarking/benchmark_core.R` producing a CSV with: `corpus()` open time
(quick and rebuild), `query()` + `scout()` on the ae corpus, `quantify()` at 5 /
50 / 150 segments with `.use_cache` off and on, and a worker-spawn counter
(`future::plan` counts via a wrapper). Run it on the current commit and commit the
CSV under `benchmarking/`. This is the reference for every energy and performance
claim in phases 2 and 3.

*Proof*: the CSV exists with today's numbers; re-running produces the same shape.

**WP0.2 Write the missing regression tests first.** These must fail before their
fix and pass after.

| Test | Asserts | Currently |
|---|---|---|
| T1 cache round-trip | `quantify(..., .use_cache = TRUE)` twice: second call is all `"hit"`; after `set_metadata(Age = ...)` the affected bundle is `"miss"`; run at 5, 50 and 150 rows to cover all three executors | **no test uses `.use_cache` at all** (verified: zero hits across `tests/`) |
| T2 cache key identity | two DSP functions with overlapping formal names do not share cache entries | fails (X7) |
| T3 `serve()` accepts a lazy result | `serve(corp, seglist = query(corp, "Phonetic == n"))` passes validation | fails (S4) |
| T4 autosync sees `METADATA.json` | editing `METADATA.json` makes `detect_metadata_changes()` report a change | fails (X3) |
| T5 nested metadata round-trip | B1: `expect_silent()` on the write, and `get_metadata()` returns `description`, `startDate`, `grantNumber` intact | fails (B1) |
| T6 demo corpus smoke | `demo_corpus()` + one EQL query + one metadata read succeed offline; this is the fixture vignettes will use | passes (2.5 s cold, 0.08 s warm); locks in the WP5.1 premise |
| T7 doc-contract: symbols | every `pkg::fun` in roxygen, `vignettes/`, `README.md` resolves via `getNamespaceExports()` | fails immediately (B5: 50 references) |
| T8 doc-contract: columns | every column named in a `@return` block exists in the object that function returns (start with `query()`, `quantify()`, `enrich()`, `collect()`) | fails (B4: `label`) |

T1 must use a **real** DSP function on the demo corpus, not `fake_dsp`: the
existing fakes never write a track file, so they cannot exercise the track-file
cache path at all. Pick the canonical entry point decided in D6.

*Proof*: `devtools::test()` shows T1–T5, T7 and T8 red, T6 green, everything
else unchanged.

**WP0.3 Make the dependency guards real.**
Add `eggstract` and `protoscribe` to `Suggests` and `Remotes`, add `RoxygenNote`,
and add them to the CI `extra-packages:` list. Clears two `R CMD check` warnings
and makes the companion code paths testable.

*Proof*: `R CMD check` no longer reports the undeclared-namespace warnings.

**Gate 0**: baseline CSV committed; T1–T6 written and behaving as specified;
`devtools::test()` otherwise green.

---

### Phase 1 — Correctness (est. 1.5 days)

**WP1.1 `serve()` accepts a `lazy_segment_list`** (S4).
In `R/reindeer_serve.R`, immediately after `emuDBhandle <- get_emuDBhandle(corpus)`
(line 64) and before the `if (is.null(seglist))` block at line 71, collect lazy
input: `if (S7::S7_inherits(seglist, lazy_segment_list)) seglist <- collect(seglist)`.
Fixes the package's own example at `:38` and four documented call sites.
*Proof*: T3. *Risk*: collection now happens at serve time; that matches the docs.

**WP1.2 Autosync detects the mandated metadata file** (X3, E3 partly).
`R/reindeer_autosync.R:224-229` scans `pattern = "^\\.meta_json$"` only. Add a
second `list.files()` call built from `metadata.filename`
(`R/metadata_core.R:12`) and concatenate. *Proof*: T4.

**WP1.3 Autosync stops rewriting state on no-op scans** (E3).
Move `save_sync_state(db_handle, state)` (`:197`) below the
`if (length(changed) == 0) return(NULL)` block (`:199-200`), and add an
`annot_mtimes` map to the state so only files whose mtime moved get hashed
(hash when the mtime differs *or* the stored checksum is missing, so a
timestamp-preserving edit still gets caught by the checksum comparison).
*Proof*: T4 extended — a second no-op call leaves `.sync_state.json`'s mtime
unchanged.

**WP1.4 Constructor uses the fast metadata gatherer** (E4).
`R/corpus_class.R:194,199` call `gather_metadata_internal()`; switch both to
`gather_metadata()` (`R/metadata_core.R:222`), which is parallel, mtime-gated,
bulk-inserting, and already reads legacy `.meta_json` where `METADATA.json` is
absent. Its refresh gate tolerates caches without the `metadata_mtime` table
(`:165-181`, error → refresh). Then delete `gather_metadata_internal()` and
re-run the metadata tests.
*Proof*: `test_metadata_optimized.R`, plus a new assertion that the cache after
`corpus(quick = FALSE)` equals the cache after `load_metadata()`.

**WP1.5 Cache keys identify the DSP function** (X7).
In `R/tidy_trackdata_helpers.R:81-85`, take the function name as an argument
instead of `deparse(substitute(...))` on a local variable, and pass it from the
call site at `:482`. In `R/reindeer_enrich.R:332-336`, add `dsp_fun_name` (already
in scope, `:88`) to the digest.
*Proof*: T2. *Risk*: existing cache entries stop matching, so caches go cold on
upgrade. That is safe (no wrong values), but it must be stated in NEWS.

**WP1.6 Caching works in the 21–100 segment band** (E2).
Either route cache-enabled calls through `.process_segments_vectorized()`, or
thread `use_cache` / `cache_conn` / `cache_format` into `.process_parallel_io()`
(`R/tidy_trackdata_helpers.R:619-620`). Prefer the routing change: it reuses the
executor that already batches cache I/O, and it is one line plus a test.
*Proof*: T1 at 50 rows.

**WP1.7 Nested metadata survives the round-trip** (B1).
Add a list branch to `serialize_metadata_value()` (`R/metadata_core.R:483-499`)
that stores `jsonlite::toJSON(value, auto_unbox = TRUE)` as `type = "json"`, and
a matching case in `deserialize_metadata_value()` (`:504-521`). Treat multi-element
atomic vectors the same way, so `c("a","b")` stops being truncated. Fix the
recycling assignment at `:435-438` to be explicit about lengths (abort on
mismatch rather than recycle).
*Proof*: T5, plus tightened `test_describe.R` cases.
*Risk*: caches written before the fix hold truncated strings; the documented
remedy is `load_metadata(corp)`, which rebuilds from `METADATA.json`. Say so in
NEWS.

**WP1.8 Corpus connections get closed** (B2).
Register a finalizer when the connection environment is created
(`R/corpus_class.R`, alongside `.connection = new.env(...)`), or on the corpus
object, that calls the existing `close_connection()` (`R/corpus_methods.R:444`)
if the handle is still valid. Keep `close_connection()` public and idempotent.
*Proof*: a test that creates a corpus, drops it, calls `gc()`, and asserts no
"call dbDisconnect()" warning is emitted; the baseline test run's trailing
warning disappears.

**WP1.9 Documented symbols and columns match reality** (B4, B5).
One sweep, three surfaces:
- Replace every `superassp::forest` / `ksvF0` / `rmsana` / `dftSpectrum`
  reference with the canonical name from D6 (`trk_formant_forest` and friends)
  across `README.md`, `vignettes/*.Rmd`, and the roxygen blocks in
  `R/corpus_class.R`, `R/reindeer_enrich.R`, `R/segment_list_classes.R`,
  `R/segment_list_pivot.R`, `R/segment_list_quantify.R`.
- Check the parameters the docs pass (`nominalF1`, `windowSize`, `numFormants`)
  against the formals of the chosen function, and correct or drop them.
- Replace `label` with `labels` in `README.md`,
  `vignettes/getting_started.Rmd`, `vignettes/interactive_annotation.Rmd`, and
  `man/query.Rd`'s `@return`.
*Proof*: T7 and T8 green; the README workflow runs end to end on
`demo_corpus()` (add it as T6's second half).
*Risk*: superassp's API may move again; T7 is the guard, and D6 decides whether
to add a reindeer-side alias layer instead of tracking upstream names.

**Gate 1**: T1–T5, T7, T8 green, full suite green, `R CMD check` no worse than
baseline, zero occurrences of the RSQLite finalizer warning in the test log, and
the README workflow running verbatim on `demo_corpus()`.

---

### Phase 2 — Energy (est. 2 days)

Worker policy first, per-call I/O second. Every item reports a before/after
number from `benchmark_core.R`.

**WP2.1 One worker policy for the package** (E1, D4).
Add a helper (e.g. `R/parallel_utils.R`) exposing
`.reindeer_workers(n_units, requested = NULL)`, which returns 1 when
`n_units < 2`, otherwise `min(requested %||% getOption("reindeer.workers") %||%
<current default>, n_units)`. Use it at every site, and skip the parallel branch
entirely when it returns 1:

| Site | Current trigger | Change |
|---|---|---|
| `R/segment_list_quantify.R:222-236` | >20 rows | require `> 100` rows *or* `n_units > 1`; clamp |
| `R/tidy_trackdata_helpers.R:629-650` | always in that executor | clamp to `length(file_groups)` |
| `R/reindeer_enrich.R:298-300` | `.parallel` on any size | skip when bundles == 1; clamp |
| `R/corpus_database.R:17,88` | >10 bundles | clamp to `nrow(sessions_bundles)` |
| `R/metadata_core.R:326-332` | >50 files | already capped at 4; add clamp |
| `R/reindeer_corpus_config.R:619-640` | dead code | delete (`process_bundles()` has no callers anywhere) |

Consider `future::multicore` on POSIX when the caller has not pinned workers:
fork avoids re-loading namespaces per worker, which is most of the 1.13 s.
Keep the plan restore in `on.exit` either way.
*Proof*: benchmark CSV shows the spawn cost gone for small jobs; suite green.
*Risk*: a genuinely large corpus could get fewer workers than before — hence
clamp-to-units rather than a hard cap, plus the option.

**WP2.2 `enrich(corpus)` uses the batched cache** (E7).
Replace the per-bundle `.get_persistent_cache()` / `.set_persistent_cache()`
calls (`R/reindeer_enrich.R:338,356`) with the batched helpers
(`R/tidy_trackdata_helpers.R:372-460`) — one transaction, one eviction pass, no
`SELECT SUM(size_bytes)` per insert.
*Proof*: T1 extended to `enrich(corpus)` on a 2-bundle corpus; identical results
with cache on and off.

**WP2.3 `serve()` streams media** (E6).
Treat `bytes=0-` as `0..size-1` so it uses the existing `206` streaming branch
(`R/reindeer_serve.R:236`, helper at `:849`) instead of `readBin` of the whole
file. *Proof*: responses are byte-identical for the same range requests
(compare digests in a test against a small fixture file).

**WP2.4 Fixed I/O costs** (E8). Each is small and independently verifiable:

- `R/interop_textgrid.R:19` — sniff the BOM with `readBin(..., n = 4)`.
- `R/query_parser.R:1417`, `R/corpus_describe.R:25` — reuse `corpus@config`
  instead of re-parsing `_DBconfig.json`.
- `R/reindeer_serve.R:614` — hash the in-memory JSON, not the file just written.
- `R/tidy_trackdata_helpers.R:221` — keep a running cache-size total instead of
  walking the directory on connection open.
- `R/segment_list_quantify.R:151-158` — fetch only the needed metadata rows,
  reusing the parameterised query at `R/reindeer_enrich.R:252-271`.
- `R/dsp_parameters_public.R:38`, `R/reindeer_enrich.R:453` — memoise the DSPP
  tibble.

**WP2.5 Incremental cache rebuild** (E5). The riskiest item; do it last in the
phase. Stop unlinking the cache (`R/corpus_database.R:56-57`); upsert per bundle
keyed on the stored `md5_annot_json` (`:172`, written at `:309`, `:515`); delete
cache rows for bundles/sessions that no longer exist on disk; hash the bytes
already read for parsing instead of a second read; keep an explicit
`rebuild = TRUE` escape hatch for suspected corruption.
*Proof*: new test — build twice → identical table row counts and identical
content; delete a bundle directory → its rows are pruned; touch one annotation
file → only that bundle is re-parsed (assert with a parse counter).
*Risk*: stale cache if a file changes without a new mtime. Mitigate by comparing
the stored checksum, as in WP1.3.

**Gate 2**: benchmark CSV with before/after for every item; suite green; no
change in any measured value.

---

### Phase 3 — Performance (est. 2–3 days)

**WP3.1 Navigation queries become set operations** (P1, P2).
Replace the per-segment loop in `scout_dt()` (`R/reindeer_sequence_ops_optimized.R:213-306`)
with one non-equi keyed join per call, and scope the `labels` / `items` / `links`
reads to the ids in hand (`:188-203`, `:425-440`, `:604`) instead of whole tables.
Preserve `ignore_bundle_boundaries`, `capture`, `steps_forward` semantics exactly.
*Proof*: before the change, capture current outputs for a matrix of
scout/ascend/descend calls on the ae corpus into a fixture; after, compare
identical. Gate: `test_query_optimized.R`, `test_lazy_segment_list.R`,
`test_provenance.R`, `test_lazy_chain.R` green.
*Risk*: the join must reproduce the boundary rule; the fixture comparison is the
guard.

**WP3.2 One connection per query** (P3, P4).
Thread the connection from `query()` / `collect_lazy_impl()` into
`deduce_item_times()` (`R/query_parser.R:1499`) rather than opening a third, and
memoise a level/attribute map per connection environment to replace the probes in
`.resolve_level_attribute()` (`:394-407`). Also reuse the built `list(sql, params)`
from plan time instead of rebuilding it in `build_sql_from_parts()`.
*Proof*: query parity suite; benchmark delta.

**WP3.3 Eager compound queries use the SQL builders** (P7, P9).
Make `execute_conjunction_query()` (`:876-886`) and
`execute_disjunction_query()` (`:899-903`) reuse `build_conjunction_query_sql()`
/ `build_disjunction_query_sql()` (one statement, parameters instead of spliced
literals). *Proof*: `test_query_optimized.R` parity vs emuR; a test asserting
identical results for the eager and lazy paths.

**WP3.4 Assemble results without per-row groups** (P6, P8, P10).
Vectorise the row expansion in `R/tidy_trackdata_helpers.R:580-612`, `:143-170`,
`:684-701`; replace `as.data.frame()` / `merge()` in the quantify driver
(`R/segment_list_quantify.R:100,147,170,174,262`) with keyed joins on the needed
columns; digest distinct parameter lists once (`R/tidy_trackdata_helpers.R:483-485`).
*Proof*: quantify and pivot tests; benchmark delta.

**WP3.5 Per-file DSP calls** (P5) — only if WP3.1–3.4 are green.
Issue one `do.call()` per signal file with recycled `listOfFiles` and vector
`beginTime`/`endTime` (sites: `R/tidy_trackdata_helpers.R:138-141`, `:535-538`,
`:669-672`, `R/reindeer_enrich.R:348-351`).
*Proof*: hard gate — identical values to the per-segment path on a fixed fixture
covering `.at` grids, all DSPs used in the tests, and both cached and uncached
runs. If any column differs, abandon this item and record why; the other phase-3
items stand on their own.

**Gate 3**: benchmark CSV showing the improvement for WP3.1–3.4; parity suite
green; WP3.5 either green or explicitly abandoned with the diff recorded.

---

### Phase 4 — Standards (est. 1.5 days)

**WP4.1 `R CMD check` to zero warnings.**
- Non-ASCII in `R/segment_list_pivot.R`.
- Non-portable filenames: `inst/praat/praatdet/examples/*` disappear with WP6.3;
  decide `tests/signalfiles/EGG/Session 1|2` separately (if the spaces are
  deliberate path-handling coverage, rename and add a targeted test that creates
  a spaced path at runtime).
- Rd defects: `ascend_to` usage/`...`, the four `enrich.*` /
  `quantify.lazy_segment_list` usage-without-alias pages, `print.lazy_segment_list`
  `preview` argument, the `list_cache_files` link in `inspect_cache.Rd`.
- Undefined globals: `.data`, `median`, `seg_params`, `seg_params_digest`,
  `.cache_status` — `importFrom(stats, median)` plus `utils::globalVariables()`
  for the data.table column names.
- The partial-argument match in `browse_corpus_gadget` (`session` →
  `session_pattern`).

*Proof*: `devtools::check()` output clean; keep the log in the PR description.

**WP4.2 Examples execute.** Convert the 39 capability-light `@examplesIf
interactive()` blocks (of 46 total) to `@examplesIf requireNamespace(...)` gates,
rewrite the 17 blocks that use placeholder paths to `demo_corpus()`, use the
canonical DSP names from WP1.9, and add examples to the ten most-used
undocumented verbs (`collect`, `glimpse`, `enrich.corpus`,
`quantify.lazy_segment_list`, `nest_by_*`, `derive_dsp_parameters`,
`extended_segment_list`).
*Proof*: the count of examples executed by `R CMD check` rises from 3 to ≥30;
check time stays under ~2× baseline (measure once).

**WP4.3 Error reporting matches the package's stated goal** (S5).
Replace the 13 catch-all `error = function(e) NULL` sites (list in the assessment)
with classed warnings that name what failed, keeping a `NULL` only where a
documented fallback exists. *Proof*: existing error-message tests extended; no
new warnings in the suite.

**WP4.4 Deprecation policy** (S6).
Either add lifecycle badges plus `@deprecated` to `add_metadata()` and
`gather_metadata()`, or drop the word "deprecated" from the vignette text. Pick
one; do not leave the mismatch.

**Gate 4**: `R CMD check` = 0 ERROR / 0 WARNING / 0 NOTE (or a written
justification for any surviving NOTE); example execution count recorded.

---

### Phase 5 — Documentation, vignettes, site (est. 2 days)

**WP5.1 Vignettes run.** Replace the global `REINDEER_EVAL_VIGNETTES` gate with
per-chunk capability gates (`requireNamespace()`), and replace the placeholder
corpora with `demo_corpus()`: 10 placeholders in `interactive_annotation.Rmd`,
2 in `getting_started.Rmd`, 1 each in the other four.
*Proof*: `devtools::build_vignettes()` locally renders all six with real output;
add the CI job that executes them.

**WP5.2 Correct the stale claims** (V3, B4): the "deprecated alias" wording (also
WP4.4), `describe_corpus()`'s artifact count (five, not three), the
`explain()`/DOI roadmap promises, the internal `corp@.cache_dir` reference in
`cache_management.Rmd:78`, and any `label` wording that WP1.9 did not already
catch (re-run T8 after every doc edit).

**WP5.3 Re-cut the vignettes** (V4):
1. `getting_started` — tighten, end on a real printed result.
2. `speaker-aware_measurement` (new) — metadata → `dsp_parameters()` →
   `quantify()` with `.at` → cache status column. This is the differentiator.
3. `metadata_management` — keep, fix claims, show the Excel round-trip.
4. `fairest_artifacts` (new) — `describe_corpus()`, the five emitted files, CMDI
   validation. Currently one section; it is the archival selling point.
5. `lazy_and_provenance` — keep, real output.
6. `interactive_annotation` — cut to ~150 lines, one end-to-end serve/review/save
   example.
Retire `end_to_end_pipeline` if the companion packages cannot be installed in
CI; otherwise keep it as the one place that shows all four integrations.

**WP5.4 Reference pages** (D1–D3): fix the leaked link definitions at
`man/query.Rd:83-85` (escape the brackets in `R/query_executor.R:57`), document
`with =` and `corpus_obj =` in `man/enrich.Rd`, replace the obsolete
`quantify(segs, corpus, tracks = "fm")` example in `man/segment_list.Rd`, and give
the internal-bucket pages a usage-first pass.

**WP5.5 Site hygiene** (PD1–PD5): move `VIGNETTES_SUMMARY.md` and
`CMDI_VALIDATION.md` into `dev/`; add `rm -f docs/CLAUDE.html` to the pkgdown
workflow (pkgdown renders every root `*.md`, `.Rbuildignore` notwithstanding);
add a "Deprecated and moved" reference section; add the three capability pillars
to the landing page; add an `og:image`; fix the README citation version and make
`inst/CITATION` read `meta$Version`.

**Gate 5**: local `pkgdown::build_site()` produces a site with no internal pages,
articles showing real output, and no leaked markup; `R CMD check` still clean.

---

### Phase 6 — Deletions and closure (est. 1 day)

**WP6.1 Delete dead corpus-config helpers.** `R/reindeer_corpus_config.R` has 26
top-level functions; only `load_DBconfig`, `store_DBconfig` and `create_ae_db`
have callers. Remove the other 23 (none are exported, none appear in `man/` or
`tests/`), including the dead PSOCK `process_bundles()`.
*Proof*: reachability scan rerun after deletion; suite and check green.

**WP6.2 Delete unreachable autosync wrappers** (X2) or move the two test-only
ones into `tests/testthat/`. The session writer hardcodes `.meta_json`
(`R/reindeer_autosync_wrappers.R:106`), contradicting the mandated filename, so
it should not survive in the package body.

**WP6.3 Delete the unused vendored Praat assets** (X6, D3): `inst/praat`,
`inst/praat/praatdet`, `inst/pymomelintsint` — 3 MB, no references from R code.
*Proof*: `grep -rn praat R/` empty; installed size drops; the non-ASCII filename
warning disappears.

**WP6.4 Delete the remaining leftovers**: `ae()` / `emu_ae()` aliases
(`R/reindeer_demodata.R:40-41`), the unreachable `tempdir()` cache fallback
(X5), and the four `.Rbuildignore` families that match deleted files.

**WP6.5 Repo hygiene** (S9): fix the eight dead pointers in `CLAUDE.md`, move
`tests/*.md` dev notes to `dev/`, drop the stale `R/deprecated` / `R/*DELETE*`
patterns from `codecov.yml`.

**WP6.6 Close out.** Full `R CMD check` *with* vignettes and manual (no
`--no-manual --no-vignettes`), site build, final benchmark table, NEWS entry
covering: worker policy change, cache-key change (expect cold caches), metadata
serializer fix (how to refresh), example/vignette execution, deletions.

**Gate 6**: all invariants hold; benchmark table shows the energy and performance
deltas; NEWS written.

## 5. Verification matrix

| Change | Primary test | Secondary evidence |
|---|---|---|
| serve lazy (WP1.1) | T3, `test_serve.R` | four doc examples run in vignettes |
| autosync (WP1.2, WP1.3) | T4 + mtime assertion | `test_autosync.R` |
| gatherer switch (WP1.4) | cache equality test | `test_metadata_optimized.R` |
| cache keys (WP1.5) | T2 | cold-cache note in NEWS |
| cache band (WP1.6) | T1 at 50 rows | `test_quantify_segment_list.R` |
| metadata serializer (WP1.7) | T5 | tightened `test_describe.R` |
| connection lifetime (WP1.8) | gc test | no RSQLite warning in test log |
| documented symbols/columns (WP1.9) | T7, T8 | README runs verbatim on `demo_corpus()` |
| worker policy (WP2.1) | benchmark CSV | suite runtime |
| enrich batching (WP2.2) | T1 for `enrich(corpus)` | cache hit counts |
| streaming media (WP2.3) | byte-equality test | `test_serve.R` |
| incremental rebuild (WP2.5) | rebuild-equivalence test | cache build timing |
| navigation joins (WP3.1) | fixture comparison | benchmark delta |
| connections/level map (WP3.2–3.3) | `test_query_optimized.R` | emuR parity |
| assembly (WP3.4) | quantify/pivot tests | benchmark delta |
| per-file DSP (WP3.5) | bit-comparison fixture | abandon if mismatch |
| check cleanup (WP4.1) | `devtools::check()` | check log |
| examples (WP4.2) | example count | check runtime |
| vignettes (WP5.1) | `devtools::build_vignettes()` | rendered HTML in `docs/` |
| site (WP5.5) | local site build | no `CLAUDE.html` |

## 6. Risk register

| Risk | Where | Mitigation |
|---|---|---|
| Cache invalidation surprises users after WP1.5/WP1.7 | quantify/enrich caches | cold caches are safe; document refresh path in NEWS |
| Parallel default change makes a large job slower | WP2.1 | clamp to work units, not a constant; `options(reindeer.workers)`; benchmark a large corpus before merging |
| Incremental rebuild serves stale rows | WP2.5 | checksum comparison, per-bundle pruning, `rebuild = TRUE` escape hatch |
| Navigation rewrite changes boundary semantics | WP3.1 | fixture comparison over a call matrix before and after |
| Per-file DSP batching changes values | WP3.5 | hard bit-comparison gate; abandon otherwise |
| Metadata serializer change alters stored types for existing fields | WP1.7 | only list/multi-element values change type; regression test on scalars, vectors and lists |
| Deleting dead helpers breaks an unseen caller | WP6.1–6.4 | unexported, no man/test references; rerun the reachability scan and the full suite |
| Vignette execution slows CI | WP5.1 | capability gates keep DSP chunks out of the default job; measure once |

## 7. Definition of done

| Area | Done means |
|---|---|
| Power | ≤2 worker-spawn sites remain, both clamped to work units; per-call corpus hashing gated by mtime; benchmark CSV shows the deltas |
| Performance | navigation is join-based with a benchmark delta; parity suite green |
| Standards | `R CMD check` 0/0/0 with vignettes included; worktree clean |
| Vignettes | six render with executed output locally and in CI |
| Documentation | every export either has an example or a written reason not to; the 90/10 rule holds on the pages in the reference index; every documented symbol and column exists (T7, T8) |
| Superseded code | dead helpers, unreachable wrappers and vendored assets removed; NEWS records the deletions |
| pkgdown | no internal pages published; articles show output; index covers deprecations |

## 8. Execution checklist

```
Phase 0  [ ] benchmark_core.R + CSV   [ ] T1-T8 written (T1-T5,T7,T8 red)   [ ] Suggests/CI deps
Phase 1  [ ] serve lazy  [ ] autosync pattern  [ ] autosync mtime  [ ] fast gatherer
         [ ] cache keys  [ ] cache band  [ ] metadata serializer  [ ] connection finalizer
         [ ] doc symbols + columns (B4, B5)
Phase 2  [ ] worker policy (- process_bundles)  [ ] enrich batching  [ ] streamed media
         [ ] fixed I/O costs  [ ] incremental rebuild
Phase 3  [ ] scout/ascend/descend joins  [ ] one connection + level map  [ ] eager SQL
         [ ] assembly vectorisation  [ ] per-file DSP (optional)
Phase 4  [ ] check warnings to zero  [ ] examples execute  [ ] error reporting  [ ] deprecation policy
Phase 5  [ ] vignettes execute  [ ] stale claims  [ ] vignette re-cut  [ ] reference pages  [ ] site hygiene
Phase 6  [ ] dead helpers  [ ] autosync wrappers  [ ] Praat assets  [ ] leftovers  [ ] repo hygiene  [ ] close-out
```


## D6 — CONFIRMED, and sharper than first stated (2026-09-17)

Evidence, in order:

1. `formals(superassp::trk_formant_forest)` on the installed 2.9.5 is
   `(listOfFiles, ...)` — the wrapper carries no tunable parameters.
2. `reindeer:::derive_dsp_parameters(superassp::trk_formant_forest,
   list(Age = 8, Gender = "Male"), character(), list())` **warns**
   ("DSP routine exposes no parameters, so Age/Gender norms are not
   applied") and returns a list of length 0. So the branch at
   `R/reindeer_enrich.R:502` is reachable, `fun_formals` is the
   wrapper's, and the norms genuinely do not flow on 2.9.5.
3. The same call through the user-facing path — `quantify()` on the demo
   corpus with `Age`/`Gender` set — produces **no warning at all**.

So the defect is not "the warning is noisy" but "the warning is correct
and the main entry point never reaches it": `quantify()` on superassp
2.9.5 silently applies default DSP parameters where age/gender-aware ones
are expected. The probe that established this is the third of the three
runs above; it is cheap to repeat.

Fixing it means tracing `quantify()` (`R/segment_list_quantify.R`) to see
whether it calls `derive_dsp_parameters()` with the wrapper or with an
unwrapped inner routine, and raising the warning at the point where the
two differ. The version floor remains unpinnable while 2.9.5 is installed
(`Remotes:` has no constraint; pinning breaks install), so surfacing the
warning is the fix that is available today.

### Addendum: the user-facing path, measured

Set `Age = 6, Gender = "Female"` on one demo bundle and
`Age = 50, Gender = "Male"` on another, then ran one `quantify()` over
both with `superassp::trk_formant_forest`. Two observations:

- no warning was emitted on either bundle;
- the `seg_params` column - the per-segment record of the parameters
  actually used - was byte-identical across the two bundles.

Caveat worth stating: if `seg_params` is a constant label rather than the
resolved parameter set, the second observation proves nothing. It should
be confirmed by printing `seg_params` itself, or by comparing a segment
analysed under both settings. The first observation (no warning) stands
regardless, and together with the direct probe below it is the evidence
that the main entry point degrades silently.

### Addendum resolved: seg_params is the parameter list, and it is empty

Printed it: `seg_params` holds the resolved DSP parameters as a list, and
it was `list()` for all seven bundles — one distinct value across the
corpus, including the 6-year-old female and the 50-year-old male bucket.
So the caveat above is discharged and the observation stands: no
parameters were derived on the user-facing path, the DSP ran on its own
defaults, and the caller was not told.

`quantify()` on superassp 2.9.5 therefore silently ignores `Age` and
`Gender`. That is the defect; the warning that would report it exists and
works when called directly, which is what makes the silence a bug rather
than a limitation.

### scout() dead end: the validator is not the culprit

Read `R/segment_list_classes.R:53-75`. The `segment_list` validator does
one thing - it compares `required_cols` against `names(self)` and reports
the difference:

```r
missing_cols <- setdiff(required_cols, names(self))
if (length(missing_cols) > 0) { return(paste0("segment_list missing required columns: ", ...)) }
```

A zero-row tibble that carries every required column therefore passes
validation. So `scout(steps_forward = 99)` is not being rejected for being
empty: the frame produced on the zero-match path must be missing at least
one required column, and the error surfaces through `S7::validate()` at
construction time (`:116`) rather than as a navigation warning.

That changes the fix. It is not "let the validator accept empty data" -
it is "make the zero-match path return the same column set as every other
path", which belongs in the navigation code, not in the class.

Next step, cheap: print the column names the nav helpers build on the
zero-match path and diff them against `required_cols` (`:62-67`). The
difference names the missing column, and the fix is to select it through
consistently. Reproduce with
`query(corp, "Phoneme =~ .+") |> scout(steps_forward = 99)` on the demo
corpus.

### scout() dead end: the reason string is empty, so neither hypothesis holds

Running the reproduction and printing the error gives:

    ERROR: <reindeer::segment_list> object is invalid:

with nothing after the colon. S7 appends the validator's return value
there, so the `required_cols` validator (`:60-75`) returned NULL - it is
not what rejected the object. Combined with the previous note, this rules
out both readings I had:

- it is not "empty data is rejected" (the validator never mentions row
  count), and
- it is not "a required column is missing" (that would have been named).

What remains is the parent class (`.tbl_df_S3_class`) or a property
validator in `properties = list(...)` at `:56-59`, which I have not read.
Next probe, one line: read `:53-60` for the property definitions, then
build the zero-match frame by hand and call `S7::validate()` on it to see
which one objects. `segment_list_classes.R:116` is where construction
happens; the frame arriving there is what needs inspecting.

### scout() dead end: CORRECTION and the actual cause

The previous note is wrong. S7's message is multi-line and I had grep'd
only the first line, so "the reason string is empty" was an artefact of my
own filter. Flattening the newlines gives the real message:

    <reindeer::segment_list> object is invalid:
    - segment_list missing required columns: labels, start, end, db_uuid,
      session, bundle, start_item_id, end_item_id, level, attribute,
      start_item_seq_idx, end_item_seq_idx, type, sample_start, sample_end,
      sample_rate

Every required column is missing, which means the zero-match path hands
the constructor a **column-less** frame, not an empty-but-well-formed one.
`R/reindeer_sequence_ops_optimized.R` constructs results at three sites -
`:330`, `:539`, `:714` - each as `data = as.data.frame(result_dt)`.
When a navigation step matches nothing, `result_dt` carries no columns and
`as.data.frame()` faithfully returns a 0x0 frame.

So the fix is to make the zero-match path return the same columns as every
other path, at those three sites. The guarded shape is:

    data = if (ncol(result_dt) == 0L) <input>[0L, , drop = FALSE] else as.data.frame(result_dt)

with `<input>` being whatever the function's incoming frame is called at
each site - read `:300-335`, `:510-545` and `:690-720` to confirm the name
and that it is eager at that point. Verify by asserting that
`query(corp, "Phoneme =~ .+") |> scout(steps_forward = 99)` returns zero
rows (rather than aborting), and that provenance still records the 100%
loss.


### scout() dead end: FIXED (three sites), one gap remains

`R/reindeer_sequence_ops_optimized.R` built its result as
`as.data.frame(result_dt)` at `:331`, `:540` and `:715`. On a zero-match
step `result_dt` is `rbindlist()` of empty tables, which carries no
columns, so a 0x0 frame reached the constructor and failed validation.

Each site now reads:

```r
data = as.data.frame(if (ncol(result_dt) == 0L) dt[0L] else result_dt),
```

`dt` is the input frame, already in scope at all three sites (`:173`,
`:432`, `:619` / `:177`, `:436`, `:623`).

Verified:

- `query(corp, "Phoneme =~ .+") |> scout(steps_forward = 99)` now
  collects to 0 rows instead of aborting;
- provenance still records the step (2 rows);
- `test_navigation_contracts.R` 18 results, 0 failures;
  `test_lazy_segment_list.R` 97 results, 0 failures.

**Gap, deliberately not papered over:** the 25% loss warning does not fire
on a total loss. With 0 rows out of 0 there is no share to compare against
a threshold, so `dropped_rows()` reports 0% and the warning stays quiet.
Deciding what it should do - warn unconditionally when a step yields
nothing, or stay silent as now - is a judgement call about navigation
semantics, and the vignette now states the current behaviour rather than
the old abort.

### Total-loss warning: where to look next (probe, not guess)

`.maybe_warn_loss()` in `R/segment_list_provenance.R:118` ends in

```r
if (lost / rows_in > thr) { cli::cli_warn(...) }
```

With `rows_in > 0` and `rows_out = 0` that is `1.0 > 0.25`, so the warning
should have fired on the dead-end step and did not. The three lines at the
head of the function - the threshold lookup and whatever guard precedes
the comparison - were not read, and one of them is the likely explanation:
either a guard that returns early when a count is `NA`, or `.nrow_or_na()`
returning `NA` for the `from` argument.

That second possibility has a specific suspect. `.record_step()` is called
as `.record_step(result, .segments, "scout", ...)`, and `.segments` may
still be a `lazy_segment_list` at that point, whose row count is not known
until collected. `NA` there would silence the warning while still
producing a provenance row - which is exactly what was observed: the step
appears in the table and nothing warns.

One-line probe, decisive either way:

```r
p <- query(corp, "Phoneme =~ .+") |> scout(steps_forward = 99)
reindeer::dropped_rows(reindeer::collect(p))   # inspect rows_in / rows_out
```

If `rows_in` is `NA`, the fix is to carry the input's real count into the
step (or seed it when the plan is built); if it is a number, the guard at
the head of `.maybe_warn_loss()` is the thing to change.

### CORRECTION: the total-loss warning does fire

The entry above is wrong, and the mistake was in my tooling twice over.
Wrapping only `collect()` in a warning handler missed it; wrapping the
whole pipeline catches it:

    rlang_warning :: scout: 223 of 223 rows lost (100.0%)

So `lost / rows_in > thr` is evaluated as expected - 223 in, 0 out is a
100% loss against a 25% threshold - and the earlier "the warning does not
fire" claim came from scoping the handler to the wrong call. It is
signalled when the step is applied rather than at collection time.

Nothing to fix here. The vignette paragraph that stated the false
behaviour has been corrected in the same commit as this note. The lesson
worth keeping: a warning handler scoped around `collect()` alone is not
evidence about a lazy pipeline.

### Phase 5, last two vignettes: blocked on uninstalled companions

Measured on this machine:

    erodex        FALSE
    protoscribe   FALSE
    eggstract     FALSE
    extract       FALSE
    superassp     TRUE
    openxlsx      TRUE
    qs2           TRUE

`vignettes/end_to_end_pipeline.Rmd` has ten chunks behind the global
`REINDEER_EVAL_VIGNETTES` gate (`:11`) and one placeholder corpus path
(`:37`); its later sections demonstrate the erodex simulation store and
the protoscribe draft-annotation workflow, neither of which can run
without those packages. `interactive_annotation` needs the EMU-webApp
stub that the serve test already uses.

So these two are not blocked on a missing pattern - the recipe that
converted the other four applies unchanged - but on dependencies that are
not installed here. Unblocking them means installing the companions
(`remotes::install_github("humlab-speech/erodex")` and
`.../protoscribe`) and then following the same four steps: per-chunk
capability gates, `demo_corpus()` for the reindeer-only sections, render
in-process after `load_all()`, and inspect the HTML for real output.

The four vignettes that could be converted without those packages are
done and rendering.

### protoscribe does not install: the GitHub URL 404s

The companion install run finished with `erodex OK`, `protoscribe FAILED`.
Diagnosing:

    curl -sI https://github.com/humlab-speech/protoscribe   ->  HTTP 404
    remotes::install_github("humlab-speech/protoscribe")     ->  error

Caveat that matters before anyone "fixes" a URL: GitHub returns 404 to
unauthenticated requests for **private** repositories as well as for
non-existent ones. So this is either a repo that was renamed/removed or
one this machine has no access to - and the two call for different
actions (update the URL vs. supply credentials).

It does matter, because `https://github.com/humlab-speech/protoscribe` is
cited as the home of the draft-annotation workflow in `CLAUDE.md`, in
`R/companion_protoscribe.R` (the redirect that tells users where the moved
draft functions went), and in the remaining vignette. If the repo is
private, every one of those pointers is a dead end for anyone without
access, and the draft-annotation path cannot be exercised at all on a
machine like this one.

Next step: confirm with the maintainer whether the repository exists
publicly. If it does, the URL needs correcting everywhere it is cited; if
it is private, the docs should say so rather than presenting it as an
installable companion.

`erodex` did install, so `end_to_end_pipeline` is now blocked only on
protoscribe - and its erodex sections can be converted meanwhile.

### CORRECTION on the protoscribe note above

The note above says the URL 404s "which is why it will not install". The
second run shows the mechanism is not that simple. With the error captured
properly:

    remotes::install_github("humlab-speech/protoscribe")  -> returns, no R error
    requireNamespace("protoscribe")                        -> FALSE
    installed.packages() matching /proto|scribe|draft/     -> "erodex" only
    .libPaths()[1] listing, newest first                   -> erodex, reindeer, superassp, colorout

So the install call does not raise an error, and it does not install a
package either. The 404 on the repository page stands, but what it means
is undetermined: a private or renamed repository, or a failure inside
remotes that surfaces as a message rather than a condition. My `tryCatch`
with `error =` would not have caught the latter.

What is established, without needing to know the mechanism:

- `erodex` installs and is available;
- `protoscribe` is not available on this machine and cannot be made
  available by the documented command;
- therefore `end_to_end_pipeline`'s protoscribe sections cannot be
  exercised here, while its erodex sections can.

Worth noting for whoever picks this up: `quiet = TRUE` hides the reason.
Re-running with `quiet = FALSE` will show what remotes actually did.

### protoscribe: SOLVED - it is the `qs` dependency, not the repository

Rerunning with `quiet = FALSE, force = TRUE` shows what actually happens:

    Downloading GitHub repo humlab-speech/protoscribe@HEAD
    Installing 2 packages: RcppTOML, reticulate
    ERROR: dependency 'qs' is not available for package 'protoscribe'
    ERROR: package installation failed
    AVAILABLE: FALSE

Two corrections to the notes above, both mine:

- **The repository is fine.** It downloads. My `curl -sI` returned 404 and I
  read that as a missing or private repo; the sensible reading was the one I
  flagged as a caveat and then argued past - GitHub answers 404 to plenty of
  unauthenticated HEAD requests that later succeed. The URL works.
- **The cause is a stale dependency.** protoscribe declares `qs`, which is no
  longer installable for this R (4.6). reindeer made exactly this migration:
  its serialization moved to `qs2`. protoscribe has not.

So the fix is in protoscribe's DESCRIPTION, not in reindeer and not in this
machine's credentials: `qs` -> `qs2`. Until that lands, `end_to_end_pipeline`'s
protoscribe sections cannot run anywhere, and the docs that present
protoscribe as an installable companion are pointing at a package that is
currently uninstallable from CRAN dependencies alone.

Worth passing to whoever maintains the companion package. For a workaround on
this machine: install `qs` from the CRAN archive if a source build succeeds,
which pins protoscribe to a retired dependency - not recommended as a fix.

### quantify(): the eager path returns no measurement columns (found while vignetting)

Running the end-to-end vignette's pipeline two ways on the demo corpus:

    # lazy, then collect
    v |> quantify(superassp::trk_formant_forest, .at = seq(0,1,0.1)) |> collect()
    -> 28 columns, including F1_Hz ... B4_Hz and .time_point

    # collect, then quantify
    v |> collect() |> quantify(superassp::trk_formant_forest, .at = seq(0,1,0.1))
    -> class extended_segment_list, 16 columns, all of them the base
       segment columns - no F1_Hz, no .time_point, no .cache_status

So the two entry paths disagree: quantifying an already-eager list returns
an object whose measurement columns are absent from `names()` (whatever
happens to them may be in an S7 property, but nothing documents or tests
that). Anything that does `collect() |> quantify()` silently gets no
measurements.

Worth deciding which shape is intended and making both paths agree;
`vignettes/end_to_end_pipeline.Rmd` now uses the lazy-then-collect order
that demonstrably carries the columns. Regression worth writing: assert
that both orders yield the measurement columns.

### quantify() eager path: two returns, one of them bare

`R/segment_list_quantify.R` has two exits on the eager path:

```r
294    return(extended_segment_list(data = as.data.frame(object)))   # no DSP columns
...
327    dsp_cols <- setdiff(names(combined), c(segment_cols, ...))
336    result <- extended_segment_list(data = combined, ...)          # with them
```

The measuring run (`collect() |> quantify()` on the demo corpus) came back
with 16 columns and no `F1_Hz`, which matches the `:294` shape exactly -
so that branch was taken, not the combining one at `:336`. What guards it
is at `:285-295` and I have not read it.

One probe settles it:

```r
eager <- reindeer::collect(query(corp, "Phonetic =~ [aeiou]"))
q <- quantify(eager, superassp::trk_formant_forest, .at = 0.5)
c(ncol = ncol(q), "F1_Hz" %in% names(q), dsp_columns = length(q@dsp_columns))
```

If `dsp_columns` is empty the DSP never produced anything and the guard is
about the input; if it is populated while `names(q)` lacks `F1_Hz`, the
columns are being dropped between `:327` and `:340`. Read `:285-295` next
either way - that condition is the whole question.

### CORRECTION: there is no eager/lazy divergence - it was an empty input

Both earlier notes on this are wrong. Measured properly:

    collect() |> quantify(trk_formant_forest, .at = 0.5)   ->  28 cols, 10 dsp_columns, F1_Hz present
    quantify(...) |> collect()                              ->  28 cols, F1_Hz present

The 16-column result came from the one input that had **0 rows**:

    query "Phonetic =~ [aeiou]" |> ascend_to("Syllable") |> filter(end - start > 30)
    -> rows: 0

So `quantify()` on an empty list returns the base columns unchanged, which
is reasonable, and the eager path is fine. My "the two entry paths
disagree" claim was an artefact of a query that matched nothing.

That also explains the `end_to_end_pipeline` render failure completely: the
vignette's query filtered syllables to `> 30` and matched zero of them, so
the following column subset (`F1_Hz`) had nothing to select. The vignette
conversion failed on my query, not on the package - the filter threshold
needs to suit syllable durations, or be dropped.

The `quantify()` warning added in the same commit stands on its own merit:
when a DSP run yields no results for a **non-empty** input, the bare
return is now reported instead of silently handing back the input. It does
not fire for empty inputs (nothing to report) or for working runs -
both verified.

### end_to_end_pipeline converts, and three API mismatches fall out

The vignette now renders (37.6 KB) with real output for load, metadata,
query, quantify, cache, export and FAIR sections, and explicit gates where
a companion is absent (protoscribe, eggstract). Making it run turned up
three genuine mismatches between helpers and the data they are handed:

1. **`autoplot(type = "formants")` looks for `F1..F5`** while `quantify()`
   returns `F1_Hz..`. The package's own plotting helper therefore cannot
   plot the package's own measurements: "No formant tracks (F1..F5) found
   to plot."
2. **`geom_formant_trajectory()` requires a `rel_time` column** that
   `pivot_tracks_longer()` does not produce ("Column `rel_time` not
   found").
3. **`erodex::quantify_simulate()` errors with "subscript out of bounds"**
   on the call shape the vignette documents - the documented example does
   not run.

Each is recorded in the vignette at the point it appears and gated so the
document builds. They are not documentation defects: the helpers and the
measurements disagree, which is worth fixing on one side or the other - and
a regression test asserting that quantify() output plots with autoplot()
would pin whichever side is chosen.

### autoplot formants: the pivot returns nothing (regex was a red herring)

Tried relaxing the filter in `.autoplot_extended_segment_list` from
`^F[12345]$` to a prefix pattern, on the theory that the `_Hz` suffix was
the mismatch. It still aborted, so I printed the data it filters:

    pivot_tracks_longer(quantified)$track   ->  (empty)

`pivot_tracks_longer()` produces **no rows** for a quantify() result, which
is why the filter finds nothing - and why relaxing the pattern changed
nothing. The `type = "formants"` path cannot work on quantify() output
because the reshape in front of it yields an empty frame, not because of a
naming mismatch. The change was reverted; the tree is clean.

Next probe, one line, and it decides the shape of the fix:

    q <- quantify(segs, superassp::trk_formant_forest, .at = seq(0,1,0.2))
    str(pivot_tracks_longer(q, .keep_metadata = TRUE), max.level = 1)

If that frame has rows and columns other than `track`, the reshape names
its track column something else and the autoplot filter should read that;
if it is genuinely empty, the reshape is what needs fixing. Whatever the
answer, the regression to write is the one the vignette asked for: a
quantified segment list must plot with `autoplot(type = "formants")`.

### pivot_tracks_longer(): root cause is a naming contract the quantifier does not honour

Reading `R/segment_list_pivot.R` with the 0x0 measurement in hand:

- `.parse_track_name()` (`:17-26`) splits a column name into `track` and
  `rel_time`, i.e. it expects wide columns named `<track>_<rel_time>`.
- The wide path (`:111-137`) drops every column whose name does not parse -
  `:116` says so: "Drop unparseable wide cols silently - they may be scalar
  measurements without a time suffix".
- With `wide_cols` empty the result is an empty frame, which is exactly the
  0x0 measured above. There is no error, because the guard that would
  complain ("No track columns detected to pivot", `:90`) tests the columns
  the *detector* finds, not the ones that survive parsing.

And what does `quantify(.at = seq(0, 1, 0.2))` actually produce? `F1_Hz`,
`F2_Hz`, ... plus a separate `.time_point` column - not `F1_0.2`,
`F2_0.2`. So the parser is looking for a naming contract the quantifier
never implemented, while `pivot_tracks_longer()`'s own documentation
(`:42`) claims the opposite ("produced by `quantify(.at = seq(0, 1,
0.1))`").

That is the defect: two halves of the package disagree about the wide
format. Fixing it means choosing one - teach the pivot that `.time_point`
carries the time and `F1_Hz` the track, or have `quantify(.at=)` emit
`<track>_<time>` - and the `autoplot(type = "formants")` failure and
`geom_formant_trajectory()`'s missing `rel_time` are both downstream of the
same choice.

Evidence to keep, all measured on the demo corpus: the pivot returns 0x0;
`autoplot(type = "formants")` aborts on quantified data; the vignette's
quantified frame has 28 columns including `F1_Hz` and `.time_point`.
