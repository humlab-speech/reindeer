# reindeer (development version)

- Quantify cache I/O is now batched: cache reads use chunked `IN`
  queries (with a single `accessed_at` touch per chunk) and misses are
  written in one transaction, replacing the per-row SELECT + UPDATE and
  per-miss SELECT-SUM + INSERT pattern.

- Fixed swapped argument order in `enrich(corpus)`'s persistent-cache
  calls (`.get_persistent_cache`/`.set_persistent_cache` were invoked as
  `(conn, key)`), which made the enrich cache path error whenever
  `.use_cache` was enabled.

- `enrich(corpus, ...)` now defaults to `.use_cache = TRUE`, so signal
  files whose cached result (keyed on file mtime + DSP params) already
  exists are skipped instead of re-running DSP corpus-wide. `.force =
  TRUE` bypasses the cache read and recomputes (still overwriting the
  cached entry), which makes the previously-ignored `.force` argument
  effective.

- Fixed `quantify()` age/gender DSP-norm derivation: parameters are now
  resolved per bundle (the single-row contract) instead of passing the
  whole metadata table to `derive_dsp_parameters()`, which recycled
  vectorised `Age`/`Gender` and returned an arbitrary norm row for
  heterogeneous corpora. Quantify cache keys now digest the effective
  per-segment parameters, preventing cross-speaker cache collisions.

- `corpus()` now defaults to `quick = TRUE`: the SQLite cache and
  metadata cache are reused when present instead of being rebuilt on
  every open. Set `quick = FALSE` to force a full re-sync after manual
  edits to `METADATA.json` or annotation files.

# reindeer 1.0.1 (2026-05-15)

Test-only patch — no production code changes.

- `tests/testthat/test_query_optimized.R` no longer calls
  `library(emuR)` at the top of the file. That call put emuR's
  `query()` last in the runner's search path so every unqualified
  `query(ae_path, ...)` resolved to `emuR::query()` and aborted in
  `check_emuDBhandle()` (`$ operator is invalid for atomic vectors`).
  emuR usages are now fully qualified (`emuR::create_emuRdemoData`,
  `emuR::load_emuDB`) and `setup_test_db()` calls `skip_if_no_emuR()`
  up front. The ~85 errors previously emitted by this file under
  `devtools::test()` and `R CMD check` are gone.

# reindeer 1.0.0 (2026-05-15)

First stable release. Bundles the six feature stages shipped in
0.9.4 - 0.9.9 into a coherent v1.0 baseline. No new code or breaking
changes vs 0.9.9 — only the version label, the consolidated release
notes below, and final test verification.

The v1.0 work targeted the three productivity axes called out in the
roadmap (`/Users/frkkan96/.claude/plans/the-reindeer-package-aims-sorted-pie.md`):
tidyverse depth, FAIR / interop exports, and sister-package glue.
Lazy-optimizer work and richer error data-slots remain on the v1.1
roadmap (see *Future Work* below).

## Highlights since 0.9.3

### Tidyverse depth
- 0.9.4: track-aware `pivot_tracks_longer()` / `pivot_tracks_wider()`,
  `nest_by_speaker()` / `nest_by_session()` / `nest_by_bundle()`, plus
  tidyselect helpers `segment_cols()`, `metadata_cols()`,
  `track_cols()`, `signal_cols()`.
- 0.9.7: `autoplot()` S3 methods for `segment_list` /
  `extended_segment_list` with auto-detected types (formants, pitch,
  spectrogram fallback, labels) and helper geoms
  `geom_formant_trajectory()`, `geom_label_tier()`,
  `geom_pitch_track()`.

### FAIR / interop exports
- 0.9.5: `describe_corpus()` now emits five FAIR artifacts in one
  call (README, CMDI, DataCite, CITATION.cff, schema.org JSON-LD).
- 0.9.6: `read_textgrid()` / `write_textgrid()` for Praat
  `.TextGrid` (long + short format, UTF-8 / UTF-16 BOM), and
  `read_eaf()` / `write_eaf()` for ELAN `.eaf` (deterministic
  `ts1..tsN` slot allocation, one TIER per level, optional media
  descriptor).

### Sister-package glue
- 0.9.8: `quantify_egg()` / `enrich_egg()` route EGG-track DSP via
  eggstract (gated by bundle-level `HasEGG`). `propose_annotations()`
  dispatches to protoscribe's draft generators (VAD / VOT / periods /
  MOMEL-INTSINT). New end-to-end vignette
  `vignettes/end_to_end_pipeline.Rmd`.

### Interactive ergonomics
- 0.9.9: RStudio addin gadgets — `browse_corpus_gadget()` for a
  tree-view session/bundle browser, `edit_metadata_gadget()` for an
  editable `DT::datatable` whose changes round-trip through
  `add_metadata()`.

## Test coverage added in v1.0

122 new test cases across `test_pivot_tracks.R`, `test_describe.R`,
`test_interop_textgrid.R`, `test_interop_elan.R`, `test_autoplot.R`,
`test_companion_glue.R`, and `test_addins.R`. All passing on
`load_all()` + `test_file()`.

## Future work (deferred to v1.1)

- Lazy `explain()` and query optimizer (filter pushdown into the SQL
  builder, lazy `biographize`, serialization-safe lazy plans).
- Richer error data-slots (`reindeer_query_error$problematic_token`,
  `$position`, `$suggested_fix`) and recovery helpers.
- DOI minting on top of `describe_corpus()` (Zenodo / DataCite API).
- BIDS-speech export.
- DuckDB cache backend as an analytic-workload option.
- EAF export hook into `R/reindeer_autosync.R` for bidirectional
  sync once round-trip semantics settle.

# reindeer 0.9.9 (2026-05-15)

Interactive ergonomics, stage 6 of v1.0 roadmap: RStudio addin gadgets
for corpus browsing and metadata editing.

- New `browse_corpus_gadget(corpus)` opens a miniUI dialog with a
  session/bundle tree and a side pane displaying
  `collect_corpus_summary()` plus the selected bundle's metadata.
  Returns the corpus invisibly so the call composes with pipelines.
- New `edit_metadata_gadget(corpus, level)` opens an editable
  `DT::datatable` over `metadata_session` or `metadata_bundle`. On
  accept, the diff is computed via `.metadata_diff()` and applied via
  [add_metadata()] so `METADATA.json` files remain authoritative.
- `inst/rstudio/addins.dcf` registers both gadgets so they appear in
  RStudio's Addins menu under "Browse Corpus" and "Edit Metadata".
- Added `shiny`, `miniUI`, `DT` to `Suggests`. Both gadgets abort with
  `reindeer_missing_companion_error` when any of those is missing.
- `.metadata_diff()` is exposed as an internal helper for unit
  testing; gadget logic is tested headlessly via 14 cases covering
  identical snapshots, value changes, additions, removals, and
  level-aware key columns.

# reindeer 0.9.8 (2026-05-15)

Sister-package glue, stage 5 of v1.0 roadmap: first-class wrappers for
the eggstract and protoscribe companions, plus an end-to-end pipeline
vignette.

- New `quantify_egg()` / `enrich_egg()` route EGG-track measurement
  to the eggstract companion. `quantify_egg()` filters input
  segments to bundles whose `HasEGG` metadata flag is truthy
  (gateable with `.require_egg_flag = FALSE`) and forwards to
  [quantify()] with `eggstract::ksvF0` as the default DSP function.
  Aborts with `reindeer_missing_companion_error` when eggstract is
  not installed.
- New `propose_annotations(corpus, type)` dispatches to one of
  protoscribe's draft generators
  (`draft_vad`, `draft_vot`, `draft_periods`, `draft_momel_intsint`)
  and optionally launches `serve_app()` for interactive review. The
  `.commit` argument is reserved for a future release; suggestions
  are returned for inspection only by default.
- New vignette `end_to_end_pipeline.Rmd` walks a corpus from raw
  recordings to publishable FAIR artifacts using reindeer +
  superassp + eggstract + protoscribe + erodex in one narrative.
- 6 new test cases in `tests/testthat/test_companion_glue.R` exercise
  the missing-companion gates and the `HasEGG` filtering logic.

# reindeer 0.9.7 (2026-05-15)

Visual analytics, stage 4 of v1.0 roadmap: ggplot2 autoplot methods and
helper geoms for segment_list / extended_segment_list.

- New `autoplot()` S3 methods for `segment_list` and
  `extended_segment_list`. The extended method auto-detects an
  appropriate `type` (formants when `F1_`/`F2_` columns are present,
  pitch when `F0_`/`pitch_` columns are present, labels otherwise) and
  pivots wide tracks through `pivot_tracks_longer()` for plotting.
  `type = "spectrogram"` is recognised but falls back to labels with
  a `cli_alert_info` when the raw signal isn't available.
- New helper layers: `geom_formant_trajectory()`,
  `geom_label_tier()`, `geom_pitch_track()` — thin wrappers that pick
  sensible aesthetic mappings for the canonical `track_long` shape so
  users don't have to memorise the plumbing.
- `ggplot2` added to `Suggests`. Methods abort with
  `reindeer_missing_companion_error` when called without it. S3
  registration happens in `.onLoad` only if `ggplot2` is installed.
- 8 new tests covering label, formant, pitch, spectrogram-fallback,
  and the three helper geoms.

# reindeer 0.9.6 (2026-05-15)

Field-standard interop, stage 3 of v1.0 roadmap: Praat TextGrid and
ELAN .eaf round-trip.

- New `read_textgrid()` / `write_textgrid()` for Praat `.TextGrid`
  files. Pure-R implementation with BOM-based UTF-8 / UTF-16 detection,
  support for both long and short text formats, and tier-per-level
  output from a `segment_list`. Returns a flat tibble of `tier`,
  `type`, `start`, `end`, `label`.
- New `read_eaf()` / `write_eaf()` for ELAN `.eaf` files. Emits a
  minimal but valid EAF 2.8 document with deterministic `ts1..tsN`
  time-slot IDs, one `<TIER>` per level, and an optional media
  descriptor. Reads `ALIGNABLE_ANNOTATION` rows back into a tibble
  with `tier`, `parent`, `linguistic_type`, `start`, `end`, `label`.
- Both writers accept any `segment_list` or compatible tibble with
  `start`, `end`, `labels`, `level` columns; segment_list times in ms
  are converted to seconds for TextGrid output and to integer ms for
  EAF output.
- Added `rPraat` and `withr` to Suggests for optional acceleration and
  tests.
- 12 new TextGrid tests and 11 new EAF tests, all passing.

# reindeer 0.9.5 (2026-05-15)

FAIR completion, stage 2 of v1.0 roadmap: CITATION.cff and schema.org
JSON-LD adjuncts to `describe_corpus()`.

- `describe_corpus()` gains two new format keys: `"cff"` writes a
  Citation File Format 1.2.0 `CITATION.cff` at the corpus root, and
  `"jsonld"` writes a schema.org `Dataset` JSON-LD document as
  `_corpus_jsonld.json`. The default `formats` argument now includes
  both, so `describe_corpus(corp)` produces five FAIR artifacts in one
  call (README, CMDI, DataCite, CFF, JSON-LD).
- Both new emitters share `collect_corpus_summary()` with the existing
  README/CMDI/DataCite path; team members in `METADATA.json` are split
  into `family-names` / `given-names` for CFF and rendered as
  `schema.org Person` for JSON-LD. With no team metadata, CFF falls
  back to a placeholder author and prints a `cli_alert_warning`.
- Force semantics match the existing emitters: pre-existing files
  remain untouched and a `*-generated` companion is written instead;
  `force = TRUE` or a dirty metadata bit overwrites in place.
- Five new test cases in `tests/testthat/test_describe.R` cover CFF
  shape, JSON-LD shape, placeholder fallback, team-driven authors,
  and the no-clobber rule.

# reindeer 0.9.4 (2026-05-15)

Tidyverse depth, stage 1 of v1.0 roadmap: track-aware pivot, nesting, and
tidyselect helpers for segment_list / extended_segment_list.

- New `pivot_tracks_longer()` / `pivot_tracks_wider()` reshape wide-form
  DSP measurements (`F1_0.0`, `F1_0.5`, …) and list-column tracks to / from
  long form. The long-form result is a `track_long` tibble that carries
  `db_uuid`, `db_path`, and provenance as attributes; one segment maps to
  many rows so the segment_list class intentionally drops on the way out.
- New `nest_by_speaker()` / `nest_by_session()` / `nest_by_bundle()`
  return a one-row-per-group tibble with a list-column whose entries are
  per-group segment_list slices. The nest step is appended to the
  provenance log.
- New tidyselect helpers `segment_cols()`, `metadata_cols()`,
  `track_cols()`, and `signal_cols()` partition columns into required
  segment, DSP-derived track, and metadata groups. Usable inside
  `dplyr::select()` and any other tidyselect-aware verb.
- `tidyselect` and `rlang` added to `Imports`.
- Internal: the required-column list is now sourced from a single
  `.required_segment_cols()` helper (previously duplicated across
  `R/segment_list_dplyr.R`).

# reindeer 0.9.3 (2026-05-13)

CI fix for the pkgdown deploy.

- `.github/workflows/pkgdown.yaml` now installs only hard
  dependencies (`dependencies: '"hard"'`) plus explicit `knitr` and
  `rmarkdown`. Skipping Suggests avoids compiling the heavy GitHub
  packages `superassp` and `av` during every docs build. All
  vignettes use `eval = FALSE`, so the runtime DSP companions are
  not needed to render the site. The first post-merge CI run failed
  at the `setup-r-dependencies` step because of these compile
  attempts; this change unblocks the deploy.

# reindeer 0.9.2 (2026-05-13)

pkgdown / GitHub Pages alignment.

- `_pkgdown.yml` rewritten to reflect the v0.9.0 API surface. Drops
  references to functions removed before v0.9 (`peek_at`, `ask_for`,
  `biographize`, `quantify_simulate`, `enrich_simulate`,
  `reminisce*`, `suggest`, `transcribe*`, `build_emuDB_cache`,
  `compute_signal_hash`, `dspp_metadataParameters_dt`) and to four
  deleted vignettes (`reindeer_workflow`, `query_benchmarks`,
  `simulation-infrastructure`, `Tidy_speech_processing`). Reference
  index reorganised into nine sections matching the canonical verbs
  (Corpus / Query / Measurement / Metadata / Navigation / Cache /
  Provenance / Psychoacoustic helpers / Interactive annotation), with
  deprecated aliases (`add_metadata`, `gather_metadata`,
  `import_metadata`, `serve`, `st`) hidden via the `internal:`
  section. Home title and description rewritten to match the
  scientific-tool framing of the new `README.md`. Author updated to
  Fredrik Nylén with ORCID link. `lang: en` and an `opengraph` block
  added.
- `.github/workflows/pkgdown.yaml` modernised to the current r-lib v2
  reference recipe: `actions/upload-pages-artifact@v3` +
  `actions/deploy-pages@v4`. Removes the `JamesIves/...` deploy step
  and the `gh-pages` branch dependency. Adds `pages: write` /
  `id-token: write` permissions and explicit `r-version: release`.
  Deployment requires switching the repo's Pages source to "GitHub
  Actions" once.
- `.github/workflows/R-CMD-check.yaml` no longer references the
  dead `S7speedy` branch; `v0.7-breaking` added until it merges to
  `main`.
- `docs/` is no longer tracked in git; `.gitignore` updated. CI
  regenerates the site on every push to `main`.
- Stale `vignettes/transcription_workflow.Rmd` deleted (the "moved
  to protoscribe" stub is already covered by `README.md`).
- `vignettes/interactive_annotation.Rmd` updated to call
  `serve_app()` instead of the deprecated `serve()` alias, and the
  broken `vignette("transcription_workflow")` cross-link removed.
- `README.md` no longer references the non-existent
  `man/figures/logo.png`.

# reindeer 0.9.1 (2026-05-13)

Documentation patch.

- `README.md` rewritten to match the v0.9.0 API and to present
  reindeer as a tool for reproducible speech-corpus analysis rather
  than a feature list. Removes stale verb names (`biographize`,
  `gather_metadata`, `add_metadata`, `import_metadata`), the
  duplicate `query()` listing, the deprecated `.meta_json`
  reference, the obsolete "Tidy speech processing" vignette link,
  and the 0.1.x citation. Adds capability tour for speaker-aware
  DSP, lazy + provenance, persistent cache with `.cache_status`,
  FAIR / CMDI export, `serve_app()`, and the `eggstract` companion.

# reindeer 0.9.0 (2026-05-12)

End of a two-sprint API + documentation overhaul. No new analysis
capability — every change is in service of making the existing
capability easier to find and use.

## API simplification (sprint 1, v0.8.1–v0.8.10)

- `enrich()` is now an S7 generic dispatching on corpus / segment_list
  / lazy_segment_list / extended_segment_list. One verb for both
  corpus-wide DSP and per-segment metadata joins.
- `scout()` / `ascend_to()` / `descend_to()` converted to proper S7
  generics with explicit eager and lazy methods.
- `load_metadata(corp, source = c("files", "excel"), path = NULL)` and
  `set_metadata()` are the canonical metadata entry points;
  `gather_metadata()` and `add_metadata()` remain as deprecated
  aliases for one cycle.
- `inspect_cache(corp)` summarises every reindeer-managed cache at
  once. `quantify()` results now carry a `.cache_status` column
  (`"hit"` / `"miss"`) when caching is enabled.
- `quantify()` cache key now hashes `.at` (extraction time points) so
  calls with different `.at` no longer collide.
- EQL parser is stricter: malformed queries like `"Phonetic =="`
  (missing right-hand value) abort at `query()` time with a
  caret-pointer error, not later inside `collect()`.
- New public inspector `dsp_parameters(age, gender)` /
  `dsp_parameters(corpus_obj)` shows which DSP parameters will be
  selected for any speaker.
- `serve_app()` is a name-clash-free alias for `serve()`, useful
  when `emuR` is also attached.
- FAIR artifacts (README, CMDI XML, DataCite JSON) auto-regenerate on
  the dirty bit when `options(reindeer.auto_cmdi = TRUE)`.
- New `dropped_rows(seg)` returns a per-step row-loss summary.

## Documentation overhaul (sprint 2, v0.8.11–v0.9.0)

- 194 → 55 man pages: 138 unexported helpers marked `@noRd`. The user
  help index now shows only user-facing verbs.
- `st()` renamed to `semitones()`; `st()` retained as a deprecated
  alias. `erb()` docs rewritten for a user audience.
- `derive_dsp_parameters()`, `get_corpus_cached()`, and
  `create_cmdi_metadata()` marked `@keywords internal` so they
  disappear from the user help index while staying callable from
  companion packages.
- All core-verb roxygen blocks rewritten to follow a user-focused
  template (one-line summary, 2–4 sentence intro, user-facing
  parameters only, copy-pasteable example, `@seealso` cross-links).
  Killed S7-slot dumps, websocket-protocol notes, qs performance
  brags, and `as of v0.7.0…` history.
- Vignettes rewritten:
  - `getting_started.Rmd` (688 → 175 LOC), recipe-focused.
  - `metadata_management.Rmd` (253 → 130 LOC), three-level inheritance.
  - `cache_management.Rmd` (257 → 95 LOC), built around
    `inspect_cache()`.
  - new `lazy_and_provenance.Rmd` documenting lazy plans,
    `provenance()`, `dropped_rows()`, and the 25 % loss warning.
- Removed: `vignettes/reindeer_workflow.qmd` (overlapped with
  `getting_started`), `vignettes/query_benchmarks.qmd` (moved to
  `benchmarking/QUERY_BENCHMARKS.md`).
- DESCRIPTION Suggests now lists `erodex` and `eggstract` (previously
  only mentioned in comments).
- New classed condition `reindeer_missing_companion_error` with
  install-hint message when a companion package is needed.

## Behavioural notes for v0.8 users

- The `quantify()` cache key changed; existing cached results from
  pre-0.8.4 will re-compute on first call after upgrade.
- Lazy `query()` rejects more malformed EQL eagerly. Wrap risky
  user-supplied EQL in `tryCatch(...,
  reindeer_query_error = ...)`.

# reindeer 0.8.0 (2026-05-12)

## New exports (companion-package API)

Three internal helpers are now part of the public API so the **erodex**
companion package can use them without `:::`:

- `get_corpus_cached(segments, from = NULL)` — resolve a `corpus` from a
  `segment_list`'s `db_path` property, with in-memory caching.
- `derive_dsp_parameters(dsp_fun, metadata, metadata_fields, user_params)` —
  map Age/Gender metadata to DSP function parameters (e.g. `nominalF1`).
- `check_cache_size(cache_path, ...)` — size-check a cache file/directory and
  emit threshold warnings.

## Documentation

- All references to the companion package renamed from `reindeer.simulation`
  → **erodex** (`CLAUDE.md`, `NEWS.md`, `README.md`, `VIGNETTES_SUMMARY.md`,
  `inst/agents/AGENT_GUIDE.md`, `doc/cache_management.R`, `R/zzz.R`).
- Removed stale simulation code examples from `README.md` and
  `VIGNETTES_SUMMARY.md` (functions live in erodex).

<hr/>

# reindeer 0.7.0 (2026-05-11)

## Breaking changes — API minimization

`reindeer` 0.7.0 trims the public surface from 44 exports in 0.6.1 to
28. Audit the changes against your scripts before upgrading.

### Renamed

- `ask_for()` is **removed**. Use `query()` everywhere. The parameter
  formerly called `query` is now `eql`. There is no soft-deprecation
  alias — calls to `ask_for()` will fail with "could not find function".

### New default: lazy queries

- `query(corp, "...")` now returns a `lazy_segment_list` by default.
  Auto-collect S3 methods on `dim` / `length` / `$` / `[` / `[[` /
  `head` / `tail` / `as.data.frame` / `as_tibble`, plus the entire
  dplyr verb family (`filter`, `mutate`, `select`, `arrange`, `slice`,
  `rename`, `distinct`, `transmute`, `group_by`, `ungroup`,
  `summarise`, `count`, `tally`, all `*_join`) materialise on first
  data access and delegate. Existing pipelines keep working.
- Pass `lazy = FALSE` to `query()` for the old eager behaviour.
- `lazy_segment_list` is now exported (was internal).

### Unexported

The following are still available as `reindeer:::name` but no longer
part of the public surface:

- `add_digests`, `update_signal_hashes`, `get_signal_hashes` — niche
  signal-digest provenance helpers.
- `get_handle` — legacy emuR-handle compatibility shim.
- `set_specOverlay`, `set_osciOverlay` — emuR-style perspective config.
- `dropped` — single-column accessor on `provenance(seg)`.
- `is_extended_segment_list` — use `inherits()` /
  `S7::S7_inherits()` directly.
- `bundle_list` — internal S7 class.
- `retreat` — equivalent to `scout(steps_forward = -n)`.
- `disable_sync` — auto-sync configuration should live on the corpus.
- `biographize(seg, corp)` — fold into `enrich(seg, corp, with = "metadata")`.

### Companion package: `erodex`

The parameter-grid simulation subsystem
(`quantify_simulate` / `enrich_simulate` / `reminisce` /
`reminisce_tracks` / `list_simulations`) and the
`update_signal_hashes` / `get_signal_hashes` helpers that validate
its caches moved to a sibling package **erodex**. ~1900
LOC of `R/simulation_*.R` no longer ship with the reindeer core.
Use `library(erodex)` to access them; the package
depends on `reindeer >= 0.7.0`.

## New features

### Auto-regenerated FAIR metadata

`add_metadata()` flips a `.cmdi_dirty` sentinel at the corpus root.
`describe_corpus()` consumes it and rewrites README / CMDI / DataCite
automatically, then clears the flag. No more `force = TRUE` after every
metadata edit.

### Classed conditions

`reindeer_query_error`, `reindeer_schema_error`, `reindeer_cache_error`
(all inheriting from `reindeer_error`) are attached to every abort
inside the query parser, query executor, schema validator, and cache
size manager. Downstream code can catch reindeer-originated errors
without string-matching the message:

```r
tryCatch(query(corp, "bad EQL"),
         reindeer_query_error = function(e) handle_parse_failure(e),
         reindeer_error       = function(e) handle_other(e))
```

### Named provenance for dplyr joins

Joins on a `segment_list` now log their specific verb in `provenance()`
(`left_join`, `right_join`, `inner_join`, `full_join`, `anti_join`,
`semi_join`) instead of the generic `"dplyr_op"`. Row-loss above
`getOption("reindeer.loss_warn")` fires a `cli::cli_warn`, closing the
silent-loss gap from the v0.7 evaluation.

### Provenance survives serialisation

New tests assert that `attr(seg, "reindeer_provenance")` round-trips
through `saveRDS` / `readRDS` and `qs::qsave` / `qs::qread`.

## Known follow-up work (v0.7.x)

Three EQL parser extensions and one positional-error feature remain on
the v0.7 roadmap and ship in 0.7.x:

- Bundle / Session filter predicates in EQL.
- Position-of-element `[n]` syntax.
- Label groups `{group}` and aliases `@alias`.
- Caret pointer (`^^^`) in EQL parse-error messages.

A small number of lazy-SQL parity gaps (INTERSECT across distinct
hierarchy levels, ORDER BY before compound operators in some function
queries, WITH wrappers inside sub-queries) also remain — exposed by
the `lazy = TRUE` default flip and tracked alongside the parser work.

<hr/>

# reindeer 0.6.1 (2026-05-11)

## Lazy SQL for sequence / dominance / function queries

`ask_for(corp, q, lazy = TRUE) |> collect()` now works for every EQL
query type that the eager path supports (simple, sequence, dominance,
the position functions Start/End/Medial, and the count function Num),
not just simple equality / regex. Conjunction and disjunction
recurse through `build_base_sql` and benefit transparently when
their children are shippable.

Internally the four eager executors in `R/query_parser.R` were
refactored to "build SQL" + "execute SQL" so the lazy stubs in
`R/query_executor.R` reuse exactly the same SQL builders — there is
now one source of truth per query type. Non-simple sub-queries of a
sequence or dominance are materialised at lazy-build time (Option B
in the implementation plan) and embedded as item-id literals; the
outer query stays a single deferred SQL statement.

Two small `collect()`-side fixes shipped alongside: `collect()` now
uses `.open_query_connection` so REGEXP is available for dominance
sub-queries, and it skips the `params =` argument to `dbGetQuery`
when the params list is empty (DBI rejects empty params lists when
the SQL has no `?` placeholders).

<hr/>

# reindeer 0.6.0 (2026-05-11)

## Deferred quantify on lazy_segment_list

`quantify()` now has a method on `lazy_segment_list`. When called on a
lazy chain it does NOT execute DSP — it appends a `"quantify"` entry to
`query_parts$post_transforms` and returns the same lazy object. The
DSP runs at `collect()` time on the materialized segment_list.

This lets a single pipeline carry through to DSP without paying the
cost unless the result is actually needed:

```r
ask_for(corp, "Phonetic == t", lazy = TRUE) |>
  scout(1) |>
  quantify(superassp::forest) |>
  collect()
```

`enrich()` is still eager (it operates on the corpus rather than on a
segment list, so the lazy semantics differ); deferring it is left for
a future release. Lazy SQL building for sequence / dominance / function
queries (the open TODOs in `R/query_executor.R`) is also deferred.

<hr/>

# reindeer 0.5.2 (2026-05-11)

## Single canonical metadata write path

`add_metadata()` and the `[<-` bracket-assignment operator on
`corpus` objects now share one writer (`corpus_assign_metadata` ->
`set_metadata_database`/`set_metadata_session`/`set_metadata_bundle`).
Internal helper `write_metadata_to_json()` has been removed.

`process_metadata_list()` now uses `INSERT OR REPLACE` so re-applying
the same field at the same level is idempotent. The "Type validation"
test was previously relying on an incidental UNIQUE-constraint
failure; it has been updated to reflect the new (correct) behavior,
with a comment that opt-in type validation may return in a future
release.

`set_metadata_bundle()` now errors instead of silently creating dirs
when a literal bundle does not exist. Use `create_session_and_bundle()`
to create a new bundle before attaching metadata.

## CLI migration complete

The remaining seven plain `stop()` / `warning()` calls in
`R/test_utilities.R` have been migrated to `cli::cli_abort` /
`cli::cli_warn`. A new `tests/testthat/test_error_messages.R` lint
guard prevents regressions: any future plain `stop()` / `warning()`
under `R/` (excluding `R/deprecated/`) will fail the test suite.

<hr/>

# reindeer 0.5.1 (2026-05-10)

## JSON-Schema validation for `_DBconfig.json` and `METADATA.json`

Two schemas ship under `inst/schemas/`. `load_DBconfig()` validates on
read; `store_DBconfig()` and the metadata write paths
(`set_metadata_*`, `write_metadata_to_json`) validate before writing.
Read-side validation is soft-warn by default to keep existing
non-conformant corpora loading; opt in to hard errors with
`options(reindeer.schema_strict = TRUE)`. Write-side validation is
always strict.

A new exported `validate_corpus(corp)` walks all corpus JSON files
and returns a tibble of validation results.

`jsonvalidate` is now in `Imports`.

## `describe_corpus()` exports README + CMDI + DataCite

A single call writes a README.md, the existing CMDI XML, and a
DataCite 4.5 JSON document for a corpus. Outputs derive from a shared
`collect_corpus_summary()` snapshot. Existing files are preserved
unless `force = TRUE` (otherwise the new file gets a `-generated`
suffix). Project / funding / team metadata at the database level
flow into both README and DataCite output.

(The function is named `describe_corpus()` rather than `describe()`
to avoid colliding with `testthat::describe()`.)

<hr/>

# reindeer 0.5.0 (2026-05-10)

## Tidyverse-friendly segment_list

`segment_list` and `extended_segment_list` now inherit from `tbl_df`, `tbl`,
and `data.frame`. Tidyverse verbs (`dplyr::filter`, `mutate`, `arrange`,
`select`) and base bracket subsetting preserve the class and `db_uuid` /
`db_path` properties. When a column required by the validator is dropped,
the result downcasts to a plain tibble.

The change is implemented through registered `vctrs::vec_proxy` /
`vec_restore` and `dplyr::dplyr_reconstruct` methods plus a class-aware `[`
method. `dplyr` and `vctrs` are now in `Suggests` so dplyr operations only
require dplyr if you actually use them.

## Pipe-loss provenance accounting (`provenance()`, `dropped()`)

Every verb that touches a `segment_list` now appends a structured row to
its `reindeer_provenance` attribute, recording verb name, deparsed call,
input/output row counts, and timestamp. Two new exported accessors:

* `provenance(seg)` returns the full log as a tibble.
* `dropped(seg)` returns cumulative loss; `dropped(seg, step)` returns
  per-step loss.

Navigation verbs (`scout`, `ascend_to`, `descend_to`) emit a `cli` warning
when more than 25% of input rows are dropped (configurable via
`options(reindeer.loss_warn = N)`). User-explicit ops (`dplyr` verbs,
bracket subsetting) record the step but stay silent. The provenance log
is capped at the last 1000 entries (`options(reindeer.provenance_max)`).

## METADATA.json auto-stub on bundle/session creation

`create_session_and_bundle()` now writes an empty `METADATA.json` skeleton
at session and bundle level so downstream `gather_metadata()` no longer
silently skips fresh bundles. Existing files are never overwritten.

<hr/>

# reindeer 0.3.2 (2026-02-05)

## Minor Update

Version bump for consistency with ongoing development.

<hr/>

# reindeer 0.3.1 (Documentation Update - 2026-02-05)

## Documentation Improvements

Updated cache management documentation to clarify that:
- reindeer manages **quantify/enrich** and **simulation** caches
- protoscribe manages **draft annotation** caches
- Added cross-references between packages

**No functional changes** - documentation only.

<hr/>

# reindeer 0.3.0 (BREAKING CHANGES - 2026-02-05)

## Major Changes: Draft Annotation Migration

All draft annotation functionality has been **removed** from reindeer and is now exclusively in the **protoscribe** package. This creates a clean separation of concerns:

- **reindeer**: Corpus management, queries, signal processing, metadata
- **protoscribe**: Draft annotation generation (MOMEL/INTSINT, periods, VOT, VAD, etc.)

### Removed Functions (NOW IN PROTOSCRIBE)

The following functions have been **removed**. Use protoscribe instead:

```r
# OLD (reindeer, REMOVED)
corp <- corpus("path/to/db")
suggestions <- draft_momel_intsint(corp, bundles)
assess(suggestions)
transcribe(suggestions)

# NEW (protoscribe)
library(protoscribe)
library(reindeer)

corp <- corpus("path/to/db")
audio_files <- # ... get audio file paths
suggestions <- protoscribe::draft_momel_intsint(audio_files, sessions, bundles)
protoscribe::assess(suggestions)
protoscribe::transcribe(suggestions)
```

**Removed R files:**
- `R/draft_cache_system.R` → Use `protoscribe::get_draft_cache()`
- `R/reindeer_annotate_momel.R` → Use `protoscribe::draft_momel_intsint()`
- `R/reindeer_annotate_python.R` → Use `protoscribe::draft_periods()`, etc.
- `R/reindeer_transcription_system_optimized.R` → Use `protoscribe::assess/prepare/transcribe()`

**Removed Python dependencies:**
- `inst/python/` directory completely removed
- `reticulate` removed from dependencies
- No Python environment setup required for reindeer

**Removed tests:**
- All annotation/transcription tests moved to protoscribe
- 6 test files (~60KB) migrated

### Migration Guide

Install protoscribe for draft annotation features:

```r
# Install protoscribe
remotes::install_github("humlab-speech/protoscribe")

# reindeer now suggests (but doesn't require) protoscribe
library(reindeer)    # Corpus management
library(protoscribe) # Draft annotations
```

**Key Benefits:**
- ✅ Cleaner package boundaries
- ✅ No Python dependencies in reindeer
- ✅ Faster installation of reindeer
- ✅ protoscribe can be developed independently
- ✅ Better code maintainability

### Cache Management Changes

Draft annotation cache management functions have been **removed** from reindeer:

**Removed functions:**
- `check_draft_cache_size()` → Use `protoscribe::check_draft_cache_size()`
- `clean_draft_cache()` → Use `protoscribe::clean_draft_cache()`

**Modified functions (draft cache support removed):**
- `check_all_cache_sizes()` - Now only checks quantify and simulation caches
- `clean_all_caches()` - Now only cleans quantify and simulation caches  
- `list_cache_files()` - No longer supports `cache_type = "draft"`
- `manage_cache()` - Removed "draft" and "simulation" from cache_type options

**Migration:**
```r
# For draft annotation cache management
library(protoscribe)
protoscribe::check_draft_cache_size(corp)
protoscribe::clean_draft_cache(corp, days_old = 30)
```

### Breaking Changes

**Action Required:** 
1. Update your code to use `protoscribe::` prefix for all draft annotation functions
2. Use `protoscribe::check_draft_cache_size()` instead of `reindeer::check_draft_cache_size()`
3. Use `protoscribe::clean_draft_cache()` instead of `reindeer::clean_draft_cache()`

**No action needed if:** You only use reindeer for corpus management, queries (`ask_for()`), signal processing (`quantify()`), or metadata operations.

<hr/>

# reindeer 0.2.5 (Development)

## Major New Features 🎉

### Corpus Creation from Scratch

* `corpus()` now supports creating new EMU databases with `create = TRUE` parameter
* Automatically appends `_emuDB` suffix if not present
* Provides helpful error messages when database doesn't exist
* Uses `emuR::create_emuDB()` internally for full compatibility

### Dynamic Session and Bundle Creation

* New function `create_session_and_bundle()` for programmatic corpus building
* Automatically creates proper directory structure (`<session>_ses/<bundle>_bndl/`)
* Generates minimal `_annot.json` files
* Updates SQLite cache automatically
* Auto-creation when importing media to non-existent sessions/bundles

### Name Validation

* New `validate_name()` function ensures valid session/bundle names
* Blocks regex special characters for literal names
* Prevents path separators and problematic characters
* Provides clear validation error messages

## Bug Fixes 🐛

### Bracket Assignment Operator

* **Fixed critical issue**: `corpus[i, j] <- value` now works correctly
* Resolved S7/S3 method dispatch conflict by adjusting class order
* Added "corpus" as first class in hierarchy for proper S3 dispatch
* Class order now: `corpus < reindeer::corpus < S7_object`
* Bracket notation now works for both metadata assignment and media import

### Media Import

* `corpus_import_media()` now validates names properly
* Auto-creates missing sessions/bundles before import
* Better error handling and progress messages

## Improvements

### Error Messages

* Significantly improved error messages throughout
* Helpful suggestions when database path doesn't exist
* Clear guidance for name validation failures
* Better context in all error conditions

### Documentation

* Added comprehensive roxygen documentation for new functions
* Created `CORPUS_CREATION_IMPLEMENTATION.md` technical guide
* Updated function examples with corpus creation workflows
* Improved parameter descriptions

## Internal Changes

* New file `R/corpus_creation.R` with creation utilities
* Modified `R/corpus_class.R` constructor to support `create` parameter
* Fixed `R/corpus_methods.R` subsetting operator registration
* Updated `R/zzz.R` to register S7 `[` method in `.onLoad()`
* Enhanced `R/corpus_metadata_io.R` with validation and auto-creation

## Breaking Changes

None - fully backward compatible. Existing code continues to work unchanged.

## Usage Example

```r
library(reindeer)

# Create new corpus from scratch
VISP <- corpus("VISP", create = TRUE)

# Add metadata
add_metadata(VISP, list(
  Project = "VISP",
  Language = "Swedish"
))

# Create session and bundle
create_session_and_bundle(VISP, "Svenska", "Annie")

# Import media (auto-creates if needed)
# VISP["Svenska", "Erik"] <- "path/to/audio.wav"
```

<hr/>

## Code Quality Improvements 🧹

### Repository Cleanup
* **Major cleanup**: Removed 5,942 lines of deprecated code (-28.5% of codebase)
  - Deleted 7 deprecated files marked with `_DELETE_` prefix
  - All deprecated code was internal, non-exported (no breaking changes)
  - Reduces maintenance burden and improves code clarity

### Naming Standardization
* **Consistent snake_case naming** across all files (100% compliance)
  - Renamed 10 files: `reindeeR_*` → `reindeer_*`
  - Fixed typo: `emuR_develoment_utils.R` → `emur_development_utils.R`
  - Expanded abbreviation: `signalextensions` → `signal_extensions`
  - Git history preserved (all tracked as renames, not deletions)

### Impact Metrics
* R files: 72 → 65 (-9.7%)
* Lines of code: 20,819 → 14,877 (-28.5%)
* File naming consistency: ~60% → 100%
* All changes backward compatible

### Large File Refactoring (Phase 3)
* **File size reduction**: Split 4 large files (>1,000 lines) into 11 focused modules
  - `reindeer-corpus.R` (1,910 lines) → 4 modules (corpus_class, corpus_methods, corpus_metadata_io, corpus_database)
  - `reindeer_simulation.R` (1,778 lines) → 3 modules (simulation_infrastructure, simulation_core, simulation_cache)
  - `reindeer_query_optimized.R` (1,296 lines) → 2 modules (query_parser, query_executor)
  - `reindeer_metadata_optimized.R` (1,247 lines) → 2 modules (metadata_core, metadata_import_export)
* **Total refactored**: 6,231 lines reorganized into maintainable modules
* **Largest file**: Now 1,062 lines (was 1,910 lines)
* **Benefits**: Improved code organization, easier navigation, clearer separation of concerns
* **No breaking changes**: All functionality preserved, fully backward compatible

### Documentation
* `CLEANUP_PHASE1_SUMMARY.md` - Detailed cleanup impact
* `CLEANUP_PHASE2_PLAN.md` - Future naming improvements
* `CODE_QUALITY_IMPROVEMENT_SUMMARY.md` - Complete metrics
* `PHASE3_REFACTORING_PLAN.md` - Large file refactoring plan
* `PHASE3_PROGRESS.md` - Refactoring progress tracking

<hr/>
# reindeer 0.2.4

## Major Performance Improvements ⚡

* **RcppSimdJson Integration** - 2-10x faster JSON parsing
  - All JSON reading operations now use RcppSimdJson for massive speedups
  - Hybrid strategy: RcppSimdJson for reading, jsonlite for writing
  - Automatic fallback to jsonlite ensures 100% compatibility
  - Database loading: 2.5x faster
  - Metadata gathering: 3-4x faster
  - Draft cache operations: 3.3x faster
  - Large files: up to 10x faster

* **Complete qs Migration** - 3-4x faster cache serialization
  - All cache operations now use qs format by default
  - Smaller cache files (~50% reduction)
  - Better handling of complex R objects
  - Simulation caches optimized
  - Draft annotation caches optimized

## Bug Fixes 🐛

* **Fixed Critical Test Failures** - All 47 tests now passing
  - Fixed infinite recursion in `build_emuDB_cache()` for empty databases
  - Fixed SQL parameter binding errors in draft cache system
  - Fixed qs deserialization issues (blob indexing)
  - Fixed type mismatch in error_occurred (INTEGER vs LOGICAL)
  - Fixed NOT NULL constraint in draft_annotations table

## New Features

* **Cache Management System**
  - `manage_cache()`: User-friendly cache management interface
  - Actions: status, list, clean (with dry-run support)
  - Works across all cache types (quantify, draft, simulation)
  - Automatic size monitoring with warnings
  - `check_all_cache_sizes()`: Monitor cache sizes
  - `clean_all_caches()`: Clean old cache files

* **JSON Utilities** (internal)
  - `read_json_fast()`: Optimized JSON file reading
  - `parse_json_fast()`: Optimized JSON string parsing
  - `write_json_compat()`: Compatible JSON writing
  - `to_json_compat()`: Compatible JSON serialization
  - `get_json_strategy()`: Inspect parser strategy

## Input Validation

* **Comprehensive assertthat Integration**
  - 30+ validation checks across core functions
  - `corpus()` constructor: 4 validation checks
  - `quantify()`: 5 validation blocks
  - `store_draft_annotations()`: 7 validations
  - `initialize_draft_cache()`: 2 validations
  - `manage_cache()`: Full input validation
  - Clear, informative error messages

## Documentation 📚

* **New Vignettes**
  - `getting_started.Rmd`: Comprehensive beginner's guide (634 lines)
  - `cache_management.Rmd`: Cache optimization guide (287 lines)

* **Enhanced pkgdown Site**
  - New "Cache Management" reference section
  - Reorganized article navigation
  - Better function discoverability
  - Ready for deployment

## Testing 🧪

* **Expanded Test Coverage** - 15% increase
  - New test file: `test_manage_cache.R` (12 scenarios, 30+ assertions)
  - Edge case coverage (empty/single segment lists)
  - Input validation tests
  - All assertthat checks verified
  - Total: 230+ tests, 0 failures

## Benchmarking

* **New Benchmark Suite**
  - `benchmarking/benchmark_json.R`: JSON parsing performance
  - Comprehensive file size testing
  - Real-world scenario simulation
  - Visualization of speedup
  - Documents 2-10x improvements

## Code Quality

* **Major Cleanup**
  - Removed 1,843 lines of deprecated code
  - Net reduction: 521 lines (after adding features)
  - Improved documentation formatting
  - Consistent error handling patterns

## Performance Summary

| Operation | Before | After | Speedup |
|-----------|--------|-------|---------|
| Corpus loading | 5ms | 2ms | 2.5x |
| Metadata (100 files) | 150ms | 40ms | 3.75x |
| Cache serialization | Base R | qs | 3-4x |
| JSON parsing (large) | 50ms | 5ms | 10x |

**Cumulative Impact:**
- Loading corpus with 100 bundles: ~3.6x faster overall
- Processing 1000 annotations: ~700ms saved
- Large corpora (10k bundles): ~11 seconds saved

## Breaking Changes

None - 100% backward compatible

<hr/>

# reindeer 0.2.3

## New Features

* **Automatic Synchronization System**
  - `enable_auto_sync()`: Configure automatic sync for database
  - `sync_database()`: Manually trigger synchronization
  - Automatic EAF file updates when `_annot.json` files change
  - Automatic CMDI updates when database structure or metadata changes
  - Change detection using MD5 checksums and state tracking
  - Sync state persisted in `.sync_config.json` and `.sync_state.json`

* **Metadata Management Functions**
  - `write_bundle_metadata()`: Write bundle-level `.meta_json` with auto-sync
  - `write_session_metadata()`: Write session-level `.meta_json` with auto-sync
  - `batch_update_metadata()`: Efficiently update multiple bundles at once
  - Merge mode for incremental metadata updates

* **Database Modification Wrappers**
  - `add_session_with_sync()`: Add session and trigger CMDI update
  - `remove_session_with_sync()`: Remove session and update CMDI
  - `update_config_with_sync()`: Update configuration and sync CMDI
  - All wrappers respect auto-sync configuration

* **Change Detection**
  - `detect_annot_changes()`: Find modified annotation files
  - `detect_metadata_changes()`: Find modified metadata files
  - `detect_config_changes()`: Detect database configuration changes
  - Efficient checksumming to avoid unnecessary syncs

## Documentation

* Added comprehensive auto-sync guide (`inst/doc/AUTO_SYNC_SYSTEM.md`)
  - Quick start and configuration
  - Metadata management workflows
  - Batch operations
  - Monitoring and troubleshooting
  - Performance optimization
  - Best practices
  - Complete usage examples

## Architecture

* Modular auto-sync system:
  - `reindeeR_autosync.R`: Core sync engine and change detection
  - `reindeeR_autosync_wrappers.R`: Database modification wrappers
  - State management with JSON persistence
  - Configurable sync triggers (EAF/CMDI independent)
  - Preserves user-edited CMDI metadata on regeneration

## Workflow Integration

* **Corpus Curation**: Auto-update CMDI when adding participant metadata
* **Annotation Pipeline**: Auto-generate EAF files for ELAN as annotations change
* **Database Evolution**: CMDI stays current as corpus structure evolves
* **Batch Processing**: Efficient bulk operations with single sync

## Performance

* Incremental sync: Only changed files are processed
* MD5-based change detection: Fast checksumming
* Batch-friendly: Defer sync until after multiple operations
* Typical overhead: <100ms for most operations

# reindeer 0.2.2

## New Features

* Added `create_cmdi_metadata()` function for generating CLARIN-compliant CMDI XML files
  - Supports multiple CMDI profiles (media-corpus, speech-corpus, speech-corpus-dlu)
  - Automatically collects metadata from database structure
  - Reads participant information from .meta_json files at session/bundle level
  - Generates comprehensive metadata including participants, resources, annotations
  - Includes placeholders for planned metadata additions

* Added CMDI validation script (`inst/scripts/validate-cmdi.sh`)
  - Validates XML well-formedness
  - Checks CMDI namespace compliance
  - Verifies required elements
  - Detects placeholder fields
  - Assesses metadata completeness
  - Validates resource references

## Documentation

* Added comprehensive CMDI generation guide (`inst/doc/CMDI_METADATA_GENERATION.md`)
  - Complete function usage examples
  - .meta_json file format specifications
  - CMDI profile descriptions
  - Placeholder field specifications (PROJECT.json, PUBLICATIONS.json, ETHICS.json, QUALITY.json)
  - Integration guide for CLARIN repositories
  - Best practices and troubleshooting

* Added metadata templates (`inst/templates/`)
  - session_meta_template.json
  - bundle_meta_template.json
  - PROJECT.json

## Metadata Infrastructure

* Session-level metadata support via `.meta_json` files
  - Participant demographics (age, gender, language, dialect)
  - Recording details (date, location, equipment, sample rate)
  - Session information (task, duration, notes)

* Bundle-level metadata support
  - Stimulus information
  - Repetition tracking
  - Annotation quality metrics

* Planned additions (with placeholders):
  - Project funding information
  - Related publications
  - Ethical approval details
  - Quality control procedures

## CLARIN Integration

* Full CMDI 1.2 specification compliance
* Three supported profiles:
  - media-corpus (clarin.eu:cr1:p_1387365569699)
  - SpeechCorpusWithParticipants (clarin.eu:cr1:p_1392642184799)
  - SpeechCorpus-DLU (clarin.eu:cr1:p_1381926654456)

* Generated CMDI files ready for upload to:
  - The Language Archive (TLA)
  - LINDAT/CLARIAH-CZ
  - Other CLARIN repositories

# reindeer 0.2.1

## Documentation

* Added comprehensive EAF validation documentation for `convert_emu_to_eaf()` function
  - **EMUR_TO_EAF_VALIDATION_GUIDE.md**: Complete EAF 3.0 specification and validation rules
  - **EMUR_TO_EAF_CHECKLIST.md**: Quick reference validation checklist
  - **CONVERT_FUNCTION_FIXES_REQUIRED.md**: Implementation requirements and testing guide

* These documents ensure the `convert_emu_to_eaf()` function produces valid EAF 3.0 files for all emuR annotation files, supporting both `align_items=TRUE` and `align_items=FALSE` modes

## Validation

* Documentation includes:
  - Complete EAF 3.0 specification requirements
  - Mode-specific conversion rules (ALIGNABLE_ANNOTATION vs REF_ANNOTATION)
  - Test cases with expected outputs
  - Common errors and troubleshooting guide
  - Integration with EAF validator tools
  - Priority-ordered implementation requirements

## Key Features Documented

* **align_items=TRUE**: ITEMs receive start/end times from dominated SEGMENTS/POINTs → `ALIGNABLE_ANNOTATION`
* **align_items=FALSE**: ITEMs become symbolic references → `REF_ANNOTATION`
* Proper TIME_ORDER generation with unique TIME_SLOT_IDs
* LINGUISTIC_TYPE definitions with correct TIME_ALIGNABLE settings
* TIER hierarchy with proper PARENT_REF and CONSTRAINTS attributes
* Time slot sharing for Time_Subdivision constraints
* PREVIOUS_ANNOTATION chains for Symbolic_Subdivision

# reindeer 0.2.0

Previous release (see prior commit history)
