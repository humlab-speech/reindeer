# reindeer (development)

## Fixed (silent wrong results)

- `quantify()`: when the DSP routine errored on a segment in the batched
  executor, every later segment's result moved up one row (or the call
  crashed). Failed segments are now dropped alone.

## Removed

- Auto-sync (`R/reindeer_autosync*.R`, ELAN autosync) was unreachable and is
  deleted. `corpus(sync_eaf=, sync_cmdi=)` now warn (class
  `reindeer_deprecated`) and are ignored; the arguments go in a later release.
- The C++ layer (`src/`, Rcpp, `useDynLib`) and ~440 lines of dead DBconfig
  helpers are deleted; the package no longer compiles anything.
- Dead code deleted: `emuR_perspectives.R`, unused JSON/metadata helpers,
  internal `retreat()` and its lazy transform, `emu_ae()`/`ae()` aliases, the
  lookup of a non-existent bundled CMDI XSD. Stubs past their window removed:
  `enrich()`, `enrich_egg()`, `st()` (use `semitones()`).
- reindeer no longer creates or changes `future::plan()` and no longer uses
  furrr. Parallel sections use `future.apply`, so your plan decides (default:
  sequential). `quantify(.parallel=, .workers=, .optimize=)` warn and are
  ignored. Worker payloads no longer carry SQLite connections (fixes the
  `bad_weak_ptr` crash in `quantify(corpus, .using=)` under multisession).
- Repository hygiene: removed `tests/signalfiles` (~57 MB), `inst/praat`,
  `inst/examples`, `inst/templates`, `aeflac.tar.xz` and
  `demo_corpus(format=)`; `default_parameters.xlsx` moved to `data-raw/`.
  Source tarball 9.6 MB -> 6.4 MB. Pre-1.0 NEWS moved to `dev/NEWS-archive.md`.
- Imports reduced from 25 to 15: assertthat (own `.assert()` checks, classed
  `reindeer_validation_error`), base64enc and RcppSimdJson (jsonlite),
  furrr/future/Rcpp (WP1.2/1.4). wrassp (own WAV-header duration reader, which
  also stops CMDI duration reads failing silently), imputeTS, httpuv and
  jsonvalidate are now Suggests, checked with `rlang::check_installed()`.
  The null-or-empty coalescing operator is renamed `%or_empty%` so it no longer
  shadows `rlang::%||%`.
- Policy: a removed name is a classed stub for one minor release, then deleted.

# reindeer 2.2.0

## Breaking changes

- **`serve()` and `serve_app()` are replaced by `annotate()` and `review()`.**
  The old names remain exported but abort with a `reindeer_moved_error` that
  points at the replacements: `serve(corp)` -> `annotate(corp)`, and
  `serve(corp, seglist = ...)` -> `review(corp, seglist)`. `annotate()`
  dispatches on a `corpus` and forwards every other call verbatim to
  `ggplot2::annotate()`, so both verbs coexist.
- **The web app served is now Artic** (a fork of EMU-webApp), not an
  EMU-webApp checkout. reindeer resolves a dist via
  `options(reindeer.artic.dir)`, `ARTIC_DIR`, the copy bundled in
  `inst/artic/dist`, the `install_artic()` cache, or a sibling
  `../artic/dist`; `reindeer.emuWebApp.dir`/`EMU_WEBAPP_DIR` are no longer
  read. `install_artic()` fetches or copies an alternative build. A review
  playlist requires a build advertising `bundleList` and `timeAnchors`.
- `annotate()` and `review()` apply editing restrictions and button
  configuration **in memory only**; the corpus `_DBconfig.json` is no longer
  written on serve. Artic's default config hides the save button, so the
  overlay enables `saveBundle` (and passes a `clear` button) to prevent edits
  being discarded on bundle change.

## New features

- **`review(corpus, seglist, ...)`**: turns a `segment_list`,
  `extended_segment_list`, `lazy_segment_list`, or data.frame into an Artic
  playlist. One time anchor per segment, played in row order
  (`dplyr::arrange()` controls order); sessions/bundles grouped for the
  sidebar; optional `label` per anchor; unknown bundles and foreign
  `db_uuid`s are rejected. `tracks` overlays registered SSFF tracks
  (defaults to an `extended_segment_list`'s `dsp_columns`) when the files
  exist for every playlist bundle.
- **`install_artic()` / `artic_info()`**: provision and inspect the Artic
  build; downloads are checksum-verified and cached under
  `tools::R_user_dir("reindeer", "cache")`.
- `bundleListName` now actually serves the named bundle list (previously only
  its `comment`/`finishedEditing` write-back was wired up).
- Bundles with registered SSFF tracks that have no file on disk now load with
  a warning instead of failing the whole bundle.
- Static serving covers Artic's asset types (`.mjs`, `.wasm`, fonts, media);
  a busy port reports a classed error suggesting `httpuv::randomPort()`.

# reindeer 2.0.1

## Bug fixes

- **EQL parser no longer splits on operator characters inside values.**
  `[Phonetic == p|t|k -> Phonetic == H]` was cut at the first `|` and read
  as a disjunction; regexes containing `|`, a `^` anchor in the left operand
  of a dominance query (`[Phoneme =~ ^a ^ Syllable == S]`), and labels such
  as `R&B` failed the same way. A top-level `&`, `|`, `->` or `^` now counts
  as an operator only outside quotes and when a new operand follows it.
  Query-level disjunction (`[Phonetic == t | Phonetic == k]`) is unchanged.
- **Label groups are resolved.** `Phonetic == stop` now expands the `stop`
  label group from the DBconfig (attribute level first, then database
  level), matching `emuR::query()`. Previously it silently returned 0 rows.
- A dangling operator (`[Phonetic == t ^]`) is now a parse error instead of
  being read as part of the label; the error caret points at the operator.
- **Quoted values are literal.** `Phonetic == 'p|t'` matches the label
  `p|t` instead of being split into alternatives, matching `emuR::query()`.
  Same for `Session`/`Bundle` scope filters. Unquoted `p|t` still means
  alternatives.

# reindeer 2.0.0

## Breaking changes

- **`propose_annotations()` is removed.** Its dispatch table
  (`draft_vad`/`draft_vot`/`draft_periods`/`draft_momel_intsint`/
  `draft_slam`/`draft_slam_plus`/`draft_slamp3`) was already stale
  against protoscribe's current `draft_*()` surface (which has since
  grown `draft_creak`, `draft_mdq`, `draft_align_words`,
  `draft_whisper(x)`, `draft_wh_import`,
  `draft_vad_brouhaha`/`ltsd`/`tandem`, `draft_sevq_gci`,
  `draft_voice_quality`, `draft_voxit_pauses`, none of which were wired
  in). protoscribe now ships a complete standalone workflow
  (`draft_*() -> assess() -> prepare() -> transcribe() -> reverse()`)
  that doesn't need a reindeer-side dispatcher; see
  `vignette("protoscribe-with-reindeer", package = "protoscribe")`.
- **The `draft_vad`, `draft_vot`, `draft_periods`, `draft_momel_intsint`,
  `quantify_simulate`, `enrich_simulate`, `reminisce`,
  `reminisce_tracks`, and `list_simulations` redirect stubs are
  removed.** These were flagged in this file as retained only until "a
  future major release" — this is that release. Call them from
  `protoscribe::` / `erodex::` directly.
- **`protoscribe` and `erodex` are dropped from `Suggests` and
  `Remotes`.** No reindeer code references either package anymore;
  both are fully independent, optional add-ons with their own vignette
  sets. `eggstract` and `superassp` remain in `Suggests` as DSP
  backends that `quantify()`/`quantify_egg()` still call into.
- `end_to_end_pipeline.Rmd` and `cache_management.Rmd` no longer
  demonstrate protoscribe/erodex usage; the former's protoscribe
  section is replaced with an EQL sequence/dominance query example.

## Other changes since 1.0.1

- **`enrich()` is removed** (hard stub, no deprecation window — same
  precedent as `enrich_egg()`'s removal). `quantify()` now covers
  everything: `quantify(corp, .using = fn)` runs a DSP routine across
  every signal file in a corpus (what `enrich(corp, .using = fn)` did),
  writes the resulting SSFF files, and — new — registers each output
  track in `ssffTrackDefinitions` with a `generator` block recording the
  producing function, package, version, and any explicit non-default
  arguments. `quantify(corp, name = ..., fileExtension = ...)` (or
  `from`/`index` for one column of an existing multi-column track)
  connects a track definition to files that already exist on disk
  without computing anything. `quantify(segs, "TrackName")` (or a
  character vector of names) reads a registered track back for a
  segment_list, recomputing on the fly if the track was registered
  without files. Segment-level metadata joins — `enrich(segs, corp)`'s
  default behaviour — are unchanged and now go through `biographize()`
  directly. `inst/schemas/dbconfig.schema.json` documents the new
  `generator`/`from`/`index` keys on `ssffTrackDefinitions` entries
  (previously accepted only via the schema's pre-existing
  `additionalProperties: true`, now explicit).

- Fixed a malformed `Authors@R` field (stray trailing paren) that made
  `DESCRIPTION` fail to parse under R CMD check.

- `protoscribe`, `eggstract`, and `erodex` are now declared in `Suggests`
  and `Remotes`, matching what the companion glue (`propose_annotations()`,
  `quantify_egg()`, `deprecated_stubs.R`) already assumed. Removed the
  unused `yardstick`/`bigstatsr` Suggests left over from before the
  simulation code moved to erodex.

- `enrich_egg()` is removed: it forwarded to `eggstract::enrich_with_egg`,
  which eggstract has never exported, so the wrapper always aborted. Calling
  it now raises the standard "moved" redirect pointing at `quantify_egg()`.
  `quantify_egg()`'s default `.using` is corrected from the non-existent
  `eggstract::ksvF0` to `eggstract::trk_f0`, and a missing export now aborts
  cleanly instead of raising a raw "object not found" error.

- `Suggests` floors superassp at 3.0.0, the first release whose `trk_*`
  wrappers expose the wrapped routine's parameters, and CI installs it from
  `github::humlab-speech/superassp` (its source tree now ships the SPTK headers
  that earlier source builds stopped on). The DSP tests and the vignette
  sections that need it therefore run in CI instead of skipping.

- The corpus constructor's metadata gather now honours the legacy
  `<name>.meta_json` fallback (via `.resolve_metadata_file()`) at the
  database, session, and bundle levels, instead of only reading
  `METADATA.json`. The metadata table-clearing DELETEs in both gather
  paths are now wrapped in a single transaction so a rebuild never leaves
  a partially cleared cache.

- Vignettes gate evaluation per chunk instead of via the removed global
  `REINDEER_EVAL_VIGNETTES` switch: the setup chunk of each vignette records
  which companions are installed (`requireNamespace()`), and every chunk that
  needs one is marked with the matching flag (`eval_dsp` for superassp,
  `has_egg`/`has_proto`/`eval_qs2` for eggstract, protoscribe, qs2). The
  vignettes therefore build and publish without the heavy DSP companions while
  showing real output wherever those companions are installed. Fixed stale
  `.meta_json` / `serve()` references in the interactive-annotation vignette.

- Corrected documentation that overstated lazy deferral (`query()` now
  states that plan-time level/attribute resolution and compound sub-query
  materialisation happen at call time; the lazy print/summary docs note
  they run a `COUNT(*)` + preview query) and refreshed stale references
  (`.meta_json` ground-truth comments, CLAUDE.md dev commands, removed
  `DEPRECATED_FUNCTIONS.md` / `render_vignette.R` / `verify_implementation.R`
  links).

- Completed the pkgdown reference index: added the ~25 exported topics
  that were missing (interop, tidy reshaping/column helpers, FAIR/CMDI,
  companion-package glue, gadgets, `derive_dsp_parameters`,
  `biographize`, `demo_corpus`), grouped into new sections, and registered
  the `end_to_end_pipeline` vignette in the navbar and articles. Removed
  the stale `cache_utils` topic.

- Removed superseded internal code: the dead `R/cache_utils.R` (duplicate
  SQLite cache helpers with a wrong `tempdir()` default), the unused
  in-memory quantify cache (`.get_quantify_cache`/`.set_quantify_cache`),
  `.make_quantify_cache_key()`, `.read_ssff_mmap()` (a TODO stub), and the
  unused batch/convert helpers. The exported `deprecated_stubs.R` redirects
  (`draft_*`, `quantify_simulate`, ...) are retained: they are public API
  and removal is a breaking change for a future major release.

- `serve()` media responses now honour HTTP byte-range requests (206
  partial content) instead of reading the whole audio file into memory,
  return 404 for missing media, and no longer kill unrelated httpuv
  servers: it stops only the previous reindeer server handle and retains
  the new one via `reindeer.serve_handle`. The EMU-webApp directory is
  resolved once per `serve()` call instead of per request.

- Unified the `reindeer.auto_cmdi` default to `FALSE` on both the metadata
  write path and the constructor (previously the write path defaulted to
  `TRUE`, regenerating all FAIR artifacts on every `add_metadata()` call).

- `inspect_cache()` / `manage_cache()` / `clean_*_cache()` now read the
  quantify/enrich SQLite cache (`quantify_cache.sqlite`) directly instead
  of scanning a per-item `.rds`/`.qs` layout that no longer exists, so
  they report the real cache and can prune entries by age. Legacy
  per-item files are still surfaced and swept.

- Sequence/dominance queries now match pre-executed sub-query results via
  a deduplicated composite-key `IN` list instead of a per-row 4-column OR
  list, shrinking the generated SQL for compound queries over large
  corpora. (A connection-scoped temp table was considered but rejected:
  the deferred/lazy plan must stay self-contained so it can run at
  `collect()` time.)

- Parameterized all SQL in the eager navigation verbs (`scout()`,
  `ascend_to()`, `descend_to()`). User-supplied `level` and `db_uuid`
  values are now bound via `?` placeholders instead of being
  `sprintf`-interpolated into the query string, closing a SQL-injection
  vector.

- Quantify/enrich DSP calls now build the per-file invariant argument
  list (file path, resolved params, `toFile`/`verbose`) once per file
  group instead of re-splicing it for every segment. Full single-decode-
  and-slice is deliberately not done: the windowed superassp API ties
  analysis context to the requested window, so decoding a whole file once
  would change boundary-adjacent results.

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

Earlier history (before 1.0.0): `dev/NEWS-archive.md`.
