# reindeer 2.5.0

This release folds in the 2.3.0 (silent-wrong-result fixes), 2.4.0 (80 of 80 emuR
parity; SEGMENT times shift by half a sample and quantify caches miss once) and
2.5.0 (power and latency; see `benchmarking/RESULTS_2.5.0.md`) work packages.

## Interface

- Website: reference index grouped by workflow (deprecated names in their own
  section), generated article menu, social-card image. Reference pages now have
  runnable examples on `demo_corpus()` and name the columns they return.

- Vignettes rewritten as eight workflow guides: getting_started, querying,
  speaker_aware, metadata, building_a_corpus, archiving, interoperability,
  annotating. Every chunk runs; a test knits them and fails on empty output.
- `dsp_parameters(age = , gender = )` is vectorised over both arguments.

- `extended_segment_list` is no longer a class. A `segment_list` that carries
  `quantify()` output has `@dsp_function` / `@dsp_columns` set;
  `is_extended_segment_list()` tests for that, and `extended_segment_list()` is
  a compatibility constructor returning a `segment_list`. One print, summary,
  glimpse and autoplot path for both; dplyr/vctrs hooks keep the dsp props.
  `is_segment_list()` is TRUE for lazy plans, and `pivot_tracks_longer()` and
  `autoplot()` collect a lazy plan first. `.cache_status` is no longer listed
  as a measurement column.

## Deprecations (removed in 3.0.0)

These exports are replaced by the consolidated interface. They still work, share
their implementation with the replacement (same result), and warn once per
session with class `reindeer_deprecated` when called from outside the package:
`add_metadata`, `import_metadata`, `load_metadata`, `gather_metadata` ->
`set_metadata` / automatic sync; `ascend_to`, `descend_to` -> `move_to`;
`import_bundle`, `import_session` -> `import_media`; `read_eaf`, `read_textgrid`
-> `read_annotations`; `write_eaf`, `write_textgrid` -> `write_annotations`;
`review` -> `annotate`; `quantify_egg` -> `quantify`; `validate_corpus`,
`validate_cmdi`, `create_cmdi_metadata` -> `describe_corpus`; `manage_cache`,
`inspect_cache`, `check_cache_size`, `peek_signals` -> `clear_cache` /
`summary(corpus)`; `dropped_rows` -> `provenance`; `metadata_cols`,
`signal_cols` -> `segment_cols` / `track_cols`; `nest_by_*` -> `tidyr::nest`;
`geom_*` -> `autoplot`; `extended_segment_list` -> `segment_list`;
`install_artic`, `artic_info` (no replacement); `serve`, `serve_app`,
`semitones`, `erb` as announced earlier.

## Argument conventions

- snake_case arguments: `query(corpus, eql, session_pattern =, bundle_pattern =)`,
  `annotate()`/`review()` (`session_pattern`, `bundle_pattern`, `auto_open_url`,
  `use_viewer`, `app_dir`, `debug_level`), `quantify(corpus, ..., column_name =,
  file_extension =, session_pattern =, bundle_pattern =)`, `set_metadata(...,
  reset =)`. The old spellings (`emuDB`, `sessionPattern`, `reset.before.add`,
  `metadataList`, ...) still work and warn with class `reindeer_deprecated`.
  Unknown options to `query()`, `annotate()` and `review()` are an error; legacy
  names to `quantify(corpus)` are no longer passed to the DSP routine as
  parameters.

## Automatic bookkeeping

- FAIR artifacts (README, CITATION.cff, datacite, JSON-LD, CMDI) are now
  regenerated automatically after metadata writes (`options(reindeer.auto_cmdi =
  FALSE)` opts out), hash-gated and never on open. reindeer keeps a manifest
  (`.cmdi_owned`) of the files it wrote: those are updated in place, while a
  hand-written file of the same name is never touched (the generated text goes
  to `*-generated.*`). The "no author" notice only appears when verbose.
- `summary(corpus)` shows schema validation, FAIR artifact and quantify cache
  status. Metadata writes already validate against the JSON schema.

## New verbs

- `move_to(segs, level)`: direction inferred from the hierarchy (`ascend_to()` /
  `descend_to()`, as `emuR::requery_hier()`).
- `biographize(segs)` takes the corpus from the segments and keeps the input
  row order (it used to sort by session/bundle).
- `set_metadata(corpus, x)`: `x` is a list, a data.frame (one row per scope,
  all-or-nothing) or the path of a workbook.
- `export_metadata()` writes raw per-level values, so the workbook round-trips
  through `import_metadata()` without copying inherited values to every bundle.
- `clear_cache(corpus, older_than = NULL, dry_run = FALSE)`.
- `read_annotations()` / `write_annotations()`: ELAN or TextGrid by extension,
  one schema (`level`, `labels`, `start`, `end` in ms).
- `annotate(segment_list)` opens a review playlist (corpus taken from the
  segments).
- `import_media(corpus, files, session, metadata)`: vectorised, validated
  before anything is written (per-file TextGrid/EAF import is not supported
  yet). `scout(n < 0)`, `dsp_parameters()`, `segment_cols()`/`track_cols()` and
  `autoplot(type =)` already existed.

## Performance

- `library(reindeer)` loads 16 namespaces instead of 25 (0.48 s -> 0.28 s): the
  ggplot2 `autoplot` methods are registered lazily when ggplot2 is loaded, and
  Rdpack (Rd macros only) is no longer imported at load time.

- FAIR artifact regeneration is gated by a content hash only: writing metadata
  that did not change rewrites no file (504 bundles: 0.64 s -> 0.05 s; a real
  change 0.4 s). The `.cmdi_dirty` sentinel is gone. A generated `README.md`
  carries a marker and is overwritten; a hand-written one never is (the text
  goes to `README-generated.md`).

- `quantify()` has ONE executor. By default (`.mode = "bundle"`) each bundle's
  file is analysed once with that bundle's speaker parameters and every
  segment's frames are sliced out; `.mode = "window"` keeps one call per
  segment. Routines that don't return a time-indexed track still run per
  segment. Cold `quantify()` of 253 segments (forest): 3.97 s -> 0.52 s; 67
  vowels: 0.93 s -> 0.30 s. Median |dF1| between the two modes is within 5 Hz,
  and begin-of-file segments no longer fail (ksv). The three older executors
  (file-batch, parallel-IO, vectorised-with-loop) are gone; the cache is still
  read and written per segment.

- The quantify cache (now ON by default, D6: `.use_cache = TRUE`, stored under
  `<corpus>/.quantify_cache`) keeps ONE entry per bundle file - its flattened
  frame table - keyed by routine identity, speaker parameters and file stamp.
  Re-running with a different `.at` or a different set of segments is a hit.
  Warm re-run of 253 segments: 0.05 s (was a full recompute). Results gain a
  `.cache_status` column (`"hit"`/`"miss"`). A warning fires when the cache
  passes 500 MB. Results of routines that are not sliceable tracks are cached
  per segment as before. Pass `.use_cache = FALSE` to opt out.

- Opening a corpus syncs the SQLite cache incrementally: a bundle is re-read
  only if its `_annot.json` mtime/size changed AND its md5 differs; added and
  removed bundles are picked up; the cache is no longer deleted and rebuilt on
  every open. One vectorised `_annot.json` parser replaces the three previous
  ones. 700 bundles: first open 153 s -> ~3.7 s (files in the OS cache; the
  remaining time is JSON parsing + inserts), reopen 0.17 s, reopen after one
  external edit 0.1-0.7 s and it sees the edit. `corpus(quick=)` is ignored
  with a warning. `annotate()`/`review()` saves update the cache through the
  same sync instead of a hand-rolled bundle upsert.

- Artic server (`annotate()`/`review()`): the Artic dist is served by httpuv
  `staticPaths` without entering R; whole media files are streamed as file
  bodies instead of being read into memory (the media URL is now `/media?...`);
  the server stops 10 s after the last browser client disconnects, and on
  package unload. Navigation verbs reuse the live corpus' connection instead
  of opening a new one per call, and `get_corpus_cached()` no longer pins
  corpora (and connections) for the whole session.

## EQL / emuR parity

- One compiler: eager `query(lazy = FALSE)` now compiles the same SQL as the
  lazy path and runs it; the duplicate eager executors are deleted. Compound
  operands are embedded as sub-selects instead of being executed (and pasted
  back as literal id lists) while compiling.
- Dominance with a sequence operand follows emuR's span semantics: a span
  dominates what any of its items dominates; a dominated span must be
  dominated as a whole. `[[#Phonetic == n -> Phonetic == t] ^ Syllable == W]`
  and `[[Syllable == S -> Syllable == W] ^ Phonetic == @]` now match emuR.

- SEGMENT times now follow emuR: start is half a sample before the first sample
  and end half a sample after the last (`(sample_start - 0.5)/sr`,
  `(sample_start + sample_dur + 0.5)/sr`). All times of segment queries shift by
  up to half a sample (25 us at 20 kHz); quantify caches miss once.
- A single `=` means `==` (it used to return every item on the level);
  quoted alternatives `'m'|'n'` work.
- `query(sessionPattern=, bundlePattern=)` now filter; `calcTimes = FALSE`,
  `timeRefSegmentLevel` and `resultType` abort with `reindeer_query_error`, as
  do unknown arguments, instead of being ignored.
- The parity suite against emuR now passes 73 of 80 golden queries (was 20);
  the rest are multi-hop/navigation cases for the next work packages.

- Hierarchy navigation is multi-hop: `Start()`/`End()`/`Medial()`/`Num()`
  in EQL and `ascend_to()`/`descend_to()` now work across any number of
  levels (Syllable -> Phoneme -> Phonetic ...), using the DBconfig link
  definitions. `descend_to()` returns ONE row per input segment spanning its
  first to last child (labels joined by `->`), as emuR's `requery_hier()` does;
  it used to return every child. ITEM-level results get times from their
  time-bearing descendants. The lazy `scout`/`ascend_to`/`descend_to` steps are
  no longer SQL fragments: `collect()` replays them through the eager verbs, so
  lazy and eager results are identical.
- `scout()` is a vectorised join instead of a per-segment loop: 18k segments in
  0.55 s (was ~39 s for 25k).

- New EQL parser (tokenizer + recursive descent, `R/eql_parser.R`) replaces the
  string-splitting one. `&` and `|` chain (`[A & B & C]`, unbracketed `A & B`),
  sequences nest, `#` projection works on any operand, compact `A==a->B==b`
  parses. Non-EQL input fails with a caret: double-quoted labels (EQL quotes
  with `'`), and unbracketed chains `[A -> B -> C]` / `[A ^ B ^ C]` (bracket
  them, as emuR requires). Unquoted regex classes (`=~ [aeiou]`) remain
  accepted although emuR rejects them.

## Fixed (silent wrong results)

- `CITATION.cff` was invalid YAML (list items mis-indented), so GitHub/Zenodo
  could not read it. Fixed and tested by parsing the file.
- `set_metadata()` could not update array-valued fields such as `team`: the
  old value silently stayed in `METADATA.json`. Arrays now replace.

- `quantify()`: when the DSP routine errored on a segment in the batched
  executor, every later segment's result moved up one row (or the call
  crashed). Failed segments are now dropped alone.

- `quantify(.use_cache = TRUE)`: the cache key now includes the routine's
  identity (package, version, formals, body, captured variables), the signal
  file's mtime and size, and a schema version. Previously replacing a WAV file
  or re-binding a name to a different function returned stale cached results.
  Existing cache entries miss once.

- Lazy `query()` plans: deriving a plan (`quantify()`, `biographize()`,
  `scout(collect = FALSE)`, ...) no longer shares the materialization cache
  with its parent, so `nrow(x)` before `quantify(x)` could return the parent's
  result without the new columns. Steps now run in declaration order
  (previously all SQL steps ran before all deferred ones), and lazy
  `ascend_to()`/`descend_to()` SQL no longer has the link direction swapped.

- `collect()` and `glimpse()` are now the dplyr/pillar generics, re-exported,
  instead of reindeer's own generics that masked them. dplyr and `pillar`
  move to Imports (Imports: 17). Print headers no longer show an unrendered
  `{nrow(x)} row{?s}`.

- **Speaker norms now reach the DSP.** `quantify()` read Age/Gender only from
  bundle-level metadata, so values set at session or database level (the
  common case) were ignored; it now resolves bundle > session > database like
  `get_metadata()`. Norms are delivered under each routine's own argument
  names (`minimum_f0`, `maximum_f0`, `number_of_formants`, `maxHzFormant`, ...),
  and routines with a `gender` switch (`trk_formant_forest`, `trk_pitch_ksv`,
  `trk_pitch_mhs`) get `gender = "u"` so the derived norms take effect (they
  were silently ignored with the default `"m"`). When a routine accepts none
  of the derived parameters `quantify()` now warns once per call
  (`reindeer_metadata_warning`). `quantify()` output no longer carries the
  helper Age/Gender columns.

- Metadata safety: the first write to a level that only has a legacy
  `.meta_json` keeps its fields; `Date`/`POSIXct` values are stored as ISO-8601
  strings; a field holding integers and doubles is a double (was truncated by
  the first row's type) and mixed numbers/text become character; `corpus()`
  reopen picks up `METADATA.json` edits made outside R; a malformed metadata
  file aborts with `reindeer_metadata_error` naming it and leaves the cache
  untouched (it used to be read as empty); `add_metadata(reset.before.add =
  TRUE)` refreshes the cache; `corp["S", ]` selectors match whole names and
  `corp["S", ] <- list()` works; Excel import validates the whole workbook
  before writing; failures in auto-CMDI regeneration warn (`reindeer_cmdi_warning`)
  instead of being swallowed.

- `quantify()` output: rows come back in input order (cold or cached, any size),
  without internal columns (`seg_params`, `signal_file`, `cache_key`, ...). `.at`
  on a routine that returns a plain list or vector is now an error instead of being
  ignored; `.at` picks the nearest frame, there is no interpolation.
  `quantify(corpus, .using=)` regenerates output files deleted since the cached
  run and its cache key now includes the routine's identity and file stamp.

- Artic server (`annotate()`/`review()`): media and bundle requests refuse
  session/bundle/extension values that could leave the bundle directory; the
  query string is parsed locally (no shiny dependency at run time); saving a
  bundle writes to a temp file and renames, and updates the cache in one
  transaction (a failed write no longer touches the cache); annotations saved
  from the editor get correct `seq_idx`/`label_idx` in the cache (they were
  all NA/0). `bundleListName` was non-functional and is removed.

- Provenance: row growth (joins that multiply rows) is recorded in a new
  `rows_added` column instead of a negative `rows_lost`; `right_join()`/
  `full_join()` that create NA-keyed rows, and `rename()` of a required column,
  now return a plain tibble instead of a broken `segment_list`; `group_by()`
  keeps the log; navigation fused into the lazy SQL (`scout(collect = FALSE)`
  etc.) records its steps and triggers the loss warning.
  Known gap: `bind_rows(s, s)` on two segment lists still errors (WP5.1).

- Connections: session/bundle creation and schema validation no longer close
  the corpus' shared SQLite handle; the finalizer is registered before any
  work that can fail when opening a corpus; creation SQL uses bound
  parameters (a session name with a quote no longer breaks it).

- **`erb()` returned wrong values in every release up to 2.2.0** (kHz constants
  applied to Hz; the result was almost constant). `erb()` and `semitones()`
  now call `superassp::ucnv_hz_to_erb()` / `ucnv_hz_to_semitone()`, warn that
  they have moved, and will be removed in 3.0.0.

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
