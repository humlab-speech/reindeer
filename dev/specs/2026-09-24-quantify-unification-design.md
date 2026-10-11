# Design: unify `quantify()`/`enrich()`, add track generator provenance, hard-deprecate `enrich()`

Date: 2026-09-24
Status: approved by user, pending spec review

## Background

Two problems converged into one design:

1. **Generator provenance** (original ask): the database config needs to record
   *how* an SSFF track was produced — which DSP function, from which package/
   version, and which non-default settings — so a companion web app (`artic`)
   can display it, without breaking `artic`'s strict JSON-schema validation.
2. **Interface consolidation** (raised during brainstorming): `enrich()` and
   `quantify()` currently overlap — `enrich()`'s segment methods are pass-
   throughs to `quantify()`/`biographize()`, and neither writes to nor reads
   back from `ssffTrackDefinitions`, so a corpus-wide DSP pass buys nothing
   more than "a cache row saying done." Investigation confirmed:
   - `.enrich_corpus_impl()` (`R/reindeer_enrich.R:201-366`) already writes
     SSFF files (`toFile = TRUE`) and already captures DSP identity
     (`dsp_fun_name`) and explicit user args (`user_params`) — it just never
     calls `load_DBconfig()`/`store_DBconfig()`.
   - `ssffTrackDefinitions` starts empty (`R/corpus_creation.R:111`) and
     nothing in reindeer ever appends to it.
   - `quantify()` always recomputes from the media file; it never checks
     whether a registered, already-computed track exists on disk.
   - superassp's `trk_*` S7 generics carry `attr(fn, "ext")`,
     `attr(fn, "tracks")`, `attr(fn, "outputType")`, `attr(fn,
     "suggestCaching")` — verified against the installed package — and
     `superassp::read_track()` is a windowed SSFF reader. None of this is
     used today; track name/extension/column names can be inferred instead
     of requiring the caller to type them.

Decision: fold both fixes into one verb, `quantify()`, dispatched by S7 class
(`corpus` vs. `segment_list`/`extended_segment_list`/`lazy_segment_list`).
`enrich()` becomes redundant and is hard-deprecated (immediate error-redirect
stub, not a silent forward, not a multi-release soft-warn — matching the
precedent already set by `enrich_egg()`'s removal earlier in this same
`NEWS.md` dev cycle).

## Goals

- One verb (`quantify`) for all DSP-track work: materialize a track across a
  corpus, connect a definition to files that already exist on disk, extract
  precomputed values for segments, or compute on the fly.
- Every track `quantify()` materializes is registered in `ssffTrackDefinitions`
  with a `generator` block recording function/package/version/explicit args.
- `artic` continues to load any `_DBconfig.json` reindeer produces.
- `enrich()` is removed as a working verb; calling it gives an actionable
  error pointing at its replacement, immediately (no deprecation window).

## Non-goals (explicitly deferred)

- Per-bundle / metadata-derived arg storage in `generator.args` — only
  explicit caller-supplied args are captured (see rationale below).
- `tracks()` / `remove_track()` registry-browsing verbs.
- Renaming `quantify.segment_list`'s `dsp_function` parameter to `.using`
  (naming harmonization) — out of scope, noted as future cleanup.
- Any `artic` UI to *display* generator info — schema acceptance only.
- `quantify_egg()` / `enrich_egg()` — unaffected. `quantify_egg()` already
  calls `quantify()` directly (`R/companion_eggstract.R`), not `enrich()`.
  `enrich_egg()` is already a hard-deprecated stub from a prior change.

## Data model: `generator` on `ssffTrackDefinitions[i]`

```json
{
  "name": "F1",
  "columnName": "fm",
  "fileExtension": "fms",
  "generator": {
    "function": "trk_formant_forest",
    "package": "superassp",
    "version": "3.1.2",
    "args": { "windowSize": 20, "nominalF1": 500 },
    "generatedAt": "2026-09-24T10:00:00Z"
  }
}
```

- `generator` is optional — manually-defined or legacy tracks lack it.
- `args` contains **only what the caller explicitly passed** to `quantify()`
  (the `user_params` already captured separately from metadata-derived
  `dsp_params` in `.enrich_corpus_impl()`) — not diffed against `formals()`,
  and not the per-bundle age/gender-resolved values, which vary per bundle
  and have no home in a single global `ssffTrackDefinitions` entry.
- `function`/`package` resolved from the captured call expression (already
  done via `deparse(substitute(.using))`-style capture); `version` via
  `utils::packageVersion()`.

### Schema changes

- **reindeer** (`inst/schemas/dbconfig.schema.json`): add an explicit,
  optional `generator` object under the `ssffTrack` definition, matching the
  shape above. The schema already sets `additionalProperties: true`, so this
  is a documentation/validation tightening, not a behavior change.
- **artic** (`src/schemaFiles/DBconfigFileSchema.json`): **required change**.
  `ssffTrackDefinitions.items` sets `additionalProperties: false`, so an
  unrecognized `generator` key fails `tv4` validation and blocks the whole
  database from loading (`App.svelte`/`TopMenu.svelte` call
  `resetToInitState()` on failure). Add `generator` explicitly to
  `properties` there, not required. Single-file change; `dist/`/`public/`
  copies rebuild from it. No new UI, no TS types (none exist project-wide for
  this config) — schema-acceptance only.

## `quantify.corpus` — new S7 method

```r
S7::method(quantify, corpus) <- function(object, .using = NULL, ...,
                                          name = NULL,
                                          columnName = NULL,
                                          fileExtension = NULL,
                                          from = NULL, index = NULL,
                                          write_files = NULL,
                                          overwrite = FALSE,
                                          sessionPattern = ".*",
                                          bundlePattern = ".*",
                                          .metadata_fields = c("Gender", "Age"),
                                          .signal_extension = NULL,
                                          .force = FALSE,
                                          .verbose = TRUE,
                                          .parallel = TRUE,
                                          .workers = NULL,
                                          .use_cache = TRUE,
                                          .cache_dir = NULL,
                                          .cache_format = c("auto", "qs", "rds"))
```

Two sub-modes, discriminated by whether `.using` is supplied — the same
discriminator emuR's own `add_ssffTrackDefinition()` already uses
(`onTheFlyFunctionName` present vs. `NULL`):

**`.using` given → compute-and-register.** Reuses
`.enrich_corpus_impl()`'s existing bundle loop (metadata-derived params,
caching, parallelism, `toFile = TRUE` write) unchanged. New: after writing,
infer `fileExtension` from `attr(.using, "ext")` and per-column `name`s from
`attr(.using, "tracks")` when not explicitly supplied; if `attr(.using,
"suggestCaching")` is `FALSE` and `write_files` wasn't explicitly set, treat
the track as read-computed-on-demand rather than forced materialization.
Build the `generator` block (function/package/version/`user_params`) and
upsert into `ssffTrackDefinitions` via `load_DBconfig()`/`store_DBconfig()`
(no new persistence mechanism). Error via `cli::cli_abort()` on a `name`
collision unless `overwrite = TRUE`.

**`.using` absent → connect-existing.** No DSP call, no file write. Requires
`name` plus either `fileExtension` (adopt a whole file — e.g. a `rms` track
someone generated outside reindeer) or `from` + `index` (name one column of
an already-registered multi-column track, e.g. `F1` = column 1 of the `F`
track — a slot emuR's plain `(name, columnName, fileExtension)` triple can't
express, so `from`/`index` are stored under the entry alongside `generator`,
still inside the `additionalProperties`-whitelisted set on the artic side).
Registers the definition only; `generator` is still recorded if the caller
also passes `.using = NULL` but describes the originating function via an
explicit `generator = list(...)` override (documentation-only, no
computation) — otherwise `generator` is omitted for adopted tracks whose
provenance is unknown.

Validation: supplying both `.using` and (`from`/`index`) is an error —
compute mode registers every column the DSP produces; naming a single
sub-column only makes sense when connecting to something that already
exists.

## `quantify.segment_list` / `.extended_segment_list` / `.lazy_segment_list` — widened

Existing signature, unchanged formals:

```r
S7::method(quantify, segment_list) <- function(object, dsp_function, ...,
                                                .at = NULL, .use_metadata = TRUE,
                                                .verbose = FALSE, .parallel = TRUE,
                                                .workers = NULL, .use_cache = FALSE,
                                                .cache_dir = NULL,
                                                .cache_format = c("auto", "qs", "rds"),
                                                .optimize = TRUE)
```

New: `dsp_function` may now be a **character vector** of registered track
names, in addition to a function. Dispatch on `is.character(dsp_function)`
vs. `is.function(dsp_function)` inside the method body — no NSE conflict,
since the existing name-capture (`.dsp_name_from_expr`) already evaluates the
argument under standard evaluation. Character path: look up each name in
`ssffTrackDefinitions`, read values via `superassp::read_track()` at `.at`
for bundles where the file exists; if the track was registered as an
on-the-fly recipe (`suggestCaching == FALSE` at registration), compute via
the stored `generator` recipe instead and persist if the track's mode calls
for materialization. Function path (existing behavior): compute on the fly,
byte-for-byte unchanged. A single call must be all-character or a lone
function — mixing is a validation error, not silent misbehavior.

`biographize()` remains the answer for segment-level metadata joins — no
change needed there; it's already exported and already does exactly what
`enrich(segs, corp)` forwarded to.

## Hard deprecation of `enrich()`

Delete `R/reindeer_enrich.R` (the S7 generic and its four methods; the
corpus-mode logic migrates into `quantify.corpus` above, the segment-mode
logic is now redundant with `quantify()`/`biographize()` directly). Add a
plain-function stub to `R/deprecated_stubs.R`, following the exact pattern
already used there for `enrich_egg()`:

```r
#' @rdname deprecated-moved-functions
#' @export
enrich <- function(...) {
  cli::cli_abort(c(
    "{.fn enrich} has been removed from {.pkg reindeer}.",
    "i" = "Corpus-level DSP (write + register a track): use {.fn quantify} \\
           instead, e.g. {.code quantify(corp, .using = fn)}.",
    "i" = "Segment-level metadata join: use {.fn biographize} instead.",
    "i" = "Segment-level DSP: use {.fn quantify} instead (same call shape \\
           you already use)."
  ), class = c("reindeer_moved_error", "reindeer_error"))
}
```

`enrich` stays in `NAMESPACE`/exported (as the stub) so `reindeer::enrich()`
resolves to this helpful error rather than "object not found" — same
convention as every other entry in `deprecated_stubs.R`. This ships in the
current dev cycle (`1.2.0`, unreleased), not gated behind a future major
version: this repo's own `NEWS.md` shows `enrich_egg()` was hard-removed in
the same dev section already open, so there's no established soft-deprecation
window to preserve here.

### Touch list

- Delete: `R/reindeer_enrich.R`.
- Add stub to: `R/deprecated_stubs.R`.
- Update call sites currently referencing `enrich(` (mix of real calls and
  `@seealso`/doc links — audit each during implementation):
  `R/corpus_class.R`, `R/query_executor.R`, `R/metadata_import_export.R`,
  `R/dsp_parameters_public.R`, `R/segment_list_quantify.R`,
  `R/segment_list_tidyselect.R`, `R/metadata_core.R`,
  `R/segment_list_classes.R`.
- Update test: `tests/testthat/test_lazy_chain.R`.
- Update vignettes: `getting_started.Rmd`, `end_to_end_pipeline.Rmd`,
  `cache_management.Rmd`, `metadata_management.Rmd`.
- `NEWS.md`: new entry under the open dev section, matching the style of the
  existing `enrich_egg()` removal entry.
- **Pre-flight check, external repos**: grep `../eggstract`, `../erodex`,
  `../protoscribe` for direct `enrich(` or `reindeer::enrich` calls before
  merging — `quantify_egg()`'s internals are confirmed clean (calls
  `quantify()` directly), but sibling repos haven't been checked and are
  outside this repo's test suite.

## Testing

- New `tests/testthat/test_quantify_corpus.R` (ae demo db): compute-and-
  register path, connect-existing path (`fileExtension` and `from`/`index`
  variants), overwrite guard, `generator` field round-trips through
  `load_DBconfig()`.
- Extend `tests/testthat/test_quantify_segment_list.R`: character-track-name
  read-back path, mixed-mode-argument validation error, fallback-to-compute
  when file missing but recipe registered.
- Update `tests/testthat/test_lazy_chain.R` to use `quantify()`/`biographize()`
  in place of `enrich()`.
- `artic`: extend a fixture under `src/testData/newFormat/` with a
  `generator`-bearing track; confirm `validationService.validateJSO` passes.

## Open risk noted, not blocking

`quantify.corpus`'s connect-existing mode and the `from`/`index` sub-column
addressing add two new keys reindeer's schema didn't have before
(`from`/`index` alongside `generator`) — the artic schema PR needs to
whitelist both, not just `generator`. Called out explicitly here so it isn't
dropped during implementation.
