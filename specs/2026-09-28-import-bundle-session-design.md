# Design: `import_bundle()` / `import_session()` — consistent import with auto track generation

Date: 2026-09-28
Status: approved by user (conversational design), proceeding directly to implementation per user instruction

## Background

Today, adding a session/bundle to a corpus and getting it into a "neat and
tidy" state is a manual, multi-step, easy-to-forget-a-step process:

1. `corpus["Session1", "Bundle1"] <- "file.wav"` (media) and/or
   `corpus["Session1", "Bundle1"] <- list(Age = 64, Gender = "F")` (metadata)
   — independent bracket verbs, order doesn't matter, both idempotently call
   `create_session_and_bundle()` if the bundle doesn't exist yet.
2. Separately, and easy to skip: re-run `quantify(corpus, .using = fn)` so the
   new bundle gets the same SSFF tracks every other bundle already has.

Nothing ties these together, and nothing hands Age/Gender to the DSP step
automatically. `quantify(corpus, .using = fn)` already records *how* a track
was produced in `ssffTrackDefinitions[i].generator` (function/package/
version/explicit args — see `specs/2026-09-24-quantify-unification-design.md`),
and `derive_dsp_parameters()`/`.quantify_corpus_compute()` already resolves
Age/Gender into literature-derived DSP norms **per bundle**. Both pieces
exist; nothing wires a new bundle through them automatically.

## Goals

- A single, consistent entry point for adding a bundle (and its session, if
  new) to a corpus: create the bundle, apply metadata, import media, and
  materialize every corpus-registered SSFF track for that bundle — one call,
  same result every time.
- Reuse the existing generator recipe (`ssffTrackDefinitions[i].generator`)
  so a newly-imported bundle gets exactly the tracks every other bundle has,
  computed with that bundle's own Age/Gender norms.
- A session-level convenience for the common case (one recording session =
  one speaker): supply Age/Gender once, import every bundle in the session
  without repeating it per bundle.

## Non-goals (explicitly deferred)

- Regenerating already-materialized tracks when metadata is edited after the
  fact (staleness detection). Out of scope — user re-runs `quantify(...,
  overwrite = TRUE)` manually if metadata changes post-import.
- A `tracks =` allow-list for selecting which registered tracks to replay.
  v1 always replays every track definition that has a resolvable `generator`
  recipe; connect-only definitions (no `generator`) are skipped. A boolean
  `tracks = FALSE` escape hatch exists to skip generation entirely for a
  given call.
- Multi-file / batch corpus-wide import (e.g. importing a whole directory
  tree of recordings in one call). `import_session()` covers one session;
  looping across sessions is left to the caller.

## Design

### `import_bundle(corpus_obj, session, bundle, media, metadata = list(), tracks = TRUE, verbose = TRUE)`

New file `R/import_bundle.R`. Sequence:

1. `create_session_and_bundle(corpus_obj, session, bundle, verbose)` —
   idempotent; creates the session directory if new, the bundle directory
   and METADATA.json/annot.json skeletons if new. If the bundle already
   exists, this is a no-op past validation (existing behavior).
2. If `length(metadata) > 0`: apply it via the same path as
   `corpus[session, bundle] <- metadata` (`corpus_assign_metadata()`), so
   Age/Gender are on disk *and* in the SQLite metadata cache before track
   generation reads them.
3. Import media via the same path as `corpus[session, bundle] <- media`
   (`corpus_import_media()`).
4. If `isTRUE(tracks)`: `.replay_registered_tracks_for_bundle()` (new
   internal helper, same file) — for every entry in
   `dbConfig$ssffTrackDefinitions` with a resolvable
   `generator$package`/`generator$function`, resolve the function and
   replay it scoped to exactly this bundle:

   ```r
   dsp_fun <- get(gen$`function`, envir = asNamespace(gen$package))
   do.call(quantify, c(
     list(object = corpus_obj, .using = dsp_fun,
          name = track_def$name, columnName = track_def$columnName,
          fileExtension = track_def$fileExtension, overwrite = TRUE,
          sessionPattern = paste0("^", session, "$"),
          bundlePattern  = paste0("^", bundle, "$"),
          .metadata_fields = c("Gender", "Age"),
          .verbose = verbose, .parallel = FALSE),
     gen$args %||% list()
   ))
   ```

   This is the *only* DSP-invocation code the feature needs — it drives the
   existing `quantify.corpus` compute path (`.quantify_corpus_compute()`),
   which already derives per-bundle Age/Gender norms via
   `derive_dsp_parameters()`. No new DSP-calling code is written.

   **On `overwrite = TRUE` — what it actually touches:** `overwrite` here
   only permits `.upsert_ssff_track_definition()` to replace the *JSON
   recipe entry* for `track_def$name` in `_DBconfig.json` (function/package/
   args/`generatedAt`) — it never touches any bundle's on-disk SSFF file.
   Because `sessionPattern`/`bundlePattern` are anchored `^session$`/
   `^bundle$` to exactly the bundle just imported, `.quantify_corpus_compute`'s
   `signal_files` filter excludes every other bundle before any DSP call
   happens — their already-materialized files are never re-touched,
   regardless of `overwrite`. The anchor is safe against regex
   metacharacters because `create_session_and_bundle()` (step 1) already
   validated `session`/`bundle` as literal names (`validate_name(...,
   allow_regex = FALSE)`, which rejects `.*+?^${}()|[]`) before this point.

   Each track's replay is wrapped in `tryCatch()`; one DSP failure (e.g. an
   unresolvable `generator$package`) is collected and reported via
   `cli::cli_warn()` after the loop, not aborted — matches the existing
   partial-failure tolerance in `.quantify_corpus_compute()`.

5. Missing/incomplete Age/Gender at generation time: no special-cased
   behavior needed — `derive_dsp_parameters()` already falls back to the DSP
   function's own defaults and warns once per session (existing behavior,
   reused as-is).

Returns `corpus_obj`, invisibly.

### `import_session(corpus_obj, session, bundles, metadata = list(), tracks = TRUE, verbose = TRUE)`

New function, same file. `bundles` is a list of per-bundle specs:

```r
import_session(corp, "Session3",
  metadata = list(Age = 64, Gender = "F"),   # session-level default
  bundles = list(
    list(bundle = "B1", media = "b1.wav"),
    list(bundle = "B2", media = "b2.wav", metadata = list(Gender = "M"))  # override
  ))
```

For each entry, calls `import_bundle()` with `metadata = modifyList(session
metadata, bundle's own metadata override)`. Session creation is already
transparent (first `import_bundle()` call creates it via
`create_session_and_bundle()`; later calls in the same `bundles` list, or a
later `import_session()` call reusing the same `session` name, just add
bundles to the existing session — no special-casing needed).

Per-bundle failures (media import error, all track replays failing, etc.)
are caught, reported via `cli::cli_warn()` with the bundle name, and do not
abort the remaining bundles in the list. Returns `corpus_obj`, invisibly.

### Naming — rejected alternatives

- Bare `import()`: too generic, no verb-target clarity, breaks the existing
  `import_<noun>()` convention already established by the exported
  `import_metadata()`.
- `inject()`: collides with `rlang::inject()`, commonly attached
  transitively (e.g. via dplyr) — real risk of masking confusion.

## Testing

New `tests/testthat/test_import_bundle.R`, using the `ae` demo corpus
(`reindeer:::create_ae_db()`) plus a tiny fixture DSP function (matching the
pattern used in `test_quantify_segment_list.R`/`test_provenance.R`) so no
network/`superassp` dependency is required:

- `import_bundle()` on a brand-new session/bundle: session + bundle
  directories and skeleton JSON created, metadata written and readable via
  `get_metadata()`, media file present at the expected path.
- With one track already registered on the corpus (via a manual
  `quantify(corp, .using = fixture_fn)` on an existing bundle first):
  `import_bundle()` on a *new* bundle materializes that track for the new
  bundle only — assert the new bundle's SSFF file exists, and assert the
  *other*, pre-existing bundle's SSFF file's mtime is unchanged (guards the
  "other bundles' files are never touched" claim above).
- Age/Gender supplied via `metadata =` is reflected in the DSP args actually
  used (assert against the fixture DSP function's captured call args, the
  way `test_provenance.R` / dsp-derivation tests already do).
- Missing metadata: generation still succeeds, using the DSP function's
  defaults (no error).
- `tracks = FALSE`: bundle + metadata + media created, no SSFF file written.
- One track's `generator$package` unresolvable (fixture: fake package name):
  `import_bundle()` still returns successfully, `cli::cli_warn()` fires, and
  any *other*, resolvable track for that bundle is still generated.
- `import_session()`: multiple bundles created in one call; session-level
  metadata applied to all; a per-bundle metadata override wins for that one
  bundle; one bundle's media path deliberately broken doesn't stop the
  others from importing.
