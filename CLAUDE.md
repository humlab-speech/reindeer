# CLAUDE.md

reindeer is an R package for emuR databases (`*_emuDB`): query them with EQL,
measure them with superassp, and keep speaker metadata alongside. emuR is
**test-only** (parity goldens); at runtime reindeer reads `_DBconfig.json`,
`_annot.json` and `_emuDBcache.sqlite` itself.

Active plan: `dev/REINDEER_WAY_FORWARD_3.2.md`; its status table (§7) says
what is open. Older `dev/` plans are history, not instructions.

## Commands

```bash
Rscript -e 'devtools::test()'                                  # full suite (~2 min)
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_eql_parity.R")'
Rscript -e 'devtools::document()'
Rscript -e 'devtools::check(args = "--as-cran")'               # must stay 0/0/0
Rscript -e 'lintr::lint_package()'                             # config in .lintr
Rscript tools/make-eql-golden.R                                # regenerate EQL goldens (needs emuR)
```

## Public surface (31 exports, see NAMESPACE)

| Verb | File |
|---|---|
| `corpus()`, `demo_corpus()` | `R/corpus_class.R`, `R/reindeer_demodata.R` |
| `query(corp, eql)` → `lazy_segment_list`; `collect()` runs it (`lazy = FALSE` for eager) | `R/query_executor.R`, `R/query_parser.R`, `R/eql_parser.R`, `R/reindeer_lazy_segment_list.R` |
| `scout()`, `move_to()` | `R/reindeer_sequence_ops_optimized.R` |
| `quantify()` | `R/segment_list_quantify.R`, `R/quantify_corpus.R`, executor + cache in `R/tidy_trackdata_helpers.R` |
| `dsp_parameters()` (Age/Gender → DSP args) | `R/dsp_parameters_public.R`, `R/dsp_parameter_derivation.R` |
| `get_metadata()`, `set_metadata()`, `corp[ses, bndl]`, `export_metadata()`, `biographize()` | `R/metadata_core.R`, `R/metadata_unified.R`, `R/metadata_import_export.R` |
| `annotate(corp)` / `annotate(segs)` (Artic; `review()` is internal) | `R/reindeer_annotate.R`, `R/reindeer_serve.R`, `R/artic_*.R` |
| `import_media()`, `read_annotations()`, `write_annotations()` | `R/import_bundle.R`, `R/annotations_io.R` |
| `describe_corpus()`, `clear_cache()`, `provenance()`, `dropped()`, `pivot_tracks_*()` | `R/corpus_describe.R`, `R/cache_size_management.R`, `R/segment_list_*.R` |

Classes are S7 (`@` for properties): `corpus`, `segment_list` (inherits
`tbl_df`; dplyr/vctrs hooks in `R/segment_list_dplyr.R`), `lazy_segment_list`.

## quantify() modes

- `quantify(segs, fn)`: run a superassp-style function. `.mode = "bundle"`
  (default) analyses each file once and slices; `"window"` analyses each
  segment. `.at` picks one frame per segment; `.use_metadata = TRUE` feeds
  Age/Gender from metadata into the DSP arguments.
- `quantify(segs, "fm")`: read a track registered in `_DBconfig.json`.
- `quantify(corp, .using = fn)`: write SSFF files into the bundles and
  register the track.

## Storage

- Ground truth: `_DBconfig.json`, `<bundle>_annot.json`, `METADATA.json` (same
  name at db, session and bundle level). Caches are rebuildable from them.
- `<db>/<name>_emuDBcache.sqlite`: annotation/metadata cache (emuR schema).
- `<db>/.quantify_cache/quantify_cache.sqlite` (`corpus@.cache_dir`):
  quantify results, keyed by function identity, parameter digest, file stamp
  and `.cache_schema` (`R/tidy_trackdata_helpers.R`). Bump `.cache_schema`
  when cached content changes meaning.

## Tests

- Fixtures: the emuR `ae` demo corpus via `demo_corpus()` or the helpers in
  `tests/testthat/helper-corpus.R` (`create_isolated_ae_corpus()` when a test
  writes to the corpus).
- EQL parity: `test_eql_parity.R` against `fixtures/eql_golden.rds`; how to
  add a case is in `helper-parity.R`.
- DSP tests use real superassp output or the `.fms` files `ae` ships, never
  fake SSFF bytes; `emuR::get_trackdata()` is the reference.
- A skip names the WP that removes it. A WP is not done while a skip names it.

## Rules

- Errors are not fallbacks: no `tryCatch(..., error = function(e) NULL)` on
  measurement paths. Signal with the classed helpers in `R/conditions.R`.
- superassp is a moving dependency: behaviour reindeer relies on belongs in a
  contract test, not in a comment.
- Write JSON ground truth, then refresh the cache; never the cache alone.
- Commits: `feat:`, `fix:`, `refactor:`, `test:`, `perf:`, `docs:`, `ci:`;
  one WP per commit.
- Companion packages: protoscribe (draft annotation), erodex (parameter
  simulation).
