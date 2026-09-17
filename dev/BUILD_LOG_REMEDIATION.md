
# Build-log remediation plan (2026-09-17)

`build.log` reports 9 errors, all from one chain: `R CMD build` re-builds the
vignettes and `metadata_management.Rmd` fails.

    * creating vignettes ... ERROR
    --- re-building 'cache_management.Rmd' ... finished
    --- re-building 'end_to_end_pipeline.Rmd' ... finished
    --- re-building 'getting_started.Rmd' ... finished
    --- re-building 'interactive_annotation.Rmd' ... finished
    --- re-building 'lazy_and_provenance.Rmd' ... finished
    --- re-building 'metadata_management.Rmd' using rmarkdown
    Quitting from metadata_management.Rmd:89-92 [bracket-read]
    Error in `[[<-`: ! Assigned data `field_values` must be compatible with existing data.
    Existing data has 1 row. Assigned data has 2 rows.
    ... vectbl_recycle_rhs_rows() -> Can't recycle input of size 2 to size 1
    --- failed re-building 'metadata_management.Rmd'
    Error: Vignette re-building failed.

Five of six vignettes re-build cleanly, so the conversion work in this
session is sound; the failure is the one chunk that exercises the corpus
bracket read.

## Issue 1 - `[.reindeer::corpus` read path assigns a longer vector than the frame (OPEN)

**Symptom.** `corp[session, bundle]` - the *read* form, no assignment - aborts.

**Reproduction.**

```r
corp <- demo_corpus()
md   <- get_metadata(corp)
corp[md$session[1], md$bundle[1]]
```

**Traceback.** `corp[...]` -> `[.reindeer::corpus` -> `tibble:::tbl_subassign`
-> `vectbl_recycle_rhs_rows`, i.e. inside the method's own loop at
`R/corpus_methods.R:522-528`:

```r
for (field_name in fields$field_name) {
  field_values <- get_metadata_field_values(con, corpus_obj@.uuid, field_name,
                                             result$session, result$bundle)
  result[[field_name]] <- field_values        # 1 row receiving 2 values
}
```

**What is already ruled out.** `get_metadata_field_values()` itself
(`:537`) resolves bundle -> session -> database correctly and writes at
most one value per index, so it returns `length(sessions)` values. The
mismatch is therefore between the index vectors handed to it and the frame
that receives the result - `result` reaches the assignment with fewer rows
than there are index entries (the first field's assignment appears to add a
row, or `result` is built from a different subset than `sessions`/`bundles`).

**Fix direction.** Make the invariant explicit and let it fail loudly:
compute `sessions`/`bundles` from the frame being filled immediately before
the loop, and assert `length(field_values) == nrow(result)` before the
assignment. Then find why the two disagree for a field that resolves
through more than one level.

**Regression test.** `corp[session, bundle]` returns the bundle's resolved
metadata - one value per field, inheritance applied. This is the test that
would have caught it: the read form is documented in the vignette and in
`?corpus`, and nothing exercised it.

**Immediate mitigation (DONE).** `metadata_management.Rmd` now uses
`get_metadata()` for the read and keeps the bracket form as a non-evaluated
example naming this defect, so the build proceeds. That is a workaround in
documentation, not a fix - Issue 1 stays open.

## Issue 2 - superassp >= 3.0.0 cannot be installed here (OPEN, environment)

Not a build error, but it blocks behaviour the docs promise: `quantify()`
currently warns that Age/Gender norms were not applied because the 2.9.5
`trk_*` wrappers expose no tunable formals. Root cause measured: the GitHub
build fails on missing SPTK headers
(`SPTK/analysis/pitch_extraction_by_rapt.h`). Plan: provide SPTK, reinstall
superassp, then close D6, `erodex::quantify_simulate()` and the gated
simulation section together, and pin `superassp (>= 3.0.0)`.

## Issue 3 - protoscribe cannot be installed here (OPEN, upstream)

Its DESCRIPTION declares `qs`, retired from CRAN, so installation stops at
`dependency 'qs' is not available`. The archive route for `qs` was tried and
did not complete. Nothing to change in reindeer; the companion needs the
`qs2` migration reindeer already made.

## State after this plan's first item is mitigated

- five vignettes re-build cleanly, `metadata_management.Rmd` renders once
  the bracket read is off the evaluated path;
- one new open defect (Issue 1) with a reproduction and a regression test
  named;
- two environment blockers (Issues 2 and 3) unchanged, each with its
  measured cause.
