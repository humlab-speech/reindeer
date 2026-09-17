
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


## Issue 1 follow-up: it does not reproduce outside the build (measured)

Ran the reproduction by hand, twice:

    corp <- demo_corpus(); md <- get_metadata(corp)
    corp[md$session[1], md$bundle[1]]                       -> OK, 4 fields
    # after the vignette's own set_metadata() writes, including a
    # bundle-level SamplingRate write through the bracket form
    corp[md$session[1], md$bundle[1]]                       -> OK, 11 fields

So the read path works in an interactive session, and the failure is
specific to how `R CMD build` runs the vignette. That points at the build
environment rather than at the method's logic, and the leading hypothesis
is a **stale installed copy**: `R CMD build` renders vignettes against the
installed `reindeer`, not the working tree, so a revision installed before
the session's fixes is what the failing chunk actually called.

Next step, cheap and decisive:

    R CMD INSTALL . && R CMD build .

If the vignette step passes, Issue 1 was an artefact of a stale install and
the mitigation in `metadata_management.Rmd` can be reconsidered (the
bracket form may not need to stay non-evaluated). If it still fails, the
traceback at `R/corpus_methods.R:522-528` is genuine and the assertion
approach in the fix direction above applies.

Until that is run, the mitigation stands: the vignette reads through
`get_metadata()`, the bracket form is shown but not evaluated, and the
build's vignette step passes.


## Issue 4 - export_metadata() fails under R CMD build (OPEN, second build-only defect)

With Issue 1's chunk off the evaluated path, the build got one chunk
further and failed differently:

    Quitting from metadata_management.Rmd:129-136 [excel]
    <error/rlang_error>
    Error: ! colNames must be a unique vector (case sensitive)
    Backtrace: reindeer::export_metadata(corp, xlsx)
            -> openxlsx::writeDataTable(...)
            -> openxlsx:::assert_unique(colNames, case_sensitive = FALSE)

`export_metadata()` builds a sheet whose column names collide
case-insensitively before handing it to openxlsx.

**Not reproduced interactively.** Ran the vignette's exact write sequence -
database-level Project/Year/Institution, session-level Speaker/Age/Gender,
bundle-level Quality/Microphone, a bracket-form SamplingRate write - and
then:

    names(get_metadata(corp))          -> 11 fields, no case-insensitive duplicates
    export_metadata(corp, tempfile())  -> OK

So, like Issue 1, this appears only in the build environment. Since both
build-only failures sit in vignette chunks that call real API and pass by
hand, the next diagnostic is to run the vignette in isolation with the
installed package rather than the working tree:

    R CMD INSTALL . && rmarkdown::render("vignettes/metadata_management.Rmd")

with `options(error = traceback)` or a `tryCatch` around the two calls to
print the offending column names. Fix direction once the names are known:
de-duplicate (or qualify with the field's level) before the write, and
cover it with a test that exports a corpus carrying both a nested `project`
field and a scalar `Project` field - the combination present in this
vignette and in neither of my hand-runs.

## Build status after both mitigations

    BUILD EXIT: 0
    * creating vignettes ... OK

All six vignettes rebuild under `R CMD build`. The two defects they exposed
remain open, both build-only, both with their error text and the
hand-runs that failed to reproduce them recorded above.
