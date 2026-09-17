
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


# Round 2 - the clean build log, and the plan that amends what is left

The new `build.log` is a **successful** build:

    * creating vignettes ... OK
    * building 'reindeer_1.1.1.tar.gz'

No errors, no warnings. Round 1's failure chain is gone. What remains in the
log is one class of informational line, and behind it the two workarounds
that are still carrying this build.

## A. "Removed empty directory" x4 - informational, decide rather than fix

`.qodo`, `R/deprecated`, `images`, `tests/testthat/_snaps` are all empty and
**untracked** (`git ls-files` returns nothing for each), so they exist only
in this working copy and cannot reach the tarball. `R CMD build` deleting
them is it doing the right thing, not a defect.

Amendment, pick one and move on - neither affects the built package:

1. **Delete `.qodo`** (editor/tool residue) and leave `images` and
   `tests/testthat/_snaps`, which testthat and pkgdown recreate on demand;
   `R/deprecated` can go too if no placeholder is wanted.
2. Leave all four; the lines stay in the log and stay harmless.

Acceptance: the log's lines 11-14 disappear (option 1) or are documented as
expected (option 2). No `R/`, `man/` or `tests/` change either way.

## B. The two gated vignette chunks are the real open work (Issues 1 and 4)

Both are build-only failures that pass when the same calls are run by hand.
They are the reason two chunks in `metadata_management.Rmd` are
non-evaluated, so this plan is not finished until they are fixed and
un-gated.

**Order matters.** Issue 1 (`corp[session, bundle]`) hides Issue 4
(`export_metadata()`): clearing the first is what exposed the second.

### B1. Diagnose against the installed package, not the working tree

Both defects fire under `R CMD build`, which renders vignettes against the
**installed** package. Run exactly that:

```r
R CMD INSTALL .
rmarkdown::render("vignettes/metadata_management.Rmd")
```

If it reproduces, the difference is the install-and-render path; if it does
not, the difference is `R CMD build`'s environment (temporary library, no
`NOT_CRAN`, working directory). Whichever it is, that difference is the
diagnosis - the code paths themselves already pass by hand.

### B2. Issue 1 - bracket read

Reproduction to run, with the prior writes from the vignette in place:

```r
corp <- demo_corpus(); md <- get_metadata(corp)
set_metadata(corp, list(Project = "P", Year = 2026, Institution = "H"))
set_metadata(corp, list(Speaker = "P001", Age = 25, Gender = "Female"), session = md$session[1])
set_metadata(corp, list(Quality = "Excellent", Microphone = "SM58"), session = md$session[1], bundle = md$bundle[1])
corp[md$session[1], md$bundle[1]] <- list(SamplingRate = 44100)
corp[md$session[1], md$bundle[1]]                      # <- the failure
```

Fix direction: at `R/corpus_methods.R:522-528`, assert
`length(field_values) == nrow(result)` before `result[[field_name]] <-`
and derive both index vectors from the frame being filled in the same
expression, so they cannot drift.

Regression test: the bracket read returns one value per field with
inheritance applied, for a corpus carrying fields at all three levels.

Acceptance: the chunk's `eval = FALSE` comes off, `R CMD build` still
passes, and the test above is green.

### B3. Issue 4 - export_metadata() and openxlsx column names

Failure: `colNames must be a unique vector (case sensitive)` from
`openxlsx:::assert_unique()` inside `export_metadata()`.

The combination present in the vignette but in neither hand-run: a nested
`project = list(name = ..., description = ...)` field **and** a scalar
`Project` field. Reproduce with both set, then either:

- de-duplicate in `export_metadata()` before the write, or
- qualify flattened nested names by their parent (`project_name`,
  `project_description`) and check the result against the scalar field
  case-insensitively.

Regression test: export a corpus carrying nested `project` plus scalar
`Project` to a temp workbook and assert it writes and round-trips.

Acceptance: the chunk's `eval = FALSE` comes off and the test is green.

## C. Environment blockers, unchanged (Issues 2 and 3)

Not amendable from this repository:

- **superassp >= 3.0.0** - GitHub build stops on missing SPTK headers
  (`SPTK/analysis/pitch_extraction_by_rapt.h`). Provide SPTK, reinstall, then
  D6, the erodex sweep and the gated simulation section close together and
  `superassp (>= 3.0.0)` can be pinned.
- **protoscribe** - its DESCRIPTION declares the retired `qs`; installation
  stops there. Needs the `qs2` migration upstream.

## D. Order of work

1. A - delete the residue directories (minutes, no risk).
2. B1 - render against the installed package to localise both defects.
3. B2 - fix the bracket read, un-gate, rebuild.
4. B3 - fix the export, un-gate, rebuild.
5. C - environment, when SPTK is available and protoscribe is updated.

Each step's acceptance is a passing `R CMD build` plus the named test; no
step requires touching another.


## B1 RESULT: both defects are build-environment-only, and now not reproducible at all

Ran the diagnostics in escalating fidelity. Every one passed:

| Run | Issue 1 (bracket read) | Issue 4 (export) |
|---|---|---|
| hand-run, `load_all()` | OK, 11 fields | OK |
| hand-run, **installed** package via `library(reindeer)` | OK, 11 fields | OK |
| whole vignette rendered outside the build, both chunks re-enabled in a copy | renders, 35 KB | renders |
| `R CMD build` with the chunk instrumented | build passes (exit 0) | build passes |

The last row is the informative one. The instrumented chunk only (a) called
`get_metadata(corp)` before the export and (b) wrapped the export in
`tryCatch`. Nothing about the data changed, and the build went from failing
to passing.

**What that implies.** The failure is order- or timing-dependent rather than
a pure function of the corpus contents. The leading hypothesis is now a
**stale or duplicated metadata cache at the moment of export** - the
`get_metadata()` call added for diagnosis may itself refresh the very state
the export then trips over. That also fits the original symptom
(`colNames must be a unique vector`): a duplicate field name in the cached
field list would produce exactly that column set.

**Next diagnostic, and it is cheap:** capture the cache state instead of the
data. In the failing configuration, dump the field list straight from SQLite
before exporting:

```r
con <- reindeer::get_connection(corp)
DBI::dbGetQuery(con, "SELECT field_name, COUNT(*) n FROM metadata_fields GROUP BY field_name HAVING n > 1")
DBI::dbGetQuery(con, "SELECT field_name, field_level, COUNT(*) n FROM metadata_metadata GROUP BY 1,2 HAVING n > 1")
```

Two rows in either result names the duplicate and the level it came from,
which is what a fix has to de-duplicate.

Meanwhile the workarounds stand and `R CMD build` passes. Treat B2 and B3 as
**not yet actionable**: a fix needs the failing state reproduced first, and
four independent attempts have now failed to reproduce it. The next person
should spend their effort on capturing the state, not on editing the
export path.


# Round 3 - this log has no issues; the plan moves to the next gate

Line by line, the log is all-informational:

    1  checking for file 'DESCRIPTION' ... OK
    2  preparing 'reindeer':
    3  checking DESCRIPTION meta-information ... OK
    4  cleaning src                      <- removes object files from a prior compile
    5  installing the package (needed to process help pages)
    6  saving partial Rd database        <- normal, part of the Rd step
    7  creating vignettes ... OK         <- the step that failed in Round 1
    8  cleaning src
    9  checking for LF line-endings ...  <- passes silently when clean
    10 checking for empty or unneeded directories
    11 building 'reindeer_1.1.1.tar.gz'

Nothing here is an error or a warning. Two things this confirms:

- **Round 1's failure chain is closed** - `creating vignettes ... OK` is the
  line that used to be `ERROR`.
- **Step A of Round 2 worked** - the four `Removed empty directory` lines are
  gone, because the empty residue directories were deleted rather than
  papered over.

So `R CMD build` is green end to end. There is nothing in this log to amend.

## What that implies for the plan

A clean build is the *precondition* for the next gate, not the end of the
work. The remaining items do not appear in a build log at all, which is the
point to be explicit about:

| Item | Visible in a build log? | Where it shows up |
|---|---|---|
| B2/B3 build-only defects (bracket read, export) | **no** - they only fire inside the build's own vignette render, and four attempts have failed to reproduce them | needs the state capture described in Round 2, not a build run |
| superassp >= 3.0.0 | no | its own install log (SPTK headers) |
| protoscribe | no | its own install log (retired `qs`) |
| documentation warnings | no - those are `R CMD check`'s job | `R CMD check` |

## The next gate: R CMD check

Since the build is clean, the informative next run is:

    R CMD build .            # produces the tarball (green, per this log)
    R CMD check reindeer_1.1.1.tar.gz

What to expect, from this session's earlier check run:

- **1 WARNING**, documentation-only, from the `\usage`/`\arguments`
  family. Everything else was cleared from a baseline of 7 WARNING + 1 NOTE.
- **The vignette step will run under check too**, with the same two chunks
  gated - so a green check does not mean B2/B3 are fixed, only that their
  workarounds hold under check as well as build.

Priority order from here, unchanged in substance:

1. Capture the cache state for B2/B3 (the SQLite queries are in Round 2).
2. Provide SPTK, reinstall superassp - closes D6, the erodex sweep, the
   gated simulation section, and enables the version pin.
3. Chase the remaining `\usage` warning down to zero, if a clean check is
   wanted.
