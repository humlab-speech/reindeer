# reindeer forward plan 3.1.0 → 3.3.0 — critique of the 2.2.0 pair + post-program work

Date: 2026-10-11. Base: `main` @ `1b7d78f` (WP1.2 of the amendment plan landed).
Precondition for everything below: **Phases 0–7 of
`dev/REINDEER_AMENDMENT_PLAN_2.2.0.md` are complete and 3.0.0 has shipped.**
This document (a) critiques `dev/REINDEER_ASSESSMENT_2.2.0.md` and
`dev/REINDEER_AMENDMENT_PLAN_2.2.0.md`, and (b) is the forward plan to be
implemented after that program completes. It supersedes
`dev/REINDEER_PACKAGE_ASSESSMENT.md`, `dev/REINDEER_ENHANCEMENT_ASSESSMENT.md`
and `dev/REINDEER_REMEDIATION_PLAN.md` as the active forward document.

Verification note: repo-state claims below are checked against the working
tree at the base commit (exports, imports, CI workflows, schema files, git
log). I did not re-run any timing or DSP measurement; critiques of
measurements rest on the methodology the two documents themselves state
(load avg ~26, `[I]` markers, M1-only hardware).

## 1. What the 2.2.0 pair gets right (kept, not re-litigated)

- **Correctness before power.** The ordering principle — "power fixes would
  faithfully reproduce wrong output faster" — is the right call, and P1/P12
  are correctly pulled forward because they are deletions that also remove
  bugs (`bad_weak_ptr`) and dev-loop cost.
- **The parity harness (WP0.1) turns prose into code.** Golden fixtures +
  skip-as-ledger (each skip names the WP that fixes it) is the single best
  structural decision in the plan; every later claim rests on it.
- **Power analysis is genuinely about energy, not latency.** CPU-s accounting
  (7× worker-pool multiplier, 150 CPU-s vs 33 s wall) is the right primary
  metric, and P2's per-bundle measurement (14–31× less DSP work) is the
  correct headline lever.
- **A1–A9 are real, reproduced, and each gets a test-first WP.** The
  assessment's best property is that its headline defects are silently wrong
  *results*, not slow paths.
- **§8 consolidation is well-argued**: an export earns its place only as an
  analysis step; bookkeeping becomes automatic; inspection moves into
  `print()`/`summary()`. Nothing removes a capability, only a name.
- **§11 reconciles the previous remediation plan** instead of orphaning it.
  The forward plan below does the same for this pair.

## 2. Critique of `REINDEER_ASSESSMENT_2.2.0.md`

The findings hold up on re-reading and where re-checkable against the tree.
The weaknesses are methodological and in coverage, not in the defects listed.

**C1. Energy is measured only by proxy.** All power claims are CPU-s + wall
time; there is no real power instrumentation (macOS `powermetrics`, Linux
RAPL/`powercap`), no idle-power measurement, and no energy-per-workflow table.
The brief asks for runtime power; §10's targets are all time. → N0.2.

**C2. Memory is never systematically assessed.** Allocations are the largest
*unmeasured* energy sink, and the assessment itself surfaced allocation bombs:
`print()` copies the table (12 MB at 100k rows), eager navigation moves 56 MB
vs 12.8 KB fused, one query inlines a 1.43 MB `IN (...)` literal, the
annotation cache build does per-item `tibble()` churn. No peak-RSS baseline,
no profmem run, no memory targets. → N0.1, N1.3.

**C3. Per-bundle DSP fidelity is proven for one routine only.** P2's 14–31×
claim is credible, but the fidelity evidence is forest ΔF1 at segment
midpoints, n = 34. Pitch and spectral routines get no whole-file equivalence
evidence, and WP4.3's gate re-uses the same narrow proof. → N1.1 verification
matrix.

**C4. On-disk SSFF reuse is stated, then dropped.** P2 says "compute (or read
the registered SSFF/JSTF track from disk)", but WP4.3 implements
read-from-disk only for character tracks. Registered formant tracks — the
common iterative workflow (compute forest once, quantify repeatedly with
different `.at`) — stay recomputed per call. This is the largest *remaining*
power miss. → N1.1.

**C5. Ecosystem version drift is unguarded.** The golden fixtures pin
emuR 2.6.0 semantics. WP2.5 wisely adds a test that superassp formals exist,
but there is no equivalent check that the fixture generator still matches the
installed emuR, and no triage procedure when emuR changes semantics. → N3.4.

**C6. The standards area (§3) is thin.** It is mostly the R CMD check NOTE
plus internal consistency; there is no lint/style policy, no coverage floor,
and all performance evidence is M1-macOS-only even though P12 acknowledges a
5-combination CI matrix. → N3.2, N3.5.

**C7. §8's 26-export target is asserted, not costed.** The interface section
is defensible (the brief allows interface adjustment), but no per-export
tradeoff is given for the contested ones (e.g. `autoplot(type =)` pulling
ggplot2 into the documented surface; `annotate(x)` dual dispatch on corpus vs
segment list changing error shapes). Decisions D1–D8 settle the questions but
not the costs. Minor.

**C8. `import_metadata` O(N²) (6.5 s at 504 bundles, §2) has no scheduled
proof.** WP2.6 rewrites Excel import for atomicity and types; it never states
an O(N) target or a benchmark. → N2.3.

**C9. The metadata schema mismatch (§3.6) is flagged and never scheduled.**
Verified in the plan: no WP mentions `metadata.schema.json`. The file
declares draft-07 while `R/schema_validation.R:38` validates with the
draft-04 `imjv` engine, and the schema constrains `participant.age` while the
API uses flat `Age`. WP1.6 demotes jsonvalidate to Suggests, which makes the
check softer, not fixed. → N3.1.

**C10. Benchmarking assets are unreviewed.** `benchmarking/` holds 17 scripts
plus tracked generated artifacts (`.rds`, `.csv`, `.png`); some measure
deleted paths (serialization, simulation — moved to erodex). §6 cleans repo
clutter but not these. → N6.2.

**C11. No cross-platform evidence.** Every `[V]` number is M1 Pro macOS.
Power claims are platform-specific; the CI matrix is never mined. → N0.1 runs
the bench harness on the Linux runner class too.

**C12. The fidelity spec has an unstated internal tension.** "Results fully
identical to emuR" is the stated goal, but P2's per-bundle DSP deliberately
departs from emuR's per-segment windowing. The plan handles this correctly
(tolerance documented, `.mode = "window"` fallback, NEWS note), but it is a
fidelity tradeoff the user brief only permits "without sacrificing fidelity"
— the default-mode change deserves a user-facing decision note, not just a
changelog line.

## 3. Critique of `REINDEER_AMENDMENT_PLAN_2.2.0.md`

Structure and discipline are good (test-first per WP, one WP one commit,
gates, "no fix twice", skip-as-ledger). The problems are arithmetic drift,
one dropped finding, and missing enforcement.

**P1. The Imports gate is already wrong.** WP1.6 gates on "Imports count =
15", but D1 (WP2.4) adds dplyr to Imports and the `vctrs::s3_register()`
pattern needs vctrs in Imports. Verified: current DESCRIPTION lists 24
Imports (Rcpp already gone via WP1.2); the eight WP1.6 removals give 16;
dplyr + vctrs give 17–18. Restate the gate as "removal list complete + final
count recorded", or fix the number before Phase 1 closes.

**P2. No WP fixes the schema mismatch (C9).** Gap, carried forward.

**P3. Measurement conditions are unspecified.** §10 targets were measured
under load (avg ~26, disclosed); Gate 4 requires the targets met, but the plan
never says to re-baseline on a quiet machine first. A quiet-machine
re-baseline must precede Phase 4 or every Gate 4 number is
condition-dependent noise. → N0.1.

**P4. WP4.3's fidelity proof is too narrow (C3).** Add a per-routine
whole-file vs windowed equivalence matrix (pitch, spectral, ksv) or an
explicit documented allow-list of routines that keep windowed analysis.

**P5. WP4.4 default-on cache has no eviction policy.** "Size warning on
write" is passive; with the cache on by default, unbounded growth is the
failure mode. Add a cap + LRU eviction and a growth test. → N1.5.

**P6. No benchmark regression guard in CI.** WPs record before/after in
commit messages; nothing fails when a later WP silently regresses an earlier
workflow. → N0.3.

**P7. No coverage floor.** `test-coverage.yaml` exists (verified) and never
gates. The parity harness invites regressions elsewhere. → N0.3, N3.5.

**P8. WP2.2's cache key has a closure hole.** "For anonymous closures,
`digest(list(formals, body))`" ignores the closure environment: two closures
identical in text but with different captured constants collide. Either
include the environment digest or refuse to cache anonymous functions. →
N1.1's key discipline.

**P9. WP5.1 "Try `new_class(..., package = NULL)`" is a two-path outcome.**
Either spike it before Phase 5 opens or accept hand registration
indefinitely; a try-else-keep branch creates two registration schemes to
maintain.

**P10. Phase 7 (3.0.0) is a stub-deletion-only release, and D4's CRAN
question has no owner.** The deferred "revisit" needs a named WP and a
recorded decision. → N3.3.

**P11. WP1.5 misses tracked generated artifacts.** `vignettes/end_to_end_pipeline.html`
is tracked (verified); `benchmarking/` `.rds`/`.csv`/`.png` are tracked.
→ N6.2.

**P12. The parity-harness workflow is undocumented for future maintainers.**
WP1.5 fixes CLAUDE.md but the harness (add a case, regenerate goldens, what a
skip means) needs a maintenance note or the ledger decays. → N3.4.

**P13. `move_to`'s direction inference (WP5.2) is never proven against the
`requery_hier` fixtures.** WP3.2 proves ascend/descend; the consolidated verb
must map onto the same CTE with the same fixtures, or §8's parity promise
breaks at the last step.

### Findings → coverage table

| Finding | Scheduled in the current plan | If not, owner here |
|---|---|---|
| A1–A9, §2–§5, §7 items | WP2.1–WP6.4 | — |
| P1–P12 power/perf | WP4.1–WP4.7 | N0.x re-baseline (P3) |
| Schema draft-07/`imjv` mismatch (§3.6) | **no** | N3.1 |
| `import_metadata` O(N) proof (§2) | partial (WP2.6) | N2.3 |
| Closure cache-key collision | partial (WP2.2) | N1.1 |
| Registered-track reuse (P2 wording) | character tracks only (WP4.3) | N1.1 |
| Benchmark artifact hygiene | no | N6.2 |
| CI bench/cov/lint guards | no | N0.3, N3.2 |

## 4. Verified repo state at base

- 68 exports (NAMESPACE `export(` count), 24 Imports, 46+ test files.
- `future::plan(multisession)` still at 4 sites (`R/parallel_utils.R:75-77`,
  `R/corpus_database.R:89-90`, `R/quantify_corpus.R:243-245`,
  `R/metadata_core.R:333`); WP1.4 open.
- `R/zzz.R` has 5 `requireNamespace()` registration hooks (P9); WP4.1 open.
- `inst/schemas/metadata.schema.json` declares draft-07;
  `R/schema_validation.R:38` validates with `jsonvalidate(engine = "imjv")`
  (draft-04). Unfixed by any WP.
- pkgdown GitHub-Pages CI exists and builds vignettes with real DSP
  (superassp installed in the workflow); no `check_pkgdown()` in it.
- `test-coverage.yaml` uploads to codecov; no threshold.
- WP0.1–0.3, WP1.1, WP1.2 landed (git log: `eacbd94`, `5f88038`, `358942e`,
  `75674c8`, `0232a80`, `1b7d78f`); WP1.3+ open.

---

# The forward plan (post-3.0.0)

Order follows the brief's priority: power → performance → standards →
vignettes → reference docs → removal → pkgdown. Rules from the amendment
plan carry over: test-first per WP, one WP one commit, gates between phases,
effort S ≤ ½ day / M 1–3 days / L > 3 days. Every power/perf WP records
before/after against `benchmarking/BASELINE_3.0.0.md` (N0.1).

## Phase N0 — Measurement infrastructure (gate for N1/N2)

**N0.1 Quiet-machine re-baseline.** M
- `tools/bench.R`: fixed script order, `gc()` between stages, records wall,
  CPU-s (user+sys), peak RSS (external `time -l`/child-PID sampling), git sha,
  machine identifier and load. Re-runs the §10 table at 3.0.0 on an idle
  machine (load < 4) → `benchmarking/BASELINE_3.0.0.md`, conditions recorded
  in the header. Run once on macOS and once on the CI Linux runner class
  (C11). `BASELINE_2.2.0.md` kept for history.
- Proof: two consecutive runs within ±10%; conditions header present.

**N0.2 Energy instrumentation.** S–M
- `tools/measure-energy.sh`: `sudo powermetrics --samplers cpu_power` (macOS,
  documented sudo step) and RAPL via `powercap`/`perf` (Linux), sampled during
  the three headline workflows (quantify 253 cold, first open 700 bundles,
  scout 25k). Output: Joules per workflow into `benchmarking/ENERGY_<rel>.md`.
  Not CI-gated (privileges); run at each release, headline numbers in NEWS.
- Proof: table exists for the 3.1.0 release with per-workflow joules and the
  measurement conditions.

**N0.3 CI guards.** S
- Benchmark regression job (weekly + on `perf:` tags): core subset of
  `tools/bench.R`, fails when any workflow exceeds 1.2× BASELINE_3.0.0 on the
  same runner class.
- Coverage floor: the existing coverage workflow fails below 80 % overall,
  with per-file minimums for core files (query_parser, query_executor,
  tidy_trackdata_helpers, metadata_core, segment_list_quantify).
- Proof: both jobs green; a deliberate +30 % regression PR fails the bench
  job.

## Phase N1 — Power (area 1)

**N1.1 Reuse registered on-disk tracks (C4, P8).** M
- Before running any routine, check DBconfig `ssffTrackDefinitions`/JSTF for
  a registered track whose name + extension + parameter digest equals the
  requested routine's outputs; when present and mtime ≥ signal mtime, read
  and slice instead of recomputing. Applies to all `trk_*` routines, not just
  character tracks (extends WP4.3). Key discipline: function identity
  per WP2.2, plus environment digest for closures (P8) or refuse to cache
  anonymous functions.
- Fidelity: verify read-vs-recompute identity on `ae` for forest, pitch and
  one spectral routine (C3 matrix, per-routine); a track computed with
  different speaker norms must not be reused — the derived-parameter digest
  (WP2.5) is part of the key.
- Proof: corpus with precomputed forest tracks → quantify cost is slice +
  assembly only (< 0.1 s for 253 segments warm); output identical to
  recompute within fp tolerance.

**N1.2 superassp sysctl cost.** S (reindeer side)
- The per-call `detectCores()` → `system("sysctl")` in superassp costs ~20 %
  of a call (P2). File/land the upstream fix (cache once per session);
  reindeer pins the fixed version and re-measures. If upstream stalls, record
  the decision and the blocker.
- Proof: profile shows the sysctl share ≈ 0; NEWS states the required
  superassp version.

**N1.3 Allocation diet (C2).** M
- `profmem` on the headline workflows (first open 700, quantify 253 cold,
  scout 25k, metadata write 504 + FAIR regen). Fix the top allocations
  (expected: whole-file reads, `unlist`-heavy JSON paths, print paths,
  duplicated strings in metadata rows). Target: peak RSS down ≥ 30 % on the
  two worst workflows.
- Proof: peak-RSS table before/after in the commit and in RESULTS.

**N1.4 Idle and teardown power.** S
- Verify Artic staticPaths + stop-on-disconnect (WP4.7) hold; measure 5-min
  idle CPU-s of a served session (< 0.5 CPU-s); `.onUnload` stops the server;
  test asserts `httpuv::listServers()` is empty after close.
- Proof: test green + measurement note.

**N1.5 Cache write discipline and eviction (P5).** S–M
- Per-call cache writes coalesce into one transaction; LRU eviction by
  access time when the cache exceeds a configurable cap (default aligned with
  `check_cache_size` thresholds); `clear_cache()` gains matching `older_than`
  semantics.
- Proof: growth test (300 bundles written twice → second run writes only
  new/changed keys); eviction test (lowered cap → oldest entries gone, hits
  still correct).

## Phase N2 — Performance (area 2)

**N2.1 Query planner hardening.** M
- Prepared-statement cache per connection; CTE materialisation for repeated
  sub-queries; covering-index audit after an EXPLAIN pass (`items`, `links`,
  `labels`); EXPLAIN-golden tests for the hot query shapes so plans cannot
  silently regress.
- Proof: §2 latency table re-run (deep dominance keeps beating emuR);
  EXPLAIN tests in CI.

**N2.2 dplyr-verb overhead.** S
- Fast reconstruct inside S7 (skip property re-validation on internal
  reconstructs; cache the provenance deparse). Target: verb on segment_list
  ≤ 1.2× a plain tibble at 100k rows.
- Proof: micro-benchmark in `benchmarking/benchmark_core.R`, threshold wired
  into the N0.3 bench job.

**N2.3 Metadata read/write paths (C8).** S
- `corp[i, j]` delegates to the `get_metadata` COALESCE path (per §8.1
  sugar); prove the 14× gap closed. `import_metadata` single-pass: 504
  bundles < 1 s (vs 6.5 s).
- Proof: benchmark numbers + a regression test for the bracket path.

**N2.4 Lazy by default end-to-end.** M
- Navigation verbs stop defaulting `collect = TRUE`; dplyr verbs on lazy
  lists avoid auto-collect where SQL-translatable. The emuR parity suite must
  stay at zero skips (it compares collected rows), and provenance must record
  one step per verb on the lazy side too.
- Proof: default navigation chain benchmark ≈ the fused numbers (74 ms /
  12.8 KB class); parity suite unchanged; provenance tests green.

**N2.5 Startup audit.** S
- `library(reindeer)` ≤ 0.2 s verified (P9 target after WP4.1); startup
  profile committed; CI asserts ggplot2 is not loaded by `library(reindeer)`.
- Proof: CI check.

## Phase N3 — Standards, code quality, maintainability (area 3)

**N3.1 Metadata schema correctness (C9/P2).** S
- `inst/schemas/metadata.schema.json`: either re-declare draft-04 to match
  the `imjv` engine or validate with a draft-07 engine; align
  `participant.age` with the flat `Age` API; schema validation runs on every
  write (the WP5.3 promise) with the fixed engine.
- Proof: valid draft file passes, invalid one fails with a classed error
  naming the file; round-trip of an exported workbook validates.

**N3.2 Static checks in CI (C6).** S
- lintr workflow with a committed `.lintr` policy; `codetools` usage checks;
  R-devel added to the check matrix. Fix violations once, keep clean.
- Proof: lint job green; no new lint regressions accepted.

**N3.3 CRAN-readiness decision (P10).** M
- Execute D4's deferred re-evaluation: Artic download-on-first-`annotate()`
  spike (removes the 3.4 MB vendored dist), tarball ≤ 5 MB, http→https URLs,
  Remotes NOTE assessment, Suggests availability (superassp GitHub-only,
  eggstract private). Deliverable: a recorded decision (CRAN target with
  named blockers, or explicit non-goal) in `dev/`.
- Proof: tarball size measurement + the decision document.

**N3.4 Property and fuzz tests (C5, P12).** M
- EQL fuzzer: grammar-driven generator → reindeer vs emuR on `ae`; nightly
  job; discrepancies triaged (some will be emuR bugs — keep a triage list).
- Metadata inheritance property tests: random hierarchy writes assert the
  bundle > session > db precedence invariant.
- Provenance invariant: after every verb, rows_kept + rows_lost == input
  rows.
- Document the parity-harness maintenance workflow (add a case, regenerate
  goldens, what a skip means) in `tests/testthat/helper-parity.R` header.
- Proof: jobs green; triage doc exists.

**N3.5 Coverage floor + dead-code sweep.** S–M
- Enforce N0.3's 80 % floor with per-file minimums; re-run a §6-style
  dead-code review post-Phases 2–5 (code orphaned by the executor/parser
  rewrites).
- Proof: coverage job green; deletion PR with line counts.

## Phase N4 — Vignettes (area 4)

**N4.1 Performance and power vignette.** S–M
- New vignette (absent from the 2.6.0 eight-vignette set): cache behaviour,
  the user's `future::plan()`, an excerpt of the benchmark table, expected
  latency per workflow, power tips (sequential default, on-disk track reuse).
  Knits with real numbers from RESULTS.
- Proof: knits in CI; numbers match `benchmarking/`.

**N4.2 Vignette CI budget.** S
- knitr caching on CI so the vignette job stays < 10 min; nightly uncached
  knit; keep the 0-row / non-zero-norm-effect gates from WP6.1.
- Proof: workflow runtime recorded; nightly job green.

**N4.3 Generated migration table.** S
- The emuR → reindeer function map generated from NEWS + NAMESPACE diff
  (not hand-maintained) and included in the migration vignette.
- Proof: regeneration script + no stale rows.

## Phase N5 — Reference docs (area 5)

**N5.1 90/10 lint.** S
- CI check over Rd files: purpose ≤ 3 sentences; Value names class + columns;
  exactly one runnable `demo_corpus()` example; no internal identifiers in
  Details (explicit allowlist). Reports violators; fails on new ones.
- Proof: check green; violation report empty.

**N5.2 `@return` completeness.** S
- Check that every exported verb names its returned class and columns; fix
  anything the 2.6.0 rewrite left behind (verify the §5 worst-pages list is
  resolved).
- Proof: check green.

## Phase N6 — Superseded functionality (area 6)

**N6.1 Post-refactor dead-code sweep.** S–M
- After Phases 2–5: delete executor leftovers, merged-class remnants
  (`track_long` / nest output classes if WP5.1 consolidated them),
  `companion_eggstract.R` glue if `quantify_egg` was absorbed, and benchmark
  scripts measuring deleted paths.
- Proof: exports unchanged; tests green; line counts in the commit.

**N6.2 Asset and repo hygiene (C10, P11).** S
- Untrack generated benchmarking artifacts (`*.rds`, `*.csv`, `*.png`),
  delete tracked `vignettes/end_to_end_pipeline.html`; archive the older
  `dev/` documents (PACKAGE_ASSESSMENT, ENHANCEMENT_ASSESSMENT,
  REMEDIATION_PLAN, BUILD_LOG_REMEDIATION) with a pointer README in `dev/`.
- Proof: clean tree; tarball size stable.

## Phase N7 — pkgdown (area 7)

**N7.1 Site CI gate.** S
- `pkgdown::check_pkgdown()` inside the existing pkgdown workflow (currently
  failing per §7; WP6.3 fixes content — keep it green thereafter).
- Proof: job green.

**N7.2 Multi-version site and polish.** S–M
- pkgdown multi-version (released reindeer versions), verified search index,
  dark theme, og:image, generated article menu, dev badges on the dev site.
- Proof: site builds; `check_pkgdown()` green.

**N7.3 Ecosystem and citation pages.** S
- Navbar/footer links to protoscribe, erodex, eggstract, emuR; citation page
  rendered from `inst/CITATION` (content fixed in WP6.4).
- Proof: links resolve; citation page shows the current version.

## 6. Releases and gates

| Release | Phases | Character |
|---|---|---|
| 3.1.0 | N0, N1, N2 | Power and latency; output identical to 3.0.0 except row order/cols already fixed earlier |
| 3.2.0 | N3, N6 | Standards, fuzzing, CRAN decision, removal sweep |
| 3.3.0 | N4, N5, N7 | Documentation program and the site |

- Gate N1: every power/perf claim has a before/after number in
  `benchmarking/RESULTS_3.1.0.md`; parity suite at zero skips; energy table
  published. **Release 3.1.0.**
- Gate N2: lint/coverage/fuzz jobs green; schema validation on write; dead-code
  sweep merged; CRAN decision recorded. **Release 3.2.0.**
- Gate N3: all vignettes knit with real outputs; doc lints clean;
  `check_pkgdown()` green; multi-version site live. **Release 3.3.0.**

## 7. Risks

- **emuR drift**: golden fixtures pin emuR semantics; N3.4's fuzzer + triage
  list absorb drift, but a major emuR change forces a documented
  re-generation decision.
- **superassp upstream dependency (N1.2)**: the sysctl fix is outside this
  repo; the WP has an explicit stall path.
- **Energy measurement needs sudo** (N0.2): cannot be CI-gated; numbers are
  release-time, recorded with conditions.
- **Multi-version pkgdown** has real infra cost for a niche package; N7.2
  should be cut to a single-version site with a NEWS-hosted release archive
  if the Pages setup fights back.

## 8. Status

| WP | State | Commit |
|---|---|---|
| N0.1–N0.3 | open | |
| N1.1–N1.5 | open | |
| N2.1–N2.5 | open | |
| N3.1–N3.5 | open | |
| N4.1–N4.3 | open | |
| N5.1–N5.2 | open | |
| N6.1–N6.2 | open | |
| N7.1–N7.3 | open | |
