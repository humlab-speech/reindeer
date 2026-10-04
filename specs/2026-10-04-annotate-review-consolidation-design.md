# Design: consolidate `serve()`/`serve_app()` into `annotate()`/`review()`, and ship Artic inside reindeer

Date: 2026-10-04
Status: approved by user; Phase A implemented

## Background

`serve()` and `serve_app()` were the same function (`serve_app` was a thin
alias). The web app they opened was a local EMU-webApp checkout, while the
project's actual app is **Artic** (a humlab fork of EMU-webApp, protocol
`EMU-webApp-websocket-protocol` 0.0.2). The verb pair also conflated two very
different jobs: editing a corpus, and walking a targeted segment list.

Artic already supports everything the second job needs (`bundleListSchema`
`timeAnchors`, first-anchor selection on bundle load, sidebar prev/next,
`curTimeAnchorIdx`), and reindeer's `serve(seglist=)` already injected anchors
— but dropped bundle metadata, ignored labels, and never advanced across
bundles.

## Naming

- `transcribe` was rejected: it collides with `protoscribe::transcribe()` in
  the same user ecosystem.
- `annotate()` + `review()` chosen. `annotate` collides with
  `ggplot2::annotate()`, so reindeer's `annotate(geom = NULL, ...)` forwards
  any non-`corpus` first argument verbatim to ggplot2 (mirroring ggplot2's
  first formal, so `p + annotate("text", x = 1, ...)` is unchanged and attach
  order does not matter).
- `serve()`/`serve_app()` become exported hard stubs (`reindeer_moved_error`)
  pointing at `annotate()`/`review()`, per repo convention. Keeping them
  exported is deliberate: otherwise `serve(corp)` resolves to `emuR::serve()`.

## `annotate(corpus, ...)`

Whole-corpus (or `sessionPattern`/`bundlePattern` subset) editing session.
Applies `.artic_edit_overlay()` in memory only (never writes `_DBconfig.json`):
editing restrictions TRUE, `saveBundle` and level buttons TRUE (Artic's
defaults hide the save button and silently discard edits on bundle switch),
perspectives made renderable (default perspective, OSCI/SPEC order,
`levelCanvases.order` filled from time-bearing `levelDefinitions`).

## `review(corpus, seglist, ...)`

Accepts `segment_list`, `extended_segment_list`, `lazy_segment_list`, or a
data.frame. Builds ordered playlist entries with per-segment `timeAnchors`:

- anchors prefer `sample_start`/`sample_end`; fall back to
  `start`/`end` × `sample_rate`;
- rows play in their current order (`dplyr::arrange()` before `review()`
  controls order);
- sessions are grouped by first appearance, then bundles within session, so
  the flat list order equals Artic's grouped sidebar (needed for cross-bundle
  advance);
- optional `label` per anchor from `labels`/`label`;
- validates columns, types, empty input, unknown bundles, and `db_uuid`.

`tracks = NULL` infers `dsp_columns` from an `extended_segment_list`; resolved
names must exist in `ssffTrackDefinitions` and have files on disk for every
playlist bundle, otherwise they are warned about and skipped, and the overlay
is added to the served perspective's `signalCanvases.assign`.

## Artic distribution

One artifact, many transports. Artic stays the source of truth and standalone
(`npm start`, static host, release tarballs, optional Pages/Docker); reindeer
consumes a pinned prebuilt dist and never forks the app.

- **Resolver** `find_artic(appDir, require, quiet)`: explicit `appDir` (hard
  override) → `reindeer.artic.dir` → `ARTIC_DIR` → bundled
  `system.file("artic/dist")` → `install_artic()` cache → sibling
  `../artic/dist`. Validates `index.html` and the build manifest; a
  `review()` session requires the `bundleList`/`timeAnchors` features.
- **Artifact**: `dist/` plus `dist/artic-manifest.json` (version, git sha,
  protocol, features). `tools/sync-artic-dist.R` vendors a trimmed copy
  (`demoDBs/` excluded; Artic disables the demo button once connected) into
  `inst/artic/dist`, with MIT `LICENSE` and a provenance `NOTICE`, and writes
  `inst/artic/pin.json`.
- **Upgrades/offline**: `install_artic(url=, sha256=, ref=)` downloads and
  checksum-verifies into `tools::R_user_dir("reindeer","cache")/artic/<id>`;
  `install_artic(local=)` copies a local build. Never runs implicitly.
- **Deployments**: desktop and RStudio/Positron/Workbench use the local server
  and Viewer; headless/HPC/CI/Docker/R-universe/conda use the bundled copy
  with no network; a hosted build can point `serverUrl` at the local WS
  (media CORS is already `*`).

## Touch list (Phase A)

- `R/reindeer_serve.R`: `serve()` generic → internal `.artic_session()`;
  unified bundle entries; `GETBUNDLELIST` returns entries; missing SSFF files
  skipped, not fatal; MIME map extended; classed port errors; dead viewer
  index-rewrite and `get_webapp_dir()` removed.
- New: `R/artic_dist.R`, `R/artic_playlist.R`, `R/reindeer_annotate.R`,
  `tools/sync-artic-dist.R`, `inst/artic/*`.
- `R/deprecated_stubs.R`: `serve`/`serve_app` stubs.
- Tests: `tests/testthat/test_annotate_review.R` (replaces `test_serve.R`).
- Docs: README, `interactive_annotation.Rmd`, `_pkgdown.yml`, NEWS, CLAUDE.md,
  `inst/agents/AGENT_GUIDE.md`.

## Artic-side follow-up (separate repo)

1. Cross-bundle anchor advance in `BundleListSidebar` (▶ at the last anchor
   loads the next bundle; ◀ at the first loads the previous bundle's last
   anchor), and initialise `curTimeAnchorIdx` to `0`.
2. Emit `dist/artic-manifest.json` from the build and publish
   `artic-dist-<version>.tar.gz` + checksum from CI.
3. Optional: anchor labels in the sidebar, autoplay on anchor jump,
   per-anchor reviewed state.
