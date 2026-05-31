# ==============================================================================
# Autosync adapter: EMU _annot.json -> ELAN .eaf
# ==============================================================================
#
# sync_annot_to_eaf() in R/reindeer_autosync.R calls convert_emu_to_eaf()
# once per changed bundle. This file is the bridge between the autosync
# driver and the user-facing write_eaf() in R/interop_elan.R.
#
# Flow:
#   _annot.json --jsonlite::read_json--> nested list
#                --.annot_levels_to_segments--> tibble(start,end,labels,level)
#                --write_eaf--> .eaf XML on disk
#
# EMU level types map onto EAF tiers like this:
#   * SEGMENT -> start = sampleStart / sampleRate * 1000
#                end   = (sampleStart + sampleDur) / sampleRate * 1000
#   * EVENT   -> start = end = samplePoint / sampleRate * 1000
#   * ITEM    -> no own timing; with align_items = TRUE the span is the
#                [min(start), max(end)] of all SEGMENT/EVENT descendants
#                reached through the bundle's links list. With
#                align_items = FALSE the ITEM rows are dropped.
#
# Each item can carry several labels (e.g. Word has Word + Accent + Text);
# we emit one tibble row per label so every annotation attribute becomes
# its own ELAN tier. The tier name is the label's `name`, prefixed with
# the EMU level name when there is more than one label per item, so
# Word.Text and Word.Accent stay distinguishable.

# ------------------------------------------------------------------------------
# Helper: ITEM-level time deduction from in-memory links.
#
# Walks the links graph downwards from `item_id` and returns the
# [min_start_samples, max_end_samples] across all SEGMENT/EVENT
# descendants. Returns NULL if no timed descendant is reachable.
#
# `timing` is a flat list: timing[[as.character(id)]] = c(start, end)
# in *samples*. `children` is a list: children[[as.character(id)]] is an
# integer vector of direct child ids.
.deduce_item_span_from_links <- function(item_id, timing, children) {
  if (length(timing) == 0L) return(NULL)
  seen <- new.env(parent = emptyenv())
  starts <- numeric(0)
  ends   <- numeric(0)
  stack  <- list(item_id)

  while (length(stack) > 0L) {
    cur <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    key <- as.character(cur)
    if (!is.null(seen[[key]])) next
    seen[[key]] <- TRUE

    t <- timing[[key]]
    if (!is.null(t)) {
      starts <- c(starts, t[[1]])
      ends   <- c(ends,   t[[2]])
    }
    ch <- children[[key]]
    if (!is.null(ch)) {
      for (c_id in ch) stack[[length(stack) + 1L]] <- c_id
    }
  }

  if (length(starts) == 0L) return(NULL)
  c(min(starts), max(ends))
}

# ------------------------------------------------------------------------------
# Helper: reshape parsed _annot.json into a write_eaf()-ready tibble.
#
# `annot_json` is the result of jsonlite::read_json(..., simplifyVector = FALSE)
# — a nested list with `sampleRate`, `levels`, and `links`. Returns a
# tibble with columns start, end, labels, level (all in milliseconds).
.annot_levels_to_segments <- function(annot_json, align_items = TRUE) {
  sample_rate <- annot_json$sampleRate %||% 0
  levels_list <- annot_json$levels %||% list()
  links_list  <- annot_json$links  %||% list()

  if (length(levels_list) == 0L || !is.numeric(sample_rate) || sample_rate <= 0) {
    return(tibble::tibble(start = numeric(), end = numeric(),
                          labels = character(), level = character()))
  }

  # First pass: collect SEGMENT/EVENT timing (in samples) keyed by item id,
  # collect items metadata, and build a parent->children adjacency map.
  timing  <- list()
  children <- list()
  for (lk in links_list) {
    f <- as.character(lk$fromID)
    children[[f]] <- c(children[[f]], as.integer(lk$toID))
  }

  # Buffer rows as lists; rbindlist at the end avoids quadratic copies.
  rows <- vector("list", 0L)

  for (lv in levels_list) {
    lv_name <- as.character(lv$name %||% "")
    lv_type <- as.character(lv$type %||% "")
    items   <- lv$items %||% list()
    if (length(items) == 0L) next

    # Determine timing per item.
    for (item in items) {
      id_chr <- as.character(item$id)

      if (identical(lv_type, "SEGMENT") &&
          !is.null(item$sampleStart) && !is.null(item$sampleDur)) {
        s <- as.numeric(item$sampleStart)
        e <- s + as.numeric(item$sampleDur)
        timing[[id_chr]] <- c(s, e)
      } else if (identical(lv_type, "EVENT") && !is.null(item$samplePoint)) {
        p <- as.numeric(item$samplePoint)
        timing[[id_chr]] <- c(p, p)
      }
      # ITEM-type timing is deduced lazily below — they have no own
      # sampleStart/Dur/Point.
    }
  }

  # Second pass: emit one row per (item, label).
  for (lv in levels_list) {
    lv_name <- as.character(lv$name %||% "")
    lv_type <- as.character(lv$type %||% "")
    items   <- lv$items %||% list()
    if (length(items) == 0L) next

    for (item in items) {
      id_chr <- as.character(item$id)
      labels_v <- item$labels %||% list()
      if (length(labels_v) == 0L) next

      # Resolve sample-domain [start, end] for this item.
      if (identical(lv_type, "ITEM")) {
        if (!isTRUE(align_items)) next
        span <- .deduce_item_span_from_links(item$id, timing, children)
        if (is.null(span)) next
        s_samples <- span[[1]]
        e_samples <- span[[2]]
      } else {
        t <- timing[[id_chr]]
        if (is.null(t)) next
        s_samples <- t[[1]]
        e_samples <- t[[2]]
      }

      start_ms <- s_samples / sample_rate * 1000
      end_ms   <- e_samples / sample_rate * 1000

      # Multi-label items (e.g. Word with Word/Accent/Text): one tier per
      # label name. Prefix the level name when the label name doesn't
      # already match it, so labels coming from auxiliary attributes
      # stay distinguishable in ELAN.
      for (lab in labels_v) {
        lab_name <- as.character(lab$name %||% lv_name)
        tier_name <- if (identical(lab_name, lv_name)) {
          lv_name
        } else {
          paste0(lv_name, ".", lab_name)
        }
        rows[[length(rows) + 1L]] <- list(
          start  = start_ms,
          end    = end_ms,
          labels = as.character(lab$value %||% ""),
          level  = tier_name
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(start = numeric(), end = numeric(),
                          labels = character(), level = character()))
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------
# Public-to-the-autosync-loop adapter.
#
# Reads one bundle's _annot.json, reshapes it, and writes the matching
# .eaf next to the audio. Honours overwrite = FALSE. media_url is the
# bundle's audio file (looked up via db_handle$mediafileExtension when
# available); written into the EAF media descriptor so the file opens
# cleanly in ELAN.
convert_emu_to_eaf <- function(db_handle, session, bundle,
                                align_items = TRUE,
                                overwrite = TRUE,
                                verbose = FALSE) {
  # Resolve paths. Tolerate both an emuDBhandle (list with $basePath)
  # and the reindeer S7 corpus shape (basePath property).
  base_path <- db_handle$basePath %||% NULL
  if (is.null(base_path) && S7::S7_inherits(db_handle, corpus)) {
    base_path <- db_handle@basePath
  }
  if (is.null(base_path) || !nzchar(base_path)) {
    cli::cli_abort("Cannot resolve basePath from {.arg db_handle}")
  }

  bundle_dir <- file.path(base_path,
                          paste0(session, "_ses"),
                          paste0(bundle,  "_bndl"))
  annot_path <- file.path(bundle_dir, paste0(bundle, "_annot.json"))
  eaf_path   <- file.path(bundle_dir, paste0(bundle, ".eaf"))

  if (!file.exists(annot_path)) {
    cli::cli_abort("Annotation file not found: {.path {annot_path}}")
  }
  if (!isTRUE(overwrite) && file.exists(eaf_path)) {
    if (verbose) {
      cli::cli_alert_info("Skipping {.path {eaf_path}} (exists, overwrite = FALSE)")
    }
    return(invisible(eaf_path))
  }

  annot_json <- jsonlite::read_json(annot_path, simplifyVector = FALSE)
  seg <- .annot_levels_to_segments(annot_json, align_items = align_items)

  # Locate the bundle's audio so ELAN can open the EAF directly. emuDBhandle
  # exposes mediafileExtension on the handle list; the corpus S7 object
  # carries it on the loaded DBconfig.
  media_ext <- db_handle$mediafileExtension %||% NULL
  if (is.null(media_ext) && !is.null(annot_json$annotates)) {
    media_ext <- tools::file_ext(annot_json$annotates)
  }
  media_url <- if (!is.null(media_ext) && nzchar(media_ext)) {
    paste0(bundle, ".", media_ext)
  } else {
    NULL
  }

  if (nrow(seg) == 0L) {
    if (verbose) {
      cli::cli_alert_warning("No timed annotations in {.path {annot_path}}; writing empty EAF.")
    }
  }

  write_eaf(seg, eaf_path, media_url = media_url, author = "reindeer")
  if (verbose) {
    cli::cli_alert_success("Wrote {.path {eaf_path}}")
  }
  invisible(eaf_path)
}
