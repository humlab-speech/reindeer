# ==============================================================================
# Praat TextGrid round-trip
# ==============================================================================
#
# Pure-R parser/writer for Praat TextGrid files. Supports both "long" and
# "short" text formats (UTF-8 and UTF-16 LE/BE with BOM detection). The
# format is line-oriented plain text, so we don't need a dedicated YAML/XML
# library here.
#
# A TextGrid is a list of tiers, where each tier is either an IntervalTier
# (segments with start/end/label) or a TextTier (events with time/label).
# We round-trip both shapes through a flat tibble with columns:
#   tier, type, start, end, label
# (TextTier rows have start == end.)

# Internal: read a TextGrid file as a single UTF-8 character vector, one
# element per line. Detect UTF-16 LE/BE BOMs and convert.
.read_textgrid_lines <- function(path) {
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  enc <- if (length(raw) >= 2 && raw[1] == as.raw(0xFF) &&
              raw[2] == as.raw(0xFE)) {
    "UTF-16LE"
  } else if (length(raw) >= 2 && raw[1] == as.raw(0xFE) &&
                raw[2] == as.raw(0xFF)) {
    "UTF-16BE"
  } else if (length(raw) >= 3 && raw[1] == as.raw(0xEF) &&
                raw[2] == as.raw(0xBB) && raw[3] == as.raw(0xBF)) {
    "UTF-8"
  } else {
    "UTF-8"
  }
  text <- iconv(list(raw), from = enc, to = "UTF-8")[[1]]
  if (is.na(text)) {
    # iconv may fail when the BOM was already stripped via encoding override.
    # Fall back to a plain readLines pass.
    return(readLines(path, encoding = "UTF-8", warn = FALSE))
  }
  strsplit(text, "\r?\n", fixed = FALSE)[[1]]
}

# Internal: pull a quoted string value out of a TextGrid line like:
#   text = "consonant"
# Returns the raw string content (with embedded quote escaping reversed).
.tg_value_str <- function(line) {
  m <- regmatches(line, regexec("=\\s*\"((?:[^\"]|\"\")*)\"", line))[[1]]
  if (length(m) < 2) return(NA_character_)
  gsub('""', '"', m[2], fixed = TRUE)
}

# Internal: pull a numeric value out of a TextGrid line like:
#   xmin = 0.0
.tg_value_num <- function(line) {
  m <- regmatches(line, regexec("=\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)",
                                  line))[[1]]
  if (length(m) < 2) return(NA_real_)
  suppressWarnings(as.numeric(m[2]))
}

# Internal: parse a long-format TextGrid given as a vector of lines.
# Returns a tibble of (tier, type, start, end, label).
.parse_textgrid_long <- function(lines) {
  rows <- list()
  cur_tier <- NA_character_
  cur_type <- NA_character_
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (grepl("class\\s*=\\s*\"IntervalTier\"", ln)) cur_type <- "IntervalTier"
    else if (grepl("class\\s*=\\s*\"TextTier\"", ln)) cur_type <- "TextTier"
    if (grepl("name\\s*=\\s*\"", ln) && !grepl("number\\s*=", ln)) {
      cur_tier <- .tg_value_str(ln)
    }
    if (grepl("^\\s*intervals\\s*\\[[0-9]+\\]\\s*:", ln) ||
        grepl("^\\s*points\\s*\\[[0-9]+\\]\\s*:", ln)) {
      is_point <- grepl("^\\s*points", ln)
      x1 <- .tg_value_num(lines[i + 1])
      x2 <- if (is_point) x1 else .tg_value_num(lines[i + 2])
      tx <- if (is_point) .tg_value_str(lines[i + 2]) else .tg_value_str(lines[i + 3])
      rows[[length(rows) + 1L]] <- list(
        tier = cur_tier,
        type = if (is_point) "TextTier" else "IntervalTier",
        start = x1,
        end = x2,
        label = tx
      )
    }
  }
  tibble::as_tibble(do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE)))
}

# Internal: parse the short format. The short format strips field names; each
# tier block lists xmin/xmax/n_intervals followed by tuples of xmin/xmax/text
# (or number/text for point tiers). This implementation handles both.
.parse_textgrid_short <- function(lines) {
  # Strip surrounding whitespace; keep blank lines as separators.
  lines <- trimws(lines)
  rows <- list()
  i <- 1L
  n <- length(lines)
  # Skip header
  while (i <= n && !grepl('^"(IntervalTier|TextTier)"$', lines[i])) i <- i + 1L
  while (i <= n) {
    if (!grepl('^"(IntervalTier|TextTier)"$', lines[i])) {
      i <- i + 1L; next
    }
    type <- gsub('"', '', lines[i], fixed = TRUE)
    tier_name <- gsub('"', '', lines[i + 1L], fixed = TRUE)
    # tier xmin, xmax, count
    n_items <- suppressWarnings(as.integer(lines[i + 4L]))
    j <- i + 5L
    for (k in seq_len(n_items %||% 0L)) {
      if (type == "IntervalTier") {
        x1 <- suppressWarnings(as.numeric(lines[j]))
        x2 <- suppressWarnings(as.numeric(lines[j + 1L]))
        tx <- gsub('^"|"$', '', lines[j + 2L])
        rows[[length(rows) + 1L]] <- list(
          tier = tier_name, type = type,
          start = x1, end = x2, label = tx
        )
        j <- j + 3L
      } else {
        x1 <- suppressWarnings(as.numeric(lines[j]))
        tx <- gsub('^"|"$', '', lines[j + 1L])
        rows[[length(rows) + 1L]] <- list(
          tier = tier_name, type = type,
          start = x1, end = x1, label = tx
        )
        j <- j + 2L
      }
    }
    i <- j
  }
  tibble::as_tibble(do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE)))
}

#' Read a Praat TextGrid into a tibble
#'
#' Pure-R parser for Praat TextGrid files (both long and short text
#' format, UTF-8 and UTF-16 with BOM). Returns a tibble with one row per
#' interval / point, suitable for feeding into a `segment_list` after
#' column renaming.
#'
#' @param path Path to a `.TextGrid` file.
#' @param encoding Optional encoding hint. The BOM is auto-detected; this
#'   argument is reserved for files without a BOM.
#' @return A tibble with columns `tier`, `type` (`"IntervalTier"` or
#'   `"TextTier"`), `start`, `end`, `label`.
#' @examplesIf interactive()
#' tg <- read_textgrid("speech.TextGrid")
#' head(tg)
#' @export
read_textgrid <- function(path, encoding = "UTF-8") {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  lines <- .read_textgrid_lines(path)
  # Short format starts with File type = "ooTextFile" and lacks "xmin =" etc.
  long_form <- any(grepl("^\\s*xmin\\s*=", lines))
  if (long_form) {
    .parse_textgrid_long(lines)
  } else {
    .parse_textgrid_short(lines)
  }
}

# Internal: escape embedded quotes in a label per Praat convention.
.tg_escape <- function(s) {
  if (is.na(s)) return("")
  gsub('"', '""', s, fixed = TRUE)
}

# Internal: render one IntervalTier or TextTier block in long format.
.tg_render_tier_long <- function(tier_name, type, rows, xmin, xmax,
                                  tier_idx) {
  out <- character()
  add <- function(...) out <<- c(out, paste0(...))
  add("    item [", tier_idx, "]:")
  add('        class = "', type, '"')
  add('        name = "', .tg_escape(tier_name), '"')
  add("        xmin = ", xmin)
  add("        xmax = ", xmax)
  if (type == "IntervalTier") {
    add("        intervals: size = ", nrow(rows))
    for (k in seq_len(nrow(rows))) {
      add("        intervals [", k, "]:")
      add("            xmin = ", rows$start[k])
      add("            xmax = ", rows$end[k])
      add('            text = "', .tg_escape(rows$label[k]), '"')
    }
  } else {
    add("        points: size = ", nrow(rows))
    for (k in seq_len(nrow(rows))) {
      add("        points [", k, "]:")
      add("            number = ", rows$start[k])
      add('            mark = "', .tg_escape(rows$label[k]), '"')
    }
  }
  out
}

#' Write a segment_list (or compatible tibble) as a Praat TextGrid
#'
#' Writes a `.TextGrid` file (long format by default; short with
#' `short = TRUE`). Each requested level becomes a separate
#' `IntervalTier` (or `TextTier` when the rows' `type` column is `EVENT`).
#'
#' Times are written in seconds. If the input columns are in milliseconds
#' (as `segment_list` start/end are), they are divided by 1000.
#'
#' @param seg A `segment_list` or compatible tibble. Must contain
#'   `start`, `end`, `labels`, `level`; optionally `type`.
#' @param path Output file path.
#' @param levels Optional character vector of levels to include. By
#'   default, all levels present in `seg`.
#' @param short Logical. Write the compact short-text format instead of
#'   the more readable long format.
#' @param encoding Either `"UTF-8"` (default) or `"UTF-16LE"`.
#' @return The file path, invisibly.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == V") |> collect()
#' write_textgrid(segs, "phonetic.TextGrid")
#' @export
write_textgrid <- function(seg, path, levels = NULL,
                            short = FALSE, encoding = "UTF-8") {
  if (!is.data.frame(seg)) {
    cli::cli_abort("{.arg seg} must be a data.frame")
  }
  required <- c("start", "end", "labels", "level")
  missing <- setdiff(required, names(seg))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg seg} missing column{?s}: {.val {missing}}")
  }

  df <- tibble::as_tibble(seg)
  if (is.null(levels)) levels <- unique(df$level)
  df <- df[df$level %in% levels, , drop = FALSE]
  if (nrow(df) == 0L) {
    cli::cli_abort("No rows match {.arg levels}: {.val {levels}}")
  }

  # ms -> seconds. segment_list times are milliseconds by convention.
  start_s <- df$start / 1000
  end_s   <- df$end   / 1000
  type_v <- if ("type" %in% names(df)) df$type else rep("SEGMENT", nrow(df))
  df$.start_s <- start_s
  df$.end_s   <- end_s
  df$.type    <- type_v
  df$.label   <- as.character(df$labels)

  xmin_all <- min(c(df$.start_s, 0), na.rm = TRUE)
  xmax_all <- max(df$.end_s, na.rm = TRUE)

  out_lines <- character()
  add <- function(...) out_lines <<- c(out_lines, paste0(...))
  if (!short) {
    add('File type = "ooTextFile"')
    add('Object class = "TextGrid"')
    add("")
    add("xmin = ", xmin_all)
    add("xmax = ", xmax_all)
    add("tiers? <exists>")
    add("size = ", length(levels))
    add("item []:")
    for (idx in seq_along(levels)) {
      lvl <- levels[idx]
      rows <- df[df$level == lvl, , drop = FALSE]
      tier_type <- if (all(rows$.type == "EVENT")) "TextTier" else "IntervalTier"
      tier_rows <- tibble::tibble(
        start = rows$.start_s, end = rows$.end_s, label = rows$.label
      )
      out_lines <- c(
        out_lines,
        .tg_render_tier_long(lvl, tier_type, tier_rows, xmin_all, xmax_all, idx)
      )
    }
  } else {
    add('File type = "ooTextFile"')
    add('Object class = "TextGrid"')
    add("")
    add(xmin_all)
    add(xmax_all)
    add("<exists>")
    add(length(levels))
    for (lvl in levels) {
      rows <- df[df$level == lvl, , drop = FALSE]
      tier_type <- if (all(rows$.type == "EVENT")) "TextTier" else "IntervalTier"
      add('"', tier_type, '"')
      add('"', .tg_escape(lvl), '"')
      add(xmin_all); add(xmax_all); add(nrow(rows))
      for (k in seq_len(nrow(rows))) {
        if (tier_type == "IntervalTier") {
          add(rows$.start_s[k]); add(rows$.end_s[k])
          add('"', .tg_escape(rows$.label[k]), '"')
        } else {
          add(rows$.start_s[k])
          add('"', .tg_escape(rows$.label[k]), '"')
        }
      }
    }
  }

  con <- file(path, open = "wb", encoding = encoding)
  on.exit(close(con))
  if (encoding == "UTF-16LE") {
    writeBin(as.raw(c(0xFF, 0xFE)), con)
    txt <- paste0(paste(out_lines, collapse = "\n"), "\n")
    raw <- iconv(txt, from = "UTF-8", to = "UTF-16LE", toRaw = TRUE)[[1]]
    writeBin(raw, con)
  } else {
    writeLines(out_lines, con, useBytes = TRUE)
  }
  invisible(path)
}
