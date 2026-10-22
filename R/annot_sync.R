# ==============================================================================
# Annotation parsing and incremental cache sync
#
# One vectorised parser for `_annot.json` (the file's bytes are read once; the
# md5 comes from the same bytes) and an incremental sync of the SQLite cache:
# on every open a bundle is re-read only if its file stamp (mtime + size)
# changed AND its md5 differs from the stored one; everything else is left as
# is. Removed bundles are deleted, new ones added, all in one transaction.
# ==============================================================================

#' Parse the text of an `_annot.json` into item/label/link tables
#'
#' @return List with `items`, `labels`, `links` (data.tables with the cache
#'   column layout, `db_uuid`/`session`/`bundle` filled in), `annotates` and
#'   `sample_rate`.
#' @noRd
parse_annot_json <- function(text, db_uuid = "", session_name = "", bundle_name = "") {
  # Plain list parse (fast) + per-level vector extraction: no per-item objects.
  a <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  sample_rate <- as.numeric(a[["sampleRate"]] %or_empty% NA_real_)

  int <- function(items, field) {
    vapply(items, function(x) { v <- x[[field]]; if (is.null(v)) NA_integer_ else as.integer(v) }, 1L)
  }
  # Plain lists per level, one rbindlist at the end: building a data.table per
  # level and per bundle dominated the parse (700 bundles: ~1 s in setalloccol).
  items <- list()
  labels <- list()
  for (lv in a[["levels"]]) {
    it <- lv[["items"]]
    n <- length(it)
    if (n == 0L) next
    ids <- int(it, "id")
    items[[length(items) + 1L]] <- list(
      db_uuid = rep(db_uuid, n), session = rep(session_name, n), bundle = rep(bundle_name, n),
      item_id = ids, level = rep(lv[["name"]], n), type = rep(lv[["type"]], n),
      seq_idx = seq_len(n), sample_rate = rep(sample_rate, n),
      sample_point = int(it, "samplePoint"), sample_start = int(it, "sampleStart"),
      sample_dur = int(it, "sampleDur"))
    lab <- lapply(it, `[[`, "labels")
    lens <- lengths(lab)
    if (sum(lens) > 0L) {
      flat <- unlist(lab, recursive = FALSE, use.names = FALSE)
      m <- sum(lens)
      labels[[length(labels) + 1L]] <- list(
        db_uuid = rep(db_uuid, m), session = rep(session_name, m), bundle = rep(bundle_name, m),
        item_id = rep(ids, lens), label_idx = sequence(lens),
        name = vapply(flat, function(l) as.character(l[["name"]]), ""),
        label = vapply(flat, function(l) { v <- l[["value"]]; if (is.null(v)) "" else as.character(v) }, ""))
    }
  }
  items <- if (length(items)) data.table::rbindlist(items) else data.table::data.table(
    db_uuid = character(), session = character(), bundle = character(), item_id = integer(),
    level = character(), type = character(), seq_idx = integer(), sample_rate = numeric(),
    sample_point = integer(), sample_start = integer(), sample_dur = integer())
  labels <- if (length(labels)) data.table::rbindlist(labels) else data.table::data.table(
    db_uuid = character(), session = character(), bundle = character(),
    item_id = integer(), label_idx = integer(), name = character(), label = character())

  lk <- a[["links"]]
  links <- if (length(lk) > 0L) {
    data.table::data.table(
      db_uuid = db_uuid, session = session_name, bundle = bundle_name,
      from_id = vapply(lk, function(l) as.integer(l[["fromID"]]), 1L),
      to_id = vapply(lk, function(l) as.integer(l[["toID"]]), 1L),
      label = vapply(lk, function(l) { v <- l[["label"]]; if (is.null(v)) NA_character_ else as.character(v) }, ""))
  } else {
    data.table::data.table(db_uuid = character(), session = character(), bundle = character(),
                           from_id = integer(), to_id = integer(), label = character())
  }
  list(items = items, labels = labels, links = links,
       annotates = as.character(a[["annotates"]] %or_empty% ""), sample_rate = sample_rate)
}

#' Read an `_annot.json` as text
#' @noRd
.read_annot_text <- function(path) {
  rawToChar(readBin(path, "raw", file.size(path)))
}

# Stamp table: lets an unchanged bundle be skipped without even reading it.
.ensure_stamp_table <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS annot_stamp (
    db_uuid TEXT, session TEXT, bundle TEXT, mtime REAL, size REAL,
    PRIMARY KEY (db_uuid, session, bundle))")
}

#' Bring the SQLite cache in line with the `_annot.json` files on disk
#'
#' Creates the cache if missing (or if it belongs to another database),
#' otherwise updates only what changed. Safe to call on every open.
#' @param database_dir The `_emuDB` directory.
#' @param verbose Print progress.
#' @return Invisibly, a list with counts `added`, `updated`, `removed`, `unchanged`.
#' @noRd
sync_emuDB_cache <- function(database_dir, verbose = FALSE) {
  if (!dir.exists(database_dir)) {
    cli::cli_abort("Database directory {.path {database_dir}} does not exist")
  }
  db_name <- sub("_emuDB$", "", basename(database_dir))
  config_path <- file.path(database_dir, paste0(db_name, database.schema.suffix))
  cache_path <- file.path(database_dir, paste0(db_name, database.cache.suffix))
  if (!file.exists(config_path)) {
    cli::cli_abort("Database config file not found: {.path {config_path}}")
  }
  db_config <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
  uuid <- db_config$UUID

  fresh <- !file.exists(cache_path)
  if (!fresh) {
    # Close before unlink(): Windows cannot delete a file that is still open.
    c0 <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
    probe <- tryCatch(DBI::dbGetQuery(c0, "SELECT uuid FROM emu_db")$uuid,
                      error = function(e) character(),
                      finally = DBI::dbDisconnect(c0))
    if (!identical(probe, uuid)) {   # corrupt, or a cache of another database
      unlink(cache_path)
      fresh <- TRUE
    }
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  if (fresh) {
    # Bulk load first, index afterwards, and skip the per-write fsync: the cache
    # is rebuildable, so durability is not worth the time on a first open.
    initialize_database_schema(con, uuid, db_name, indices = FALSE)
    DBI::dbExecute(con, "PRAGMA synchronous = OFF")
    DBI::dbExecute(con, "PRAGMA journal_mode = MEMORY")
  }
  initialize_metadata_schema(con)
  .ensure_stamp_table(con)

  disk <- data.table::as.data.table(discover_sessions_bundles(database_dir))
  disk[, path := file.path(database_dir, paste0(session, "_ses"), paste0(bundle, "_bndl"),
                           paste0(bundle, "_annot.json"))]
  disk <- disk[file.exists(path)]
  info <- file.info(disk$path)
  disk[, `:=`(mtime = as.numeric(info$mtime), size = as.numeric(info$size))]

  stored <- data.table::as.data.table(DBI::dbGetQuery(con, "
    SELECT b.session, b.name AS bundle, b.md5_annot_json AS md5, s.mtime AS s_mtime, s.size AS s_size
    FROM bundle b LEFT JOIN annot_stamp s
      ON s.db_uuid = b.db_uuid AND s.session = b.session AND s.bundle = b.name
    WHERE b.db_uuid = ?", params = list(uuid)))
  m <- stored[disk, on = .(session, bundle)]
  is_new <- is.na(m$md5)
  stamp_same <- !is_new & !is.na(m$s_mtime) & abs(m$mtime - m$s_mtime) < 1e-3 & m$size == m$s_size
  removed <- stored[!disk, on = .(session, bundle)]

  to_read <- which(!stamp_same)
  parsed <- list()
  restamp <- integer()
  # md5 of all candidates in one vectorised call (digest::digest on raw
  # vectors costs ~5 ms per file).
  md5 <- unname(tools::md5sum(disk$path[to_read]))
  names(md5) <- as.character(to_read)
  for (i in to_read) {
    if (!is_new[i] && identical(md5[[as.character(i)]], m$md5[i])) {   # touched, not edited
      restamp <- c(restamp, i)
      next
    }
    parsed[[as.character(i)]] <- c(
      parse_annot_json(.read_annot_text(disk$path[i]), uuid, disk$session[i], disk$bundle[i]),
      list(md5 = md5[[as.character(i)]], i = i))
  }

  counts <- list(added = sum(is_new[to_read] & as.character(to_read) %in% names(parsed)),
                 updated = sum(!is_new[to_read] & as.character(to_read) %in% names(parsed)),
                 removed = nrow(removed), unchanged = sum(stamp_same) + length(restamp))
  if (counts$added + counts$updated + counts$removed + length(restamp) == 0L) {
    return(invisible(counts))
  }

  DBI::dbWithTransaction(con, {
    for (r in seq_len(nrow(removed))) {
      DBI::dbExecute(con, "DELETE FROM bundle WHERE db_uuid = ? AND session = ? AND name = ?",
                     params = list(uuid, removed$session[r], removed$bundle[r]))
      DBI::dbExecute(con, "DELETE FROM annot_stamp WHERE db_uuid = ? AND session = ? AND bundle = ?",
                     params = list(uuid, removed$session[r], removed$bundle[r]))
    }
    ii <- vapply(parsed, `[[`, integer(1), "i")
    # Replaced bundles: ON DELETE CASCADE clears their old items, labels, links.
    for (i in ii[!is_new[ii]]) {
      DBI::dbExecute(con, "DELETE FROM bundle WHERE db_uuid = ? AND session = ? AND name = ?",
                     params = list(uuid, disk$session[i], disk$bundle[i]))
    }
    if (length(ii) > 0L) {
      DBI::dbExecute(con, "INSERT OR IGNORE INTO session (db_uuid, name) VALUES (?, ?)",
                     params = list(rep(uuid, length(unique(disk$session[ii]))), unique(disk$session[ii])))
      DBI::dbAppendTable(con, "bundle", data.frame(
        db_uuid = uuid, session = disk$session[ii], name = disk$bundle[ii],
        annotates = vapply(parsed, `[[`, "", "annotates"),
        sample_rate = vapply(parsed, `[[`, 1, "sample_rate"),
        md5_annot_json = vapply(parsed, `[[`, "", "md5"), stringsAsFactors = FALSE))
    }
    bind <- function(tbl, field) {
      parts <- lapply(parsed, `[[`, field)
      if (length(parts)) {
        DBI::dbAppendTable(con, tbl, as.data.frame(data.table::rbindlist(parts)))
      }
    }
    bind("items", "items"); bind("labels", "labels"); bind("links", "links")
    stamp_rows <- unique(c(restamp, ii))
    if (length(stamp_rows) > 0L) {
      DBI::dbExecute(con, "INSERT OR REPLACE INTO annot_stamp (db_uuid, session, bundle, mtime, size)
                           VALUES (?, ?, ?, ?, ?)",
                     params = list(rep(uuid, length(stamp_rows)), disk$session[stamp_rows],
                                   disk$bundle[stamp_rows], disk$mtime[stamp_rows],
                                   disk$size[stamp_rows]))
    }
  })
  if (fresh) {
    .create_cache_indices(con)
    DBI::dbExecute(con, "PRAGMA synchronous = FULL")
    DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
  }
  if (verbose) {
    cli::cli_alert_success("Cache synced: {counts$added} added, {counts$updated} updated, {counts$removed} removed, {counts$unchanged} unchanged")
  }
  invisible(counts)
}
