# ==============================================================================
# ELAN .eaf round-trip
# ==============================================================================
#
# ELAN's Annotation Format (.eaf) is an XML file with a TIME_ORDER section
# enumerating time slots (ms granularity) and TIER sections referencing those
# slots by ID. We write a minimal but valid EAF 2.8 doc that ELAN accepts.
#
# Hierarchy is encoded via TIER@PARENT_REF + LINGUISTIC_TYPE_REF; we surface
# it from DBconfig$linkDefinitions when available, otherwise emit flat tiers
# with a single default linguistic type.

# Internal: ISO 8601 timestamp for the EAF DATE attribute.
.eaf_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

# Internal: read all annotation rows from an .eaf into a flat tibble.
.parse_eaf_xml <- function(doc) {
  ns <- xml2::xml_ns(doc)
  # TIME_ORDER -> named vector slot_id -> ms
  slots <- xml2::xml_find_all(doc, "//TIME_SLOT")
  slot_ids <- xml2::xml_attr(slots, "TIME_SLOT_ID")
  slot_ms <- suppressWarnings(as.numeric(xml2::xml_attr(slots, "TIME_VALUE")))
  ms_lookup <- stats::setNames(slot_ms, slot_ids)

  # Each TIER may hold ALIGNABLE_ANNOTATION (time-anchored) or
  # REF_ANNOTATION (parent-anchored). For now we only round-trip the
  # alignable ones; ref annotations are flagged via type="REF".
  tiers <- xml2::xml_find_all(doc, "//TIER")
  rows <- list()
  for (t in tiers) {
    tier_id <- xml2::xml_attr(t, "TIER_ID")
    parent  <- xml2::xml_attr(t, "PARENT_REF")
    ling    <- xml2::xml_attr(t, "LINGUISTIC_TYPE_REF")
    aligned <- xml2::xml_find_all(t, ".//ALIGNABLE_ANNOTATION")
    for (a in aligned) {
      ref1 <- xml2::xml_attr(a, "TIME_SLOT_REF1")
      ref2 <- xml2::xml_attr(a, "TIME_SLOT_REF2")
      txt  <- xml2::xml_text(xml2::xml_find_first(a, ".//ANNOTATION_VALUE"))
      rows[[length(rows) + 1L]] <- list(
        tier = tier_id,
        parent = parent %||% NA_character_,
        linguistic_type = ling %||% NA_character_,
        start = ms_lookup[[ref1]],
        end = ms_lookup[[ref2]],
        label = txt
      )
    }
  }
  tibble::as_tibble(do.call(rbind.data.frame,
                            c(rows, stringsAsFactors = FALSE)))
}

#' Read an ELAN .eaf file into a tibble
#'
#' Returns a flat tibble with one row per `ALIGNABLE_ANNOTATION` plus the
#' tier metadata (tier name, parent, linguistic type). REF annotations
#' (anchored to a parent's interval, not to absolute time) are skipped
#' for now.
#'
#' @param path Path to a `.eaf` file.
#' @return A tibble with columns `tier`, `parent`, `linguistic_type`,
#'   `start`, `end`, `label`.
#' @examplesIf interactive()
#' eaf <- read_eaf("elan/example.eaf")
#' @export
read_eaf <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  doc <- xml2::read_xml(path)
  .parse_eaf_xml(doc)
}

# Internal: write a minimal EAF 2.8 document from a long-form tibble.
.build_eaf_doc <- function(seg, levels, media_url, author) {
  df <- tibble::as_tibble(seg)
  if (is.null(levels)) levels <- unique(df$level)
  df <- df[df$level %in% levels, , drop = FALSE]

  # Times in EAF are integer milliseconds. segment_list start/end are
  # already ms by convention; cast and round defensively.
  df$.start_ms <- as.integer(round(df$start))
  df$.end_ms   <- as.integer(round(df$end))

  # Build deterministic TIME_SLOT IDs ts1..tsN over the sorted unique time
  # points so each slot maps to exactly one (ms) value.
  all_times <- sort(unique(c(df$.start_ms, df$.end_ms)))
  slot_ids <- paste0("ts", seq_along(all_times))
  time_index <- stats::setNames(slot_ids, as.character(all_times))

  doc <- xml2::xml_new_root(
    "ANNOTATION_DOCUMENT",
    AUTHOR = author,
    DATE = .eaf_now(),
    FORMAT = "2.8",
    VERSION = "2.8",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:noNamespaceSchemaLocation" = "http://www.mpi.nl/tools/elan/EAFv2.8.xsd"
  )

  header <- xml2::xml_add_child(doc, "HEADER",
                                  MEDIA_FILE = "",
                                  TIME_UNITS = "milliseconds")
  if (!is.null(media_url) && nzchar(media_url)) {
    xml2::xml_add_child(header, "MEDIA_DESCRIPTOR",
                        MEDIA_URL = media_url,
                        MIME_TYPE = "audio/x-wav")
  }

  time_order <- xml2::xml_add_child(doc, "TIME_ORDER")
  for (i in seq_along(all_times)) {
    xml2::xml_add_child(time_order, "TIME_SLOT",
                        TIME_SLOT_ID = slot_ids[i],
                        TIME_VALUE = as.character(all_times[i]))
  }

  ann_id <- 0L
  for (lvl in levels) {
    rows <- df[df$level == lvl, , drop = FALSE]
    tier <- xml2::xml_add_child(
      doc, "TIER",
      LINGUISTIC_TYPE_REF = "default-lt",
      TIER_ID = lvl
    )
    for (k in seq_len(nrow(rows))) {
      ann_id <- ann_id + 1L
      ann <- xml2::xml_add_child(tier, "ANNOTATION")
      align <- xml2::xml_add_child(
        ann, "ALIGNABLE_ANNOTATION",
        ANNOTATION_ID = paste0("a", ann_id),
        TIME_SLOT_REF1 = time_index[[as.character(rows$.start_ms[k])]],
        TIME_SLOT_REF2 = time_index[[as.character(rows$.end_ms[k])]]
      )
      xml2::xml_add_child(align, "ANNOTATION_VALUE", as.character(rows$labels[k]))
    }
  }

  xml2::xml_add_child(
    doc, "LINGUISTIC_TYPE",
    LINGUISTIC_TYPE_ID = "default-lt",
    TIME_ALIGNABLE = "true",
    GRAPHIC_REFERENCES = "false"
  )
  xml2::xml_add_child(doc, "LOCALE", LANGUAGE_CODE = "und")

  doc
}

#' Write a segment_list as an ELAN .eaf file
#'
#' Writes a minimal but valid EAF 2.8 document derived from a
#' `segment_list` (or compatible tibble). Each requested level becomes a
#' `<TIER>`; all annotations are `ALIGNABLE_ANNOTATION` referring to
#' deterministically allocated `<TIME_SLOT>` IDs.
#'
#' The function does not currently emit `<PARENT_REF>` linkage; that
#' requires resolving `DBconfig$linkDefinitions`. A warning is issued when
#' link definitions are present in the corpus but tiers are written flat;
#' use the `levels` argument to restrict output to a known sub-hierarchy.
#'
#' @param seg A `segment_list` or compatible tibble.
#' @param path Output file path.
#' @param levels Optional character vector of levels to include. Default:
#'   all levels present in `seg`.
#' @param media_url Optional URL or relative path written into the EAF
#'   media descriptor.
#' @param author Author string written into the EAF root element.
#' @return The file path, invisibly.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == V") |> collect()
#' write_eaf(segs, "phonetic.eaf", media_url = "audio/clip.wav")
#' @export
write_eaf <- function(seg, path, levels = NULL,
                       media_url = NULL, author = "reindeer") {
  if (!is.data.frame(seg)) {
    cli::cli_abort("{.arg seg} must be a data.frame")
  }
  required <- c("start", "end", "labels", "level")
  missing <- setdiff(required, names(seg))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg seg} missing column{?s}: {.val {missing}}")
  }
  doc <- .build_eaf_doc(seg, levels = levels,
                        media_url = media_url, author = author)
  xml2::write_xml(doc, path, options = c("format", "as_xml"))
  invisible(path)
}
