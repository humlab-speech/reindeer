# ==============================================================================
# describe() — emit standards-compliant corpus documentation (Item 5)
# ==============================================================================
#
# A single entry point that produces README.md, CMDI XML, and DataCite 4.5
# JSON for a corpus. All three formats consume one shared summary built by
# collect_corpus_summary(); CMDI delegates to the existing
# create_cmdi_metadata() implementation in R/reindeer_cmdi.R.

#' Build a shared, format-agnostic snapshot of a corpus
#'
#' Returns a list with database, participant, level/track, and duration
#' summaries. Used as the input to README, CMDI, and DataCite emitters.
#'
#' @param corpus_obj A reindeer `corpus` object.
#' @param verbose Logical; passed through to underlying collectors.
#' @return A list with named summary fields.
#' @keywords internal
#' @noRd
collect_corpus_summary <- function(corpus_obj, verbose = FALSE) {
  if (!S7::S7_inherits(corpus_obj, corpus)) {
    cli::cli_abort("Input must be a {.cls corpus} object")
  }
  db_handle <- get_handle(corpus_obj)
  db_config <- load_DBconfig(db_handle)

  db <- collect_database_metadata(db_handle, db_config, verbose = verbose)
  participants <- collect_participant_metadata(corpus_obj, verbose = verbose)

  # Project / funding / team metadata, if present at database level
  proj <- list(name = NULL, description = NULL, funder = NULL,
               grantNumber = NULL, team = list())
  meta_file <- file.path(corpus_obj@basePath, "METADATA.json")
  if (file.exists(meta_file)) {
    db_meta <- tryCatch(
      jsonlite::read_json(meta_file, simplifyVector = TRUE),
      error = function(e) list()
    )
    if (!is.null(db_meta$project)) {
      proj$name <- db_meta$project$name %||% NULL
      proj$description <- db_meta$project$description %||% NULL
      proj$startDate <- db_meta$project$startDate %||% NULL
      proj$website <- db_meta$project$website %||% NULL
    }
    if (!is.null(db_meta$funding)) {
      proj$funder <- db_meta$funding$funder %||% NULL
      proj$grantNumber <- db_meta$funding$grantNumber %||% NULL
    }
    if (!is.null(db_meta$team)) proj$team <- db_meta$team
  }

  # Also honour flat database-level fields set via add_metadata() — the metadata
  # API writes flat keys (Project, Funder, ...), not the nested project/funding
  # objects read above, so without this user-set metadata never reaches the
  # generated artifacts.
  md <- tryCatch(get_metadata(corpus_obj), error = function(e) NULL)
  if (!is.null(md) && nrow(md) > 0) {
    flat <- as.list(md[1, ])  # db-level defaults are inherited into every row
    if (is.null(proj$name)        && .nz(flat$Project))     proj$name        <- as.character(flat$Project)
    if (is.null(proj$description) && .nz(flat$Description)) proj$description <- as.character(flat$Description)
    if (is.null(proj$funder)      && .nz(flat$Funder))     proj$funder      <- as.character(flat$Funder)
    if (is.null(proj$grantNumber) && .nz(flat$GrantNumber)) proj$grantNumber <- as.character(flat$GrantNumber)
    if (is.null(proj$website)     && .nz(flat$Website))    proj$website     <- as.character(flat$Website)
  }

  list(
    name = db$name,
    uuid = db$uuid,
    base_path = corpus_obj@basePath,
    media_extension = db$media_extension %||% "wav",
    n_sessions = db$n_sessions,
    n_bundles = db$n_bundles,
    n_levels = db$n_levels,
    n_links = db$n_links,
    n_ssff_tracks = db$n_ssff_tracks,
    levels = db$levels,
    links = db$links,
    ssff_tracks = db$ssff_tracks,
    bundle_list = db$bundle_list,
    total_duration_sec = db$total_duration_sec,
    total_duration_hms = db$total_duration_hms,
    participants = participants,
    project = proj
  )
}

#' @keywords internal
.emit_readme <- function(summary, path) {
  lines <- character()
  add <- function(...) lines <<- c(lines, paste0(...))
  add("# ", summary$name)
  add("")
  if (!is.null(summary$project$description) &&
      nzchar(summary$project$description)) {
    add(summary$project$description)
    add("")
  } else {
    add("Speech corpus packaged with the reindeer R package.")
    add("")
  }
  add("## Contents")
  add("")
  add("- Sessions: ", summary$n_sessions)
  add("- Bundles: ", summary$n_bundles)
  add("- Annotation levels: ", summary$n_levels)
  add("- Hierarchy links: ", summary$n_links)
  add("- SSFF signal tracks: ", summary$n_ssff_tracks)
  if (!is.null(summary$total_duration_hms) &&
      nzchar(summary$total_duration_hms)) {
    add("- Total media duration: ", summary$total_duration_hms)
  }
  add("- Media file extension: `", summary$media_extension, "`")
  add("")
  if (length(summary$levels) > 0) {
    add("## Annotation levels")
    add("")
    for (lvl in summary$levels) {
      add("- **", lvl$name, "** (`", lvl$type, "`)")
    }
    add("")
  }
  if (length(summary$ssff_tracks) > 0) {
    add("## Signal tracks")
    add("")
    for (tr in summary$ssff_tracks) {
      add("- ", tr$name, ": `", tr$fileExtension %||% "?", "`")
    }
    add("")
  }
  if (NROW(summary$participants) > 0) {
    n_part <- if (is.data.frame(summary$participants)) {
      nrow(summary$participants)
    } else {
      length(summary$participants)
    }
    add("## Participants")
    add("")
    add("- Total participants with metadata: ", n_part)
    add("")
  }
  if (!is.null(summary$project$funder) &&
      nzchar(summary$project$funder)) {
    add("## Funding")
    add("")
    add("- Funder: ", summary$project$funder)
    if (!is.null(summary$project$grantNumber)) {
      add("- Grant number: ", summary$project$grantNumber)
    }
    add("")
  }
  add("## Citation")
  add("")
  add("Please cite the corpus using its UUID `", summary$uuid, "`.")
  add("")
  add("---")
  add("")
  add("_Generated by reindeer::describe()._")

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' @keywords internal
.emit_datacite <- function(summary, path) {
  publication_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(summary$project$startDate)) {
    yr <- suppressWarnings(as.integer(substr(summary$project$startDate, 1, 4)))
    if (!is.na(yr)) publication_year <- yr
  }

  creators <- list()
  team <- summary$project$team
  if (is.data.frame(team) && nrow(team) > 0) {
    for (i in seq_len(nrow(team))) {
      creators[[length(creators) + 1L]] <- list(
        name = team$name[i],
        affiliation = if (!is.null(team$affiliation)) {
          list(list(name = team$affiliation[i]))
        } else {
          list()
        }
      )
    }
  } else if (is.list(team) && length(team) > 0) {
    for (member in team) {
      creators[[length(creators) + 1L]] <- list(
        name = member$name %||% "Unknown",
        affiliation = if (!is.null(member$affiliation)) {
          list(list(name = member$affiliation))
        } else {
          list()
        }
      )
    }
  } else {
    creators[[1L]] <- list(name = "Unknown")
  }

  doc <- list(
    data = list(
      type = "dois",
      attributes = list(
        identifiers = list(list(
          identifier = summary$uuid,
          identifierType = "UUID"
        )),
        creators = creators,
        titles = list(list(title = summary$name)),
        publisher = summary$project$funder %||% "Unspecified",
        publicationYear = publication_year,
        types = list(
          resourceTypeGeneral = "Audiovisual",
          resourceType = "Speech corpus"
        ),
        descriptions = list(list(
          description = summary$project$description %||%
            "Speech corpus packaged with reindeer.",
          descriptionType = "Abstract"
        )),
        formats = list(paste0("audio/", summary$media_extension)),
        sizes = list(
          paste0(summary$n_bundles, " bundles"),
          paste0(round(summary$total_duration_sec %||% 0, 1), " s")
        )
      )
    )
  )
  jsonlite::write_json(
    doc, path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  invisible(path)
}

#' @keywords internal
.emit_citation_cff <- function(summary, path) {
  # Build authors list — CFF requires at least one author.
  team <- summary$project$team
  authors <- list()
  if (is.data.frame(team) && nrow(team) > 0) {
    for (i in seq_len(nrow(team))) {
      nm <- team$name[i]
      entry <- if (grepl("\\s", nm)) {
        parts <- strsplit(nm, "\\s+")[[1]]
        list(
          `family-names` = utils::tail(parts, 1L),
          `given-names` = paste(parts[-length(parts)], collapse = " ")
        )
      } else {
        list(name = nm)
      }
      if (!is.null(team$affiliation) && nzchar(team$affiliation[i])) {
        entry$affiliation <- team$affiliation[i]
      }
      authors[[length(authors) + 1L]] <- entry
    }
  } else if (is.list(team) && length(team) > 0) {
    for (member in team) {
      nm <- member$name %||% "Unknown"
      entry <- if (grepl("\\s", nm)) {
        parts <- strsplit(nm, "\\s+")[[1]]
        list(
          `family-names` = utils::tail(parts, 1L),
          `given-names` = paste(parts[-length(parts)], collapse = " ")
        )
      } else {
        list(name = nm)
      }
      if (!is.null(member$affiliation) && nzchar(member$affiliation)) {
        entry$affiliation <- member$affiliation
      }
      authors[[length(authors) + 1L]] <- entry
    }
  }
  if (length(authors) == 0L) {
    cli::cli_alert_warning(
      "No corpus author in METADATA.json; writing CITATION.cff with a placeholder."
    )
    authors[[1L]] <- list(name = "Unknown")
  }

  cff <- list(
    `cff-version` = "1.2.0",
    message = paste0(
      "If you use this corpus, please cite it using the metadata in this file."
    ),
    title = summary$name,
    type = "dataset",
    authors = authors,
    identifiers = list(list(type = "other", value = summary$uuid,
                            description = "Corpus UUID")),
    abstract = summary$project$description %||%
      "Speech corpus packaged with the reindeer R package.",
    keywords = c("speech corpus", "linguistic data"),
    `date-released` = format(Sys.Date(), "%Y-%m-%d")
  )
  if (!is.null(summary$project$website) && nzchar(summary$project$website)) {
    cff$url <- summary$project$website
  }

  # Write a YAML 1.2 document by hand so we don't add a yaml dep.
  lines <- character()
  add <- function(...) lines <<- c(lines, paste0(...))
  esc <- function(x) {
    if (is.null(x) || is.na(x)) return("")
    s <- gsub('"', '\\"', as.character(x), fixed = TRUE)
    paste0('"', s, '"')
  }
  add('cff-version: "', cff$`cff-version`, '"')
  add("message: ", esc(cff$message))
  add("title: ", esc(cff$title))
  add("type: ", cff$type)
  add("date-released: ", esc(cff$`date-released`))
  if (!is.null(cff$url)) add("url: ", esc(cff$url))
  add("abstract: ", esc(cff$abstract))
  add("authors:")
  for (a in cff$authors) {
    if (!is.null(a$`family-names`)) {
      add("  - family-names: ", esc(a$`family-names`))
      add("    given-names: ", esc(a$`given-names`))
    } else {
      add("  - name: ", esc(a$name))
    }
    if (!is.null(a$affiliation)) {
      add("    affiliation: ", esc(a$affiliation))
    }
  }
  add("identifiers:")
  for (id in cff$identifiers) {
    add("  - type: ", id$type)
    add("    value: ", esc(id$value))
    add("    description: ", esc(id$description))
  }
  add("keywords:")
  for (kw in cff$keywords) add("  - ", esc(kw))

  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' @keywords internal
.emit_jsonld <- function(summary, path) {
  publication_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(summary$project$startDate)) {
    yr <- suppressWarnings(as.integer(substr(summary$project$startDate, 1, 4)))
    if (!is.na(yr)) publication_year <- yr
  }

  creators <- list()
  team <- summary$project$team
  if (is.data.frame(team) && nrow(team) > 0) {
    for (i in seq_len(nrow(team))) {
      creators[[length(creators) + 1L]] <- list(
        `@type` = "Person",
        name = team$name[i],
        affiliation = if (!is.null(team$affiliation) &&
                            nzchar(team$affiliation[i])) {
          list(`@type` = "Organization", name = team$affiliation[i])
        } else NULL
      )
    }
  } else if (is.list(team) && length(team) > 0) {
    for (member in team) {
      creators[[length(creators) + 1L]] <- list(
        `@type` = "Person",
        name = member$name %||% "Unknown",
        affiliation = if (!is.null(member$affiliation) &&
                            nzchar(member$affiliation)) {
          list(`@type` = "Organization", name = member$affiliation)
        } else NULL
      )
    }
  }
  if (length(creators) == 0L) {
    creators <- list(list(`@type` = "Person", name = "Unknown"))
  }

  doc <- list(
    `@context` = "https://schema.org",
    `@type` = "Dataset",
    name = summary$name,
    identifier = summary$uuid,
    description = summary$project$description %||%
      "Speech corpus packaged with the reindeer R package.",
    creator = creators,
    datePublished = paste0(publication_year),
    encodingFormat = paste0("audio/", summary$media_extension),
    keywords = c("speech corpus", "linguistic data"),
    inLanguage = "und",
    distribution = list(list(
      `@type` = "DataDownload",
      encodingFormat = "application/x-emudb",
      contentSize = paste0(summary$n_bundles, " bundles")
    ))
  )
  if (!is.null(summary$project$funder) && nzchar(summary$project$funder)) {
    doc$funder <- list(`@type` = "Organization",
                       name = summary$project$funder)
  }
  if (!is.null(summary$total_duration_sec)) {
    # ISO 8601 duration approximation: PT<sec>S
    doc$duration <- paste0("PT",
                           round(summary$total_duration_sec, 1), "S")
  }

  jsonlite::write_json(
    doc, path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  invisible(path)
}

#' Emit standards-compliant corpus documentation
#'
#' Produces README, CMDI XML, DataCite 4.5 JSON, CITATION.cff, and
#' schema.org JSON-LD for a corpus in one call. All formats are derived
#' from a single shared summary built by `collect_corpus_summary()`.
#' CMDI delegates to `create_cmdi_metadata()`.
#'
#' Existing files at the target paths are never overwritten unless
#' `force = TRUE`.
#'
#' @param corpus_obj A reindeer `corpus` object.
#' @param output_dir Directory to write outputs into. Defaults to the
#'   corpus base path.
#' @param formats Character vector of formats to emit. Any subset of
#'   `c("readme", "cmdi", "datacite", "cff", "jsonld")`.
#' @param profile CMDI profile (`"speech-corpus"`, `"media-corpus"`,
#'   `"speech-corpus-dlu"`).
#' @param force Logical; overwrite existing files.
#' @param verbose Logical; print progress.
#' @return A named character vector of paths written, invisibly.
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' describe_corpus(corp)
#' describe_corpus(corp, formats = c("cff", "jsonld"), force = TRUE)
#'
#' @export
describe_corpus <- function(corpus_obj,
                     output_dir = NULL,
                     formats = c("readme", "cmdi", "datacite", "cff", "jsonld"),
                     profile = "speech-corpus",
                     force = FALSE,
                     verbose = TRUE) {
  if (!S7::S7_inherits(corpus_obj, corpus)) {
    cli::cli_abort("Input must be a {.cls corpus} object")
  }
  valid <- c("readme", "cmdi", "datacite", "cff", "jsonld")
  formats <- match.arg(formats, choices = valid, several.ok = TRUE)
  if (is.null(output_dir)) output_dir <- corpus_obj@basePath
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # The drift guard and state file only govern emission into the corpus's own
  # directory (the automatic default). Explicit exports to another directory
  # always emit.
  is_default_dir <- normalizePath(output_dir, mustWork = FALSE) ==
    normalizePath(corpus_obj@basePath, mustWork = FALSE)
  state_file <- file.path(output_dir, ".cmdi_state")

  # Auto-regenerate: if add_metadata() / create_session_and_bundle() flipped
  # the dirty bit, force a rewrite of the FAIR artifacts to keep them in
  # sync with the underlying METADATA.json files.
  dirty <- .is_metadata_dirty(corpus_obj)
  if (dirty) {
    if (verbose) {
      cli::cli_alert_info(
        "Metadata changed since last {.fn describe_corpus} - regenerating artifacts."
      )
    }
    force <- TRUE
  }

  # Drift guard: skip regeneration when the metadata state is unchanged since
  # the last emission into this corpus dir (keeps auto-regeneration cheap and
  # idempotent). force = TRUE and custom output dirs bypass it.
  state_hash <- .metadata_state_hash(corpus_obj)
  if (is_default_dir && !force && !dirty) {
    prev_hash <- if (file.exists(state_file)) readLines(state_file, warn = FALSE)[1] else ""
    if (identical(state_hash, prev_hash)) {
      if (verbose) cli::cli_alert_info("FAIR artifacts up-to-date; nothing to regenerate.")
      return(invisible(character()))
    }
  }

  summary <- collect_corpus_summary(corpus_obj, verbose = verbose)

  written <- character()
  if ("readme" %in% formats) {
    target <- file.path(output_dir, "README.md")
    if (file.exists(target) && !force) {
      target <- file.path(output_dir, "README-generated.md")
    }
    .emit_readme(summary, target)
    written <- c(written, readme = target)
    if (verbose) cli::cli_alert_success("Wrote {.path {target}}")
  }
  if ("cmdi" %in% formats) {
    target <- file.path(output_dir, paste0(summary$name, "_cmdi.xml"))
    if (file.exists(target) && !force) {
      target <- file.path(output_dir, paste0(summary$name, "_cmdi-generated.xml"))
    }
    create_cmdi_metadata(corpus_obj, output_file = target,
                         profile = profile, verbose = verbose)
    written <- c(written, cmdi = target)
  }
  if ("datacite" %in% formats) {
    target <- file.path(output_dir, "datacite.json")
    if (file.exists(target) && !force) {
      target <- file.path(output_dir, "datacite-generated.json")
    }
    .emit_datacite(summary, target)
    written <- c(written, datacite = target)
    if (verbose) cli::cli_alert_success("Wrote {.path {target}}")
  }
  if ("cff" %in% formats) {
    target <- file.path(output_dir, "CITATION.cff")
    if (file.exists(target) && !force) {
      target <- file.path(output_dir, "CITATION-generated.cff")
    }
    .emit_citation_cff(summary, target)
    written <- c(written, cff = target)
    if (verbose) cli::cli_alert_success("Wrote {.path {target}}")
  }
  if ("jsonld" %in% formats) {
    target <- file.path(output_dir, "_corpus_jsonld.json")
    if (file.exists(target) && !force) {
      target <- file.path(output_dir, "_corpus_jsonld-generated.json")
    }
    .emit_jsonld(summary, target)
    written <- c(written, jsonld = target)
    if (verbose) cli::cli_alert_success("Wrote {.path {target}}")
  }

  # Record the emitted state so the drift guard can skip unchanged reruns.
  if (is_default_dir) {
    tryCatch(writeLines(state_hash, state_file), error = function(e) NULL)
  }

  # Artifacts are back in sync — clear the dirty bit.
  .clear_metadata_dirty(corpus_obj)

  invisible(written)
}
