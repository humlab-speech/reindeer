#' Generate CMDI metadata file for an EMU speech corpus
#'
#' Internal CMDI generator. End users should call
#' `describe_corpus(corp, formats = "cmdi")` instead, which handles
#' output paths, dirty-bit logic, and writes README + DataCite in the
#' same call. This function remains exported for the auto-regen
#' machinery and companion tooling.
#'
#' @keywords internal
#'
#' @param corpus A reindeer corpus object or emuDB handle
#' @param output_file Path for the output CMDI XML file. If NULL, writes to
#'   corpus_name_cmdi.xml in the database base directory.
#' @param profile Character. CMDI profile to use:
#'   - "media-corpus" (default): General media corpus profile
#'   - "speech-corpus": Speech corpus with participants
#'   - "speech-corpus-dlu": Speech corpus DLU variant
#' @param corpus_title Character. Title of the corpus (defaults to database name)
#' @param corpus_description Character. Description of the corpus
#' @param author Character. Author/creator of the corpus
#' @param institution Character. Institution responsible for the corpus
#' @param contact_email Character. Contact email for corpus inquiries
#' @param license Character. License under which corpus is distributed
#' @param availability Character. Availability status ("available", "restricted", etc.)
#' @param include_placeholders Logical. If TRUE, includes placeholder fields for
#'   planned metadata additions
#' @param verbose Logical. Print progress messages
#'
#' @return Path to created CMDI file (invisibly)
#'
#' @details
#' The function collects metadata from multiple sources:
#' 
#' **From Database:**
#' - Database name, UUID
#' - Level definitions (annotation tiers)
#' - Link definitions (tier hierarchy)
#' - SSFF track definitions
#' - Media file extension
#' 
#' **From .meta_json files:**
#' - Participant information (age, gender, language, dialect)
#' - Recording information (date, location, equipment)
#' - Session information (tasks, conditions)
#' 
#' **Planned additions (placeholders):**
#' - Funding information
#' - Project details
#' - Publications related to corpus
#' - Annotation procedures
#' - Quality control measures
#' - Ethical approval information
#' 
#' The generated CMDI file follows the CLARIN metadata standards and can be
#' ingested into CLARIN repositories.
#'
#' @keywords internal
#' @export
create_cmdi_metadata <- function(corpus,
                                  output_file = NULL,
                                  profile = "speech-corpus",
                                  corpus_title = NULL,
                                  corpus_description = NULL,
                                  author = NULL,
                                  institution = NULL,
                                  contact_email = NULL,
                                  license = NULL,
                                  availability = "available",
                                  self_link = NULL,
                                  include_placeholders = TRUE,
                                  verbose = TRUE) {
  
  # Check if corpus is emuDB handle or reindeer corpus object
  if (inherits(corpus, "emuDBhandle")) {
    db_handle <- corpus
  } else if (S7::S7_inherits(corpus, reindeer::corpus)) {
    db_handle <- get_handle(corpus)
  } else {
    cli::cli_abort("corpus must be an emuDBhandle or reindeer corpus object")
  }

  # Load database config
  db_config <- load_DBconfig(db_handle)
  db_name <- db_config$name
  db_uuid <- db_config$UUID
  db_path <- db_handle$basePath

  if (verbose) {
    cli::cli_alert_info("Creating CMDI metadata for database: {db_name}")
    cli::cli_alert_info("Database UUID: {db_uuid}")
  }
  
  # Set defaults
  if (is.null(corpus_title)) {
    corpus_title <- paste(db_name, "Speech Corpus")
  }
  if (is.null(output_file)) {
    output_file <- file.path(db_path, paste0(db_name, "_cmdi.xml"))
  }
  
  # Collect metadata from database structure
  db_metadata <- collect_database_metadata(db_handle, db_config, verbose)

  # Annotation tier names (for AnnotationTypes) and corpus size (for Size),
  # sourced from the loaded DB config and the bundle listing.
  db_metadata$annotation_levels <- {
    lv <- vapply(db_config$levelDefinitions %||% list(),
                 function(l) l$name %||% NA_character_, character(1))
    lv[!is.na(lv) & nzchar(lv)]
  }
  db_metadata$n_bundles <- tryCatch(nrow(.list_bundles(corpus)),
                                    error = function(e) NA_integer_)

  # Flat database-level fields set via add_metadata() (Project/Funder/...), so
  # the CMDI builder can populate Project/Location like the other artifacts.
  md0 <- tryCatch(get_metadata(corpus), error = function(e) NULL)
  if (!is.null(md0) && nrow(md0) > 0) {
    flat <- as.list(md0[1, ])
    pick <- function(x) if (.nz(x)) as.character(x) else NULL
    db_metadata$project      <- db_metadata$project      %||% pick(flat$Project)
    db_metadata$funder       <- db_metadata$funder       %||% pick(flat$Funder)
    db_metadata$website      <- db_metadata$website      %||% pick(flat$Website)
    db_metadata$country      <- db_metadata$country      %||% pick(flat$Country)
    db_metadata$country_code <- db_metadata$country_code %||% pick(flat$CountryCode)
    db_metadata$licence      <- db_metadata$licence      %||% pick(flat$Licence) %||% pick(flat$License)
    db_metadata$availability <- db_metadata$availability %||% pick(flat$Availability)
  }

  # Collect metadata from .meta_json files
  participant_metadata <- collect_participant_metadata(corpus, verbose)
  
  # Create CMDI XML based on profile
  if (verbose) cat("Generating CMDI XML...\n")
  
  cmdi_xml <- generate_cmdi_xml(
    profile = profile,
    db_name = db_name,
    db_uuid = db_uuid,
    corpus_title = corpus_title,
    corpus_description = corpus_description,
    author = author,
    institution = institution,
    contact_email = contact_email,
    license = license,
    availability = availability,
    db_metadata = db_metadata,
    participant_metadata = participant_metadata,
    self_link = self_link,
    include_placeholders = include_placeholders
  )
  
  # Write XML file
  xml2::write_xml(cmdi_xml, output_file, options = "format")
  
  if (verbose) {
    cat("CMDI file written to:", output_file, "\n")
  }
  
  invisible(output_file)
}


#' Collect metadata from database structure
#' @keywords internal
#' @noRd
collect_database_metadata <- function(db_handle, db_config, verbose = TRUE) {
  
  if (verbose) cat("Collecting database metadata...\n")
  
  metadata <- list()
  
  # Basic information
  metadata$name <- db_config$name
  metadata$uuid <- db_config$UUID
  metadata$media_extension <- db_config$mediafileExtension
  
  # Count sessions and bundles
  sessions <- .list_sessions(db_handle)
  metadata$n_sessions <- nrow(sessions)
  
  bundles <- .list_bundles(db_handle)
  metadata$n_bundles <- nrow(bundles)
  metadata$bundle_list <- bundles
  
  # Level definitions (annotation tiers)
  metadata$levels <- db_config$levelDefinitions
  metadata$n_levels <- length(db_config$levelDefinitions)
  
  # Link definitions (tier relationships)
  if (!is.null(db_config$linkDefinitions)) {
    metadata$links <- db_config$linkDefinitions
    metadata$n_links <- length(db_config$linkDefinitions)
  } else {
    metadata$links <- list()
    metadata$n_links <- 0
  }
  
  # SSFF tracks (signal processing)
  if (!is.null(db_config$ssffTrackDefinitions)) {
    metadata$ssff_tracks <- db_config$ssffTrackDefinitions
    metadata$n_ssff_tracks <- length(db_config$ssffTrackDefinitions)
  } else {
    metadata$ssff_tracks <- list()
    metadata$n_ssff_tracks <- 0
  }
  
  # Calculate total duration if possible
  total_duration_sec <- 0
  for (i in 1:nrow(bundles)) {
    bundle_dir <- file.path(db_handle$basePath, 
                           paste0(bundles$session[i], "_ses"),
                           paste0(bundles$name[i], "_bndl"))
    wav_file <- file.path(bundle_dir, paste0(bundles$name[i], ".", metadata$media_extension))
    
    if (file.exists(wav_file)) {
      tryCatch({
        info <- wrassp::read.AsspDataObj(wav_file, begin = 0, end = 1, attributes = TRUE)
        duration_sec <- attr(info, "endRecord") / attr(info, "sampleRate")
        total_duration_sec <- total_duration_sec + duration_sec
      }, error = function(e) {
        # Skip if can't read
      })
    }
  }
  metadata$total_duration_sec <- total_duration_sec
  metadata$total_duration_hms <- format_duration(total_duration_sec)
  
  if (verbose) {
    cat("  Sessions:", metadata$n_sessions, "\n")
    cat("  Bundles:", metadata$n_bundles, "\n")
    cat("  Annotation levels:", metadata$n_levels, "\n")
    cat("  Total duration:", metadata$total_duration_hms, "\n")
  }
  
  return(metadata)
}


#' Collect participant metadata for FAIR artifacts
#'
#' Sources from the canonical resolved metadata (flat `METADATA.json` fields via
#' [get_metadata()]) so metadata set with [add_metadata()] actually reaches the
#' generated CMDI/DataCite/JSON-LD. Falls back to legacy nested `participant`
#' objects in `.meta_json` files when no resolved metadata is available.
#'
#' @param corpus_or_handle A reindeer `corpus` (preferred) or emuDB handle.
#' @param verbose Logical; print progress.
#' @return Named list of participant records with lowercase field keys.
#' @keywords internal
#' @noRd
collect_participant_metadata <- function(corpus_or_handle, verbose = TRUE) {
  if (S7::S7_inherits(corpus_or_handle, corpus)) {
    md <- tryCatch(get_metadata(corpus_or_handle), error = function(e) NULL)
    if (!is.null(md) && nrow(md) > 0) {
      participants <- .participants_from_metadata(md)
      if (length(participants) > 0) {
        if (verbose) {
          cat("  Found metadata for", length(participants), "participant(s)\n")
        }
        return(participants)
      }
    }
  }
  handle <- if (S7::S7_inherits(corpus_or_handle, corpus)) {
    get_handle(corpus_or_handle)
  } else {
    corpus_or_handle
  }
  .collect_participant_metadata_legacy(handle, verbose)
}

#' TRUE for a length-1, non-NA, non-empty scalar
#' @keywords internal
#' @noRd
.nz <- function(v) length(v) == 1L && !is.na(v) && nzchar(as.character(v))

#' Build participant records from a resolved metadata tibble
#'
#' One record per distinct speaker (keyed by a Speaker/Participant field if
#' present, else by session). Field names are lowercased so downstream emitters
#' that read `age`/`gender`/`language` pick up `Age`/`Gender`/`Language`.
#' @keywords internal
#' @noRd
.participants_from_metadata <- function(md) {
  key <- c("session", "bundle", "db_uuid")
  id_candidates <- c("Speaker", "SpeakerID", "Participant", "ParticipantID",
                     "speaker", "participant")
  id_col <- intersect(id_candidates, names(md))
  id_col <- if (length(id_col)) id_col[[1]] else NA_character_
  fields <- setdiff(names(md), c(key, id_col))

  participants <- list()
  for (i in seq_len(nrow(md))) {
    row <- as.list(md[i, ])
    pid <- if (!is.na(id_col) && .nz(row[[id_col]])) {
      as.character(row[[id_col]])
    } else {
      as.character(row$session)
    }
    if (!is.null(participants[[pid]])) next
    rec <- list(id = pid)
    for (f in fields) {
      if (.nz(row[[f]])) rec[[tolower(f)]] <- as.character(row[[f]])
    }
    participants[[pid]] <- rec
  }
  participants
}

#' Legacy: collect participant metadata from nested `.meta_json` files
#' @keywords internal
#' @noRd
.collect_participant_metadata_legacy <- function(db_handle, verbose = TRUE) {

  if (verbose) cat("Collecting participant metadata...\n")
  
  participants <- list()
  sessions <- .list_sessions(db_handle)
  
  for (i in 1:nrow(sessions)) {
    session_name <- sessions$name[i]
    session_dir <- file.path(db_handle$basePath, paste0(session_name, "_ses"))
    
    # Check for session-level .meta_json
    session_meta_file <- file.path(session_dir, ".meta_json")
    if (file.exists(session_meta_file)) {
      meta <- jsonlite::fromJSON(session_meta_file, simplifyVector = FALSE)
      if (!is.null(meta$participant)) {
        participant_id <- meta$participant$id %||% session_name
        participants[[participant_id]] <- meta$participant
      }
    }
    
    # Check for bundle-level .meta_json
    bundles <- .list_bundles(db_handle, session = session_name)
    for (j in 1:nrow(bundles)) {
      bundle_name <- bundles$name[j]
      bundle_dir <- file.path(session_dir, paste0(bundle_name, "_bndl"))
      bundle_meta_file <- file.path(bundle_dir, ".meta_json")
      
      if (file.exists(bundle_meta_file)) {
        meta <- jsonlite::fromJSON(bundle_meta_file, simplifyVector = FALSE)
        if (!is.null(meta$participant)) {
          participant_id <- meta$participant$id %||% bundle_name
          participants[[participant_id]] <- meta$participant
        }
      }
    }
  }
  
  if (verbose && length(participants) > 0) {
    cat("  Found metadata for", length(participants), "participants\n")
    
    # Summary statistics
    ages <- sapply(participants, function(p) p$age %||% NA)
    genders <- sapply(participants, function(p) p$gender %||% NA)
    
    ages <- ages[!is.na(ages)]
    genders <- genders[!is.na(genders)]
    
    if (length(ages) > 0) {
      cat("  Age range:", min(ages), "-", max(ages), "years\n")
    }
    if (length(genders) > 0) {
      cat("  Gender distribution:", table(genders), "\n")
    }
  }
  
  return(participants)
}


#' Generate CMDI XML document
#' @keywords internal
#' @noRd
#' Aggregate participant demographics for the CMDI Participants component
#'
#' From the per-speaker records built by [collect_participant_metadata()]
#' (lowercased fields), compute the aggregate statistics the corpus-level
#' profile expects: speaker count, mean age, age range, and a sex breakdown.
#' @keywords internal
#' @noRd
.aggregate_demographics <- function(participants) {
  n <- length(participants)
  ages <- suppressWarnings(as.numeric(unlist(lapply(participants, function(p) p$age))))
  ages <- ages[!is.na(ages)]
  sexes <- tolower(unlist(lapply(participants, function(p) p$gender %||% p$sex)))
  sexes <- sexes[nzchar(sexes)]
  sex_tab <- if (length(sexes)) table(sexes) else integer(0)
  list(
    n_speakers  = n,
    mean_age    = if (length(ages)) round(mean(ages), 1) else NA_real_,
    age_range   = if (length(ages)) paste0(min(ages), "-", max(ages)) else NA_character_,
    sex_summary = if (length(sex_tab))
      paste(names(sex_tab), as.integer(sex_tab), sep = ": ", collapse = ", ")
      else NA_character_
  )
}

#' Map a reindeer annotation level name to a profile AnnotationType enum value
#'
#' The profile's `AnnotationType` is a closed vocabulary; map common EMU level
#' names onto it, defaulting to "Unspecified".
#' @keywords internal
#' @noRd
.map_annotation_type <- function(level_name) {
  ln <- tolower(level_name %||% "")
  if (grepl("phonetic", ln)) "Phonetics"
  else if (grepl("phonem|phonolog", ln)) "Phonology"
  else if (grepl("lemma", ln)) "Lemma"
  else if (grepl("\\bword\\b|token", ln)) "Word form"
  else if (grepl("morph", ln)) "Morphosyntax"
  else if (grepl("syll|tone|prosod|accent|foot|intonation|intermediate", ln)) "Prosody"
  else if (grepl("orth|utter|sentence|text|chunk", ln)) "Orthography"
  else "Unspecified"
}

#' Best-effort ISO 639-3 code for a language name (defaults to "und")
#' @keywords internal
#' @noRd
.iso639_3 <- function(language) {
  if (is.null(language) || !nzchar(language)) return("und")
  m <- c(english = "eng", swedish = "swe", norwegian = "nor", danish = "dan",
         finnish = "fin", german = "deu", french = "fra", spanish = "spa",
         italian = "ita", dutch = "nld", icelandic = "isl",
         "northern sami" = "sme", sami = "smi")
  key <- tolower(language)
  if (!is.null(m[[key]])) m[[key]]
  else if (grepl("^[a-z]{3}$", key)) key
  else "und"
}

#' Map a reindeer profile name to a CLARIN Component Registry profile ID
#' @keywords internal
#' @noRd
.cmdi_profile_id <- function(profile) {
  profile <- profile %||% "media-corpus"
  switch(profile,
    "media-corpus"      = "clarin.eu:cr1:p_1387365569699",
    "speech-corpus"     = "clarin.eu:cr1:p_1392642184799",
    "speech-corpus-dlu" = "clarin.eu:cr1:p_1381926654456",
    if (grepl("^clarin\\.eu:cr", profile)) profile
    else "clarin.eu:cr1:p_1387365569699"
  )
}

#' Profile payload namespace (the cmdp namespace) for a CMDI profile ID
#' @keywords internal
#' @noRd
.cmdi_profile_ns <- function(profile_id) {
  paste0("http://www.clarin.eu/cmd/1/profiles/", profile_id)
}

#' Component Registry XSD URL for a CMDI profile ID
#' @keywords internal
#' @noRd
.cmdi_profile_xsd_url <- function(profile_id) {
  paste0("https://catalog.clarin.eu/ds/ComponentRegistry/rest/registry/1.2/",
         "profiles/", profile_id, "/xsd")
}

#' Path to a bundled CMD envelope XSD, if present under inst/cmdi/
#' @keywords internal
#' @noRd
.cmdi_bundled_envelope_xsd <- function() {
  p <- system.file("cmdi", "cmd-envelop.xsd", package = "reindeer")
  if (nzchar(p)) p else NULL
}

#' Validate a generated CMDI record
#'
#' Two layers. **Structural** checks always run offline: a CMD 1.2 envelope
#' root, `CMDVersion = "1.2"`, the required `Header` fields, a
#' `ResourceProxyList`, and an `xsi:schemaLocation` that binds a profile
#' schema. **XSD** validation is attempted when a schema is available — a
#' local `xsd`, a bundled envelope schema under `inst/cmdi/`, or (with
#' `online = TRUE`) the CMD envelope schema fetched from CLARIN. The envelope
#' schema validates the envelope/Header/Resources; validating the profile
#' `Components` subtree needs the profile XSD (Component-level conformance is
#' delivered in a later phase). Full CLARIN compliance additionally requires
#' Schematron assertions applied by the external Java CMDI Instance Validator,
#' which is out of scope for this in-package check. See `CMDI_VALIDATION.md`
#' in the package sources for how to run full XSD + Schematron validation.
#'
#' @param path Path to a `*_cmdi.xml` file.
#' @param xsd Optional path to a local XSD to validate against.
#' @param online If `TRUE`, may fetch the CMD envelope schema from CLARIN when
#'   no local XSD is available. Default `FALSE` (offline-safe).
#' @return Invisibly, a list with `structural` (logical), `problems`
#'   (character), and `xsd` (logical, or `NA` when not attempted).
#' @seealso [create_cmdi_metadata()], [describe_corpus()].
#' @export
validate_cmdi <- function(path, xsd = NULL, online = FALSE) {
  if (!file.exists(path)) {
    cli::cli_abort("CMDI file not found: {.path {path}}")
  }
  cmd_ns <- c(cmd = "http://www.clarin.eu/cmd/1")
  doc <- xml2::read_xml(path)
  problems <- character(0)

  if (!grepl("CMD$", xml2::xml_name(doc))) {
    problems <- c(problems, "root element is not CMD")
  }
  if (!identical(xml2::xml_attr(doc, "CMDVersion"), "1.2")) {
    problems <- c(problems, "CMDVersion is not 1.2")
  }
  sl <- xml2::xml_attr(doc, "schemaLocation")
  if (is.na(sl) || !grepl("profiles/clarin\\.eu:cr", sl)) {
    problems <- c(problems, "xsi:schemaLocation does not bind a profile schema")
  }
  for (h in c("MdCreator", "MdCreationDate", "MdSelfLink", "MdProfile",
              "MdCollectionDisplayName")) {
    hit <- xml2::xml_find_all(doc, paste0("//cmd:Header/cmd:", h), ns = cmd_ns)
    if (length(hit) == 0) problems <- c(problems, paste0("Header missing ", h))
  }
  if (length(xml2::xml_find_all(doc, "//cmd:Resources/cmd:ResourceProxyList",
                                ns = cmd_ns)) == 0) {
    problems <- c(problems, "missing Resources/ResourceProxyList")
  }
  # CMDI requires the Components wrapper to hold exactly one profile root
  # component (in the profile payload namespace).
  comp_children <- xml2::xml_find_all(doc, "//cmd:Components/*", ns = cmd_ns)
  if (length(comp_children) != 1) {
    problems <- c(problems,
                  "Components must contain exactly one profile root component")
  }
  structural <- length(problems) == 0

  xsd_ok <- NA
  local_xsd <- xsd %||% .cmdi_bundled_envelope_xsd()
  if (!is.null(local_xsd) && file.exists(local_xsd)) {
    xsd_ok <- tryCatch(
      isTRUE(as.logical(xml2::xml_validate(doc, xml2::read_xml(local_xsd)))),
      error = function(e) NA)
  } else if (isTRUE(online)) {
    xsd_ok <- tryCatch(
      isTRUE(as.logical(xml2::xml_validate(
        doc,
        xml2::read_xml("https://infra.clarin.eu/CMDI/1.2/xsd/cmd-envelop.xsd")))),
      error = function(e) NA)
  }

  invisible(list(structural = structural, problems = problems, xsd = xsd_ok))
}

generate_cmdi_xml <- function(profile, db_name, db_uuid, corpus_title,
                              corpus_description, author, institution,
                              contact_email, license, availability,
                              db_metadata, participant_metadata,
                              self_link = NULL,
                              include_placeholders) {

  # Determine profile ID based on profile name
  profile_id <- .cmdi_profile_id(profile)
  profile_ns  <- .cmdi_profile_ns(profile_id)
  profile_xsd <- .cmdi_profile_xsd_url(profile_id)

  # Create root CMD element. CMDI 1.2 needs two namespaces — the envelope
  # (cmd:) and the profile payload (cmdp:) — and an xsi:schemaLocation that
  # binds BOTH, so a validator can resolve the profile schema for the
  # Components subtree.
  doc <- xml2::xml_new_root("cmd:CMD",
    "xmlns:cmd"  = "http://www.clarin.eu/cmd/1",
    "xmlns:cmdp" = profile_ns,
    "xmlns:xsi"  = "http://www.w3.org/2001/XMLSchema-instance",
    "CMDVersion" = "1.2",
    "xsi:schemaLocation" = paste(
      "http://www.clarin.eu/cmd/1",
      "https://infra.clarin.eu/CMDI/1.2/xsd/cmd-envelop.xsd",
      profile_ns, profile_xsd
    )
  )

  # Add Header
  header <- xml2::xml_add_child(doc, "cmd:Header")
  xml2::xml_add_child(header, "cmd:MdCreator", author %||% "PLACEHOLDER_AUTHOR")
  xml2::xml_add_child(header, "cmd:MdCreationDate",
                     format(Sys.time(), "%Y-%m-%d"))
  # MdSelfLink: a persistent identifier for this record. Prefer a caller-
  # supplied PID; fall back to a urn:uuid built from the database UUID so the
  # element is never empty (CLARIN B-centres require it).
  self <- self_link %||% (if (!is.null(db_uuid) && nzchar(db_uuid))
                          paste0("urn:uuid:", db_uuid) else NULL)
  if (!is.null(self)) {
    xml2::xml_add_child(header, "cmd:MdSelfLink", self)
  } else {
    xml2::xml_add_child(header, "cmd:MdSelfLink")
  }
  xml2::xml_add_child(header, "cmd:MdProfile", profile_id)
  xml2::xml_add_child(header, "cmd:MdCollectionDisplayName", corpus_title)
  
  # Add Resources (will contain ResourceProxy entries)
  resources <- xml2::xml_add_child(doc, "cmd:Resources")
  resource_proxy_list <- xml2::xml_add_child(resources, "cmd:ResourceProxyList")
  
  # Add resource proxies for each recording
  proxy_id <- 1
  if (!is.null(db_metadata$bundle_list)) {
    for (i in seq_len(nrow(db_metadata$bundle_list))) {  # every bundle, no truncation
      bundle_name <- db_metadata$bundle_list$name[i]
      resource_proxy <- xml2::xml_add_child(resource_proxy_list, "cmd:ResourceProxy",
                                           id = paste0("rp", proxy_id))
      xml2::xml_add_child(resource_proxy, "cmd:ResourceType", 
                         mimetype = paste0("audio/", db_metadata$media_extension), 
                         "Resource")
      xml2::xml_add_child(resource_proxy, "cmd:ResourceRef", 
                         paste0(bundle_name, ".", db_metadata$media_extension))
      proxy_id <- proxy_id + 1
    }
  }
  
  # Journal file references (not used in this context)
  xml2::xml_add_child(resources, "cmd:JournalFileProxyList")
  xml2::xml_add_child(resources, "cmd:ResourceRelationList")
  
  # Add Components (metadata content). Emit the profile-conformant payload in
  # the cmdp: namespace, generated to match the profile XSD. speech-corpus
  # (default) and media-corpus are fully conformant; anything else falls back to
  # the legacy structure until its profile is mapped.
  components <- xml2::xml_add_child(doc, "cmd:Components")

  if (identical(profile_id, "clarin.eu:cr1:p_1392642184799")) {
    .add_speech_corpus_participants_components(
      components, corpus_title, corpus_description, author, institution,
      contact_email, license, availability, db_metadata, participant_metadata)
  } else if (identical(profile_id, "clarin.eu:cr1:p_1387365569699")) {
    .add_media_corpus_components(components, corpus_title, corpus_description,
                                 author, institution, db_metadata,
                                 participant_metadata)
  } else {
    corpus_comp <- xml2::xml_add_child(components, "cmd:SpeechCorpus")
    add_speech_corpus_components(corpus_comp, db_name, db_uuid, corpus_title,
                                 corpus_description, author, institution,
                                 contact_email, license, availability,
                                 db_metadata, participant_metadata,
                                 include_placeholders)
  }

  return(doc)
}

#' Build the fully-populated Components subtree for speech-corpus-with-participants
#'
#' Emits `cmdp:SpeechCorpusWithParticipants` matching profile
#' `clarin.eu:cr1:p_1392642184799`, in schema order, mapping every reindeer
#' field with a matching component. Closed-vocabulary leaves use valid enum
#' values; per-bundle Age/Gender are aggregated into the Participants component;
#' annotation tiers map to AnnotationTypes; unmapped user fields are folded into
#' `GeneralInfo/Descriptions` so nothing is silently dropped.
#' @keywords internal
#' @noRd
.add_speech_corpus_participants_components <- function(components, corpus_title,
                                                       corpus_description, author,
                                                       institution, contact_email,
                                                       license, availability,
                                                       db_metadata,
                                                       participant_metadata) {
  A <- function(parent, name, text = NULL) {
    if (is.null(text)) xml2::xml_add_child(parent, paste0("cmdp:", name))
    else xml2::xml_add_child(parent, paste0("cmdp:", name), as.character(text))
  }
  title <- corpus_title %||% "Unnamed speech corpus"
  demo  <- .aggregate_demographics(participant_metadata)

  # Distinct subject languages from participants (else undetermined).
  langs <- unique(tolower(stats::na.omit(
    unlist(lapply(participant_metadata, function(p) p$language)))))
  langs <- langs[nzchar(langs)]
  if (length(langs) == 0) langs <- ""
  n_lang <- max(1L, length(langs))
  multiling <- if (n_lang == 1L) "Monolingual"
               else if (n_lang == 2L) "Bilingual" else "Multilingual"

  # Unmapped participant fields -> folded into Descriptions. Corpus-level fields
  # that map to their own components are excluded so they are not emitted twice.
  mapped <- c("id", "age", "gender", "sex", "language",
              "project", "funder", "website", "country", "countrycode",
              "licence", "license", "availability", "creator", "author",
              "contact", "institution", "grantnumber")
  extra <- list()
  for (p in participant_metadata) for (k in setdiff(names(p), mapped)) {
    if (.nz(p[[k]])) extra[[k]] <- unique(c(extra[[k]], as.character(p[[k]])))
  }

  root <- A(components, "SpeechCorpusWithParticipants")

  ## GeneralInfo (required) -------------------------------------------------
  gi <- A(root, "GeneralInfo")
  A(gi, "ResourceName", title)
  A(gi, "ResourceTitle", title)
  A(gi, "ResourceClass", "SpeechCorpus")                       # enum
  loc <- A(gi, "Location")
  ctry <- A(loc, "Country")
  A(ctry, "CountryName", db_metadata$country %||% "Unspecified")
  A(ctry, "CountryCoding", db_metadata$country_code %||% "SE") # ISO-3166 enum
  A(gi, "ModalityInfo")                                        # empty valid
  gi_desc <- A(gi, "Descriptions")
  A(gi_desc, "Description", corpus_description %||% "Speech corpus.")
  for (k in names(extra)) {
    A(gi_desc, "Description", paste0(k, ": ", paste(extra[[k]], collapse = "; ")))
  }

  ## Access (required) ------------------------------------------------------
  ac <- A(root, "Access")
  A(ac, "Availability", availability %||% db_metadata$availability %||% "Unspecified")
  A(ac, "DistributionMedium", "download")
  A(ac, "Licence", license %||% db_metadata$licence %||% "Unspecified")
  acc <- A(ac, "Contact")
  if (.nz(author)) A(acc, "Person", author)
  if (.nz(contact_email)) A(acc, "Email", contact_email)
  if (.nz(institution)) A(acc, "Organisation", institution)

  ## Creation (required) ----------------------------------------------------
  cr <- A(root, "Creation")
  crs <- A(cr, "Creators")
  crt <- A(crs, "Creator")
  crc <- A(crt, "Contact")
  if (.nz(author)) A(crc, "Person", author)
  if (.nz(institution)) A(crc, "Organisation", institution)

  ## Project (optional) -----------------------------------------------------
  proj_name <- db_metadata$project %||% NULL
  if (.nz(proj_name) || .nz(db_metadata$funder) || .nz(db_metadata$website)) {
    pj <- A(root, "Project")
    A(pj, "ProjectName", proj_name %||% "Unspecified")
    A(pj, "ProjectTitle", proj_name %||% "Unspecified")
    if (.nz(db_metadata$funder)) A(pj, "Funder", db_metadata$funder)
    A(pj, "Url", db_metadata$website %||% "http://example.org")
    A(pj, "Institution", institution %||% "Unspecified")
  }

  ## SubjectLanguages (required) -------------------------------------------
  sl <- A(root, "SubjectLanguages")
  A(sl, "NumberOfLanguages", n_lang)
  for (lg in langs) {
    sll <- A(sl, "SubjectLanguage")
    lang <- A(sll, "Language")
    A(lang, "LanguageName", if (nzchar(lg)) tools::toTitleCase(lg) else "Undetermined")
    iso <- A(lang, "ISO639")
    A(iso, "iso-639-3-code", .iso639_3(lg))                   # enum
  }

  ## SpeechCorpusSpecific (required) ---------------------------------------
  scs <- A(root, "SpeechCorpusSpecific")
  A(scs, "Modalities", "spoken")                              # enum
  A(scs, "MediaType", "audio/wav")
  sc <- A(scs, "SpeechCorpus")
  if (!is.na(demo$n_speakers) && demo$n_speakers > 0) {
    A(sc, "NumberOfSpeakers", demo$n_speakers)
  }
  ml <- A(scs, "Multilinguality"); A(ml, "Multilinguality", multiling)  # enum
  at_wrap <- A(scs, "AnnotationTypes")
  levels <- db_metadata$annotation_levels
  if (length(levels) == 0) levels <- "Unspecified"
  for (lv in unique(vapply(levels, .map_annotation_type, character(1)))) {
    at <- A(at_wrap, "AnnotationType"); A(at, "AnnotationType", lv)      # enum leaf
  }
  sz <- A(scs, "Size"); tsz <- A(sz, "TotalSize")
  A(tsz, "Number", db_metadata$n_bundles %||% length(participant_metadata) %||% 0)
  A(tsz, "SizeUnit", "bundles")

  ## Participants (required) -----------------------------------------------
  pt <- A(root, "Participants")
  if (!is.na(demo$sex_summary)) {
    sx <- A(pt, "SexDistribution"); sxi <- A(sx, "SexDistributionInfo")
    sxd <- A(sxi, "Descriptions"); A(sxd, "Description", demo$sex_summary)
  }
  if (!is.na(demo$mean_age) || !is.na(demo$age_range)) {
    ad <- A(pt, "AgeDistribution")
    if (!is.na(demo$mean_age))  A(ad, "ParticipantMeanAge", demo$mean_age)
    if (!is.na(demo$age_range)) A(ad, "ParticipantAgeRange", demo$age_range)
  }

  ## TechnicalInfo (required) ----------------------------------------------
  ti <- A(root, "TechnicalInfo")
  lscr <- A(ti, "LanguageScripts"); lsg <- A(lscr, "LanguageScriptGrp")
  A(lsg, "ScriptName", "Latin")
  A(lsg, "LanguageScript", if (nzchar(langs[[1]])) tools::toTitleCase(langs[[1]]) else "Undetermined")

  invisible(root)
}

#' Build the profile-conformant Components subtree for the media-corpus profile
#'
#' Emits `cmdp:media-corpus-profile` matching profile
#' `clarin.eu:cr1:p_1387365569699`: the required tree
#' (`Collection > GeneralInfo > Name`, `Collection > OriginLocation > Location`,
#' and `Corpus`) plus the safe optional fields we hold, respecting the
#' profile's element order and content models (verified against the profile
#' XSD). Elements go in the profile payload namespace (`cmdp:`), declared on
#' the CMD root.
#' @keywords internal
#' @noRd
.add_media_corpus_components <- function(components, corpus_title,
                                         corpus_description, author,
                                         institution, db_metadata,
                                         participant_metadata) {
  root <- xml2::xml_add_child(components, "cmdp:media-corpus-profile")

  # -- Collection (required). Sequence order: GeneralInfo, OriginLocation, ...
  coll <- xml2::xml_add_child(root, "cmdp:Collection")

  gi <- xml2::xml_add_child(coll, "cmdp:GeneralInfo")               # required
  # GeneralInfo sequence order: Name, Title, ID, Version, Owner, PublicationYear
  xml2::xml_add_child(gi, "cmdp:Name", corpus_title %||% "Unnamed corpus")  # required (text)
  if (.nz(corpus_title)) xml2::xml_add_child(gi, "cmdp:Title", corpus_title)
  owner <- author %||% institution
  if (.nz(owner)) xml2::xml_add_child(gi, "cmdp:Owner", owner)
  # PublicationYear is xs:gYear -> only a bare 4-digit year is valid.
  yr <- db_metadata$publication_year %||% format(Sys.Date(), "%Y")
  if (grepl("^[0-9]{4}$", as.character(yr))) {
    xml2::xml_add_child(gi, "cmdp:PublicationYear", as.character(yr))
  }

  ol <- xml2::xml_add_child(coll, "cmdp:OriginLocation")           # required
  xml2::xml_add_child(ol, "cmdp:Location")   # required; empty valid (all children optional)

  # -- Corpus (required; empty is valid) --
  xml2::xml_add_child(root, "cmdp:Corpus")

  # -- SpeechCorpus (optional): carry the speaker count when we have it --
  n_spk <- length(participant_metadata)
  if (n_spk > 0) {
    sc <- xml2::xml_add_child(root, "cmdp:SpeechCorpus")
    xml2::xml_add_child(sc, "cmdp:NumberOfSpeakers", as.character(n_spk))
  }

  invisible(root)
}


#' Add speech corpus components to CMDI
#' @keywords internal
#' @noRd
add_speech_corpus_components <- function(parent, db_name, db_uuid, corpus_title,
                                        corpus_description, author, institution,
                                        contact_email, license, availability,
                                        db_metadata, participant_metadata,
                                        include_placeholders) {
  
  # General Info
  general_info <- xml2::xml_add_child(parent, "cmd:GeneralInfo")
  xml2::xml_add_child(general_info, "cmd:Name", corpus_title)
  xml2::xml_add_child(general_info, "cmd:ID", db_uuid)
  xml2::xml_add_child(general_info, "cmd:Description", 
                     corpus_description %||% "PLACEHOLDER_CORPUS_DESCRIPTION")
  
  # Access
  access <- xml2::xml_add_child(parent, "cmd:Access")
  xml2::xml_add_child(access, "cmd:Availability", availability)
  if (!is.null(license)) {
    xml2::xml_add_child(access, "cmd:License", license)
  } else if (include_placeholders) {
    xml2::xml_add_child(access, "cmd:License", "PLACEHOLDER_LICENSE")
  }
  if (!is.null(contact_email)) {
    contact <- xml2::xml_add_child(access, "cmd:Contact")
    xml2::xml_add_child(contact, "cmd:Email", contact_email)
  } else if (include_placeholders) {
    contact <- xml2::xml_add_child(access, "cmd:Contact")
    xml2::xml_add_child(contact, "cmd:Email", "PLACEHOLDER_CONTACT_EMAIL")
  }
  
  # Creation
  creation <- xml2::xml_add_child(parent, "cmd:Creation")
  creators <- xml2::xml_add_child(creation, "cmd:Creators")
  if (!is.null(author)) {
    creator <- xml2::xml_add_child(creators, "cmd:Creator")
    xml2::xml_add_child(creator, "cmd:Name", author)
    if (!is.null(institution)) {
      xml2::xml_add_child(creator, "cmd:Organization", institution)
    }
  } else if (include_placeholders) {
    creator <- xml2::xml_add_child(creators, "cmd:Creator")
    xml2::xml_add_child(creator, "cmd:Name", "PLACEHOLDER_CREATOR_NAME")
    xml2::xml_add_child(creator, "cmd:Organization", "PLACEHOLDER_INSTITUTION")
  }
  xml2::xml_add_child(creation, "cmd:CreationDate", 
                     format(Sys.time(), "%Y-%m-%d"))
  
  # Project (placeholder for planned addition)
  if (include_placeholders) {
    project <- xml2::xml_add_child(parent, "cmd:Project")
    xml2::xml_add_child(project, "cmd:Name", "PLACEHOLDER_PROJECT_NAME")
    xml2::xml_add_child(project, "cmd:Description", "PLACEHOLDER_PROJECT_DESCRIPTION")
    xml2::xml_add_child(project, "cmd:Funder", "PLACEHOLDER_FUNDER")
    xml2::xml_add_child(project, "cmd:GrantNumber", "PLACEHOLDER_GRANT_NUMBER")
  }
  
  # Publications (placeholder)
  if (include_placeholders) {
    publications <- xml2::xml_add_child(parent, "cmd:Publications")
    publication <- xml2::xml_add_child(publications, "cmd:Publication")
    xml2::xml_add_child(publication, "cmd:Title", "PLACEHOLDER_PUBLICATION_TITLE")
    xml2::xml_add_child(publication, "cmd:Author", "PLACEHOLDER_PUBLICATION_AUTHOR")
    xml2::xml_add_child(publication, "cmd:Year", "PLACEHOLDER_YEAR")
  }
  
  # Subject Languages
  subject_languages <- xml2::xml_add_child(parent, "cmd:SubjectLanguages")
  
  # Extract unique languages from participant metadata
  languages <- unique(sapply(participant_metadata, function(p) p$language %||% NA))
  languages <- languages[!is.na(languages)]
  
  if (length(languages) > 0) {
    for (lang in languages) {
      lang_elem <- xml2::xml_add_child(subject_languages, "cmd:SubjectLanguage")
      xml2::xml_add_child(lang_elem, "cmd:Language", lang)
    }
  } else {
    lang_elem <- xml2::xml_add_child(subject_languages, "cmd:SubjectLanguage")
    xml2::xml_add_child(lang_elem, "cmd:Language", "PLACEHOLDER_LANGUAGE")
  }
  
  # Speech Corpus Specific
  speech_specific <- xml2::xml_add_child(parent, "cmd:SpeechCorpusSpecific")
  xml2::xml_add_child(speech_specific, "cmd:Modality", "SpokenLanguage")
  xml2::xml_add_child(speech_specific, "cmd:SpeechCorpusType", "AnnotatedSpeech")
  xml2::xml_add_child(speech_specific, "cmd:Size",
                     as.character(db_metadata$n_bundles))
  xml2::xml_add_child(speech_specific, "cmd:TotalDuration",
                     db_metadata$total_duration_hms)
  
  # Annotation Info
  annotation_info <- xml2::xml_add_child(speech_specific, "cmd:AnnotationInfo")
  for (level in db_metadata$levels) {
    annotation_level <- xml2::xml_add_child(annotation_info, "cmd:AnnotationLevel")
    xml2::xml_add_child(annotation_level, "cmd:Name", level$name)
    xml2::xml_add_child(annotation_level, "cmd:Type", level$type)
  }
  
  # Participants
  if (length(participant_metadata) > 0) {
    participants_comp <- xml2::xml_add_child(parent, "cmd:Participants")
    xml2::xml_add_child(participants_comp, "cmd:NumberOfParticipants",
                       as.character(length(participant_metadata)))
    
    # Participant details
    for (participant in participant_metadata) {
      part_elem <- xml2::xml_add_child(participants_comp, "cmd:Participant")
      xml2::xml_add_child(part_elem, "cmd:ParticipantID", 
                         participant$id %||% "unknown")
      
      if (!is.null(participant$age)) {
        xml2::xml_add_child(part_elem, "cmd:Age", as.character(participant$age))
      }
      if (!is.null(participant$gender)) {
        xml2::xml_add_child(part_elem, "cmd:Gender", participant$gender)
      }
      if (!is.null(participant$language)) {
        xml2::xml_add_child(part_elem, "cmd:MotherTongue", participant$language)
      }
    }
  }
  
  # Technical Info
  technical_info <- xml2::xml_add_child(parent, "cmd:TechnicalInfo")
  xml2::xml_add_child(technical_info, "cmd:MediaFormat", db_metadata$media_extension)
  
  # SSFF Tracks
  if (db_metadata$n_ssff_tracks > 0) {
    for (track in db_metadata$ssff_tracks) {
      track_elem <- xml2::xml_add_child(technical_info, "cmd:SignalProcessing")
      xml2::xml_add_child(track_elem, "cmd:TrackName", track$name)
      xml2::xml_add_child(track_elem, "cmd:FileExtension", track$fileExtension)
    }
  }
  
  # Placeholders for planned additions
  if (include_placeholders) {
    quality_control <- xml2::xml_add_child(parent, "cmd:QualityControl")
    xml2::xml_add_child(quality_control, "cmd:Method", "PLACEHOLDER_QC_METHOD")
    xml2::xml_add_child(quality_control, "cmd:Description", "PLACEHOLDER_QC_DESCRIPTION")
    
    ethics <- xml2::xml_add_child(parent, "cmd:EthicalApproval")
    xml2::xml_add_child(ethics, "cmd:ApprovalNumber", "PLACEHOLDER_ETHICS_NUMBER")
    xml2::xml_add_child(ethics, "cmd:Institution", "PLACEHOLDER_ETHICS_INSTITUTION")
  }
}


#' Format duration in HH:MM:SS
#' @keywords internal
#' @noRd
format_duration <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)
  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}
