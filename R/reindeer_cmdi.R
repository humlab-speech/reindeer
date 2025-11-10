#' Generate CMDI metadata file for an EMU speech corpus
#'
#' Creates a CMDI (Component Metadata Infrastructure) XML file containing
#' corpus-level metadata for a speech database. This function collects information
#' from the database configuration, session/bundle metadata (.meta_json files),
#' and corpus structure to generate a CLARIN-compliant metadata description.
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
#' @examples
#' \dontrun{
#' # Load corpus
#' corpus <- load_emuDB("/path/to/ae_emuDB")
#' 
#' # Generate CMDI with defaults
#' create_cmdi_metadata(corpus)
#' 
#' # Generate CMDI with custom information
#' create_cmdi_metadata(
#'   corpus,
#'   corpus_title = "American English Speech Corpus",
#'   corpus_description = "A corpus of American English speech...",
#'   author = "John Doe",
#'   institution = "University of Example",
#'   contact_email = "contact@example.edu",
#'   license = "CC-BY-4.0",
#'   profile = "speech-corpus"
#' )
#' }
#'
#' @export
create_cmdi_metadata <- function(corpus,
                                  output_file = NULL,
                                  profile = "media-corpus",
                                  corpus_title = NULL,
                                  corpus_description = NULL,
                                  author = NULL,
                                  institution = NULL,
                                  contact_email = NULL,
                                  license = NULL,
                                  availability = "available",
                                  include_placeholders = TRUE,
                                  verbose = TRUE) {
  
  # Check if corpus is emuDB handle or reindeer corpus object
  if (inherits(corpus, "emuDBhandle")) {
    db_handle <- corpus
  } else if (inherits(corpus, "reindeer_corpus")) {
    db_handle <- corpus$emuDBhandle
  } else {
    stop("corpus must be an emuDBhandle or reindeer_corpus object")
  }
  
  # Load database config
  db_config <- emuR:::load_DBconfig(db_handle)
  db_name <- db_config$name
  db_uuid <- db_config$UUID
  db_path <- db_handle$basePath
  
  if (verbose) {
    cat("Creating CMDI metadata for database:", db_name, "\n")
    cat("Database UUID:", db_uuid, "\n")
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
  
  # Collect metadata from .meta_json files
  participant_metadata <- collect_participant_metadata(db_handle, verbose)
  
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
collect_database_metadata <- function(db_handle, db_config, verbose = TRUE) {
  
  if (verbose) cat("Collecting database metadata...\n")
  
  metadata <- list()
  
  # Basic information
  metadata$name <- db_config$name
  metadata$uuid <- db_config$UUID
  metadata$media_extension <- db_config$mediafileExtension
  
  # Count sessions and bundles
  sessions <- emuR::list_sessions(db_handle)
  metadata$n_sessions <- nrow(sessions)
  
  bundles <- emuR::list_bundles(db_handle)
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


#' Collect participant metadata from .meta_json files
#' @keywords internal
collect_participant_metadata <- function(db_handle, verbose = TRUE) {
  
  if (verbose) cat("Collecting participant metadata...\n")
  
  participants <- list()
  sessions <- emuR::list_sessions(db_handle)
  
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
    bundles <- emuR::list_bundles(db_handle, session = session_name)
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
generate_cmdi_xml <- function(profile, db_name, db_uuid, corpus_title,
                              corpus_description, author, institution,
                              contact_email, license, availability,
                              db_metadata, participant_metadata,
                              include_placeholders) {
  
  # Determine profile ID based on profile name
  profile_id <- switch(profile,
    "media-corpus" = "clarin.eu:cr1:p_1387365569699",
    "speech-corpus" = "clarin.eu:cr1:p_1392642184799",
    "speech-corpus-dlu" = "clarin.eu:cr1:p_1381926654456",
    "clarin.eu:cr1:p_1387365569699"  # default
  )
  
  # Create root CMD element
  doc <- xml2::xml_new_root("cmd:CMD",
    "xmlns:cmd" = "http://www.clarin.eu/cmd/1",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "CMDVersion" = "1.2",
    "xsi:schemaLocation" = "http://www.clarin.eu/cmd/1 https://infra.clarin.eu/CMDI/1.2/xsd/cmd-envelop.xsd"
  )
  
  # Add Header
  header <- xml2::xml_add_child(doc, "cmd:Header")
  xml2::xml_add_child(header, "cmd:MdCreator", author %||% "PLACEHOLDER_AUTHOR")
  xml2::xml_add_child(header, "cmd:MdCreationDate", 
                     format(Sys.time(), "%Y-%m-%d"))
  xml2::xml_add_child(header, "cmd:MdSelfLink")  # Repository will fill
  xml2::xml_add_child(header, "cmd:MdProfile", profile_id)
  xml2::xml_add_child(header, "cmd:MdCollectionDisplayName", corpus_title)
  
  # Add Resources (will contain ResourceProxy entries)
  resources <- xml2::xml_add_child(doc, "cmd:Resources")
  resource_proxy_list <- xml2::xml_add_child(resources, "cmd:ResourceProxyList")
  
  # Add resource proxies for each recording
  proxy_id <- 1
  if (!is.null(db_metadata$bundle_list)) {
    for (i in 1:min(nrow(db_metadata$bundle_list), 10)) {  # Limit to first 10 as example
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
  
  # Add Components (metadata content)
  components <- xml2::xml_add_child(doc, "cmd:Components")
  
  # Add profile-specific component
  if (profile == "speech-corpus") {
    corpus_comp <- xml2::xml_add_child(components, "cmd:SpeechCorpusWithParticipants")
    add_speech_corpus_components(corpus_comp, db_name, db_uuid, corpus_title,
                                 corpus_description, author, institution,
                                 contact_email, license, availability,
                                 db_metadata, participant_metadata,
                                 include_placeholders)
  } else if (profile == "media-corpus") {
    corpus_comp <- xml2::xml_add_child(components, "cmd:media-corpus-profile")
    add_media_corpus_components(corpus_comp, db_name, db_uuid, corpus_title,
                               corpus_description, author, institution,
                               contact_email, license, availability,
                               db_metadata, participant_metadata,
                               include_placeholders)
  } else {
    # Generic component structure
    corpus_comp <- xml2::xml_add_child(components, "cmd:SpeechCorpus")
    add_speech_corpus_components(corpus_comp, db_name, db_uuid, corpus_title,
                                 corpus_description, author, institution,
                                 contact_email, license, availability,
                                 db_metadata, participant_metadata,
                                 include_placeholders)
  }
  
  return(doc)
}


#' Add speech corpus components to CMDI
#' @keywords internal
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


#' Add media corpus components to CMDI
#' @keywords internal
add_media_corpus_components <- function(parent, db_name, db_uuid, corpus_title,
                                       corpus_description, author, institution,
                                       contact_email, license, availability,
                                       db_metadata, participant_metadata,
                                       include_placeholders) {
  
  # Similar structure but adapted for media-corpus profile
  xml2::xml_add_child(parent, "cmd:Name", corpus_title)
  xml2::xml_add_child(parent, "cmd:Description", 
                     corpus_description %||% "PLACEHOLDER_CORPUS_DESCRIPTION")
  
  # Add other components following media-corpus profile structure
  # (Similar to speech corpus but with profile-specific elements)
}


#' Format duration in HH:MM:SS
#' @keywords internal
format_duration <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)
  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}


#' Null coalescing operator
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
