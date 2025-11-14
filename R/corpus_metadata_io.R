corpus_assign_metadata <- function(corpus_obj, session_pattern, bundle_pattern, metadata_list) {
  if (!is.list(metadata_list) || is.null(names(metadata_list))) {
    cli::cli_abort("Metadata must be a named list")
  }
  
  # Determine level based on patterns
  is_session_all <- is.null(session_pattern) || session_pattern == ".*"
  is_bundle_all <- is.null(bundle_pattern) || bundle_pattern == ".*"
  
  if (is_session_all && is_bundle_all) {
    # Database level
    set_metadata_database(corpus_obj, metadata_list)
  } else if (!is_session_all && is_bundle_all) {
    # Session level
    set_metadata_session(corpus_obj, session_pattern, metadata_list)
  } else if (!is_session_all && !is_bundle_all) {
    # Bundle level
    set_metadata_bundle(corpus_obj, session_pattern, bundle_pattern, metadata_list)
  } else {
    cli::cli_abort("Invalid pattern combination: bundle requires session")
  }
}

#' Set database-level metadata
#' @keywords internal
set_metadata_database <- function(corpus_obj, metadata_list) {
  # Write to METADATA.json in database root
  db_meta_file <- file.path(corpus_obj@basePath,
                            metadata.filename)
  
  # Read existing
  if (file.exists(db_meta_file)) {
    existing <- jsonlite::read_json(db_meta_file, simplifyVector = TRUE)
  } else {
    existing <- list()
  }
  
  # Merge
  updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
  
  # Write
  jsonlite::write_json(updated, db_meta_file, auto_unbox = TRUE, pretty = TRUE)
  
  # Update cache
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  process_metadata_list(con, corpus_obj@.uuid, NULL, NULL, metadata_list, "database")
  
  cli::cli_alert_success("Database metadata updated")
}

#' Set session-level metadata
#' @keywords internal
set_metadata_session <- function(corpus_obj, session_pattern, metadata_list) {
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find matching sessions
  sessions <- list_sessions_from_cache(con, corpus_obj@.uuid)
  matches <- grepl(session_pattern, sessions$name)
  matching_sessions <- sessions$name[matches]
  
  if (length(matching_sessions) == 0) {
    cli::cli_abort("No sessions match pattern {.val {session_pattern}}")
  }
  
  if (length(matching_sessions) > 1 && !grepl("[.*+?^${}()|\\[\\]]", session_pattern)) {
    # Looks like literal match but got multiple - warn user
    cli::cli_alert_warning("Pattern {.val {session_pattern}} matches {length(matching_sessions)} sessions")
  }
  
  # Update each matching session
  for (session_name in matching_sessions) {
    session_meta_file <- file.path(
      corpus_obj@basePath,
      paste0(session_name, "_ses"),
      metadata.filename
    )
    
    # Read existing
    if (file.exists(session_meta_file)) {
      existing <- jsonlite::read_json(session_meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    # Merge
    updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
    
    # Ensure directory exists
    session_dir <- dirname(session_meta_file)
    if (!dir.exists(session_dir)) {
      dir.create(session_dir, recursive = TRUE)
    }
    
    # Write
    jsonlite::write_json(updated, session_meta_file, auto_unbox = TRUE, pretty = TRUE)
    
    # Update cache
    process_metadata_list(con, corpus_obj@.uuid, session_name, NULL, metadata_list, "session")
  }
  
  cli::cli_alert_success("Metadata updated for {length(matching_sessions)} session{?s}")
}

#' Set bundle-level metadata
#' @keywords internal
set_metadata_bundle <- function(corpus_obj, session_pattern, bundle_pattern, metadata_list) {
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find matching bundles
  bundles <- list_bundles_from_cache(con, corpus_obj@.uuid)
  session_matches <- grepl(session_pattern, bundles$session)
  bundle_matches <- grepl(bundle_pattern, bundles$name)
  
  matching_bundles <- bundles[session_matches & bundle_matches, ]
  
  if (nrow(matching_bundles) == 0) {
    cli::cli_abort("No bundles match session={.val {session_pattern}}, bundle={.val {bundle_pattern}}")
  }
  
  # Update each matching bundle
  for (i in seq_len(nrow(matching_bundles))) {
    session_name <- matching_bundles$session[i]
    bundle_name <- matching_bundles$name[i]
    
    bundle_meta_file <- file.path(
      corpus_obj@basePath,
      paste0(session_name, "_ses"),
      paste0(bundle_name, "_bndl"),
      metadata.filename
    )
    
    # Read existing
    if (file.exists(bundle_meta_file)) {
      existing <- jsonlite::read_json(bundle_meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    # Merge
    updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
    
    # Ensure directory exists
    bundle_dir <- dirname(bundle_meta_file)
    if (!dir.exists(bundle_dir)) {
      dir.create(bundle_dir, recursive = TRUE)
    }
    
    # Write
    jsonlite::write_json(updated, bundle_meta_file, auto_unbox = TRUE, pretty = TRUE)
    
    # Update cache
    process_metadata_list(con, corpus_obj@.uuid, session_name, bundle_name, metadata_list, "bundle")
  }
  
  cli::cli_alert_success("Metadata updated for {nrow(matching_bundles)} bundle{?s}")
}

#' Import media files to corpus bundles
#' @keywords internal
corpus_import_media <- function(corpus_obj, session_pattern, bundle_pattern, media_spec) {
  # media_spec can be:
  # - Single file path: imports to mediafileExtension
  # - Vector with channel assignments: c("egg", "path.mp3", NULL, "flow")
  # - Vector with file first: c("path.mp3", "egg", NULL, "flow")
  
  if (grepl("[.*+?^${}()|\\[\\]]", session_pattern) || 
      grepl("[.*+?^${}()|\\[\\]]", bundle_pattern)) {
    cli::cli_abort("Regex patterns not allowed for media import. Specify exact session and bundle names.")
  }
  
  # Find the unique bundle
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  bundles <- list_bundles_from_cache(con, corpus_obj@.uuid)
  session_matches <- bundles$session == session_pattern
  bundle_matches <- bundles$name == bundle_pattern
  
  matching_bundles <- bundles[session_matches & bundle_matches, ]
  
  if (nrow(matching_bundles) == 0) {
    cli::cli_abort("Bundle {.val {bundle_pattern}} not found in session {.val {session_pattern}}")
  }
  
  if (nrow(matching_bundles) > 1) {
    cli::cli_abort("Multiple bundles found. Be more specific.")
  }
  
  # Parse media specification
  file_path <- NULL
  channel_map <- list()
  
  if (length(media_spec) == 1) {
    # Simple case: just a file path
    file_path <- media_spec
    channel_map[[corpus_obj@config$mediafileExtension %||% "wav"]] <- 1
  } else {
    # Complex case: channel mappings
    # Find the file path (should be the only element that exists as a file)
    for (elem in media_spec) {
      if (!is.null(elem) && file.exists(elem)) {
        file_path <- elem
        break
      }
    }
    
    if (is.null(file_path)) {
      cli::cli_abort("No valid file path found in media specification")
    }
    
    # Build channel map
    channel_num <- 1
    for (elem in media_spec) {
      if (!is.null(elem)) {
        if (elem == file_path) {
          # This is the file path - assign to mediafileExtension
          channel_map[[corpus_obj@config$mediafileExtension %||% "wav"]] <- channel_num
        } else {
          # This is a track extension
          channel_map[[elem]] <- channel_num
        }
      }
      channel_num <- channel_num + 1
    }
  }
  
  # Now import the media file
  import_media_to_bundle(
    corpus_obj = corpus_obj,
    session_name = session_pattern,
    bundle_name = bundle_pattern,
    file_path = file_path,
    channel_map = channel_map
  )
}

#' Import media file to a specific bundle
#' @keywords internal
import_media_to_bundle <- function(corpus_obj, session_name, bundle_name, 
                                  file_path, channel_map) {
  
  if (!file.exists(file_path)) {
    cli::cli_abort("Media file not found: {.path {file_path}}")
  }
  
  if (!requireNamespace("av", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg av} required for media import")
  }
  
  # Get target extension from config or default to wav
  target_ext <- corpus_obj@config$mediafileExtension %||% "wav"
  
  # Create bundle directory if it doesn't exist
  bundle_dir <- file.path(
    corpus_obj@basePath,
    paste0(session_name, "_ses"),
    paste0(bundle_name, "_bndl")
  )
  
  if (!dir.exists(bundle_dir)) {
    cli::cli_abort(
      "Bundle directory does not exist: {.path {bundle_dir}}. Create bundle first."
    )
  }
  
  # Get audio info
  audio_info <- av::av_media_info(file_path)
  n_channels <- audio_info$audio$channels
  sample_rate <- audio_info$audio$sample_rate
  
  if (is.null(n_channels) || n_channels == 0) {
    cli::cli_abort("No audio channels found in {.path {file_path}}")
  }
  
  # Validate channel numbers
  requested_channels <- unique(unlist(channel_map))
  if (any(requested_channels > n_channels)) {
    cli::cli_abort(
      "File has {n_channels} channel{?s}, but channel {max(requested_channels)} was requested"
    )
  }
  
  # Convert and extract channels
  for (ext in names(channel_map)) {
    channel_num <- channel_map[[ext]]
    
    # Determine output file
    output_file <- file.path(bundle_dir, paste0(bundle_name, ".", ext))
    
    # For single channel files, can convert directly
    if (n_channels == 1 && channel_num == 1) {
      # Direct conversion
      av::av_audio_convert(
        audio = file_path,
        output = output_file,
        format = target_ext,
        sample_rate = sample_rate,
        channels = 1
      )
    } else {
      # For multi-channel, need to extract specific channel
      # Use av to convert to temporary wav, then extract channel with av filters
      temp_wav <- tempfile(fileext = ".wav")
      on.exit(unlink(temp_wav), add = TRUE)
      
      # Convert to wav with all channels
      av::av_audio_convert(
        audio = file_path,
        output = temp_wav,
        format = "wav",
        sample_rate = sample_rate
      )
      
      # Extract specific channel using av audio filter
      # The format is: channelmap=map=FL filter extracts left (first) channel
      # For channel N, use: pan=mono|c0=c{N-1}
      channel_filter <- sprintf("pan=mono|c0=c%d", channel_num - 1)
      
      av::av_audio_convert(
        audio = temp_wav,
        output = output_file,
        format = target_ext,
        sample_rate = sample_rate,
        channels = 1,
        audio_filters = channel_filter
      )
    }
    
    cli::cli_alert_success("Wrote channel {channel_num} to {.file {basename(output_file)}}")
  }
  
  # Update bundle annotation if needed (sample rate, annotates field)
  annot_file <- file.path(bundle_dir, paste0(bundle_name, "_annot.json"))
  if (file.exists(annot_file)) {
    annot <- jsonlite::read_json(annot_file, simplifyVector = FALSE)
    annot$sampleRate <- sample_rate
    annot$annotates <- paste0(bundle_name, ".", target_ext)
    jsonlite::write_json(annot, annot_file, auto_unbox = TRUE, pretty = TRUE)
  }
  
  cli::cli_alert_success("Media imported to {.val {session_name}/{bundle_name}}")
}




#' Corpus cache file update
#'
#' Builds an Emu-DBMS compliant cache file for a corpus.
#'
#' @param database_dir Path to the Emu-DBMS corpus base directory
#' @param parallel Use parallel processing (default: TRUE)
#' @param workers Number of parallel workers (default: all but one available cores are used)
#' @param batch_size Number of bundles to process in each batch for DB insertion
#' @param verbose Show progress information
#' @return A corpus object
