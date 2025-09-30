
#' Import a speech recordings into an Emu database session
#'
#' This function imports speech recordings stored in a directory into session in
#' an Emu database. The new session will be named according to the name the user
#' provides using the `targetSessionName` argument. A new bundle will be created
#' for every file with the file extension specified in the "mediafileExtension"
#' setting in the database configuration file (usually "wav"). ALternatively,
#' the user may set up sub-directories in the import folder, which will then be
#' used as recording session names.
#'
#' In order to ensure that the database always contains a mono sound file, the
#' single channel indicated by the `speech.channel` argument will be extracted
#' and stored in the database.
#'
#' The function also makes sure that 'OSCI' and 'SPEC' perspectives are defined
#' for the database.
#'
#' The user may also indicate that an additional channel of the speech
#' recording, containing an electroglottography (EGG) track, should be stored in
#' an '.egg' file by indicating its channel number in the `egg.channel`
#' argument. The EGG track is often stored in the second channel of the speech
#' recording file, which means that `egg.channel=2` is usually appropriate. The
#' user should however verify this and a sound editor before importing speech
#' recordings.
#'
#'
#' @param emuDBhandle The Emu database handle.
#' @param dir The directory containing speech recordings or session directories.
#' @param targetSessionName The default session name, if not specified using sub-directories in the import folder.
#' @param speech.channel The channel number containing the audio track to be stored in the database.
#' @param egg.channel An optional electroglottography channel number, which will
#'   be stored in a separate ".egg" file.
#' @param verbose Should additional information and progress bar be displayed to the user?
#'
#' @export
#'
import_recordings <- function (emuDBhandle, dir, targetSessionName = "0000", speech.channel=1,egg.channel=NULL, verbose = TRUE)
{
  emuR:::check_emuDBhandle(emuDBhandle)
  dbCfg = emuR:::load_DBconfig(emuDBhandle)

  #Prepare an extension to channel list
  split.to <- list("wav"=speech.channel)
  if(!is.null(egg.channel)){
    split.to$egg <- egg.channel
  }

  #In this case, the user has opted to rely on the definition "mediafileExtension" in selecting which
  # file extension to use for speech signal files. "wav" is by far the most common case.
  if (is.null(dbCfg[["mediafileExtension"]])) {
    #If no definition is present, revert to "wav"
    pattern = ".*[.]wav$"
    warning("Using \'wav\' as the signal file extension since no definition was found in the database file or in the user arguments to this function. Please use the split.to argument to provide a default.")
  }
  else {
    pattern = paste0(".*[.]", dbCfg[["mediafileExtension"]],
                     "$")
  }

  mfList = list.files(dir, pattern = pattern)
  if (length(mfList) > 0) {
    sessDir = file.path(emuDBhandle$basePath, paste0(targetSessionName,
                                                     emuR:::session.suffix))
    if (!file.exists(sessDir)) {
      dir.create(sessDir)
    }
    qSessSql = paste0("SELECT * ", "FROM session ", "WHERE db_uuid='",
                      emuDBhandle$UUID, "' ", " AND name='", targetSessionName,
                      "'")
    sessDf <- DBI::dbGetQuery(emuDBhandle$connection, qSessSql)
    if (nrow(sessDf) == 0) {
      emuR:::add_sessionDBI(emuDBhandle, sessionName = targetSessionName)
    }
  }
  mediaAdded = FALSE
  progress = 0
  if (verbose) {
    cat("INFO: Importing ", length(mfList), " media files...\n")
    pb = utils::txtProgressBar(min = 0, max = length(mfList),
                               initial = progress, style = 3)
    utils::setTxtProgressBar(pb, progress)
  }
  for (mf in mfList) {
    mfFullPath = file.path(dir, mf)
    bundleName = sub("[.][^.]*$", "", mf)
    bundleDir = file.path(sessDir, paste0(bundleName, emuR:::bundle.dir.suffix))
    dir.create(bundleDir)
    newMediaFileFullPath = file.path(bundleDir, mf)

    #file.copy(from = mfFullPath, to = newMediaFileFullPath)
    pfAssp = wrassp::read.AsspDataObj(mfFullPath)
    sampleRate = attr(pfAssp, "sampleRate")


    if(as.numeric(split.to$wav) > ncol(pfAssp$audio)){
      stop("The channel number indicating the WAV track data does not exist.")
    }
    # Separate out an EGG track if available
    if(! is.null(split.to$egg) && as.numeric(split.to$egg) <= ncol(pfAssp$audio) ){
      eggAssp <- pfAssp # Make an EGG signal track
      eggAssp$audio[,as.numeric(split.to$egg)] -> eggAssp$audio
      dim(eggAssp$audio) <- c(length(eggAssp$audio),1)
      eggFileFullPath <- paste0(tools::file_path_sans_ext(newMediaFileFullPath),".egg")
      wrassp::write.AsspDataObj(eggAssp,eggFileFullPath)
    }
    # Now finally write the wave file track
    if(! is.null(split.to$wav) && as.numeric(split.to$wav) <= ncol(pfAssp$audio) ){
      pfAssp$audio[,as.numeric(split.to$wav)] -> pfAssp$audio
      dim(pfAssp$audio) <- c(length(pfAssp$audio),1)
      wrassp::write.AsspDataObj(pfAssp,newMediaFileFullPath)
    }



    b = list(name = bundleName, annotates = mf, sampleRate = sampleRate,
             levels = list(), links = list())
    for (ld in dbCfg[["levelDefinitions"]]) {
      b$levels[[length(b$levels) + 1]] = list(name = ld[["name"]],
                                              type = ld[["type"]], items = list())
    }
    annotJSONchar = jsonlite::toJSON(b, auto_unbox = T,
                                     pretty = T)
    newAnnotFileFullPath = file.path(bundleDir, paste0(bundleName,
                                                       emuR:::bundle.annotation.suffix, ".json"))
    writeLines(annotJSONchar, newAnnotFileFullPath, useBytes = TRUE)
    MD5annotJSON = tools::md5sum(newAnnotFileFullPath)
    emuR:::add_bundleDBI(emuDBhandle, sessionName = targetSessionName,
                         name = bundleName, annotates = mf, sampleRate = sampleRate,
                         MD5annotJSON = MD5annotJSON)
    progress = progress + 1
    if (verbose) {
      utils::setTxtProgressBar(pb, progress)
    }
    mediaAdded = TRUE
  }
  perspectives = dbCfg[["EMUwebAppConfig"]][["perspectives"]]
  if (mediaAdded & (is.null(perspectives) | length(perspectives) ==
                    0)) {
    sc = list(order = c("OSCI", "SPEC"), assign = list(),
              contourLims = list())
    defPersp = list(name = "default", signalCanvases = sc,
                    levelCanvases = list(order = list()), twoDimCanvases = list(order = list()))
    dbCfg[["EMUwebAppConfig"]][["perspectives"]] = list(defPersp)
    emuR:::store_DBconfig(emuDBhandle, dbConfig = dbCfg)
  }
  return(invisible(NULL))
}

#' Save the state of a speech database
#'
#' This function saves a snapshot of an Emu speech database into a git repository and
#' optionally pushes changes to a remote git server. All altered files will be included in the snapshot.
#'
#' If a remote server has been specified for the git repository, the changes will be pushed there into the "master" branch by default.
#'
#'
#' @param emuDBhandle The database handle
#' @param push.changes If `TRUE` all changes will pushed into the remote git server.
#' @param remote.name The name of the remote repository.
#' @param remote.ref The reference in the remote repository where changes should be pushed.
#'
#' @export
#'
snapshot <- function(emuDBhandle,push.changes=TRUE,remote.name="origin",remote.ref="master"){

  if(! git2r::in_repository(emuDBhandle$basePath)){
    git2r::init(emuDBhandle$basePath)
  }

  mess <- paste0("Snapshot of database '", emuDBhandle$dbName ,"' created at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  #Add all modified files
  git2r::commit(repo=emuDBhandle$basePath, message = mess,all=TRUE )

  if(push.changes && length(git2r::remotes(emuDBhandle$basePath) ) > 0 ){
    git2r::push(object=emuDBhandle$basePath,remote.name,remote.ref)

  }



}


### For interactive testing
#
#
# library(wrassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db() -> emuDBhandle
# import_mediaFiles(emuDBhandle,dir="~/Desktop/egg",egg.channel = 2)




#' @title Convert Emu-SDMS Annotation to ELAN EAF Format
#'
#' @description This function converts an Emu-SDMS annotation file (JSON format) into
#'   an ELAN Annotation Format (EAF) \insertCite{EAF_v3.0_MPI_2017}{reindeer}.
#'
#' @details The function handles the structural differences between EMU-SDMS and EAF formats,
#'   including time calculation, tier structure, and media linking.
#'
#'   When there are multiple labeltypes associated with a level in the Emu-SDMS annotation, the EAF are constructed
#'   using a combination of annotation types to signify that they are in some way different in the following way
#'
#'   \itemize{
#'     \item The **first label** of a time-aligned Emu ITEM, SEGMENT or EVENT becomes the content for the
#'       primary \code{<ALIGNABLE_ANNOTATION>} on the base tier.
#'     \item All **subsequent labels** within the same ITEM, SEGMENT or EVENT generate
#'       new, symbolically associated tiers that link back to the primary annotation,
#'       correctly representing multi-layered data.
#'   }
#'
#'   When \code{align_items} is \code{FALSE}, Emu ITEM tiers that are structurally linked (not time-aligned) are
#'       converted into symbolic tiers based on the Emu link hierarchy.
#'
#'   When time alignment of ITEMs are asked for, i.g. \code{align_items} is \code{TRUE}, which is the default,
#'   they are treated as SEGMENTs, and the start and end times of them are derived from the first and last
#'   dominated SEGMENTs start and end times.
#'
#'   The time calculations the following considerations have been made to comply with the EAF specifications
#'   \itemize{
#'     \item Time points are calculated from Eme sample rates and written to the
#'       \code{<TIME_ORDER>} block, as required by the EAF specification
#'       (EAF Schema v3.0, section 2.4).
#'     \item For EVENT (point-in-time) tiers, a small duration (\code{event_duration},
#'       default 1ms) is artificially assigned. This workaround prevents
#'       the ELAN EAF validator from throwing errors for zero-duration annotations e.g.
#'       by looping labels or time references with identical associated time points.
#'   }
#'
#' @param json_file Path to the input EMU-SDMS annotation JSON file (e.g., "msajc012_annot.json").
#' @param eaf_file Optional path for the output EAF file. If \code{NULL}, it defaults
#'   to changing the input extension from "_annot.json" to ".eaf".
#' @param author The author string to be written into the EAF \code{<ANNOTATION_DOCUMENT>} tag.
#' @param media_file Optional path to the media file. If \code{NULL},
#'   it is derived from the \code{annotates} field in the JSON, assuming the media
#'   file resides in the same directory as the JSON file.
#' @param align_items Logical. If \code{TRUE} (the default), ITEM tiers are calculated
#'   to span the time of their time-aligned children, making them \code{<ALIGNABLE_ANNOTATION>}s.
#'   If \code{FALSE} (default), they are symbolic \code{<REF_ANNOTATION>}s.
#' @param event_duration Integer. The duration (in milliseconds) assigned to EMU 'EVENT' tiers.
#'   Defaults to 1ms to satisfy the ELAN content validator.
#' @param verbose Logical. Should messages be printed during processing.
#'
#' @return The path to the created EAF file (invisibly).
#' @references
#'  \insertAllCited{}



convert_emu_to_eaf <- function(json_file, eaf_file = NULL,
                               author = "EMU-to-ELAN Converter",
                               media_file = NULL,
                               align_items = TRUE,
                               event_duration = 1,
                               verbose=FALSE) {

  if (base::is.null(eaf_file)) {
    eaf_file <- base::sub("_annot\\.json$", ".eaf", json_file)
  }

  get_item_time_range <- function(item_id, emu_data, children_map, time_points_map, level_map, level_type_map, event_duration) {

    level_name <- level_map[[item_id]]
    level_type <- level_type_map[[level_name]]

    # Base case: if item_id belongs to a segment or event level (i.e., time-aligned)
    if (level_type %in% base::c("SEGMENT", "EVENT")) {
      level_list_index <- which(base::sapply(emu_data$levels, function(l) l$name == level_name))
      level <- emu_data$levels[[level_list_index]]
      item <- level$items[[which(base::sapply(level$items, function(i) base::as.character(i$id) == item_id))]]

      if (level$type == "SEGMENT") {
        start_ms <- base::round((item$sampleStart / emu_data$sampleRate) * 1000)
        end_ms <- base::round(((item$sampleStart + item$sampleDur) / emu_data$sampleRate) * 1000)
      } else { # EVENT
        point_ms <- base::round((item$samplePoint / emu_data$sampleRate) * 1000)
        start_ms <- point_ms
        # Use event_duration for calculating parent span when align_items=TRUE
        end_ms <- point_ms + event_duration
      }
      return(base::list(start = start_ms, end = end_ms))
    }

    # Recursive case: find children's time ranges
    children_ids <- children_map[[item_id]]
    if (base::is.null(children_ids)) {
      return(NULL)
    }

    min_start <- Inf
    max_end <- -Inf

    for (child_id in children_ids) {
      # Pass event_duration to recursive call
      range <- get_item_time_range(child_id, emu_data, children_map, time_points_map, level_map, level_type_map, event_duration)
      if (!base::is.null(range)) {
        min_start <- base::min(min_start, range$start)
        max_end <- base::max(max_end, range$end)
      }
    }

    if (base::is.infinite(min_start) || base::is.infinite(max_end)) {
      return(NULL)
    }

    return(base::list(start = min_start, end = max_end))
  }

  # Read JSON
  emu_data <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)

  # Set media_file if not provided (assume same directory)
  if (base::is.null(media_file) && !base::is.null(emu_data$annotates)) {
    media_file <- base::file.path(base::dirname(json_file), base::basename(emu_data$annotates))
  }

  # --- MEDIA METADATA RETRIEVAL (MIME type is required in HEADER) ---
  media_mimetype <- "audio/x-wav"

  # Create document
  date_string <- base::format(base::Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  date_string <- base::sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", date_string) # Fix timezone format

  doc <- xml2::xml_new_root("ANNOTATION_DOCUMENT",
                            AUTHOR = author,
                            DATE = date_string,
                            FORMAT = "3.0",
                            VERSION = "3.0",
                            "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                            "xsi:noNamespaceSchemaLocation" = "http://www.mpi.nl/tools/elan/EAFv3.0.xsd")

  # Add HEADER (EAF Schema v3.0, section 2.3)
  header <- xml2::xml_add_child(doc, "HEADER", TIME_UNITS = "milliseconds")

  if (!base::is.null(media_file) && base::file.exists(media_file)) {

    # Paths
    relative_url <- base::basename(media_file)
    media_url <- base::paste0("file://", base::normalizePath(media_file, mustWork = FALSE))

    # 1. Add MEDIA_DESCRIPTOR (EAF Schema v3.0, section 2.3.1)
    xml2::xml_add_child(header, "MEDIA_DESCRIPTOR",
                        MEDIA_URL = media_url,
                        MIME_TYPE = media_mimetype,
                        RELATIVE_MEDIA_URL = relative_url)

    # 2. Add LINKED_FILE_DESCRIPTOR (EAF Schema v3.0, section 2.3.2)
    xml2::xml_add_child(header, "LINKED_FILE_DESCRIPTOR",
                        LINK_URL = media_url,
                        MIME_TYPE = media_mimetype)
  }

  # Build link maps (Parent/Child relations)
  parent_map <- base::list()
  children_map <- base::list()

  if (!base::is.null(emu_data$links)) {
    for (link in emu_data$links) {
      from_id <- base::as.character(link$fromID)
      to_id <- base::as.character(link$toID)
      parent_map[[to_id]] <- from_id

      if (base::is.null(children_map[[from_id]])) {
        children_map[[from_id]] <- base::list()
      }
      children_map[[from_id]] <- base::append(children_map[[from_id]], to_id)
    }
  }

  # Store level info and map level name to type
  level_map <- base::list()
  level_type_map <- base::list()
  for (level in emu_data$levels) {
    level_type_map[[level$name]] <- level$type
    for (item in level$items) {
      level_map[[base::as.character(item$id)]] <- level$name
    }
  }

  # Create TIME_ORDER (EAF Schema v3.0, section 2.4)
  time_order <- xml2::xml_add_child(doc, "TIME_ORDER")
  time_slot_map <- base::list()
  time_slot_counter <- 1
  all_time_points <- base::c()

  # Collect time points for SEGMENTs and EVENTs
  for (level in emu_data$levels) {
    if (level$type %in% base::c("SEGMENT", "EVENT")) {
      for (item in level$items) {
        if (level$type == "SEGMENT") {
          start_ms <- base::round((item$sampleStart / emu_data$sampleRate) * 1000)
          end_ms <- base::round(((item$sampleStart + item$sampleDur) / emu_data$sampleRate) * 1000)
          all_time_points <- base::c(all_time_points, start_ms, end_ms)
        } else if (level$type == "EVENT") {
          point_ms <- base::round((item$samplePoint / emu_data$sampleRate) * 1000)
          # Collect the start point and the padded end point (point_ms + event_duration)
          all_time_points <- base::c(all_time_points, point_ms, point_ms + event_duration)
        }
      }
    }
  }

  # Collect time points for ITEM tiers if align_items = TRUE
  if (align_items) {
    for (level in emu_data$levels) {
      if (level$type == "ITEM") {
        for (item in level$items) {
          item_id <- base::as.character(item$id)
          time_range <- get_item_time_range(item_id, emu_data, children_map, time_slot_map, level_map, level_type_map, event_duration)
          if (!base::is.null(time_range)) {
            all_time_points <- base::c(all_time_points, time_range$start, time_range$end)
          }
        }
      }
    }
  }

  # Create unique TIME_SLOT entries
  unique_times <- base::sort(base::unique(all_time_points))
  for (time_val in unique_times) {
    ts_id <- base::paste0("ts", time_slot_counter)
    xml2::xml_add_child(time_order, "TIME_SLOT",
                        TIME_SLOT_ID = ts_id,
                        TIME_VALUE = base::as.character(time_val))
    time_slot_map[[base::as.character(time_val)]] <- ts_id
    time_slot_counter <- time_slot_counter + 1
  }

  # Store annotation IDs and TIER XML objects for reuse
  annotation_map <- base::list()
  eaf_tiers <- base::list()

  # 1. Create all tiers with time-aligned annotations (SEGMENT, EVENT, and ITEM if align_items=TRUE)
  for (level in emu_data$levels) {
    if (level$type %in% base::c("SEGMENT", "EVENT") || (level$type == "ITEM" && align_items)) {

      base_tier_id <- level$name

      # Create the BASE TIER if it doesn't exist
      if (base::is.null(eaf_tiers[[base_tier_id]])) {
        eaf_tiers[[base_tier_id]] <- xml2::xml_add_child(doc, "TIER",
                                                         TIER_ID = base_tier_id,
                                                         LINGUISTIC_TYPE_REF = "default-lt")
      }
      base_tier <- eaf_tiers[[base_tier_id]]

      for (item in level$items) {
        item_id <- base::as.character(item$id)

        # Check for labels. If none, skip
        if (base::is.null(item$labels) || base::length(item$labels) == 0) next

        start_ms <- NULL
        end_ms <- NULL

        if (level$type == "SEGMENT") {
          start_ms <- base::round((item$sampleStart / emu_data$sampleRate) * 1000)
          end_ms <- base::round(((item$sampleStart + item$sampleDur) / emu_data$sampleRate) * 1000)
        } else if (level$type == "EVENT") {
          point_ms <- base::round((item$samplePoint / emu_data$sampleRate) * 1000)
          start_ms <- point_ms
          end_ms <- point_ms + event_duration
        } else if (level$type == "ITEM" && align_items) {
          time_range <- get_item_time_range(item_id, emu_data, children_map, time_slot_map, level_map, level_type_map, event_duration)
          if (base::is.null(time_range)) next
          start_ms <- time_range$start
          end_ms <- time_range$end
        } else {
          next
        }

        # --- A. BASE ALIGNABLE ANNOTATION (from the first label) ---
        base_label <- item$labels[[1]]
        ann_id <- base::paste0("a", item_id)

        # Use the two now-different time slots
        ref1_ts_id <- time_slot_map[[base::as.character(start_ms)]]
        ref2_ts_id <- time_slot_map[[base::as.character(end_ms)]]

        annotation <- xml2::xml_add_child(base_tier, "ANNOTATION")
        alignable <- xml2::xml_add_child(annotation, "ALIGNABLE_ANNOTATION",
                                         ANNOTATION_ID = ann_id,
                                         TIME_SLOT_REF1 = ref1_ts_id,
                                         TIME_SLOT_REF2 = ref2_ts_id)

        xml2::xml_add_child(alignable, "ANNOTATION_VALUE", base_label$value)
        annotation_map[[item_id]] <- ann_id

        # --- B. SYMBOLIC REF ANNOTATIONS (from subsequent labels) ---
        if (base::length(item$labels) > 1) {
          for (i in 2:base::length(item$labels)) {
            sub_label <- item$labels[[i]]
            sub_tier_id <- sub_label$name

            # 1. Ensure the Symbolic Tier object exists or create it
            if (base::is.null(eaf_tiers[[sub_tier_id]])) {
              # Create a new Tier for the symbolic label type, linked to the base tier
              eaf_tiers[[sub_tier_id]] <- xml2::xml_add_child(doc, "TIER",
                                                              TIER_ID = sub_tier_id,
                                                              PARENT_REF = base_tier_id,
                                                              LINGUISTIC_TYPE_REF = "symbolic-association")
            }
            sub_tier <- eaf_tiers[[sub_tier_id]]

            # 2. Create the REF ANNOTATION on the symbolic tier
            # Using a combination ID to ensure global uniqueness across tiers
            sub_ann_id <- base::paste0("a", item_id, "_", sub_tier_id)

            sub_annotation <- xml2::xml_add_child(sub_tier, "ANNOTATION")
            ref_annot <- xml2::xml_add_child(sub_annotation, "REF_ANNOTATION",
                                             ANNOTATION_ID = sub_ann_id,
                                             ANNOTATION_REF = ann_id) # Links back to the time-aligned annotation

            xml2::xml_add_child(ref_annot, "ANNOTATION_VALUE", sub_label$value)
          }
        }
      }
    }
  }

  # 2. Structural Symbolic Tiers (EMU ITEM tiers when align_items=FALSE)
  # This section handles symbolic links based on EMU's link structure (e.g., Utterance -> Word)
  if (!align_items) {
    for (level in emu_data$levels) {
      if (level$type == "ITEM") {

        tier_id <- level$name

        # Skip if the tier was already created dynamically in the multi-label step
        if (!base::is.null(eaf_tiers[[tier_id]])) next

        parent_tier_name <- NULL
        for (item in level$items) {
          parent_id <- parent_map[[base::as.character(item$id)]]
          if (!base::is.null(parent_id) && !base::is.null(level_map[[parent_id]])) {
            parent_tier_name <- level_map[[parent_id]]
            break
          }
        }

        tier_attrs <- base::list(TIER_ID = tier_id)

        if (!base::is.null(parent_tier_name)) {
          tier_attrs$PARENT_REF <- parent_tier_name
          parent_level_obj <- emu_data$levels[[which(base::sapply(emu_data$levels, function(l) l$name == parent_tier_name))]]

          # Determine symbolic type
          if (!base::is.null(parent_level_obj) && parent_level_obj$type %in% base::c("SEGMENT", "EVENT")) {
            tier_attrs$LINGUISTIC_TYPE_REF <- "symbolic-subdivision"
          } else {
            tier_attrs$LINGUISTIC_TYPE_REF <- "symbolic-association"
          }
        } else {
          tier_attrs$LINGUISTIC_TYPE_REF <- "symbolic-association"
        }

        tier <- base::do.call(xml2::xml_add_child, base::c(base::list(doc, "TIER"), tier_attrs))
        eaf_tiers[[tier_id]] <- tier

        prev_annotation_id <- NULL

        for (item in level$items) {
          item_id <- base::as.character(item$id)
          parent_id <- parent_map[[item_id]]

          parent_ann_id <- NULL
          if (!base::is.null(parent_id) && !base::is.null(annotation_map[[parent_id]])) {
            parent_ann_id <- annotation_map[[parent_id]]
          }

          if (tier_attrs$LINGUISTIC_TYPE_REF %in% base::c("symbolic-subdivision", "symbolic-association") && !base::is.null(parent_ann_id)) {

            annotation <- xml2::xml_add_child(tier, "ANNOTATION")

            annot_value <- ""
            if (!base::is.null(item$labels) && base::length(item$labels) > 0) {
              label_values <- base::sapply(item$labels, function(l) l$value)
              # Use only the first label value, as multi-label items should have been
              # time-aligned or are incorrectly structured as ITEM tiers with multiple labels.
              annot_value <- label_values[[1]]
            }

            ann_id <- base::paste0("a", item_id)

            ref_attrs <- base::list(
              ANNOTATION_ID = ann_id,
              ANNOTATION_REF = parent_ann_id
            )

            if (!base::is.null(prev_annotation_id) && tier_attrs$LINGUISTIC_TYPE_REF == "symbolic-subdivision") {
              ref_attrs$PREVIOUS_ANNOTATION <- prev_annotation_id
            }

            ref_annot <- base::do.call(xml2::xml_add_child, base::c(base::list(annotation, "REF_ANNOTATION"), ref_attrs))
            xml2::xml_add_child(ref_annot, "ANNOTATION_VALUE", annot_value)

            annotation_map[[item_id]] <- ann_id
            prev_annotation_id <- ann_id
          }

          # Simplified handling for an orphan structural tier, associating to the first found annotation
          else if (base::is.null(parent_tier_name) && tier_attrs$LINGUISTIC_TYPE_REF == "symbolic-association") {
            if (base::length(annotation_map) > 0) {
              first_ann_id <- annotation_map[[1]]

              annotation <- xml2::xml_add_child(tier, "ANNOTATION")

              annot_value <- ""
              if (!base::is.null(item$labels) && base::length(item$labels) > 0) {
                label_values <- base::sapply(item$labels, function(l) l$value)
                annot_value <- label_values[[1]]
              }

              ann_id <- base::paste0("a", item_id)

              ref_attrs <- base::list(
                ANNOTATION_ID = ann_id,
                ANNOTATION_REF = first_ann_id
              )

              ref_annot <- base::do.call(xml2::xml_add_child, base::c(base::list(annotation, "REF_ANNOTATION"), ref_attrs))
              xml2::xml_add_child(ref_annot, "ANNOTATION_VALUE", annot_value)

              annotation_map[[item_id]] <- ann_id
            }
          }
        }
      }
    }
  }

  # Add linguistic types and constraints (EAF Schema v3.0, sections 2.6 and 2.7)
  xml2::xml_add_child(doc, "LINGUISTIC_TYPE",
                      LINGUISTIC_TYPE_ID = "default-lt",
                      TIME_ALIGNABLE = "true",
                      GRAPHIC_REFERENCES = "false")

  xml2::xml_add_child(doc, "LINGUISTIC_TYPE",
                      LINGUISTIC_TYPE_ID = "symbolic-subdivision",
                      TIME_ALIGNABLE = "false",
                      CONSTRAINTS = "Symbolic_Subdivision",
                      GRAPHIC_REFERENCES = "false")

  xml2::xml_add_child(doc, "LINGUISTIC_TYPE",
                      LINGUISTIC_TYPE_ID = "symbolic-association",
                      TIME_ALIGNABLE = "false",
                      CONSTRAINTS = "Symbolic_Association",
                      GRAPHIC_REFERENCES = "false")

  xml2::xml_add_child(doc, "CONSTRAINT",
                      STEREOTYPE = "Time_Subdivision",
                      DESCRIPTION = "Time subdivision of parent annotation's time interval, no time gaps allowed within this interval")

  xml2::xml_add_child(doc, "CONSTRAINT",
                      STEREOTYPE = "Symbolic_Subdivision",
                      DESCRIPTION = "Symbolic subdivision of a parent annotation. Annotations refering to the same parent are ordered")

  xml2::xml_add_child(doc, "CONSTRAINT",
                      STEREOTYPE = "Symbolic_Association",
                      DESCRIPTION = "1-1 association with a parent annotation")

  xml2::xml_add_child(doc, "CONSTRAINT",
                      STEREOTYPE = "Included_In",
                      DESCRIPTION = "Time alignable annotations within the parent annotation's time interval, gaps are allowed")

  # Write file
  xml2::write_xml(doc, eaf_file)

  if(verbose) cli::cli_inform("Successfully converted {.file json_file} to {.file eaf_file}")
  return(base::invisible(eaf_file))
}
