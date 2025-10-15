

fake_two_df_fun <- function( svDF,
                              csDF,
                              speaker.name = NULL,
                              speaker.ID = NULL,
                              speaker.dob = NULL,
                              session.datetime = NULL,
                              pdf.path = NULL,
                              simple.output = FALSE,
                              overwrite.pdfs = FALSE){
  Sys.sleep(.1)
  return(list(svDF=paste0(dim(svDF),collapse=","),
                csDF=paste0(dim(csDF),collapse=","),
                speaker.name=speaker.name ,
                speaker.ID = speaker.ID,
                speaker.dob = speaker.dob,
                session.datetime = session.datetime,
                pdf.path = pdf.path,
                simple.output = simple.output,
                overwrite.pdfs = overwrite.pdfs)
         )
}

fake_voice_report <- function(listOfFiles,
                              beginTime = NULL,
                              endTime = NULL,
                              selectionOffset = NULL,
                              selectionLength = NULL,
                              windowShape = "Gaussian1",
                              relativeWidth = 1,
                              minF = 75,
                              maxF = 600,
                              max_period_factor = 1.3,
                              max_ampl_factor = 1.6,
                              silence_threshold = 0.03,
                              voicing_threshold = 0.45,
                              octave_cost = 0.01,
                              octave_jump_cost = 0.35,
                              voiced_unvoiced_cost = 0.14,
                              praat_path = NULL) {


  Sys.sleep(1/10)

  out <- list(`Fake row UUID`=uuid::UUIDgenerate(), `Median pitch` = 120.393, `Mean pitch` = 120.631, `Standard deviation` = 5.429,
              `Minimum pitch` = 120.631, `Maximum pitch` = 194.358, `Number of pulses` = 303L,
              `Number of periods` = 302L, `Mean period` = 0.008319711,
              `Standard deviation of period` = 0.000121988, `Fraction of locally unvoiced frames` = 0.3483,
              `Number of voice breaks` = 0L, `Degree of voice breaks` = 0L,
              `Jitter (local)` = 0.00315, `Jitter (local, absolute)` = 2.6216e-05,
              `Jitter (rap)` = 0.0014, `Jitter (ppq5)` = 0.00173, `Jitter (ddp)` = 0.0042,
              `Shimmer (local)` = 0.04002, `Shimmer (local, dB)` = 0.333,
              `Shimmer (apq3)` = 0.01624, `Shimmer (apq5)` = 0.02398, `Shimmer (apq11)` = 0.04275,
              `Shimmer (dda)` = 0.04871, `Mean autocorrelation` = 0.981421,
              `Mean noise-to-harmonics ratio` = 0.028639, `Mean harmonics-to-noise ratio` = 22.562)
  return(out)
}

double_fake_voice_report <- function(listOfFiles,
                                     beginTime = NULL,
                                     endTime = NULL,
                                     selectionOffset = NULL,
                                     selectionLength = NULL,
                                     windowShape = "Gaussian1",
                                     relativeWidth = 1,
                                     minF = 75,
                                     maxF = 600,
                                     max_period_factor = 1.3,
                                     max_ampl_factor = 1.6,
                                     silence_threshold = 0.03,
                                     voicing_threshold = 0.45,
                                     octave_cost = 0.01,
                                     octave_jump_cost = 0.35,
                                     voiced_unvoiced_cost = 0.14,
                                     praat_path = NULL) {


  Sys.sleep(1/10)

  out <- list(`Fake row UUID`=uuid::UUIDgenerate(), `Median pitch` = 120.393, `Mean pitch` = 120.631, `Standard deviation` = 5.429,
              `Minimum pitch` = 120.631, `Maximum pitch` = 194.358, `Number of pulses` = 303L,
              `Number of periods` = 302L, `Mean period` = 0.008319711,
              `Standard deviation of period` = 0.000121988, `Fraction of locally unvoiced frames` = 0.3483,
              `Number of voice breaks` = 0L, `Degree of voice breaks` = 0L,
              `Jitter (local)` = 0.00315, `Jitter (local, absolute)` = 2.6216e-05,
              `Jitter (rap)` = 0.0014, `Jitter (ppq5)` = 0.00173, `Jitter (ddp)` = 0.0042,
              `Shimmer (local)` = 0.04002, `Shimmer (local, dB)` = 0.333,
              `Shimmer (apq3)` = 0.01624, `Shimmer (apq5)` = 0.02398, `Shimmer (apq11)` = 0.04275,
              `Shimmer (dda)` = 0.04871, `Mean autocorrelation` = 0.981421,
              `Mean noise-to-harmonics ratio` = 0.028639, `Mean harmonics-to-noise ratio` = 22.562)
  return(as.data.frame(rbind(out,out)))
}


## ae |>
# track("ae",forest)

# > query(ae,"Phonetic=V") %>% glimpse()
# Rows: 3
# Columns: 16
# $ labels             <chr> "V", "V", "V"
# $ start              <dbl> 187.425, 340.175, 1943.175
# $ end                <dbl> 256.925, 426.675, 2037.425
# $ db_uuid            <chr> "0fc618dc-8980-414d-8c7a-144a649ce199", "0fc618dc-8980-414d-8c7a-144…
# $ session            <chr> "0000", "0000", "0000"
# $ bundle             <chr> "msajc003", "msajc003", "msajc057"
# $ start_item_id      <int> 147, 149, 189
# $ end_item_id        <int> 147, 149, 189
# $ level              <chr> "Phonetic", "Phonetic", "Phonetic"
# $ attribute          <chr> "Phonetic", "Phonetic", "Phonetic"
# $ start_item_seq_idx <int> 1, 3, 28
# $ end_item_seq_idx   <int> 1, 3, 28
# $ type               <chr> "SEGMENT", "SEGMENT", "SEGMENT"
# $ sample_start       <int> 3749, 6804, 38864
# $ sample_end         <int> 5138, 8533, 40748
# $ sample_rate        <int> 20000, 20000, 20000

# Legacy function - replaced by ask_for in reindeer_query_optimized.r
# Keeping for backward compatibility but not exporting
ask_for_legacy <- function(inside_of, query,sessions_regex = ".*", bundles_regex = ".*",times_from = NULL, calculate_times = TRUE,interactive=FALSE){
  if(missing(inside_of)) cli::cli_abort("Please provide an Emu database handle or the full path to an Emu database in the {.args inside_of}  argument")
  if(missing(query)) cli::cli_abort("Please specify a query in the Emu Query Language.")


  if("emuDBhandle" %in% class(inside_of)){
    #reload the database just to make sure that the handle is still valid
    inside_of <- emuR::load_emuDB(inside_of$basePath,verbose = FALSE)
  }else{
    if( is.character(inside_of) && stringr::str_ends(inside_of,"_emuDB") && dir.exists(inside_of)){
      # We then need to create a handle object
      utils::capture.output(
        .inside_of <- emuR::load_emuDB(inside_of,verbose = FALSE)
      ) -> dbload.info
      logger::log_info(paste(dbload.info,collapse = "\n"))

    }else{
      strAttr <- attr(inside_of,"basePath")
      if(! is.null(strAttr) && stringr::str_ends(strAttr,"_emuDB") && dir.exists(strAttr)){
        # We then need to create a handle object
        utils::capture.output(
          inside_of <- emuR::load_emuDB(strAttr, verbose = FALSE)
        ) -> dbload.info
        logger::log_info(paste(dbload.info,collapse = "\n"))

      }else{
        #This is the fallback
        cli::cli_abort(c("Could not determine the location of the database",
                         "x"="The database location will be deduced from the {.arg inside_of} argument to the function.",
                         "x"="Derivation of the location will be attempted using the ",
                         "i"="{.arg inside_of} is of class {.val {class(inside_of)}}",
                         "i"="{.arg inside_of} has the attributes  {.val {names(attributes(inside_of))}}."))
      }

    }
  }

  res <- emuR::query(emuDBhandle=inside_of, query=query,sessionPattern = sessions_regex, bundlePattern = bundles_regex ,timeRefSegmentLevel = times_from, calcTimes = calculate_times,verbose = interactive, resultType = "tibble")

  attr(res,"basePath") <- inside_of$basePath #This ensures that we can reattach the database later
  class(res) <- c("segmentlist","tbl_df", "tbl","data.frame")

  return(res)
}

relate_to <- function(inside_of, segment_list,anchor_against=NULL){
  if(missing(inside_of)) cli::cli_abort("Missing {.args inside_of} argument.")
  if(missing(segment_list)) cli::cli_abort("A segment list is required")

  if("character" %in% class(source) ){
    # We then need to create a handle object
    utils::capture.output(
      handle <- emuR::load_emuDB(attr(inside_of,"basePath"),verbose = FALSE)
    ) -> dbload.info
    logger::log_info(paste(dbload.info,collapse = "\n"))
  }
  if( "emuDBhandle" %in% class(inside_of)){
    handle <- inside_of
  }

  res <- emuR::requery_seq(inside_of,seglist = segment_list,calcTimes = TRUE,offset=0,offsetRef = "START",length=1,resultType = "tibble",timeRefSegmentLevel = anchor_against)
  attr(res,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later
}


ascend_to <- function(.data,  .attribute_name ,.collapse = TRUE, .skip_times = FALSE, .times_from = NULL, .interactive=FALSE, .inside_of=NULL) {

  if(is.null(.inside_of) && ! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .source  <- emuR::load_emuDB(attr(.data,"basePath"),verbose = FALSE)
    ) -> dbload.info
  }else{
    logger::log_error("Could not derive the database path from the segment list.\n Please provide an explicit database handle object .source the .inside_of argument. See ?emuR::load_emuDB for details.")
  }

  res <- emuR::requery_hier(emuDBhandle = .source,seglist= .data,level=.attribute_name,timeRefSegmentLevel = .times_from, calcTimes = !.skip_times,verbose = .interactive, resultType = "tibble")
  attr(res,"basePath") <- .source$basePath #This ensures that we can reattach the database later
  if( is.null(.inside_of) ) {
    #This means that we created the emuDB database handlere here
    DBI::dbDisconnect(.source$connection) # Gracefully disconnect the connection
    rm(.source)
  }
  return(res)
}

descend_to <- ascend_to

#skip_forward
#skip_backward

scout <- function(.data,  steps_forward, count_from="START" , capture=1, ignore_bundle_boundaries=FALSE, calculate_times = TRUE, times_from = NULL, interactive=FALSE, .inside_of=NULL) {

  if(is.null(.inside_of) && ! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .inside_of <- emuR::load_emuDB(attr(.data,"basePath"),verbose = TRUE)
    ) -> dbload.info
  }else{
    stop("Could not derive the database path. Please provide an explicit database handle object .source the .inside_of argument. See ?emuR::load_emuDB for details.")
  }

  res <- emuR::requery_seq(emuDBhandle = .inside_of,seglist= .data,offset=steps_forward, offsetRef=count_from,length=capture, ignoreOutOfBounds = ignore_bundle_boundaries, timeRefSegmentLevel = times_from, calcTimes = calculate_times,verbose = interactive, resultType = "tibble")
  attr(res,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later
  #DBI::dbDisconnect(.inside_of$connection) # Gracefully disconnect the connection
  return(res)
}

retreat <- function() {1}

harvest <- function() {1}



peek_at <- function(.x, what=c("levels","links","labelgroups","tracks","bundles","sessions","perspectives","files","attributes","signals"),...){

  if("emuDBhandle" %in% class(.x)){
    .inside_of <- .x
  }else if("corpus" %in% class(.x)){
    .inside_of <- .x
  }else{
    if( ! is.null(attr(.x,"basePath")) && dir.exists(attr(.x,"basePath"))) {
      # We then need to create a handle object
      utils::capture.output(
        .inside_of <- emuR::load_emuDB(attr(.x,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      stop("Could not derive the database path. Please provide an explicit database handle object as '.x'. See ?emuR::load_emuDB for details.")
    }
  }
 al <- list(...)

  what <- match.arg(what, c("levels","links","labelgroups","tracks","bundles","sessions","perspectives","files","attributes","signals"))

  if(what == "labelgroups") {
    #A label can be globally or locally defined, so we need to check whether extra arguments were given.
    what <- ifelse(length(al) == 0, "global_lg","local_lg")
  }

  if(what == "signals") {
    return(peek_signals(.inside_of, ...))
  }

  res <- switch(what,
                levels = tibble::as_tibble(emuR::list_levelDefinitions(.inside_of)), # name         type    nrOfAttrDefs attrDefNames
                links = tibble::as_tibble(emuR::list_linkDefinitions(.inside_of)), #type         superlevelName sublevelName
                global_lg =tibble::as_tibble(emuR::list_labelGroups(.inside_of)), # name values
                perspectives = tibble::as_tibble(emuR::list_perspectives(.inside_of)), #name          signalCanvasesOrder levelCanvasesOrder
                tracks =tibble::as_tibble(emuR::list_ssffTrackDefinitions(.inside_of)), #name  columnName fileExtension
                bundles =tibble::as_tibble(emuR::list_bundles(.inside_of,...)), #session name
                sessions =tibble::as_tibble(emuR::list_sessions(.inside_of,...)), # name
                files = tibble::as_tibble(emuR::list_files(.inside_of,...)), #session bundle   file                absolute_file_path
                attributes = tibble::as_tibble(emuR::list_attributeDefinitions(.inside_of,...)), # name    level   type hasLabelGroups hasLegalLabels
                local_lg = tibble::as_tibble(emuR::list_attrDefLabelGroups(.inside_of,...)) #name values
  )

  attr(res,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later
  # We neeed these to fully specify the results later, and to separate global_lg and local_lg output from each other
  if(what == "local_lg") attr(res,"local_to") <-  al[c("levelName","attributeDefinitionName")]
  if(what == "attributes") attr(res,"local_to") <-  al[c("levelName")]

  #DBI::dbDisconnect(.inside_of$connection) # Gracefully disconnect the connection
  return(res)
}

peek_levels <- purrr::partial(peek_at, what="levels")
peek_links <- purrr::partial(peek_at, what="links")
peek_labelgroups <- purrr::partial(peek_at, what="labelgroups")
peek_tracks <- purrr::partial(peek_at, what="tracks")
peek_bundles <- purrr::partial(peek_at, what="bundles")
peek_sessions <- purrr::partial(peek_at, what="sessions")
peek_perspectives <- purrr::partial(peek_at, what="perspectives")
peek_attributes <- purrr::partial(peek_at, what="attributes")

#' List signal files in an emuDB or corpus
#' 
#' Returns a tibble with session, bundle, filename, extension, and full_path
#' for all signal files (audio files) in the database. Filters to only include
#' files with common audio extensions and the mediafileExtension from config.
#' 
#' @param .x An emuDBhandle, corpus object, or object with basePath attribute
#' @param ... Additional arguments passed to list_files
#' @return A tibble with columns: session, bundle, name, extension, full_path
#' @export
#' @examples
#' \dontrun{
#' signals <- peek_signals(corpus_obj)
#' # Or use peek_at
#' signals <- peek_at(corpus_obj, "signals")
#' }
peek_signals <- function(.x, ...) {
  
  # Get database handle or load from path
  if ("corpus" %in% class(.x)) {
    .inside_of <- .x
    basePath <- .x@basePath
    config <- .x@config
  } else if ("emuDBhandle" %in% class(.x)) {
    .inside_of <- .x
    basePath <- .x$basePath
    config <- load_DBconfig(basePath)
  } else {
    if (!is.null(attr(.x, "basePath")) && dir.exists(attr(.x, "basePath"))) {
      basePath <- attr(.x, "basePath")
      utils::capture.output(
        .inside_of <- emuR::load_emuDB(basePath, verbose = TRUE)
      ) -> dbload.info
      config <- load_DBconfig(basePath)
    } else {
      stop("Could not derive the database path. Please provide an explicit database handle object as '.x'.")
    }
  }
  
  # OPTIMIZED: Use direct file system scan instead of emuR::list_files
  # This is much faster for large databases
  
  # Common audio extensions plus mediafileExtension from config
  audio_exts <- c("wav", "WAV", "mp3", "MP3", "flac", "FLAC", "ogg", "OGG", 
                   "aiff", "AIFF", "aif", "AIF")
  if (!is.null(config$mediafileExtension)) {
    audio_exts <- unique(c(audio_exts, config$mediafileExtension))
  }
  
  # Scan file system directly
  sessions <- list.dirs(basePath, full.names = TRUE, recursive = FALSE)
  sessions <- sessions[grepl("_ses$", sessions)]
  
  signal_files_list <- list()
  idx <- 1
  
  for (session_dir in sessions) {
    session_name <- sub("_ses$", "", basename(session_dir))
    bundles <- list.dirs(session_dir, full.names = TRUE, recursive = FALSE)
    bundles <- bundles[grepl("_bndl$", bundles)]
    
    for (bundle_dir in bundles) {
      bundle_name <- sub("_bndl$", "", basename(bundle_dir))
      
      # Get all files in bundle
      files <- list.files(bundle_dir, full.names = TRUE)
      
      for (file_path in files) {
        ext <- tools::file_ext(file_path)
        if (ext %in% audio_exts) {
          signal_files_list[[idx]] <- list(
            session = session_name,
            bundle = bundle_name,
            name = tools::file_path_sans_ext(basename(file_path)),
            extension = ext,
            full_path = file_path
          )
          idx <- idx + 1
        }
      }
    }
  }
  
  # Convert to tibble
  if (length(signal_files_list) > 0) {
    signal_files <- tibble::tibble(
      session = vapply(signal_files_list, `[[`, character(1), "session"),
      bundle = vapply(signal_files_list, `[[`, character(1), "bundle"),
      name = vapply(signal_files_list, `[[`, character(1), "name"),
      extension = vapply(signal_files_list, `[[`, character(1), "extension"),
      full_path = vapply(signal_files_list, `[[`, character(1), "full_path")
    )
  } else {
    signal_files <- tibble::tibble(
      session = character(),
      bundle = character(),
      name = character(),
      extension = character(),
      full_path = character()
    )
  }
  
  attr(signal_files, "basePath") <- basePath
  
  return(signal_files)
}
peek_tracks <- purrr::partial(peek_at, what="tracks")
peek_bundles <- purrr::partial(peek_at, what="bundles")
peek_sessions <- purrr::partial(peek_at, what="sessions")
peek_perspectives <- purrr::partial(peek_at, what="perspectives")
peek_attributes <- purrr::partial(peek_at, what="attributes")

# Setter functions


prepare <- function(...) {
  UseMethod("prepare")
}

prepare.character <- function(.x,...){
  if(!file.exists(.x)){
    cli::cli_abort("Unable to find database location {.file {(.x)}}")
  }
  # We then need to create a handle object
  utils::capture.output(
    .inside_of <- emuR::load_emuDB(.x,verbose = TRUE)
  ) -> dbload.info
  logger::log_info(paste(dbload.info,collapse ="\n"))
  prepare(.inside_of,...)
}

prepare.data.frame <- function(.x,...){
  if( ! is.null(attr(.x,"basePath")) && dir.exists(attr(.x,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .inside_of <- emuR::load_emuDB(attr(.x,"basePath"),verbose = TRUE)
    ) -> dbload.info
    logger::log_info(paste(dbload.info,collapse ="\n"))
  }else{
    cli::cli_abort(c("Could not derive the database path.",
      "i"= "Please provide an explicit database handle object in {.arg .x}.",
      "i"= "See ?emuR::load_emuDB for details."))

  }
  prepare(.inside_of,...)
}

prepare.emuDBhandle <- function(.inside_of, what=c("level","link","global_lg","perspective","attribute","local_lg","legals"),...){

  what <- match.arg(what, c("level","link","global_lg","bundle","session","perspective","files","attributes","local_lg"))

  al <- list(...)
  if(what == "level" ) {
    if(al[["name"]] %in% emuR::list_levelDefinitions(emuDBhandle = .inside_of)[["name"]]){
      logger::log_warn('The level with the name \'{al[["name"]]}\' is already defined. .source the existing definition')
    }else{
      if(all(names(al) %in% methods::formalArgs(emuR::add_levelDefinition))) {
        logger::log_info('Adding a level named {al[["name"]]} to the database.')
        utils::capture.output(
          emuR::add_levelDefinition(emuDBhandle = .inside_of,...)
        ) -> dbload.info
        logger::log_info(paste(dbload.info,collapse ="\n"))
      }else{
        miss <- setdiff(names(al), methods::formalArgs(emuR::add_levelDefinition))
        logger::log_error('Arguments {paste(miss,sep=",")} are not defined. No level named {al[["name"]]} was added.')
      }
    }
    res <- describe(.x=.inside_of,what)
  }


  return(res)
}

add_tier <- purrr::partial(prepare,what="level")
add_link <- purrr::partial(prepare,what="link")
add_perspective <- purrr::partial(prepare,what="perspective")
add_attribute <- purrr::partial(prepare,what="attribute")
add_labelgroup <- purrr::partial(prepare,what="labelgroup")
add_glabelgroup <- purrr::partial(prepare,what="glabelgroup")


describe_level <- function(.x,name,type= c("SEGMENT","EVENT","ITEM")){
  type <- head(type,1)
  if(is.null(type)) stop("Missing level type")
  if(missing(name)) stop("A level name is required")

  res <- prepare(.x,what="level",name=name,type=toupper(type))
}




quantify <- function(...) {
  UseMethod("quantify")
}

quantify.character <- function(.what,...){
  if(missing(.what)) cli::cli_abort("Missing manditory argument {.arg .what}.")
  if(! file.info(path.expand(.what))[["isdir"]] || ! dir.exists(path.expand(.what))){
    cli::cli_abort(c("The {.arg .what} is not a valid path.",
                     "i"="The database path searched was {.path {path.expand(.what)}}"))
  }
  if(! stringr::str_ends(basename(path.expand(.what)), emuR:::emuDB.suffix)){
    cli::cli_abort(c("The {.arg .what} does not seem to point to the location of a database",
                     "x"="The database directory should end with {.val {emuR:::emuDB.suffix}}.",
                     "i"="The database path searched was {.path {path.expand(.what)}}"))
  }
  utils::capture.output(
    .what <- emuR::load_emuDB(attr(.what,"basePath"),verbose = FALSE)
  ) -> dbload.info
  logger::log_info(paste(dbload.info,collapse = "\n"))
  #Now of class emuDBhandle
  quantify(.what,...)
}


quantify.data.frame <- function(.what,...,.inside_of){
  mandatory <- c("labels", "start", "end", "db_uuid", "session", "bundle", "start_item_id",
                 "end_item_id", "level", "attribute", "start_item_seq_idx", "end_item_seq_idx",
                 "type", "sample_start", "sample_end", "sample_rate")
  if(! base::setequal(mandatory,names(.what))) {
    cli::cli_abort(c("The {.arg .what} does not contain the columns required to make a {.cls segmentlist}.",
                   "i"="Missing field{?s} {.val {base::setdiff(mandatory,names(.what))}}."))
  }
  attr(.what,"basePath") <- .inside_of$basePath
  class(.what) <- c("segmentlist",class(.what) )
  quantify(.what,...)
}


quantify.segmentlist <- function(.what,.source,...,.where=NULL,.n_preceeding=NULL,.n_following=NULL,.by_maxFormantHz=TRUE,.cache_in_file=FALSE,.metadata_defaults=list("Gender"="Undefined","Age"=35),.naively=FALSE,.recompute=FALSE,.dsp_settings_by=c("Gender","Age"),.package="superassp",.parameter_log_excel=NULL,.inside_of=NULL,.input_signal_file_extension=NULL,.verbose=FALSE,.quiet=FALSE){


  ## Explicit and Hard coded arguments ----------------------------------------------------

  dotdotArgs <- rlang::list2(...)

  .cache_connection <- NULL

  if(is.null(.cache_in_file) || missing(.cache_in_file)) {
    .cache_in_file <- FALSE
    .cache_connection <- NULL
  }else{
    if(is.character(.cache_in_file)){
      if(file.exists(.cache_in_file)){
        .cache_in_file <- normalizePath(.cache_in_file)
        cli::cli_alert_info("I will use the existing cache file {.file {(.cache_in_file)}} to store results.")
      }
      if(isTRUE(.cache_in_file)){
        .cache_in_file <- file.path(tempdir(check=TRUE),format(Sys.time(), paste0(funName,"%Y%m%d.sqlite")))
      }

    }

  }


  ## Cache system setup -----------------------------------------



  if(isFALSE(.cache_in_file)){
    #the case when no cache file should be used
    cli::cli_alert_info("Intermediate results are {.strong not} cached.")
  }else{

    .cache_in_file <- path.expand(.cache_in_file)
    cacheFileExists <- file.exists(.cache_in_file)
    cli::cli_alert_info("Using  cache file {.file {(.cache_in_file)}}")

    #Clear cache file if indicated
    if(.clear_cache && cacheFileExists){
      suppressWarnings( unlink(.cache_in_file))
      cacheFileExists <- file.exists(.cache_in_file)
    }
    suppressWarnings(
      .cache_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                          user="root",
                                          password="",
                                          host="localhost",
                                          dbname=.cache_in_file,
                                          flags=RSQLite::SQLITE_RWC,
                                          loadable.extensions=FALSE)
    )
    #Check table format requirements
    if(length(RSQLite::dbListObjects(.cache_connection)) > 0 &&
       (! "cache" %in% RSQLite::dbListTables(.cache_connection) ||
        ! setequal(c("sl_rowIdx","obj"),RSQLite::dbListFields(.cache_connection,"cache")))
    ){
      cli::cli_alert_info("Initializing cache file {.file {(.cache_in_file)}}")
      RSQLite::dbExecute(.cache_connection,"DROP TABLE IF EXISTS cache;")
    }
    RSQLite::dbExecute(.cache_connection,
                       "CREATE TABLE IF NOT EXISTS cache(sl_rowIdx INTEGER PRIMARY KEY, obj BLOB );")


  }


  # Initial check of arguments ----------------------------------------------

  #Check for the special case where the function was called by furnish
  # TODO Remove and instead use a special bundlelist class / object
  calledByFurnish <- FALSE
  if(setequal(names(.what),c("start_item_id","bundle", "end", "end_item_id", "session", "start"))){
    calledByFurnish <- TRUE
    #Now check for strange arguments
    if(! is.null(.where) || ! is.null(.n_preceeding) ||! is.null(.n_follwing)){
      if(.verbose && ! .quiet) cli::cli_alert_info(c("Ignoring {.arg .where}, {.arg .n_preceeding}, and {.arg .n_follwing} arguments",
                            "i"="Specifying a subset of data points to be returned does not make sense for {.fun furnish}."
                            )
                          )
    }
    #Clear erroneous args
    dotdotArgs[".where"] <- NULL
    dotdotArgs[".n_preceeding"] <- NULL
    dotdotArgs[".n_follwing"] <- NULL
    if("toFile" %in% names(dotdotArgs) && (.verbose || ! .quiet)) {
      cli::cli_alert_info(c("The function {.fun quantify} returns an acoustic summary of segments and never changes the database",
                                                                                        "x"="Ignoring the {.arg toFile} argument to the DSP function."))
    }

  }
  #To file if furnish, not to file if not furnish
  dotdotArgs["toFile"] <- calledByFurnish

  logger::log_debug("... args contains : {names(dotdotArgs)}")
  logger::log_debug("... is a  : {class(dotdotArgs)}")


  ## Attach the database ------------------
  # * Use an expliclitly given .inside_of as the database path
  # * If the segmentlist has a "basePath" attribute that works, use it
  # - Otherwise, error out
  .inside_of <- dplyr::coalesce(.inside_of, attr(.what,"basePath"),NA)
  if(! is.na(.inside_of) &&
     dir.exists(path.expand(.inside_of)) &&
     stringr::str_ends(basename(path.expand(.inside_of)), emuR:::emuDB.suffix)){
      utils::capture.output(
      .inside_of <- emuR::load_emuDB(.inside_of,verbose = FALSE)
    ) -> dbload.info #Maybe useful later?
  }else{
    #Everything failed
    whatName <- as.character(rlang::call_args(rlang::current_call())$.what)
    cli::cli_abort(c("Unable to establish a connection to the database from which the segmentlist {.args {whatName}} came from.",
                     "i"="Tried the {.path {c(attr(.what,\"basePath\"),.inside_of)}} database location{s}.")
    )
  }

  assertthat::assert_that( "emuDBhandle" %in% class(.inside_of))
  #From now on we can be sure to have a valid database handle in .inside_of

  # This sets the default input media file extension, which handled the case when a
  #function is called to compute a list or SSFF track result based on a wave file
  # This variable will then be set to something else if
  # we want to read in a pre-computed SSFF track stored on disk instead.

  inputSignalsExtension <- ifelse(is.null(.input_signal_file_extension),
                                  emuR:::load_DBconfig(.inside_of)$mediafileExtension,
                                  .input_signal_file_extension)
  ## Source setup ------------------------------------------------------------


  definedTracks <- emuR::list_ssffTrackDefinitions(.inside_of)
  #The reason why we need to split off .source into a separate .f is that is needed to make the readtrack application case below behave identically parameter-wise to this case when we apply a DSP function
  .f <- NULL

  ### The Block where character .source argument is handled ----
  if(is.character(.source)) {
    if(.source %in% definedTracks$name){
      ### The .source is just the name of a track --------
      if("field" %in% names(dotdotArgs)){
        te <- c(field,.source)

        if(!.quiet) cli::cli_warn(c("Both a {.args field} and a {.arg {(.source)}} argument was supplied.",
                                    "i"="Reading the {.field field} track in {.val {inputSignalsExtension}} signal files."))
      } else {
        dotdotArgs$field <- .source
        if(! .quiet) cli::cli_alert_info("Reading data from the {.field {(.source)}} in {.val {inputSignalsExtension}} signal files.")

      }

      .f <- readtrack
      funName <- "readtrack"

      #Get the file extension of the input track , overwriting the default "wav" if
      inputSignalsExtension <- definedTracks[definedTracks$name == dotdotArgs$field,"fileExtension"]

      #It makes no sense to recompute or deduce DSP settings if the data is to be loaded from a stored track
      if(( !.naively || .recompute) && (!.quiet || .verbose)) {
        cli::cli_bullets(c("Directly reading the {.field .source} track require no DSP parameter deduction.",
                           ">"="The processing will ignore the {.arg .recompute} argument and assume {.arg .naively=TRUE}."))
      }
      .naively <- TRUE
      .recompute <- FALSE

      # Disable the automatic cache file
      .cache_in_file <- FALSE

    }else{ ## The character .source is not the name of a track
      safe_get <- purrr::safely(get)

      if(is.function(safe_get(.source)$result)){
        funName <- .source
        .f <- safe_get(.source)$result

      }
    }
  }

  ### Preparation of application of .source as a function -----------------

  if(is.null(.f) ){  #Skip if .f it set already
    if( is.function(.source) ){
      #Just need to establish the name of the function from the call
      funName <- as.character(rlang::call_args(rlang::current_call())$.source)
      .f <- .source
    }else{ # Not of a function class, and we already concluded before that it is not a track name or a function name in a string
      cli::cli_abort(c("Unable to deduce the source of acoustic data",
                       "x"="The {.arg .source} argument must be either an existing track defined in the database, a function (or the name of one in a character string).",
                       "i"="You indicated {.arg .source={(.source)}} in the call to {.fun quantify}.",
                       "i"="Defined tracks {?is/are} {.field {definedTracks$name}}."))
    }
  }

  assertthat::assert_that(!is.null(.f))

  # This version of the original function .f that is guarantee to return a list of $result (which is possibly NA) and $error
  safe_f <- purrr::possibly(.f, otherwise=NA)

  assertthat::assert_that(!is.null(safe_f) && is.function(safe_f))

  if(! .quiet) cli::cli_alert_info("Applying the function {.fun {funName}} to the segment list.")

  ## Definition and setup directly related to the inner applied function --------

  processAndStore <- function(.sl_rowIdx,.session,.bundle,.where=NULL, .n_preceeding=NULL,.n_following=NULL,...){

    dotdotdot <- list(...)
    dotdotdotS <- toString(dotdotdot)
    result <- NULL


    if(!is.null(.cache_connection)){
      #Check existing values)

      cachedObj <- RSQLite::dbGetQuery(.cache_connection,"select obj from cache where sl_rowIdx = ?",.sl_rowIdx)

      if(nrow(cachedObj) > 0){
        #Load rather than comput
        logger::log_debug("[{.session}:{.bundle})] Loading data from cache file.\n")

        result <- base::unserialize(
          base::charToRaw(
            cachedObj$obj[[1]])
        )
      }else{
        logger::log_debug("[{.session}:{.bundle})] No data existing to load from cache file.\n")
      }

    }
    logger::log_trace("[{.session}:{.bundle})] Before applying results.\n")


    #Compute if the results is still not defined
    if(is.null(result) || is.na(result) ){

      result <- safe_f(...)

      glimpse(class(result))

      logger::log_trace("[{.session}:{.bundle})] Result is NULL :: {is.null(result)}")
      #Optionally make the cutout from the result directly, if the result was not NA
      if(!is.null(.where) && ! all(is.na(result)) && "AsspDataObj" %in% class(result) ){

        logger::log_debug("[{.session}:{.bundle})] Extracting points at {.where} {.n_preceeding}--{.n_following}\n")
        .n_preceeding <- ifelse(is.null(.n_preceeding) ||  .n_preceeding < 1 ,0,.n_preceeding)
        .n_following <- ifelse(is.null(.n_following) || .n_following < 1 ,0,.n_following)
        result <- cut(result,where=.where,n_preceeding=.n_preceeding,n_following=.n_following)
        logger::log_debug("[{.session}:{.bundle})] Extracting points at {.where} {.n_preceeding}--{.n_following}\n")
      }
    }


    #Store the results in cache, if there is one
    if(!is.null(.cache_connection)){
      objser <- base::rawToChar(base::serialize(result, connection = NULL,ascii = TRUE))
      RSQLite::dbExecute(.cache_connection,"INSERT OR REPLACE INTO cache (sl_rowIdx,obj) VALUES (?,?);", c(.sl_rowIdx,objser))
    }
    return(result)
  }


  ### Define what information will be given in the spinner -----
  spinner <- "{cli::pb_spin} Processing ({cli::pb_current} of {cli::pb_total}) {cli::pb_bar} | ETA: {cli::pb_eta}"
  mergespinner = "{cli::pb_spin} Merging track portions a single tibble ({cli::pb_current} of {cli::pb_total}) {cli::pb_bar} | ETA: {cli::pb_eta}"
  ## Rewrite the spinner if doing extraction from an AsspDataObj ------------------------------------------------
  if(!is.null(.where) && ! missing(.where) ){
    spinner <- ifelse(.n_preceeding == 0 && .n_following == 0,
                      paste0("{cli::pb_spin} Exctacting from the ",.where*100,"% relative time point from segment {cli::pb_current} of {cli::pb_total}) {cli::pb_bar} | ETA: {cli::pb_eta}"),
                      paste0("{cli::pb_spin} Extracting from the ",.where*100,"% relative time point (-",.n_preceeding," and +",.n_following," points) from segment {cli::pb_current} of {cli::pb_total}) {cli::pb_bar} | ETA: {cli::pb_eta}")
    )
  }


  pb <- list(
    type = "iterator",
    format = spinner,
    clear = TRUE)

  pbmerge <- list(
    type = "iterator",
    format = mergespinner,
    clear = TRUE)

  if(nrow(.what ) < 1){
    #Nonsense segment list
    cli::cli_abort(c("Erroneous segment list argument",
                     "x"="The list of segments or signals is empty",
                     "i"="The {.arg .what} segmentlist contains {nrow(.what)} rows."))
  }
  if(any(is.na(.what[c("start","end")]))){
    #We need to have all start and end times specified to proceed
    if(!.quiet) cli::cli_alert_info(c("Missing time references",
                                      "x"="Some start or end times in the segment list supplied as the {.param .what} argument is missing",
                                      "i"="I will try to deduce times from associated levels with time information."))
    .what <- anchor(.what)
  }


  #Logic that concerns formal arguments
  formalArgsNames <- methods::formalArgs(.f)
  functionDefaults <- as.list(formals(.f))
  #toFile can never be TRUE in this function
  if("toFile" %in% formalArgsNames) functionDefaults$toFile <- FALSE

  if(! "listOfFiles" %in%  formalArgsNames) cli::cli_abort(c("No method to supply input files to {.fun {funName}}",
                                                             "x"="Functions provided as {.arg .source} to {.fun quantify} is assumed to have a {.arg listOfFiles} formal argument where file paths are given"))




  signalFiles <- emuR::list_files(.inside_of,inputSignalsExtension)  |>
    dplyr::rename(listOfFiles=absolute_file_path)  |>
    dplyr::select(-file) |>
    dplyr::mutate(tibble::as_tibble(dotdotArgs))  # Set explicitly set arguments

  willRemove <- setdiff(names(dotdotArgs),formalArgsNames)
  if(!.quiet && length(willRemove) > 0) cli::cli_alert_info("Ignoring the {.arg {willRemove}} arguments as they are not used by {.fun {funName}}")


  ## Prepare the environment for DSP function application

  if(! .naively){
    ### Deduce DSP settings based on metadata -----------------------------------
    if(!.quiet || .verbose) cli::cli_alert_info("Using metadata to derive DSP settings where not explicitly set by the user.")

    # Make sure that we have a DSP default settings data.frame
    dsp <- reindeer:::dspp_metadataParameters(recompute=.recompute)  |>
      tidyr::replace_na(list("Gender"="Undefined"))

    if(( !.quiet || .verbose ) && nrow(dsp) > 0 ){
      if(.recompute){
        cli::cli_alert_success(cli::ansi_strwrap("Recomputed DSP settings age and gender appropriate DSP settings based on metanalysis data in the {.file { file.path(system.file(package = \"reindeer\",mustWork = TRUE),\"default_parameters.xlsx\")  }} file."))
      }else{
        cli::cli_alert_success("Loaded precomputed age and gender appropriate DSP settings")
      }
    }

    #After this we can be sure that parameters set per session or bundle are available
    # but have been overwritten by explicitly set settings given to this function as we proceed
    meta <- reindeer:::get_metadata(.inside_of,manditory=names(.metadata_defaults))  |>
      dplyr::mutate(Gender=as.character(Gender),Age=as.integer(round(Age,digits = 0)))  |>
      tidyr::replace_na(.metadata_defaults)


    completedStoredDSPSettings <- meta |>
      dplyr::left_join(dsp,by = .dsp_settings_by)


    # We need to choose whether to guide a formant tracker by number of extracted formants in a fixed 0-5000 Hz frequency range
    # or if the 5000 Hz ceiling is instead increased and the default number of formants extracted is kept constant

    if(.by_maxFormantHz && "numFormants" %in% names(completedStoredDSPSettings)){
      completedStoredDSPSettings <- completedStoredDSPSettings  |>
        dplyr::select(-numFormants)
    }
    # The names of columns to keep are now the columns that are defined either in signalFiles or
    # in dspCompletedSettings, and which will be used by the DSP function
    settingsThatWillBeUsed <- intersect(formalArgsNames,
                                        union(names(completedStoredDSPSettings),
                                              union(
                                                names(signalFiles),
                                                names(dotdotArgs)
                                              )
                                        )
    ) # This will be used to remove unwanted columns

    #Here we construct the settings that we want to use when applying the specific DSP
    # function to bundles (in specific sessions)
    sessionBundleDSPSettingsDF <-  completedStoredDSPSettings  |>
      dplyr::left_join(signalFiles,by=c("session","bundle"))  |>
      dplyr::select(session,bundle,all_of(settingsThatWillBeUsed))


  }else{
    ## Naive DSP arm -----------------------------------

    sessionBundleDSPSettingsDF <- signalFiles
    # The names of columns to keep are now the columns that are defined  in signalFiles and which will be used by the DSP function
    settingsThatWillBeUsed <- intersect(formalArgsNames,
                                        union(
                                          names(signalFiles),
                                          names(dotdotArgs)
                                          )
    )


  }

  ### Assert that we have a time window, if it should be defined

  if(all(c("start","end") %in% names(.what))){
    #From now on, .what is "fixed" so that the name of start and end time specifications are aligned with what
    #DSP functions (including IO) expects

    .what <- .what |>
      dplyr::mutate(beginTime = start /1000, endTime = end /1000)

    settingsThatWillBeUsed <- c(settingsThatWillBeUsed, c("beginTime","endTime"))
  }

  ## Optionally log the parameter table to an Excel output -------------------

  if(!is.null(.parameter_log_excel)) {

    #There is no reason for file path normalization to not work in this call
    # so the warnings given by default for the not yet existing file will just be ignored
    .parameter_log_excel <- path.expand(.parameter_log_excel)

    if(is.character(.parameter_log_excel) && dir.exists(dirname(.parameter_log_excel))){
      #Write a new excel file, named either according to the specified name in
      #.parameter_log_excel or as a generated file name (if the string is a directory path)
      #cli::cli_abort("{.path {dirname(.parameter_log_excel)}} {.path {(.parameter_log_excel)}}")
      excel_filename <- ifelse( file.info(.parameter_log_excel)[["isdir"]] && dir.exists(.parameter_log_excel),
                                file.path(.parameter_log_excel,
                                          paste0(stringr::str_replace_all(format(Sys.time(),usetz = TRUE)," ","_"),".xlsx")),
                                .parameter_log_excel)


      openxlsx::write.xlsx(x = sessionBundleDSPSettingsDF,file=excel_filename, asTable=FALSE, overwrite = TRUE)


      cli::cli_alert_success("Wrote DSP parameters used into {.file {excel_filename}}")
      cli::cli_bullets(c(">"="Please note that default parameters of the called DSP function are not included in the log as they cannot unambigiously be deduced."))

    }else{
      #Not able to create a parameter file or deduce a name for it
      cli::cli_abort(c("Unable to write DSP parameter file",
                       "x"= "A parameter file could not be created in {.file {(.parameter_log_excel)}}"))
    }

  } #

  ## End of arm where a DSP function is applied to an input signal








  ## Finish up creating the environment suitable for collecting quantification parameters --------

  # Now we need to transfer the session / bundle settings to the segment list
  # to get start and end times into the call also
  segmentDSPDF <- .what  |>
    tibble::rownames_to_column(var = "sl_rowIdx")  |>
    dplyr::left_join(sessionBundleDSPSettingsDF,by=c("session","bundle"))  |>
    dplyr::mutate(tibble::as_tibble(dotdotArgs)) |> # Set explicitly set arguments
    dplyr::select(all_of(c("session","bundle",settingsThatWillBeUsed)))  |>
    dplyr::rename(.session=session,.bundle=bundle)  |>
    dplyr::mutate(dplyr::across(where(is.integer), as.numeric)) |> ## Fix for wrassp functions that expect "numeric" values, not integers
    tibble::rownames_to_column(var = ".sl_rowIdx")  |>
    dplyr::mutate(.sl_rowIdx = as.integer(.sl_rowIdx)) #Make sure we have a link back to the segmentlist row


  fillableArgs <- intersect(
    formalArgs(.f), #List of default settings
    names(which(colSums(is.na(segmentDSPDF)) > 0)) #Columns with NAs that needs to be filled in
    )
  defaultArgs <- as.list(formals(.f))[fillableArgs]

  segmentDSPDF <- segmentDSPDF  |>
    tidyr::replace_na(replace=defaultArgs) |>
    dplyr::mutate(.where=.where, .n_preceeding=.n_preceeding,.n_following=.n_following, .after=.bundle)

  # Do the actual quantification  work --------------------------------------------


  # Here we apply the DSP function once per row and with settings comming from
  # the columns in the data frame


  appliedDFResultInList <- segmentDSPDF |>
    dplyr::rowwise()  |>
    purrr::pmap(.f=processAndStore,.progress = pb) # This is the busy line

  ## [special] The case where .what is called by furnish() --------------------------------------------

  if(calledByFurnish){
    #Activate the special case at the end of processing where SSFF tracks and lists are not expanded
    # but just returned
    logger::log_debug("Preparing to return a tibble of bundles and AsspDataObj")

    provideOutDF <-  emuR::list_bundles(.inside_of)  |>
      dplyr::mutate(dobj=appliedDFResultInList)  |>
      dplyr::rename(bundle=name)


    attr(provideOutDF,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later

    return(provideOutDF)

  }else{
    ##  [default] The case in which we are processing a segment list --------------------------------------------

    resTibble <- appliedDFResultInList  |>
      purrr::map_dfr(tibble::as_tibble, .id="sl_rowIdx") |>
      dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))


    ### Prepare return set  ------------------------------------------------

    quantifyOutDF <- .what  |>
      tibble::rownames_to_column(var = "sl_rowIdx")  |>
      dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))  |>
      dplyr::left_join(resTibble, by="sl_rowIdx",multiple="all")  |>
      dplyr::arrange(sl_rowIdx,start_item_id,end_item_id)

    attr(quantifyOutDF,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later

    return(quantifyOutDF)
  }

}

quantify_naively <- purrr:::partial(quantify, .naively=TRUE)


# furnish.data.frame <- function(.inside_of,...){
#   if(!"basePath" %in% attr(.inside_of)){
#     cli::cli_abort(c("Unable to deduce databse path",
#                      "x"="The function {.fun furnish} has to derive a database path to perform processing",
#                      "i"="The function was given a {.cls {class(.inside_of)}} with the attributes {.field {attributes(.inside_of)}}",
#                      "x"="The {.arg .inside_of} {.cls data.frame} needs to have a {.field basePath} attribute attached to it with the full path."))
#   }
#   utils::capture.output(
#     .inside_of <- emuR::load_emuDB(attr(.inside_of,"basePath"),verbose = FALSE)
#   ) -> dbload.info
#   logger::log_info(paste(dbload.info,collapse = "\n"))
#   furnish(.inside_of,...)
# }




quantify.emuDBhandle <- function(.what,.source, ... ,.force=FALSE,.really_force=FALSE,.metadata_defaults=list("Gender"="Undefined","Age"=35),.by_maxFormantHz=TRUE,.recompute=FALSE,.naively=FALSE,.parameter_log_excel=NULL,.verbose=TRUE,.quiet=FALSE){


  # Check inputs ------------------------------------------------------------

  if(missing(.source)) {
    cli::cli_abort(c("Missing source name",
                                      "i"="You need to state a function name or the file extension from which the data columns should be gathered",
                                      "x"="The argument {.var .source} is missing"))
  }


  if(!is.null(.where)){
    #Correct cutout specification if strange.
    .n_preceeding <- ifelse(is.null(.n_preceeding) || ceiling(.n_preceeding) < 1 ,0L,.n_preceeding)
    .n_following <- ifelse(is.null(.n_following) || ceiling(.n_preceeding) < 1 ,0L, .n_following)
  }

  # Investigate what is already in the database -----------------------------

  ## File extensions of track files in the database that the user can attach a track specification to -----------------------------

  availableDataFileExtensions <- emuR::list_files(.what)  |>
    dplyr::mutate(ext=tools::file_ext(absolute_file_path) ) |>
    dplyr::filter(!ext %in% c("json",
                              emuR:::load_DBconfig(.what)$mediafileExtension,
                              reindeer:::metadata.extension))  |>
    dplyr::select(ext)  |>
    dplyr::distinct()  |>
    purrr::pluck("ext")


  ## Check that track specifications given in ... are ok
  dotdotArgs <- rlang::dots_list(...,.named = TRUE, .homonyms="error", .check_assign=TRUE)

  #Overwrite the file extension used by the default by the function is the user said so


  ## Deduce file extension of output files -----------------------------------


  fileExtension <- NULL
  if(!is.null(dotdotArgs$explicitExt)){
    #An explicit file extension is given
    fileExtension <- dotdotArgs$explicitExt
  }else{
    if(is.character(.source) && (as.character(.source) %in% availableDataFileExtensions)){
      #The .source is the file extension of a signal file
      fileExtension <- as.character(.source)
    }else{
      #Our last bet is that .source is a function name  or a function

      if(is.character(.source) || is.function(.source)){
        fileExtension <- purrr::safely(superassp::get_extension(.source), otherwise = NULL, quiet = TRUE)
      }

      if(is.null(fileExtension)){
        #We failed to derive a file extension
        cli::cli_abort(c("Failed to derive a file extension to use",
                         "x"="To furnish a database with new pre-computed data, we need to derive a file extension",
                         "i"="The extension supplied as the optional {.arg explicitExt} argument is {.val {dotdotArgs$explicitExt}}.",
                         "i"="The {.arg .source} argument is set to {.val {.source}}.",
                         "i"="If considered a function, the {.arg .source} argument is reported to default to using a {.val {.source}} file extension.")
                       )
      }
    }
  }

  # We need to make a data.frame structure from the track specifications given as arguments,
  # as this will simplify comparison of track name and field with the already created
  # later in the code.

  tracksToDefine <- data.frame(name=names(dotdotArgs),
                               columnName=unlist(dotdotArgs,use.names = FALSE),
                               fileExtension=fileExtension)  |>
    dplyr::mutate(across(everything(), ~ stringr::str_remove_all(.,"[\'\"]") ))


  #This is the base structure which says what is already defined,
  # which we will use later to deduce if the user supplied track specifications are
  # valid or not
  tracksAlreadyDefined <- emuR::list_ssffTrackDefinitions(.what)



  # Create tracks and connect fields -------------------------------------------

  ## [a] The application of a function to create tracks -------------------------------------------

  #If we have a function, then we should use it to create new tracks
  if(is.function(.source) || ( ! is.null(get0(.source)) && is.function( get0(.source)))){

    ### SSFF functions ----------------------------------------------------------

    # First check that we have an SSFF track returning funciton

    if(!is.null(attr(.source,"outputType")) &&  attr(.source,"outputType") =="SSFF"){

      #### Really make sure that we do not overwrite signal files unless intended -------------------------------------------

      if(.force && ! .really_force){
        cli::cli_alert_warning("You have indicated that existing signal tracks should be overwritten.")
        cli::cli_alert_warning("This is a very invasive process and requires confirmation.")
        if(cli::has_keypress_support()){
          cli::cli_alert_warning("Please confirm that processing should proceed (y/n)")
          kp <- cli::keypress(block = TRUE)
          if(! kp %in% c("y","Y") ) cli::cli_abort(c("Processing aborted",
                                                     "x"="Files will only be overwritten if you press 'y' or 'Y",
                                                     "x"="You can also confirm overwriting by supplying {.arg .really_force=TRUE} to the function",
                                                     "i"="The {.fun furnish} commant was called with {.arg .force={(.force)}} and {.arg .really_force={(.really_force)}}"),call=NULL)
        }else{
          cli::cli_abort(c("Processing aborted",
                           "x"="You must confirm overwriting by supplying {.arg .really_force=TRUE} to the function",
                           "i"="The {.fun furnish} commant was called with {.arg .force={(.force)}} and {.arg .really_force={(.really_force)}}"))
        }

      }

      if(.verbose) cli::cli_alert_info("Using the {.val {fileExtension}} file extension for stored signal files.")

      #### Prepare the segment list that "quantify" will need -------------------------------------------

      # We here make a fake segment list with start and end times that suggest whole file processing
      # so that we can reuse quantify to deduce parameters, optionally log parameters, do the DSP work, and all that stuff.

      bundles <- emuR::list_bundles(.what)  |>
        dplyr::rename(bundle=name)  |>
        dplyr::mutate(start=0, end=0,start_item_id=0,end_item_id=0)  |>
        dplyr::mutate(.file= file.path(.what$basePath,
                                      paste0(session,emuR:::session.suffix),
                                      paste0(bundle,emuR:::bundle.dir.suffix),
                                      paste(bundle,fileExtension,sep=".")))
      bundles <- bundles  |>
        dplyr::mutate(.force=.force)  |>
        dplyr::filter( .force |  !file.exists(.file))  |>
        dplyr::select(-.file,-.force) #Make sure this variable does not mess up execution of the function

      attr(bundles,"basePath") <- .what$basePath

      if(nrow(bundles) < 1){
        #We deliberately do not raise an error here since we want to be able to continue processing even when
        # nothing needs to be done
        if(!.quiet) cli::cli_alert_success("Skipping the signal processing")
        if(!.quiet) cli::cli_alert_info("No signal files needs updating and updating is not forced.")
        if(!.quiet && .verbose) cli::cli_alert_info("The database contains {.val {nrow(emuR::list_bundles(.what))}} bundles")
        if(!.quiet && .verbose ) cli::cli_alert_info("There are {.val {nrow(emuR::list_files(.what,fileExtension))}} signal files with the file extension {.val {fileExtension}}")

        # In this case we are not doing any DSP and all that is left is to return the database handle.
        return(.what)
      }

      # We start fuzzing about ... arguments that are not valid function arguments
      # and that do not describe a connection to a field in the output as reported by the function
      makeAFuzzAbout <- tracksToDefine  |>
        dplyr::filter(! name %in% methods::formalArgs(.source), ! columnName %in% superassp::get_definedtracks(.source))

      if(nrow(makeAFuzzAbout) > 0 ){
        cli::cli_alert_warning(c("Skipping computation of {.var {makeAFuzzAbout$name}}",
                                 "x"="The fields {.val {makeAFuzzAbout$columnName}} are not computed by the function supplied as {.arg .source}",
                                 "x"="The fields {.val {makeAFuzzAbout$columnName}} also not not a formal argument to the function supplied as {.arg .source}",
                                 "i"="The formal arguments of the function is {.val {formalArgs(.source)}}"))

      }


      #Explicitly remove track name specifications with names that are also formal
      # args of the function, or are not computed by the .source function
      tracksToDefine <- tracksToDefine  |>
        dplyr::filter(! name %in% methods::formalArgs(.source), columnName %in% superassp::get_definedtracks(.source))


      cli::cli_alert_info("Will compute tracks and attach tracks {.field {tracksToDefine$name}} for {.val {nrow(bundles)}} bundles.")


      #### Finally call quantify to do the DSP work --------------------------------


      # Here we envoke a special mode of quantify where an intermediate nested result is returned
      quantifications <- quantify(.what=bundles,
                                  .source={{.source}},
                                   {{as.list(...)}},
                                  .metadata_defaults=.metadata_defaults,
                                  .by_maxFormantHz=.by_maxFormantHz,
                                  .recompute=.recompute,
                                  .package=.package,
                                  .naively = .naively,
                                  .parameter_log_excel=.parameter_log_excel)


      quantifications <- quantifications  |>
        dplyr::mutate(file= file.path(.what$basePath,
                                             paste0(session,emuR:::session.suffix),
                                             paste0(bundle,emuR:::bundle.dir.suffix),
                                             paste(bundle,fileExtension,sep=".")))  |>
        dplyr::select(dobj,file)  |>
        dplyr::filter(.force || !file.exists(file)) #Filter out files that exists if not .force(d)


     #Now finally write SSFF files
     quantifications  |>
       furrr::future_pwalk(wrassp::write.AsspDataObj)


    }else{

  # [b] The attachment of tracks to fields in already prepared files ------------

      # we have a file extension only

      if(!.quiet ||.verbose) cli::cli_alert_info("Considering signal files with the {.val {fileExtension}} file extension.")

      #First check that the files actually exist
      wantedSignalfiles <- emuR::list_files(.what,fileExtension = fileExtension)

      nbundles <- nrow(emuR::list_bundles(.what))
      if(nrow(wantedSignalfiles) < nbundles ) {
        if(nbundles > 0){
          #There are indeed some bundles that should have signal files
          cli::cli_abort(c("Missing signal files for some bundles",
                           "x"="Some bundles do not have the required signal files. Please generate them first by supplying a DSP function as .source.",
                           "i"= "There are {.val {nrow(wantedSignalfiles)}} signal files with the file extension {.arg {(.source)}}.",
                           "i"="The database contains {.val {nbundles}} bundles which should have signal files.",
                           "i"="Data files with the extensions {availableDataFileExtensions} have been computed and are available for attachement to a track name.")
                         )

        }else{
          cli::cli_abort(c("No bundles in the database",
                           "x"="The database does not contain any bundles, and there is then not possible to attach at track name to a valid derived data files"
                           ))
        }
      }
      # Now finally make sure that tracksToDefine contains only
      # track names that are fields in the signal file

      exampleSignalFile <- purrr:::pluck(emuR::list_files(ae,fileExtension),"absolute_file_path",1)

      tracksToDefine <- tracksToDefine  |>
        dplyr::filter(columnName %in% superassp::get_definedtracks(exampleSignalFile))
    }

    for(track in 1:nrow(tracksToDefine)){
      if(! tracksToDefine[track,"name"] %in% tracksAlreadyDefined$name){
        emuR::add_ssffTrackDefinition(emuDBhandle = .what,
                                      columnName = tracksToDefine[[track,"columnName"]],
                                      name = tracksToDefine[[track,"name"]],
                                      fileExtension = tracksToDefine[[track,"fileExtension"]]
                                      )

        cli::cli_alert_success("Connected the field {.field {tracksToDefine[[track,\"columnName\"]]}} in {.code <bundle name>}.{.field {tracksToDefine[[track,\"fileExtension\"]]}} signal files and named it {.var {tracksToDefine[[track,\"name\"]]}} ")
      }else{
        cli::cli_alert_info(c("Not defining a new track {.field {tracksToDefine[track,\"name\"]}}",
                              " since it is already connected with the field {.field {tracksToDefine[track,\"columnName\"]}}",
                              " in {.strong .{tracksToDefine[track,\"fileExtension\"]}} SSFF signal files."))
      }
    }
  }
  res <- emuR::list_ssffTrackDefinitions(.what)
  attr(res,"basePath") <- .what$basePath #This ensures that we can reattach the database later

  return(res)
}


provide_perspective <- function(.inside_of, name, levels_order, signals_order)


tier <- function(inside_of,tier_name,tier_type, parent_tier=NULL){

  if(missing(tier_name) ) {
    cli::cli_abort(c("Invalid tier name",
                     "x"="A tier name must be specified"))
  }

  #The first possible case is when we have no explicitly set inside_of argument, but a segment list with a
  # valid "basePath" attribute set. We then create the database handle from that basePath.
  if(is.null(inside_of)) {
    if(! is.null(attr(.what,"basePath")) && dir.exists(attr(.what,"basePath"))) {
      logger::log_debug("No explicit .inside_of argument found")
      # We then need to create a handle object
      utils::capture.output(
        inside_of <- emuR::load_emuDB(attr(.what,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      logger::log_debug("Got inside_of={inside_of}")
      stop("Could not derive the database path. Please provide an explicit database handle in the inside_of argument. See ?emuR::load_emuDB for details.")
    }
  }else{
    # we have an explicitly given database handle, but we don not know if it is a path or if the SQLite connection is still valid
    if(is.character(.inside_of) && stringr::str_ends(.inside_of,"_emuDB") && dir.exists(.inside_of)){
      .inside_of <- emuR::load_emuDB(.inside_of,verbose = FALSE)
    }
    if("emuDBhandle" %in% class(.inside_of)){
      #reload the database just to make sure that the handle is still valid
      .inside_of <- emuR::load_emuDB(.inside_of$basePath,verbose = FALSE)
    }else{
      cli::cli_abort(c("Not appropriate .inside_of argument",
                       "i"="The 'handle' argument can only be either a character vector indicating the path to the database, or an emuR database handle.",
                       "x"="The .inside_of argument supplied is a {class(.inside_of)}",
                       "x"="The database {dplyr::coalesce(.inside_of$basePath,.inside_of)} does not exits."))

    }
  }


  existingTiers <- emuR::list_levelDefinitions(inside_of)


  typeFirst <- stringr::str_to_lower(substring(tier_type,1,1))

  if(! typeFirst %in% c("s","i","e")){
    allowed <- c("EVENT","SEGMENT","ITEM")
    cli::cli_abort(c("Unable to deduce annotation type",
                     "x"="The annotation type must be one of {.val {allowed}}",
                     "i"="The type specified was {.val {tier_type}} which could not be linked with any of the allowed types."))
  }
  typeDF <- data.frame("s"="SEGMENT","e"="EVENT","i"="ITEM")
  emuR_type <- purrr::pluck(typeDF,typeFirst,1)



  #Second option for failure re tier name is that it already exists
  if( tier_name %in% existingTiers$name) {
    cli::cli_alert_info(c("Skipping the creation of existing tier",
                             "x"="A tier with the name {.var tier_name} already exist in the database and will be skipped.",
                             "i"="The database used is {.path inside_of$basePath}"))
  }else{
    #If not null and not existing, we can se if we can set it up
    emuR::add_levelDefinition(inside_of,name = tier_name,type = emuR_type, verbose = FALSE, rewriteAllAnnots = TRUE)

  }
  res <- emuR::list_levelDefinitions(inside_of)
  attr(res,"basePath") <- inside_of$basePath #This ensures that we can reattach the database later

  return(res)
}

readtrack <- function(listOfFiles,field=1,beginTime=0, endTime=0,sample_start=0, sample_end=0){

  if(is.null(field) || missing(field)) field <- 1


  if(length(listOfFiles) > 1) cli::cli_abort("The 'readtrack' function is unable to process more than one file at a time")
  if(! file.exists(listOfFiles)) cli::cli_abort("The file {.path {listOfFiles}} does not exist")


  samples <- TRUE
  begin <- sample_start
  end <- sample_end

  #Only activate time based subsetting of samples are not set
  if( ( !is.null(beginTime) && (beginTime > 0 && sample_start == 0 )) || ( !is.null(endTime) && (endTime > 0 && sample_end == 0 )) ){
    samples <- FALSE
    begin <- beginTime
    end <- endTime
  }

  trackObj <- wrassp::read.AsspDataObj(listOfFiles,begin=begin, end=end,samples=samples)

  # Now construct the SSFF data object
  outDataObj = list()

  fieldTable <- as.data.frame(trackObj[[field]] )

  fieldName <- ifelse(is.numeric(field),names(trackObj)[[field]],as.character(field) )


  #Copy attributes over
  attr(outDataObj, "trackFormats") <- attr(trackObj, "trackFormats")[match(fieldName,names(trackObj))]
  attr(outDataObj,"filePath") <- attr(trackObj,"filePath")
  attr(outDataObj, "sampleRate") <- attr(trackObj, "sampleRate")
  attr(outDataObj, "origFreq") <-  attr(trackObj, "origFreq")
  attr(outDataObj, "startTime") <- attr(trackObj, "startTime")
  attr(outDataObj, "startRecord") <- attr(trackObj, "startRecord")
  attr(outDataObj, "endRecord") <- attr(trackObj, "endRecord")
  class(outDataObj) = "AsspDataObj"

  wrassp::AsspFileFormat(outDataObj) <- "SSFF"
  wrassp::AsspDataFormat(outDataObj) <- as.integer(2) # == binary



  names(fieldTable) <- NULL

  outDataObj = wrassp::addTrack(outDataObj,  fieldName , as.matrix(fieldTable), "INT16")


  return(outDataObj)

}


#' Apply DSP Function to Two Segment Lists
#'
#' Apply a DSP function that requires two segment lists as input, processing
#' them by session (default) or by bundle. This function is useful for analyses
#' that compare two different segmentation levels or annotations.
#'
#' @param .what1 Either a segment_list object, an EQL query string, or a corpus object
#' @param .what2 Either a segment_list object or an EQL query string
#' @param .from corpus object (required if .what1 or .what2 are EQL queries)
#' @param .using DSP function to apply (must accept two data frame arguments)
#' @param ... Additional arguments passed to the DSP function
#' @param .by_bundle Logical; if TRUE, process by bundle instead of by session (default: FALSE)
#' @param .quiet Logical; suppress messages (default: FALSE)
#' @param .parallel Logical; use parallel processing (default: FALSE)
#'
#' @return A list of results from applying the DSP function
#'
#' @details
#' The function processes segments by merging two segment lists on a per-session
#' (or per-bundle if `.by_bundle = TRUE`) basis, then applying the DSP function
#' to each group. The DSP function must accept at least two data frame arguments.
#'
#' If EQL query strings are provided, they are evaluated against the corpus to
#' generate segment lists.
#'
#' @examples
#' \dontrun{
#' # Using segment lists
#' vowels <- ask_for(corpus, "Phonetic = V")
#' consonants <- ask_for(corpus, "Phonetic = C")
#' results <- quantify2(vowels, consonants, .using = my_comparison_func)
#'
#' # Using EQL queries directly
#' results <- quantify2("Phonetic = V", "Phonetic = C", 
#'                      .from = corpus, .using = my_comparison_func)
#'
#' # Process by bundle instead of by session
#' results <- quantify2(vowels, consonants, .using = my_func, .by_bundle = TRUE)
#' }
#'
#' @export
quantify2 <- function(.what1, .what2, .from = NULL, .using, ...,
                      .by_bundle = FALSE, .quiet = FALSE, .parallel = FALSE) {
  
  # Validate inputs ----
  if (missing(.using) || !is.function(.using)) {
    cli::cli_abort(c(
      "Missing or invalid {.arg .using} argument",
      "x" = "The {.arg .using} argument must be a function"
    ))
  }
  
  # Get function name for messages
  fun_name <- deparse(substitute(.using))
  
  # Check function has at least 2 parameters
  formal_args <- methods::formalArgs(.using)
  if (length(formal_args) < 2) {
    cli::cli_abort(c(
      "Invalid DSP function",
      "x" = "The function {.fn {fun_name}} must accept at least 2 arguments",
      "i" = "Found only {length(formal_args)} formal argument{?s}"
    ))
  }
  
  # Determine grouping variable
  group_by_vars <- if (.by_bundle) c("session", "bundle") else "session"
  
  if (!.quiet) {
    cli::cli_inform(c(
      "i" = "Applying {.fn {fun_name}} to segment lists",
      "i" = "Processing by: {.field {group_by_vars}}"
    ))
  }
  
  # Handle corpus and queries ----
  corp <- NULL
  
  # Detect if .what1 is a corpus
  if (S7::S7_inherits(.what1, reindeer::corpus)) {
    corp <- .what1
    if (is.null(.from)) {
      .from <- corp
    }
  } else if (!is.null(.from)) {
    if (S7::S7_inherits(.from, reindeer::corpus)) {
      corp <- .from
    }
  }
  
  # Convert queries to segment lists if needed
  if (is.character(.what1)) {
    if (is.null(corp)) {
      cli::cli_abort(c(
        "Missing corpus",
        "x" = "When using EQL query strings, provide a corpus in {.arg .from}"
      ))
    }
    .what1 <- ask_for(corp, .what1)
  }
  
  if (is.character(.what2)) {
    if (is.null(corp)) {
      cli::cli_abort(c(
        "Missing corpus",
        "x" = "When using EQL query strings, provide a corpus in {.arg .from}"
      ))
    }
    .what2 <- ask_for(corp, .what2)
  }
  
  # Validate segment lists
  if (!S7::S7_inherits(.what1, reindeer::segment_list)) {
    if (!all(c("session", "bundle", "start", "end") %in% names(.what1))) {
      missing_cols <- setdiff(c("session", "bundle", "start", "end"), names(.what1))
      cli::cli_abort(c(
        "Invalid first segment list",
        "x" = "First argument must be a segment_list or have required columns",
        "i" = "Missing: {paste(missing_cols, collapse = ', ')}"
      ))
    }
  }
  
  if (!S7::S7_inherits(.what2, reindeer::segment_list)) {
    if (!all(c("session", "bundle", "start", "end") %in% names(.what2))) {
      missing_cols <- setdiff(c("session", "bundle", "start", "end"), names(.what2))
      cli::cli_abort(c(
        "Invalid second segment list",
        "x" = "Second argument must be a segment_list or have required columns",
        "i" = "Missing: {paste(missing_cols, collapse = ', ')}"
      ))
    }
  }
  
  # Get corpus if not already available
  if (is.null(corp)) {
    # Try to get from segment list properties
    if (S7::S7_inherits(.what1, reindeer::segment_list)) {
      db_path <- S7::prop(.what1, "db_path")
      if (nzchar(db_path) && dir.exists(db_path)) {
        corp <- corpus(db_path)
      }
    }
  }
  
  if (is.null(corp)) {
    cli::cli_abort(c(
      "Cannot determine corpus",
      "x" = "Please provide a corpus via {.arg .from} argument"
    ))
  }
  
  # Add file paths to segment lists ----
  media_ext <- corp@config$mediafileExtension
  
  # Get signal files for corpus
  signal_files <- peek_signals(corp) |>
    dplyr::select(session, bundle, listOfFiles = full_path)
  
  # Prepare first dataset
  param1_name <- formal_args[1]
  
  # Convert S7 segment_list to data.frame if needed
  df1_base <- .what1
  if (S7::S7_inherits(.what1, reindeer::segment_list)) {
    df1_base <- as.data.frame(S7::S7_data(.what1))
  } else if (!is.data.frame(.what1)) {
    df1_base <- as.data.frame(.what1)
  }
  
  df1 <- df1_base |>
    as.data.frame() |>
    dplyr::left_join(signal_files, by = c("session", "bundle"), multiple = "all") |>
    tidyr::nest(.by = dplyr::all_of(group_by_vars), .key = param1_name)
  
  # Prepare second dataset  
  param2_name <- formal_args[2]
  
  # Convert S7 segment_list to data.frame if needed
  df2_base <- .what2
  if (S7::S7_inherits(.what2, reindeer::segment_list)) {
    df2_base <- as.data.frame(S7::S7_data(.what2))
  } else if (!is.data.frame(.what2)) {
    df2_base <- as.data.frame(.what2)
  }
  
  df2 <- df2_base |>
    as.data.frame() |>
    dplyr::left_join(signal_files, by = c("session", "bundle"), multiple = "all") |>
    tidyr::nest(.by = dplyr::all_of(group_by_vars), .key = param2_name)
  
  # Check for empty datasets
  if (nrow(df1) == 0) {
    cli::cli_abort("First segment list contains no rows after processing")
  }
  if (nrow(df2) == 0) {
    cli::cli_abort("Second segment list contains no rows after processing")
  }
  
  # Merge datasets ----
  combined_df <- dplyr::inner_join(df1, df2, by = group_by_vars)
  
  # Warn about non-matching groups
  only_in_1 <- dplyr::anti_join(df1, df2, by = group_by_vars)
  only_in_2 <- dplyr::anti_join(df2, df1, by = group_by_vars)
  
  if (nrow(only_in_1) > 0 && !.quiet) {
    cli::cli_warn(c(
      "!" = "{nrow(only_in_1)} group{?s} in first list not found in second list",
      "i" = "These groups will be excluded from processing"
    ))
  }
  
  if (nrow(only_in_2) > 0 && !.quiet) {
    cli::cli_warn(c(
      "!" = "{nrow(only_in_2)} group{?s} in second list not found in first list",
      "i" = "These groups will be excluded from processing"
    ))
  }
  
  if (nrow(combined_df) == 0) {
    cli::cli_abort(c(
      "No matching groups found",
      "x" = "The two segment lists have no overlapping {group_by_vars}"
    ))
  }
  
  # Add explicit arguments from ...
  dots <- rlang::list2(...)
  if (length(dots) > 0) {
    # Check which arguments are valid for the function
    invalid_args <- setdiff(names(dots), formal_args)
    if (length(invalid_args) > 0 && !.quiet) {
      cli::cli_warn(c(
        "!" = "Ignoring invalid arguments: {.arg {invalid_args}}",
        "i" = "Function {.fn {fun_name}} accepts: {.arg {formal_args}}"
      ))
    }
    
    # Add valid arguments as columns
    valid_dots <- dots[names(dots) %in% formal_args]
    for (arg_name in names(valid_dots)) {
      combined_df[[arg_name]] <- valid_dots[[arg_name]]
    }
  }
  
  # Apply DSP function ----
  if (!.quiet) {
    cli::cli_progress_step(
      "Processing {nrow(combined_df)} group{?s}",
      msg_done = "Processed {nrow(combined_df)} group{?s}"
    )
  }
  
  # Create safe version of function
  safe_fun <- purrr::safely(.using, otherwise = NULL, quiet = .quiet)
  
  # Prepare data for mapping
  map_data <- combined_df |>
    dplyr::select(-dplyr::all_of(group_by_vars))
  
  # Apply function
  if (.parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      cli::cli_warn(c(
        "!" = "Package {.pkg furrr} not available",
        "i" = "Falling back to sequential processing"
      ))
      .parallel <- FALSE
    }
  }
  
  if (.parallel) {
    # Set up parallel processing
    future::plan(future::multisession)
    on.exit(future::plan(future::sequential), add = TRUE)
    
    results <- furrr::future_pmap(
      map_data,
      safe_fun,
      .options = furrr::furrr_options(seed = TRUE),
      .progress = !.quiet
    )
  } else {
    results <- purrr::pmap(
      map_data,
      safe_fun,
      .progress = !.quiet
    )
  }
  
  # Check for errors
  errors <- purrr::map_lgl(results, ~ !is.null(.x$error))
  if (any(errors)) {
    error_indices <- which(errors)
    cli::cli_warn(c(
      "!" = "{sum(errors)} error{?s} occurred during processing",
      "i" = "Failed groups: {error_indices}"
    ))
    
    if (!.quiet) {
      # Show first few errors
      first_errors <- head(results[errors], 3)
      for (i in seq_along(first_errors)) {
        cli::cli_alert_danger(
          "Group {error_indices[i]}: {first_errors[[i]]$error$message}"
        )
      }
    }
  }
  
  # Extract successful results
  results <- purrr::map(results, "result")
  
  # Add grouping information back
  results_df <- combined_df |>
    dplyr::select(dplyr::all_of(group_by_vars)) |>
    dplyr::mutate(result = results)
  
  if (!.quiet) {
    cli::cli_progress_done()
  }
  
  return(results_df)
}



# Code used for interactive testing #######


library(tidyverse)
library(purrr)
library(progressr)
library(tibble)
library(superassp)
library(furrr)
library(progress)




reindeer:::unlink_emuRDemoDir()
reindeer:::create_ae_db(verbose = FALSE) -> ae
reindeer:::make_dummy_metafiles(ae)
#
# ask_for(ae,"Phonetic = V|E|a|A") |>
#   slice(1:2)  -> ae_a
#
# # ae_a |>
# #   quantify("fm",.quiet=TRUE, .verbose=FALSE) -> readfm
#
#
#  ae_a |>
#    quantify(forest,.quiet=TRUE, .verbose=FALSE) -> outfm
#
# ae_a |>
#   quantify(forest,.where=0.5,.n_preceeding=1,.n_following=1) -> cutfm

# ae_a |>
#   quantify(praat_voice_report) -> egm


