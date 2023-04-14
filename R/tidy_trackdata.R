

ITEM <- TRUE

fake_two_df_fun <- function( svDF,
                              csDF,
                              speaker.name = NULL,
                              speaker.ID = NULL,
                              speaker.dob = NULL,
                              session.datetime = NULL,
                              pdf.path = NULL,
                              simple.output = FALSE,
                              overwrite.pdfs = FALSE){

  return(list(svDF=paste0(dim(svDF),collapse=","),
                csDF=aste0(dim(csDF),collapse=","),
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



ask_for <- function(inside_of, query,sessions_regex = ".*", bundles_regex = ".*",times_from = NULL, calculate_times = TRUE,interactive=FALSE){
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
          inside_of <- emuR::load_emuDB(attr(strAttr,"basePath"),verbose = FALSE)
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

  return(res)
}

anchor <- function(inside_of, segment_list,anchor_against=NULL){
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


climb_to <- function(.data,  .attribute_name ,.collapse = TRUE, .skip_times = FALSE, .times_from = NULL, .interactive=FALSE, .inside_of=NULL) {

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



peek_at <- function(.x, what=c("levels","links","labelgroups","tracks","bundles","sessions","perspectives","files","attributes"),...){

  if("emuDBhandle" %in% class(.x)){
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

  what <- match.arg(what, c("levels","links","labelgroups","tracks","bundles","sessions","perspectives","files","attributes"))

  if(what == "labelgroups") {
    #A label can be globally or locally defined, so we need to check whether extra arguments were given.
    what <- ifelse(length(al) == 0, "global_lg","local_lg")
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



define <- function(.x, what=c("level","link","global_lg","track","bundle","session","perspective","file","attribute","local_lg"),...){

  if("emuDBhandle" %in% class(.x)){
    .inside_of <- .x
  }else{
    if( ! is.null(attr(.x,"basePath")) && dir.exists(attr(.x,"basePath"))) {
      # We then need to create a handle object
      utils::capture.output(
        .inside_of <- emuR::load_emuDB(attr(.x,"basePath"),verbose = TRUE)
      ) -> dbload.info
      logger::log_info(paste(dbload.info,collapse ="\n"))
    }else{
      stop("Could not derive the database path. Please provide an explicit database handle object as '.x'. See ?emuR::load_emuDB for details.")
    }
  }

  what <- match.arg(what, c("level","link","global_lg","track","bundle","session","perspective","files","attributes","local_lg"))

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

describe_level <- function(.x,name,type= c("SEGMENT","EVENT","ITEM")){
  type <- head(type,1)
  if(is.null(type)) stop("Missing level type")
  if(missing(name)) stop("A level name is required")

  res <- define(.x,what="level",name=name,type=toupper(type))
}

quantify <- function(...){
  UseMethod("quantify")
}

quantify.data.frame <- function(.what,.source,...,.where=NULL,.n_preceeding=NULL,.n_following=NULL,.by_maxFormantHz=TRUE,.cache_file=NULL,.clear_cache=FALSE,.metadata_defaults=list("Gender"="Undefined","Age"=35),.recompute=FALSE,.package="superassp",.naively=FALSE,.parameter_log_excel=NULL,.inside_of=NULL){

  fcall <- match.call(expand.dots = FALSE)
  fcallS <- toString(fcall)
  idCols <- c("Gender","Age")


  ## Explicit and Hard coded arguments ----------------------------------------------------



  dotArgs <- list(...)
  dotArgsNames <- names(dotArgs)
  if("toFile" %in% dotArgsNames){
    cli::cli_alert_info("Ignoring {.arg toFile} argument given to {.fun quantify}.")
  }
  #We will use this list later for filling in arguments before applying a function  in .source
  explicitOrDefaultParams <- utils::modifyList(dotArgs,list(toFile=FALSE))

  # Initial check of arguments ----------------------------------------------


  #Check for the special case where the function was called by furnish
  calledByFurnish <- FALSE
  if(setequal(names(.what),c("start_item_id","bundle", "end", "end_item_id", "session", "start"))){
    calledByFurnish <- TRUE
    #Now check for strange arguments
    if(! is.null(.n_preceeding) || ! is.null(.n_preceeding) ||! is.null(.where)){
      cli::cli_alert_info(c("Ignoring {.arg .where}, {.arg .n_preceeding}, and {.arg .n_follwing} arguments",
                            "i"="Specifying a subset of data points to be returned does not make sense for {.fun furnish}."
                            )
                          )
    }
  }



  if(nrow(.what ) < 1){
    #Nonsense segment list
    cli::cli_abort(c("Erroneous segment list argument",
                     "i"="The list of segments or signals is empty",
                     "x"="The .what argument contains {nrow(.what)} rows."))
  }
  if(any(is.na(.what[c("start","end")]))){
    #We need to have all start and end times specified to proceed
    cli::cli_alert_info(c("Missing time references",
                     "x"="Some start or end times in the segment list supplied as the {.param .what} argument is missing",
                     "i"="I will try to deduce times from associated levels with time information."))
    .what <- anchor(.what)
  }


  logger::log_debug("Function quantify called with arguments {fcallS}")

  ## What to compute from (the .what argument and the .inside_of object ) ------------------


  #The first possible case is when we have no explicitly set .inside_of argument, but a segment list with a
  # valid "basePath" attribute set. We then create the database handle from that basePath.
  if(is.null(.inside_of)) {
    if(! is.null(attr(.what,"basePath")) && dir.exists(attr(.what,"basePath"))) {
      logger::log_debug("No explicit .inside_of argument found")
      # We then need to create a handle object
      utils::capture.output(
        .inside_of <- emuR::load_emuDB(attr(.what,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      logger::log_debug("Got .inside_of={(.inside_of)}")
      cli::cli_abort(c("Could not derive the database path.",
                       "x"="Please provide an explicit database handle object in the {.arg .inside_of} argument.",
                       "x"="See ?emuR::load_emuDB for details on how to load it."))
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

  # This sets the default input media file extension, which handled the case when a
  #function is called to compute a list or SSFF track result based on a wave file
  # This variable will then be set to something else if
  # we want to read in a pre-computed SSFF track stored on disk instead.
  inputSignalsExtension <- emuR:::load_DBconfig(.inside_of)$mediafileExtension


  ## Source setup ------------------------------------------------------------


  # This section handles the case where we want to compute signals or a
  # list of results from the wave file directly??
  if(is.function(.source) || is.function(purrr::safely(get)(.source)$result)){
    if(is.function(.source)){
      .f <- .source
    }else{
      .f <- purrr::safely(get)(.source)$result

      cli::cli_alert_info("Applying the function {.fun {funName}} to the segment list.")
    }

    funName <- fcall[".source"]
    willRemove <- setdiff(names(explicitOrDefaultParams),formalArgs(.source))
    cli::cli_alert_info("Ignoring the {.arg {willRemove}} arguments as they are not used by {.fun .source}")

    explicitOrDefaultParams <- explicitOrDefaultParams[ intersect(names(explicitOrDefaultParams),formalArgs(.source)) ]

  }else{
    # Ok, so not a function, so we need to make sure that the .source argument is a string
    # and the name of a track in the database
    if(is.character(.source) && .source %in% emuR::list_ssffTrackDefinitions(.inside_of)$name){
      #The string should here be the name of a track
      logger::log_debug("We got a SSFF track field in the .source argument : {(.source)}")
      .f <- readtrack
      funName <- "readtrack"
      inputSignalsExtension <- emuR::list_ssffTrackDefinitions(ae)  |>
        dplyr::filter(name==.source)  |>
        dplyr::select(fileExtension)  |>
        purrr::pluck(1)


      cli::cli_alert_info("Reading data from the {.field {(.source)}} in {.val {inputSignalsExtension}} signal files.")

      explicitOrDefaultParams <- utils::modifyList(explicitOrDefaultParams,list(field=.source))

      #It makes no sense to recompute or deduce DSP settings if the data is to be loaded from a stored track
      if(!.naively || .recompute ) {
        cli::cli_bullets(c("i"="Directly reading the {.field {(.source)}} track require no DSP parameter deduction.",
                                 ">"="The processing will ignore the {.arg .recompute} argument and assume {.arg .naively=TRUE}."))
        #cli::cli_alert_info(" - The processing will ignore the {.arg .recompute} argument and assume {.arg .naively=TRUE}.")
      }
      .naively <- TRUE
      .recompute <- FALSE
    }else{
      cli::abort(c("Cannot use the indicated .source",
                   "i"="The .source argument contains {(.source)} and is of class {class(.source)}",
                   "i"="The .source is reported to be able to return the columns {superassp::get_definedtracks(.source)} when converted to a tibble",
                   "x"="The .source needs to be the name of a track in the database or a function that should do the signal processing."))
    }
  }


  ## Optional cache system setup -----------------------------------------

  .cache_connection <- NULL

  ## Make a date specific cache file if
  if(!is.null(.cache_file) && ! isFALSE(.cache_file)){
    if(.cache_file) {
      #Set a date specific cache file
      .cache_file <- file.path(tempdir(check=TRUE),format(Sys.time(), paste0(funName,"%Y%m%d.sqlite")))
    }
    #Now, since we set up a default file name above if .cache_file is just TRUE
    # we should be able to use the character string as the file name
    if(is.character(.cache_file)){
      #There is no reason for file path normalization to not work in this call
      # so the warnings given by default for the not yet existing file will just be ignored
      suppressWarnings(.cache_file <- normalizePath(.cache_file,mustWork = NA))
      cacheFileExists <- file.exists(.cache_file)

      #Clear cache file if indicated
      if(.clear_cache && cacheFileExists){

        suppressWarnings( unlink(.cache_file))
        cacheFileExists <- file.exists(.cache_file)
      }
      suppressWarnings(
        .cache_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                            user="root",
                                            password="",
                                            host="localhost",
                                            dbname=.cache_file,
                                            flags=RSQLite::SQLITE_RWC,
                                            loadable.extensions=FALSE)
      )
#
#       if(!RSQLite::dbIsValid(.cache_connection)) cli::cli_abort(c("Could not connect to the cache file",
#                                                                   "i","The {arg {(.cache_file)}} argument you cave was {.file {(.cache_file)}}."))
      #Check table format requirements
      if(length(RSQLite::dbListObjects(.cache_connection)) > 0 &&
         (! "cache" %in% RSQLite::dbListTables(.cache_connection) || ! setequal(c("sl_rowIdx","obj"),RSQLite::dbListFields(.cache_connection,"cache")))
         ){

        cli::cli_alert_info("Initializing cache file {.file {(.cache_file)}}")

        RSQLite::dbExecute(.cache_connection,"DROP TABLE IF EXISTS cache;")

      }else{
        cli::cli_alert_info("Using existing cache file {.file {(.cache_file)}}")
      }

      RSQLite::dbExecute(.cache_connection,"CREATE TABLE IF NOT EXISTS cache(sl_rowIdx INTEGER PRIMARY KEY, obj BLOB );")

    }else{
      #The cache file is not NULL, not TRUE (auto-generated), and not a character vector
      cli::cli_abort(c("The cache file name could not be deduced",
                       "x"="A full path can be given; a value TRUE vill generate a default file name based on the current date.",
                       "i"="The {.arg .cache_file} provided to the fucntion was {.val {(.cache_file)}}."))
    }


  }else{
    #the case when no cache file should be used
    cli::cli_alert_info("Intermediate results are {.strong not} cached.")
  }




  ## Definition and setup directly related to the inner applied funct --------


  #This version of the original function .f that is guarantee to return a list of $result (which is possibly NA) and $error
  safe_f <- purrr::possibly(.f, otherwise=NA)

  #This function wraps a call so that the call parameters may be logged and so that we get a progress bar
  innerMapFunction <- function(.sl_rowIdx,.session,.bundle,...){

    dotdotdot <- list(...)
    dotdotdotS <- toString(dotdotdot)
    result <- NULL

    if(!is.null(.cache_connection)){
      #Check existing values

      cachedObj <- RSQLite::dbGetQuery(.cache_connection,"select obj from cache where sl_rowIdx = ?",.sl_rowIdx)

      if(nrow(cachedObj) > 0){
        #Load rather than comput
        logger::log_debug("[{.session}:{.bundle})] Loading data from cache file.\n")

        result <- base::unserialize(
          base::charToRaw(
            cachedObj$obj[[1]]))
      }

    }
    #Compute if the results is still not defined
    if(is.null(result)){

      logger::log_debug("[{.session}:{.bundle}] .source settings \n{dotdotdotS}\n when applying the function {funName}.\n")
      result <- safe_f(...)
    }

    #Maybe store the results in cache?
    if(!is.null(.cache_connection)){
      objser <- base::rawToChar(base::serialize(result, connection = NULL,ascii = TRUE))


      RSQLite::dbExecute(.cache_connection,"INSERT OR REPLACE INTO cache (sl_rowIdx,obj) VALUES (?,?);", c(.sl_rowIdx,objser))
    }

    return(result)
  }


  # Deduction of DSP processing arguments -----------------------------------



  #Logic that concerns formal arguments
  formalArgsNames <- methods::formalArgs(.f)
  functionDefaults <- as.list(formals(.f))


  # What we now need is an 'listOfFiles' to supply to the DSP functionŒ
  signalFiles <- emuR::list_files(.inside_of,inputSignalsExtension)  |>
    dplyr::rename(listOfFiles=absolute_file_path)  |>
    dplyr::select(-file)


  if(! .naively){
    ## Deduce DSP settings based on metadata -----------------------------------

    #If we are just reading a track, the user is already aware that this will be done naively
    if(!funName =="readtrack") cli::cli_alert_info("Using metadata to derive DSP settings where not explicitly set by the user.")

    # Make sure that we have a DSP default settings data.frame
    dsp <- reindeer:::dspp_metadataParameters(recompute=.recompute)  |>
      tidyr::replace_na(list(Gender="Unspecified"))

    if(nrow(dsp) > 0 ){
      if(.recompute){
          cli::cli_alert_success(cli::ansi_strwrap("Recomputed DSP settings age and gender appropriate DSP settings based on metanalysis data in the {.file { file.path(system.file(package = \"reindeer\",mustWork = TRUE),\"default_parameters.xlsx\")  }} file."))
      }else{
        cli::cli_alert_success("Loaded precomputed age and gender appropriate DSP settings")
      }
    }
    #After this we can be sure that parameters set per session or bundle are  available
    # but have been overwritten by explicitly set settings given to this function as we proceed
    meta <- reindeer:::get_metadata(.inside_of,manditory=idCols)  |>
      dplyr::mutate(Gender=as.character(Gender),Age=as.integer(round(Age,digits = 0)))  |>
      tidyr::replace_na(.metadata_defaults)


    #This variable is used for filling in missing values in arguments explicitly set in metadata
    precomputedVariableNames <- intersect(names(meta),names(dsp))
    dspColumnsToAdd <- setdiff(names(dsp),names(meta))

    filledMeta <- meta  |>
      dplyr::rows_update(y=dsp  |>
                           dplyr::select(all_of(precomputedVariableNames)),
                         by=idCols,unmatched = "ignore")


    # This block will add settings that were set in DSP defaults only
    completedStoredDSPSettings <- filledMeta  |>
      dplyr::ungroup()  |>
      dplyr::left_join(
        dplyr::select(dsp,all_of(idCols),all_of(dspColumnsToAdd))
        ,by=idCols)  |>
      tidyr::replace_na(functionDefaults)   |>
      dplyr::mutate(toFile=FALSE)  #Forcefully inject a toFile argument


    # We need to choose whether to guide a formant tracker by number of extracted formants in a fixed 0-5000 Hz frequency range
    # or if the 5000 Hz ceiling is instead increased and the default number of formants extracted is kept constant

    if(.by_maxFormantHz && "numFormants" %in% names(completedStoredDSPSettings)){
      completedStoredDSPSettings <- completedStoredDSPSettings  |>
        dplyr::select(-numFormants)

      # The names of columns to keep are now the columns that are defined either in signalFiles or
      # in dspCompletedSettings, and which will be used by the DSP function
      settingsThatWillBeUsed <- intersect(formalArgsNames,
                                          union(names(completedStoredDSPSettings),
                                                names(signalFiles))
      ) # This will be used to remove unwanted columns

      #Here we construct the settings that we want to use when applying the specific DSP
      # function to bundles (in specific sessions)
      sessionBundleDSPSettingsDF <-  completedStoredDSPSettings  |>
        dplyr::left_join(signalFiles,by=c("session","bundle"))  |>
        dplyr::select(session,bundle,all_of(settingsThatWillBeUsed))

    }
  }else{
    ## Naive DSP arm -----------------------------------

    #If we are just reading a track, the user is already aware that this will be done naively
    if(!funName =="readtrack") cli::cli_alert_info("Using no implicitly derived DSP settings based on metadata.")

    sessionBundleDSPSettingsDF <- signalFiles
    # The names of columns to keep are now the columns that are defined  in signalFiles and which will be used by the DSP function
    settingsThatWillBeUsed <- intersect(formalArgsNames,names(signalFiles) )

  }


  ## Make the environment suitable for collecting quantification parameters --------

  # Now we need to transfer the session / bundle settings to the segment list
  # to get start and end times into the call also
  segmentDSPDF <- .what  |>
    tibble::rownames_to_column(var = "sl_rowIdx")  |>
    dplyr::left_join(sessionBundleDSPSettingsDF,by=c("session","bundle"))  |>
    dplyr::rename(beginTime=start,endTime=end)  |>
    dplyr::select(all_of(c("session","bundle",settingsThatWillBeUsed)))  |>
    dplyr::rename(.session=session,.bundle=bundle)  |>
    dplyr::mutate(dplyr::across(where(is.integer), as.numeric)) ## Fix for wrassp functions that expect "numeric" values, not integers

  #Set up the progress bar
  num_to_do <- nrow(segmentDSPDF)


  p <- progressr::progressor(num_to_do)


  ## Optionally log the parameter table to an Excel output -------------------


  if(!is.null(.parameter_log_excel)) {

    #There is no reason for file path normalization to not work in this call
    # so the warnings given by default for the not yet existing file will just be ignored
    suppressWarnings(.parameter_log_excel <- normalizePath(.parameter_log_excel,mustWork = NA))

    if(is.character(.parameter_log_excel) && dir.exists(dirname(.parameter_log_excel))){
      #Write a new excel file, named either according to the specified name in
      #.parameter_log_excel or as a generated file name (if the string is a directory path)
      #cli::cli_abort("{.path {dirname(.parameter_log_excel)}} {.path {(.parameter_log_excel)}}")
      excel_filename <- ifelse( file.info(.parameter_log_excel)[["isdir"]] && dir.exists(.parameter_log_excel),
                                file.path(.parameter_log_excel,
                                          paste0(stringr::str_replace_all(format(Sys.time(),usetz = TRUE)," ","_"),".xlsx")),
                                .parameter_log_excel)
      parameterData <- segmentDSPDF  |>
        dplyr::rename(bundle=.bundle,session=.session)

      openxlsx::write.xlsx(x = parameterData,file=excel_filename, asTable=FALSE, overwrite = TRUE)


      cli::cli_alert_success("Wrote DSP parameters used into {.file {excel_filename}}")
      cli::cli_bullets(c(">"="Please note that default parameters of the called DSP function are not included in the log as they cannot unambigiously be deduced by {.fun quantify}."))

    }else{
      #Not able to create a parameter file or deduce a name for it
      cli::cli_warn(c("Unable to write DSP parameter file",
                      "x"= "A parameter file could not be created in {.file {(.parameter_log_excel)}}"))
    }

  }


  # Do the actual quantification  work --------------------------------------------

  pb <- list(
    type = "iterator",
    format = "{cli::pb_spin} Processing ({cli::pb_current} of {cli::pb_total}) {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = TRUE)

  explicitOrDefaultParamsDF <- as_tibble(explicitOrDefaultParams)


  # Here we apply the DSP function once per row and with settings comming from
  # the columns in the data frame
  appliedDFResultInList <- segmentDSPDF  |>
    tibble::rownames_to_column(var = ".sl_rowIdx")  |>
    dplyr::mutate(.sl_rowIdx = as.integer(.sl_rowIdx))




  if(nrow(explicitOrDefaultParamsDF) > 0 && ncol(explicitOrDefaultParamsDF) > 0) {
    # This row ovewrites all deduced parameters with explicitly set ones, including toFile=FALSE if used by the called function
    appliedDFResultInList <-  appliedDFResultInList |>
    dplyr::mutate(explicitOrDefaultParamsDF)
  }


  # Do the final grouping and apply the DSP function inside of a wrapper
  appliedDFResultInList <- appliedDFResultInList |>
    dplyr::rowwise()  |>
    purrr::pmap(.f=innerMapFunction,.progress = pb) # This is the busy line


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
      purrr::map_dfr(as_tibble, .id="sl_rowIdx") |>
      dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))

    ### Perform 'cut' extraction ------------------------------------------------

    if(!is.null(.where)){
      if(is.null(.n_preceeding) && is.null(.n_following)){
        cli::cli_inform(c("i"="Extracting one data point nearest the {(.where*100)}% time point for each segment"))
        .n_preceeding <- 0
        .n_following <- 0
      }else{
        .n_preceeding <- ifelse(is.null(.n_preceeding),0, .n_preceeding)
        .n_following <- ifelse(is.null(.n_following),0, .n_following)

        cli::cli_inform(c("i"="Extracting the data point nearest the {(.where*100)}% time point and at most {.val {(.n_preceeding)}} before and {.val {(.n_following)}} points following."))
      }

      resTibble <- resTibble |>
        dplyr::group_by(sl_rowIdx) |>
        dplyr::mutate(dist = (frame_time / max(frame_time)) -.where) |>
        dplyr::mutate(ind_of_min = which.min(abs(dist) ),
                      maxind=which.max(dist)) |>
        dplyr::slice(seq(max(first(ind_of_min) - .n_preceeding ,0),min(first(maxind),first(ind_of_min) + .n_following))) |>
        dplyr::select(-dist,-ind_of_min,-maxind)
      cat("blopp")
      return(resTibble)
    }


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

furnish <- function(...) {
  UseMethod("furnish")
}

furnish.character <- function(.inside_of,...){
  .inside_of <- normalizePath(.inside_of)
  if(! stringr::str_ends(.inside_of,"_emuDB") || ! dir.exists(.inside_of)){
    cli::cli_abort(c("Non-existing database path",
                     "x"="A database path was given as the {.arg .inside_of} argument but the location does not exist.")
    )
  }
  utils::capture.output(
    .inside_of <- emuR::load_emuDB(.inside_of,verbose = FALSE)
  ) -> dbload.info
  logger::log_info(paste(dbload.info,collapse = "\n"))
  furnish(.inside_of,...)
}

furnish.data.frame <- function(.inside_of,...){
  if(!"basePath" %in% attr(.inside_of)){
    cli::cli_abort(c("Unable to deduce databse path",
                     "x"="The function {.fun furnish} has to derine a database path to perform processing",
                     "i"="The function was given a {.cls {class(.inside_of)}} with the attributes {.field {attributes(.inside_of)}}",
                     "x"="The {.arg .inside_of} {.cls data.frame} needs to have a {.field basePath} attribute attached to it with the full path."))
  }
  utils::capture.output(
    .inside_of <- emuR::load_emuDB(attr(.inside_of,"basePath"),verbose = FALSE)
  ) -> dbload.info
  logger::log_info(paste(dbload.info,collapse = "\n"))
  furnish(.inside_of,...)
}

furnish.emuDBhandle <- function(.inside_of,.source, ... ,.force=FALSE,.really_force=FALSE,.metadata_defaults=list("Gender"="Undefined","Age"=35),.by_maxFormantHz=TRUE,.recompute=FALSE,.naively=FALSE,.parameter_log_excel=NULL){


  # Check inputs ------------------------------------------------------------

  if(missing(.source)) {
    cli::cli_abort(c("Missing source name",
                                      "i"="You need to state a function name or the file extension from which the data columns should be gathered",
                                      "x"="The argument {.var .source} is missing"))
  }



  # Investigate what is already in the database -----------------------------

  ## Available extensions of track files in the database -----------------------------


  availableDataFileExtensions <- emuR::list_files(.inside_of)  |>
    dplyr::mutate(file=stringr::str_replace(file,"^.*[.]",""))  |>
    dplyr::filter(!file %in% c("json",emuR:::load_DBconfig(.inside_of)$mediafileExtension,reindeer:::metadata.extension))  |>
    dplyr::select(file)  |>
    dplyr::distinct()  |>
    purrr::pluck("file")


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
  tracksAlreadyDefined <- emuR::list_ssffTrackDefinitions(.inside_of)



  # Create tracks and connect fields -------------------------------------------

  ## [a] The application of a function to create tracks -------------------------------------------

  #If we have a function, then we should use it to create new tracks
  if(is.function(.source) || ( ! is.null(get0(.source)) && is.function( get0(.source)))){



    ### SSFF functions ----------------------------------------------------------


    # First check that we have an SSFF track returining funciton

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


      cli::cli_alert_info("Using the {.val {fileExtension}} file extension for stored signal files.")

      #### Prepare the segment list that quantify needs -------------------------------------------


      # We here make a fake segment list with start and end times that suggest whole file processing
      # so that we can reuse quantify to deduce parameters, optionally log parameters, do the DSP work, and all that stuff.

      bundles <- emuR::list_bundles(.inside_of)  |>
        dplyr::rename(bundle=name)  |>
        dplyr::mutate(start=0, end=0,start_item_id=0,end_item_id=0)  |>
        dplyr::mutate(.file= file.path(.inside_of$basePath,
                                      paste0(session,emuR:::session.suffix),
                                      paste0(bundle,emuR:::bundle.dir.suffix),
                                      paste(bundle,fileExtension,sep=".")))
      bundles <- bundles  |>
        dplyr::mutate(.force=.force)  |>
        dplyr::filter( .force |  !file.exists(.file))  |>
        dplyr::select(-.file,-.force) #Make sure this variable does not mess up execution of the function

      attr(bundles,"basePath") <- .inside_of$basePath

      if(nrow(bundles) < 1){
        #We deliberately do not raise an error here since we want to be able to continue processing even when
        # nothing needs to be done
        cli::cli_alert_success("Skipping the signal processing")
        cli::cli_alert_info("No signal files needs updating and updating is not forced.")
        cli::cli_alert_info("The database contains {.val {nrow(emuR::list_bundles(.inside_of))}} bundles")
        cli::cli_alert_info("There are {.val {nrow(emuR::list_files(.inside_of,fileExtension))}} signal files with the file extension {.val {fileExtension}}")

        # In this case we are not doing any DSP and all that is left is to return the database handle.
        return(.inside_of)
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
        dplyr::mutate(file= file.path(.inside_of$basePath,
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

      cli::cli_alert_info("Considering signal files with the {.val {fileExtension}} file extension.")

      #First check that the files actually exist
      allSignalfiles <- emuR::list_files(.inside_of)
      wantedSignalfiles <- allSignalfiles  |>
        dplyr::filter(stringr::str_detect(file,paste0(".",.source,"$")))

      nbundles <- nrow(emuR::list_bundles(.inside_of))
      if(nrow(wantedSignalfiles) < nbundles ) {
        if(nbundles > 0){
          #There are indeed some bundles that should have signal files
          cli::cli_abort(c("Missing signal files for some bundles",
                           "x"="Some bundles do not have the required signal files. Please generate them first by supplying a DSP function as .source.",
                           "i"= "There are {.val {nrow(signalfiles)}} signal files with the file extension {.arg {(.source)}}.",
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
        emuR::add_ssffTrackDefinition(emuDBhandle = .inside_of,
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
  res <- emuR::list_ssffTrackDefinitions(.inside_of)
  attr(res,"basePath") <- .inside_of$basePath #This ensures that we can reattach the database later

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

readtrack <- function(listOfFiles,field="1",beginTime=0, endTime=0,sample_start=0, sample_end=0,toFile=FALSE){

  if(! all(file.exists(listOfFiles))){
    msg <- paste0("The input files ",paste(listOfFiles,collapse = ", "), " do not exist.")
    logger::log_error(msg)
    stop(msg)
  }
  if(length(listOfFiles) > 1) stop("The 'readtrack' function is unable to process more than one file at a time")

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

  if(is.null(field) || length(field) == 0 || missing(field) ){
    .field <- type.convert(field,as.is = TRUE)
    field <- names(trackObj)[[.field]]
  }

  if(!is.character(field)) stop("Please supply a field name as a string.")
  # Now construct the SSFF data obhect
  outDataObj = list()

  fieldTable <- as.data.frame(trackObj[[field]] )

  #Copy attributes over
  attr(outDataObj, "trackFormats") <- attr(trackObj, "trackFormats")[match(field,names(trackObj))]
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

  outDataObj = wrassp::addTrack(outDataObj,  field, as.matrix(fieldTable), "INT16")


  return(outDataObj)

}


quantify2 <- function(.what1, .what2, .source,...,.glue_arguments=TRUE,.by_bundle=FALSE,.naively=TRUE, .inside_of=NULL){



  # Basic checks of input ---------------------------------------------------

  if(missing(.source) || !is.function(.source)) {
    cli::cli_abort(c("Missing or not appropriate {.arg .source} argument",
                     "x"="The {.arg .source} argument should be a function",
                     "i"="{.arg .source} is a {.cls {class(.source)}}. It needs to be a {.cls function}."))
  }

  fun <-rlang::enquos(.source)
  funName <- as.character(fun)

# cat(funName)
  # Base setup --------------------------------------------------------------

  quiet_query <- purrr::quietly(emuR::query)

  #Define how the two datasets will be split before the function .source is applied

  set_split_by <- "session" #Default: A session is usually a record of speech of a person in a specific state
  if(.by_bundle) set_split_by <- c("session", "bundle") # This case applies when a session directory is used keep e.g. recordings of a specific speaker together



  required_fields <- c("labels", "start", "end", "db_uuid", "sample_start", "sample_end", "sample_rate", "listOfFiles")

  ## Definition and setup directly related to the inner applied funct --------


  #This version of the original function .f that is guarantee to return a list of $result (which is possibly NA) and $error
  safe_f <- purrr::possibly(.source, otherwise=NA)

  #This function wraps a call so that the call parameters may be logged and so that we get a progress bar
  innerMapFunction <- function(.sl_rowIdx,.session,.bundle,...){


    result <- NULL
    .cache_connection <- NULL

    if(!is.null(.cache_connection)){
      #Check existing values

      #cachedObj <- RSQLite::dbGetQuery(.cache_connection,"select obj from cache where sl_rowIdx = ?",.sl_rowIdx)

      if(nrow(cachedObj) > 0){
        #Load rather than comput
        logger::log_debug("[{.session}:{.bundle})] Loading data from cache file.\n")

        result <- base::unserialize(
          base::charToRaw(
            cachedObj$obj[[1]]))
      }

    }
    #Compute if the results is still not defined
    if(is.null(result)){

      #logger::log_debug("[{.session}:{.bundle}] .source settings \n{dotdotdotS}\n when applying the function {funName}.\n")
      result <- safe_f(...)
    }

    #Maybe store the results in cache?
    #if(!is.null(.cache_connection)){
    #  objser <- base::rawToChar(base::serialize(result, connection = NULL,ascii = TRUE))


    #  RSQLite::dbExecute(.cache_connection,"INSERT OR REPLACE INTO cache (sl_rowIdx,obj) VALUES (?,?);", c(.sl_rowIdx,objser))
    #}

    return(result)
  }

  # Setup of the first dataset ----------------------------------------------


  #The first possible case is when we have no explicitly set .inside_of argument, but a segment list with a
  # valid "basePath" attribute set. We then create the database handle from that basePath.
  if(is.null(.inside_of)) {
    logger::log_debug("No explicit .inside_of argument found")

    if(! is.null(attr(.what1,"basePath")) && dir.exists(normalizePath(attr(.what1,"basePath")))) {

      # We have a database path but need to create a handle object
      utils::capture.output(
        .inside_of1 <- emuR::load_emuDB(attr(.what1,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      logger::log_debug("Got .inside_of={(.inside_of)}")
      cli::cli_abort(c("Could not derive the database path.",
                       "x"="Please provide an explicit database handle object in the {.arg .inside_of} argument.See {.help emuR::load_emuDB} for details.",
                       "i"="The {.obj_type_friendly {(.what1)}} supplied in the {.arg .what1} argument did not have an attached attribute {.field basePath}."))
    }
  }else{
    # we have an explicitly given database handle, but we don not know if it is a path or if the SQLite connection is still valid
    if(is.character(.inside_of) && stringr::str_ends(.inside_of,"_emuDB") && dir.exists(.inside_of)){
      .inside_of1 <- emuR::load_emuDB(.inside_of,verbose = FALSE)
    }
    if("emuDBhandle" %in% class(.inside_of)){
      #reload the database just to make sure that the handle is still valid
      .inside_of1 <- emuR::load_emuDB(.inside_of$basePath,verbose = FALSE)
    }else{
      cli::cli_abort(c("Not appropriate {.arg .inside_of} argument",
                       "x"="The 'handle' argument can only be either a character vector indicating the path to the database, or an emuR database handle.",
                       "i"="The .inside_of argument supplied is a {class(.inside_of)}",
                       "i"="The database {dplyr::first(.inside_of$basePath,.inside_of)} does not exits."))

    }
  }
  #Now complete the data set by adding a full path to signal files
  inputSignalsExtension <- emuR:::load_DBconfig(.inside_of1)$mediafileExtension
  # What we now need is an 'listOfFiles' to supply to the DSP functionŒ
  signalFiles_what1 <- emuR::list_files(.inside_of1,inputSignalsExtension)  |>
    dplyr::rename(listOfFiles=absolute_file_path)  |>
    dplyr::select(-file)

  # Now replace the query with the result of the query
  if(is.character(.what1)){
    .what1 <- emuR::query(emuDBhandle = .inside_of1,
                          query = .what1,
                          calcTimes = TRUE,
                          verbose=FALSE)
  }

  .what1_name <- purrr::pluck(methods::formalArgs(.source),1)

  .what1_df <-.what1   |>
    dplyr::left_join(signalFiles_what1, by = set_split_by,multiple="all") |>
    tidyr::nest(.by=all_of(set_split_by))

  names(.what1_df) <- c(set_split_by,.what1_name)


  # Setup of the second dataset ----------------------------------------------


  #The first possible case is when we have no explicitly set .inside_of argument, but a segment list with a
  # valid "basePath" attribute set. We then create the database handle from that basePath.
  if(is.null(.inside_of)) {
    logger::log_debug("No explicit .inside_of argument found")

    if(! is.null(attr(.what2,"basePath")) && dir.exists(normalizePath(attr(.what2,"basePath")))) {

      # We have a database path but need to create a handle object
      utils::capture.output(
        .inside_of2 <- emuR::load_emuDB(attr(.what2,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      logger::log_debug("Got .inside_of={(.inside_of)}")
      cli::cli_abort(c("Could not derive the database path.",
                       "x"="Please provide an explicit database handle object in the {.arg .inside_of} argument.See {.help emuR::load_emuDB} for details.",
                       "i"="The {.obj_type_friendly {(.what2)}} supplied in the {.arg .what2} argument did not have an attached attribute {.field basePath}."))
    }
  }else{
    # we have an explicitly given database handle, but we don not know if it is a path or if the SQLite connection is still valid
    if(is.character(.inside_of) && stringr::str_ends(.inside_of,"_emuDB") && dir.exists(.inside_of)){
      .inside_of2 <- emuR::load_emuDB(.inside_of,verbose = FALSE)
    }
    if("emuDBhandle" %in% class(.inside_of)){
      #reload the database just to make sure that the handle is still valid
      .inside_of2 <- emuR::load_emuDB(.inside_of$basePath,verbose = FALSE)
    }else{
      cli::cli_abort(c("Not appropriate {.arg .inside_of} argument",
                       "x"="The 'handle' argument can only be either a character vector indicating the path to the database, or an emuR database handle.",
                       "i"="The .inside_of argument supplied is a {class(.inside_of)}",
                       "i"="The database {dplyr::first(.inside_of$basePath,.inside_of)} does not exits."))

    }
  }
  #Now complete the data set by adding a full path to signal files
  inputSignalsExtension <- emuR:::load_DBconfig(.inside_of2)$mediafileExtension
  # What we now need is an 'listOfFiles' to supply to the DSP functionŒ
  signalFiles_what2 <- emuR::list_files(.inside_of2,inputSignalsExtension)  |>
    dplyr::rename(listOfFiles=absolute_file_path)  |>
    dplyr::select(-file)

  # Now replace the query with the result of the query
  if(is.character(.what2)){
    .what2 <- emuR::query(emuDBhandle = .inside_of2,
                          query = .what2,
                          calcTimes = TRUE,
                          verbose=FALSE)
  }

  .what2_name <- purrr::pluck(methods::formalArgs(.source),2)

  .what2_df <-.what2   |>
    dplyr::left_join(signalFiles_what2, by = c("session", "bundle")) |>
    tidyr::nest(.by=all_of(set_split_by))

  names(.what2_df) <- c(set_split_by,.what2_name)


  .what_df <- .what1_df |>
    dplyr::inner_join(.what2_df,by = set_split_by) |>
    dplyr::rowwise()


  # Check result set compatibility ------------------------------------------


  ## Check that all required fields are returned in the tibbles in list columns ----
  to_check <- .what_df |>
   dplyr::ungroup()|>
   dplyr::slice(1)


  # Check that all required fields are returned in the tibbles in list columns
  if(length(setdiff(required_fields, names(purrr::pluck(to_check,.what1_name,1)))) > 0 ||
     length(setdiff(required_fields, names(purrr::pluck(to_check,.what2_name,1)))) > 0
    ){
    cli::cli_abort(c("Missing required fields",
                     "!"="The {.cls tibble} / {.cls data.frame} needs to have some required fields for them to be passed to the function given as the {.arg .source} argument.",
                     "i"="Required fields are {.field {required_fields}}.",
                     "i"="The first data set has the columns {.field {names(purrr::pluck(to_check,3,1))}}",
                     "i"="The second data set has the columns {.field {names(purrr::pluck(to_check,4,1))}}"))
  }


  ## Report on incompatibilities ---------------------------------------------


  only_in_df1 <- dplyr::anti_join(.what1_df,.what2_df,by=set_split_by)
  only_in_df2 <- dplyr::anti_join(.what2_df,.what1_df,by=set_split_by)
  if(nrow(only_in_df1) > 0 ){
    cli::cli_alert_warning(c("!"="The {.field {(.what2_name)}} argument of the function given as {.arg .source} has {nrow(only_in_df1)} rows with no mathch in {.field {(.what1_name)}} and will be ignored."))
  }

  if(nrow(only_in_df2) > 0 ){
    cli::cli_alert_warning(c("!"="The {.field {(.what1_name)}} argument of the function given as {.arg .source} has {nrow(only_in_df2)} rows with no mathch in {.field {(.what2_name)}} and will be ignored."))
  }

  # Do the actual processing work -------------------------------------------


  ## Final preparations -------------------------------------------------------


  .what_df <- .what_df |>
    dplyr::ungroup() |>
    tibble::rownames_to_column(var = ".sl_rowIdx")  |>
    dplyr::mutate(.sl_rowIdx = as.integer(.sl_rowIdx))  |>
    dplyr::mutate(!!!rlang::enexprs(...)) |> #Here we pass on the ... arguments to mutate so that we can set both a=session,b="dsds" and a will be a copy of session and b will be a string
    dplyr::glimpse() |>
    dplyr::rename_with( ~ stringr::str_replace(.x,"^","."),dplyr::all_of(set_split_by)) |>
    dplyr::rowwise() # Row-wise processing is assumed post-nesting

  if(.glue_arguments){
    .what_df <- .what_df |>
      dplyr::mutate_if(is.character, glue)
  }

  ## Apply the DSP function -------------------------------------------------------
  .what_df <- .what_df |>
    purrr::pmap(.f=innerMapFunction,.progress = TRUE) # This is the busy line



  # Finalize function -------------------------------------------------------


  return(.what_df)

}



## INTERACTIVE TESTING
#
library(tidyverse)
library(purrr)
library(progressr)
library(tibble)
library(superassp)
library(furrr)
library(progress)
library(reindeer)


ask_for(ae,"Phonetic = @|I|E")  -> svDF
ask_for(ae,"Word =~ .*") -> csDF


quantify2(svDF,csDF,.source=fake_two_df_fun,.by_bundle = TRUE) -> to_checkBundle
quantify2(svDF,csDF,.source=fake_two_df_fun,.by_bundle = FALSE,speaker.name=session,speaker.dob="dsd") |> glimpse() -> to_checkSession

# reindeer:::create_ae_db() -> ae
# reindeer:::make_dummy_metafiles(ae)
# q1 <- "Utterance != ''"
# q2 <- "Phonetic = V"
# reindeer::query(ae, q2) |>
#   group_by(session,bundle) |>
#   nest()
# (quantify2(q1,
#           q2,
#           .source=fake_two_df_fun,
#           .inside_of = ae,
#           .by_bundle = TRUE) -> out)
#
#
# df <- data.frame(time=seq(0,1,0.1),label=as.character(1:11))


