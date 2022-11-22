



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

ask_for <- function(.source, .query,.sessions_regex = ".*", .bundles_regex = ".*",.times_from = NULL, .calculate_times = TRUE,.interactive=FALSE){
  if(missing(.source)) stop("Please provide an Emu database handle or the full path to an Emu database in the .source argument")
  if(missing(.query)) stop("Please specify a query in the Emu Query Language.")

  if("character" %in% class(.source) ){
    # We then need to create a handle object
    utils::capture.output(
      .handle <- emuR::load_emuDB(attr(.source,"basePath"),verbose = FALSE)
    ) -> dbload.info
    logger::log_info(paste(dbload.info,collapse = "\n"))
  }
  if( "emuDBhandle" %in% class(.source)){
    .handle <- .source
  }

  res <- emuR::query(emuDBhandle=.handle, query=.query,sessionPattern = .sessions_regex, bundlePattern = .bundles_regex ,timeRefSegmentLevel = .times_from, calcTimes = .calculate_times,verbose = .interactive, resultType = "tibble")
  attr(res,"basePath") <- .handle$basePath #This ensures that we can reattach the database later

  if("character" %in% class(.source) && ! "emuDBhandle" %in% class(.source)){
    DBI::dbDisconnect(.handle$connection) # Gracefully disconnect the connection
    rm(.handle)
  }

  return(res)
}



climb_to <- function(.data,  .attribute_name ,.collapse = TRUE, .skip_times = FALSE, .times_from = NULL, .interactive=FALSE, .handle=NULL) {

  if(is.null(.handle) && ! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .source  <- emuR::load_emuDB(attr(.data,"basePath"),verbose = FALSE)
    ) -> dbload.info
  }else{
    logger::log_error("Could not derive the database path from the segment list.\n Please provide an explicit database handle object using the .handle argument. See ?emuR::load_emuDB for details.")
  }

  res <- emuR::requery_hier(emuDBhandle = .source,seglist= .data,level=.attribute_name,timeRefSegmentLevel = .times_from, calcTimes = !.skip_times,verbose = .interactive, resultType = "tibble")
  attr(res,"basePath") <- .source$basePath #This ensures that we can reattach the database later
  if( is.null(.handle) ) {
    #This means that we created the emuDB database handlere here
    DBI::dbDisconnect(.source$connection) # Gracefully disconnect the connection
    rm(.source)
  }
  return(res)
}

#skip_forward
#skip_backward

scout_forward <- function(.data,  steps_forward, count_from="START" , capture=1, ignore_bundle_boundaries=FALSE, calculate_times = TRUE, times_from = NULL, interactive=FALSE, .handle=NULL) {

  if(is.null(.handle) && ! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .handle <- emuR::load_emuDB(attr(.data,"basePath"),verbose = TRUE)
    ) -> dbload.info
  }else{
    stop("Could not derive the database path. Please provide an explicit database handle object using the .handle argument. See ?emuR::load_emuDB for details.")
  }

  res <- emuR::requery_seq(emuDBhandle = .handle,seglist= .data,offset=steps_forward, offsetRef=count_from,length=capture, ignoreOutOfBounds = ignore_bundle_boundaries, timeRefSegmentLevel = times_from, calcTimes = !skip_times,verbose = interactive, resultType = "tibble")
  attr(res,"basePath") <- .handle$basePath #This ensures that we can reattach the database later
  #DBI::dbDisconnect(.handle$connection) # Gracefully disconnect the connection
  return(res)
}

retreat <- function() {1}

harvest <- function() {1}

peek_at <- function(.x, what=c("levels","links","global_lg","tracks","bundles","sessions","perspectives","files","attributes","local_lg"),...){

  if("emuDBhandle" %in% class(.x)){
    .handle <- .x
  }else{
    if( ! is.null(attr(.x,"basePath")) && dir.exists(attr(.x,"basePath"))) {
      # We then need to create a handle object
      utils::capture.output(
        .handle <- emuR::load_emuDB(attr(.x,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      stop("Could not derive the database path. Please provide an explicit database handle object as '.x'. See ?emuR::load_emuDB for details.")
    }
  }
 al <- list(...)

  what <- match.arg(what, c("levels","links","global_lg","tracks","bundles","sessions","perspectives","files","attributes","local_lg"))

  res <- switch(what,
                levels = tibble::as_tibble(emuR::list_levelDefinitions(.handle)), # name         type    nrOfAttrDefs attrDefNames
                links = tibble::as_tibble(emuR::list_linkDefinitions(.handle)), #type         superlevelName sublevelName
                global_lg =tibble::as_tibble(emuR::list_labelGroups(.handle)), # name values
                perspectives = tibble::as_tibble(emuR::list_perspectives(.handle)), #name          signalCanvasesOrder levelCanvasesOrder
                tracks =tibble::as_tibble(emuR::list_ssffTrackDefinitions(.handle)), #name  columnName fileExtension
                bundles =tibble::as_tibble(emuR::list_bundles(.handle,...)), #session name
                sessions =tibble::as_tibble(emuR::list_sessions(.handle,...)), # name
                files = tibble::as_tibble(emuR::list_files(.handle,...)), #session bundle   file                absolute_file_path
                attributes = tibble::as_tibble(emuR::list_attributeDefinitions(.handle,...)), # name    level   type hasLabelGroups hasLegalLabels
                local_lg = tibble::as_tibble(emuR::list_attrDefLabelGroups(.handle,...)) #name values
  )

  attr(res,"basePath") <- .handle$basePath #This ensures that we can reattach the database later
  # We neeed these to fully specify the results later, and to separate global_lg and local_lg output from each other
  if(what == "local_lg") attr(res,"local_to") <-  al[c("levelName","attributeDefinitionName")]
  if(what == "attributes") attr(res,"local_to") <-  al[c("levelName")]

  #DBI::dbDisconnect(.handle$connection) # Gracefully disconnect the connection
  return(res)
}


define <- function(.x, what=c("level","link","global_lg","track","bundle","session","perspective","file","attribute","local_lg"),...){

  if("emuDBhandle" %in% class(.x)){
    .handle <- .x
  }else{
    if( ! is.null(attr(.x,"basePath")) && dir.exists(attr(.x,"basePath"))) {
      # We then need to create a handle object
      utils::capture.output(
        .handle <- emuR::load_emuDB(attr(.x,"basePath"),verbose = TRUE)
      ) -> dbload.info
      logger::log_info(paste(dbload.info,collapse ="\n"))
    }else{
      stop("Could not derive the database path. Please provide an explicit database handle object as '.x'. See ?emuR::load_emuDB for details.")
    }
  }

  what <- match.arg(what, c("level","link","global_lg","track","bundle","session","perspective","files","attributes","local_lg"))

  al <- list(...)
  if(what == "level" ) {
    if(al[["name"]] %in% emuR::list_levelDefinitions(emuDBhandle = .handle)[["name"]]){
      logger::log_warn('The level with the name \'{al[["name"]]}\' is already defined. Using the existing definition')
    }else{
      if(all(names(al) %in% methods::formalArgs(emuR::add_levelDefinition))) {
        logger::log_info('Adding a level named {al[["name"]]} to the database.')
        utils::capture.output(
          emuR::add_levelDefinition(emuDBhandle = .handle,...)
        ) -> dbload.info
        logger::log_info(paste(dbload.info,collapse ="\n"))
      }else{
        miss <- setdiff(names(al), methods::formalArgs(emuR::add_levelDefinition))
        logger::log_error('Arguments {paste(miss,sep=",")} are not defined. No level named {al[["name"]]} was added.')
      }
    }
    res <- describe(.x=.handle,what)
  }


  return(res)
}

describe_level <- function(.x,name,type= c("SEGMENT","EVENT","ITEM")){
  type <- head(type,1)
  if(is.null(type)) stop("Missing level type")
  if(missing(name)) stop("A level name is required")

  res <- define(.x,what="level",name=name,type=toupper(type))
}
#


furnish <- function(inside,from_what, ... ,.metadata_defaults=list("Gender"="Undefined","Age"=35),.by_maxFormantHz=TRUE,.recompute=FALSE,.package="superassp"){
  if(missing(from_what)) stop("You need to state a function name or the file extension from which the data columns should be gathered")

  #We have an explicitly given database handle, but we don not know if it is a path or if the SQLite connection is still valid
  if(is.character(inside) && stringr::str_ends(inside,"_emuDB") && dir.exists(inside)){
    .handle <- emuR::load_emuDB(inside,verbose = FALSE)
  }
  if("emuDBhandle" %in% class(inside)){
    #reload the database just to make sure that the handle is still valid
    inside <- emuR::load_emuDB(inside$basePath,verbose = FALSE)
  }else{
    stop("The 'inside' argument can only be either a character vector indicating the path to the database, or an emuR database handle.")
  }

  #If we have a function, then we should get some base data for the track calculations
  if(is.function(from_what) || is.function(purrr::safely(get)(from_what)$result)){
    bundles <- emuR::list_bundles(inside) %>%
      dplyr::rename(bundle=name) %>%
      dplyr::mutate(start=0, end=0,start_item_id=0,end_item_id=0)
    attr(bundles,"basePath") <- inside$basePath

    # Here we envoke the special mode of quantify where an intermediate nested result is returned
    quantifications <- quantify(.data=bundles,.from={{from_what}},...,.metadata_defaults=.metadata_defaults,.by_maxFormantHz=.by_maxFormantHz,.recompute=.recompute,.package=.package)

    #parameters <- quantifications %>%
    #  dplyr::select(parameters) %>%
    #  tidyr::unnest(parameters)
    #Overwrite the file extension used by the default by the function is the user said so
    fileExtension <- ifelse(is.null(purrr::pluck(quantifications,"parameters",1,"explicitExt")),
                            superassp::get_extension(from_what),
                            purrr::pluck(quantifications,"parameters",1,"explicitExt"))

    quantifications <- quantifications %>%
      dplyr::rename(dobj= temp) %>%
      dplyr::mutate(file= file.path(inside$basePath,
                                           paste0(session,emuR:::session.suffix),
                                           paste0(bundle,emuR:::bundle.dir.suffix),
                                           paste(bundle,fileExtension,sep="."))) %>%
      dplyr::select(dobj,file)

   #Now finally write teh SSFF files
   quantifications %>%
     furrr::future_pwalk(wrassp::write.AsspDataObj)

  }else{
    # we have a file extension only
    fileExtension <- from_what
  }


  #TODO: Make sure all mentioned tracks are connected up to the file now

  return(quantifications)
}

#print(furnish(ae,forest,nominalF1=2000,explicitExt="ds") -> out)


quantify <- function(.data,.from,...,.by_maxFormantHz=TRUE,.metadata_defaults=list("Gender"="Undefined","Age"=35),.recompute=FALSE,.package="superassp",.handle=NULL){



  #Capture dot arguments for if we want to manipulate them
  dotArgs <- list(...)
  fcall <- match.call(expand.dots = FALSE)
  fcallS <- toString(fcall)
  idCols <- c("Gender","Age")

  logger::log_debug("Function quantify called with arguments {fcallS}")

  ###
  # Derive .handle object
  ##

  #The first possible case is when we have no explicitly set .handle argument, but a segment list with a
  # valid "basePath" attribute set. We then create the database handle from that basePath.
  if(is.null(.handle)) {
    if(! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
      logger::log_debug("No explicit .handle argument found")
      # We then need to create a handle object
      utils::capture.output(
        .handle <- emuR::load_emuDB(attr(.data,"basePath"),verbose = TRUE)
      ) -> dbload.info
    }else{
      logger::log_debug("Got .handle={.handle}")
      stop("Could not derive the database path. Please provide an explicit database handle object using the .handle argument. See ?emuR::load_emuDB for details.")
    }
  }else{
    # we have an explicitly given database handle, but we don not know if it is a path or if the SQLite connection is still valid
    if(is.character(.handle) && stringr::str_ends(.handle,"_emuDB") && dir.exists(.handle)){
      .handle <- emuR::load_emuDB(.handle,verbose = FALSE)
    }
    if("emuDBhandle" %in% class(.handle)){
      #reload the database just to make sure that the handle is still valid
      .handle <- emuR::load_emuDB(.handle$basePath,verbose = FALSE)
    }else{
      stop("The .handle argument can only be either a character vector indicating the path to the database, or an emuR database handle.")
    }
  }

  # This sets the default input media file extension, which handled the case when a
  #function is called to compute a list or SSFF track result based on a wave file
  # This variable will then be set to something else if
  # we want to read in a pre-computed SSFF track stored on disk instead.
  inputSignalsExtension <- emuR:::load_DBconfig(.handle)$mediafileExtension


  # This section handles the case where we want to compute signals or a
  # list of results from the wave file directly??p
  if(is.function(.from) || is.function(purrr::safely(get)(.from)$result)){
    if(is.function(.from)){
      .f <- .from
    }else{
      .f <- purrr::safely(get)(.from)$result
    }

    funName <- fcall$.from
    logger::log_debug("We got a function in the .from argument : '{funName}'")
  }else{
    # Ok, so not a function, so we need to make sure that the .from argument is a string
    # and the name of a track in the database
    if(is.character(.from) && .from %in% emuR::list_ssffTrackDefinitions(.handle)$name){
      #The string should here be the name of a track
      logger::log_debug("We got a SSFF track field in the .from argument : {.from}")
      .f <- readtrack
      funName <- "readtrack"
      inputSignalsExtension <- emuR::list_ssffTrackDefinitions(ae) %>%
        dplyr::filter(name==.from) %>%
        dplyr::select(fileExtension) %>%
        purrr::pluck(1)
      dotArgs["field"] <- .from
    }else{
      stop("The .from argument needs to be the name of a track in the database or a function that should do the signal processing.")
    }
  }



  #This function wraps a call so that the call parameters may be logged and so that we get a progress bar
  innerMapFunction <- function(.session,.bundle,...){

    p(message=sprintf("Processing session %s (%s)",.session,.bundle))
    dotdotdot <- list(...)
    dotdotdotS <- toString(dotdotdot)

    logger::log_debug("[{.session}:{.bundle})] Using settings \n{dotdotdotS}\n when applying the function {funName}.")
    result <- .f(...)

    return(result)
  }


  progressr::handlers(global = TRUE)
  old_handlers <- progressr::handlers(c("progress"))
  on.exit(progressr::handlers(old_handlers), add = TRUE)


  #Logic that concerns formal arguments
  formalArgsNames <- methods::formalArgs(.f)
  functionDefaults <- as.list(formals(.f))


  #Logic that concerns actual arguments
  dotArgsRT <- tibble::as_tibble_row(dotArgs)
  dotArgsNames <- names(dotArgs)

  # Make sure that we have a DSP default settings data.frame
  dsp <- reindeer:::dspp_metadataParameters(recompute=.recompute) %>%
    tidyr::replace_na(list(Gender="Unspecified"))

  #After this we can be sure that parameters set per session or bundle are  available
  # but have been overwritten by explicitly set settings given to this function as we proceed
  meta <- reindeer:::get_metadata(.handle,manditory=idCols) %>%
    dplyr::mutate(Gender=as.character(Gender),Age=as.integer(round(Age,digits = 0))) %>%
    tidyr::replace_na(.metadata_defaults) %>%
    dplyr::mutate(dotArgsRT)



  #This variable is used for filling in missing values in arguments explicitly set in metadata
  precomputedVariableNames <- intersect(names(meta),names(dsp))
  dspColumnsToAdd <- setdiff(names(dsp),names(meta))

  filledMeta <- meta %>%
    dplyr::rows_update(y=dsp %>%
                         dplyr::select(all_of(precomputedVariableNames)),
                       by=idCols,unmatched = "ignore")


  # This block will add settings that were set in DSP defaults only
  completedStoredDSPSettings <- filledMeta %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      dplyr::select(dsp,all_of(idCols),all_of(dspColumnsToAdd))
      ,by=idCols) %>%
    tidyr::replace_na(functionDefaults) %>%
    dplyr::mutate(toFile=FALSE)


  # We need to choose whether to guide a formant tracker by number of extracted formants in a fixed 0-5000 Hz frequency range
  # or if the 5000 Hz ceiling is instead increased and the default number of formants extracted is kept constant

  if(.by_maxFormantHz && "numFormants" %in% names(completedStoredDSPSettings)){
    completedStoredDSPSettings <- completedStoredDSPSettings %>%
      dplyr::select(-numFormants)
  }

  # What we now need is an 'listOfFiles' to supply to the DSP function
  signalFiles <- emuR::list_files(.handle,inputSignalsExtension) %>%
      dplyr::rename(listOfFiles=absolute_file_path) %>%
      dplyr::select(-file)

  # The names of columns to keep are now the columns that are defined either in signalFiles or
  # in dspCompletedSettings, and which will be used by the DSP function
  settingsThatWillBeUsed <- intersect(formalArgsNames,
                                      union(names(completedStoredDSPSettings),
                                            names(signalFiles))
                                      ) # This will be used to remove unwanted columns

  #Here we construct the settings that we want to use when applying the specific DSP
  # function to bundles (in specific sessions)
  sessionBundleDSPSettingsDF <-  completedStoredDSPSettings %>%
      dplyr::left_join(signalFiles,by=c("session","bundle")) %>%
      dplyr::select(session,bundle,all_of(settingsThatWillBeUsed))

  # Now we need to transfer the session / bundle settings to the segment list
  # to get start and end times too into the call
  segmentDSPDF <- .data %>%
    tibble::rownames_to_column(var = "sl_rowIdx") %>%
    dplyr::left_join(sessionBundleDSPSettingsDF,by=c("session","bundle")) %>%
    dplyr::rename(beginTime=start,endTime=end) %>%
    dplyr::select(all_of(c("session","bundle",settingsThatWillBeUsed)))

  #Set up the progress bar
  num_to_do <- nrow(segmentDSPDF)
  cat("\nProcessing",num_to_do,"segments.\n")
  p <- progressor(num_to_do)


  # Here we apply the DSP function once per row and with settings comming from
  # the columns in the data frame
  appliedDFResultInList <- segmentDSPDF %>%
    dplyr::rename(.session=session,.bundle=bundle) %>%
    dplyr::mutate(dplyr::across(where(is.integer), as.numeric)) %>%
    dplyr::rowwise() %>% ## Fix for wrassp functions that expect "numeric" values, not integers
    dplyr::mutate(temp = list(pmap(cur_data(),.f=innerMapFunction))) %>%
    #dplyr::select(-any_of(settingsThatWillBeUsed),-.bundle,-.session) %>%
    dplyr::select(-.bundle,-.session) %>%
    tidyr::nest(parameters=c(tidyselect::everything(), -temp,-toFile)) %>%
    tidyr::unnest(temp)


  #Mark the special case where .data is called by provide()
  if(setequal(names(.data),c("start_item_id","bundle", "end", "end_item_id", "session", "start"))){
    #Activate the special case at the end of processing where SSFF tracks and lists are not expanded
    # but just returned
    logger::log_debug("Preparing to return a tibble of bundles and AsspDataObj")
    provideOutDF <-  list_bundles(.handle) %>%
      dplyr::bind_cols(appliedDFResultInList) %>%
      dplyr::rename(bundle=name)

    return(provideOutDF)
  }else{
    #The default case, in which we are processing a segment list
    resTibble <- appliedDFResultInList %>%
      dplyr::select(-any_of(settingsThatWillBeUsed),-parameters) %>%
      tibble::rownames_to_column(var = "sl_rowIdx") %>%
      dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))%>%
      dplyr::ungroup() %>%
      dplyr::mutate(out = map(temp,as_tibble)) %>%
      dplyr::select(-temp) %>%
      tidyr::unnest(out)


    quantifyOutDF <- .data %>%
      tibble::rownames_to_column(var = "sl_rowIdx") %>%
      dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx)) %>%
      dplyr::left_join(resTibble, by="sl_rowIdx") %>%
      dplyr::arrange(sl_rowIdx,start_item_id,end_item_id)

    return(quantifyOutDF)
  }

}

readtrack <- function(listOfFiles,field="1",beginTime=0, endTime=0,sample_start=0, sample_end=0){

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

# reindeer:::create_ae_db() -> ae
# reindeer:::make_dummy_metafiles(ae)
#add_ssffTrackDefinition(ae,"bw","bw","bw","forest")

out <- ae |>
  ask_for("Phonetic =~ '^.*[i:]'") |>
 quantify(.from=forest,windowSize=30)|>
  glimpse()
#
# out2 <- ae |>
#   ask_for("Phonetic =~ '^.*[i:]'") |>
#   quantify(.from=fake_voice_report,windowSize=30) %>%
#   glimpse()

# out3 <- ae |>
#   ask_for("Phonetic =~ '^.*[i:]'") |>
#   quantify("fm") %>%
#   glimpse()

out <- ae |>
  furnish(forest) |>
  glimpse()

# ae |>
#  search_for("Syllable = S | W" ) |>
#  climb_to("Word") |>
#  scout_ahead(steps_forward=2, look_ahead=2, look_backwards=2) |>
#  quantify_from(.source=ksvF0, minF=50)

# ae |>
#  search_for("Syllable = S | W" ) |>
#  scout_backwards(steps_forward=2, look_ahead=2, look_backwards=2)
#  quantify_from(.source="f0", .transform=st)

#ae |>
  #  ask_for("Word =~ .*") |>
  #  capture_all(level="Phonetic") |>

#forest("~/Desktop/a1.wav",toFile=FALSE) -> fms
#readtrack("~/Desktop/a1.fms") -> fms
#print(as_tibble(fms))



