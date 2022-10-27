



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



quantify <- function(.data,.from,...,.metadata.defaults=list(Gender="Unspecified",Age=35),.by_maxFormantHz=TRUE,.recompute=FALSE,.package="superassp",.handle=NULL){

  #Capture dot arguments for if we want to manipulate them
  dotArgs <- list(...)
  logger::log_debug(paste(dotArgs))

  if(is.null(.handle) && ! is.null(attr(.data,"basePath")) && dir.exists(attr(.data,"basePath"))) {
    # We then need to create a handle object
    utils::capture.output(
      .handle <- emuR::load_emuDB(attr(.data,"basePath"),verbose = TRUE)
    ) -> dbload.info
  }else{
    stop("Could not derive the database path. Please provide an explicit database handle object using the .handle argument. See ?emuR::load_emuDB for details.")
  }

  # We now have a database handle in the .handle variable
  if(is.function(.from)){
    .f <- .from # Here we apply a function to all *.wav files in the database
    logger::log_debug("We got a function in the .from argument")
  }


  if(is.character(.from)){
    if(exists(.from) ){
      logger::log_debug("We got a function name as a character in the .from argument")
      .f <- utils::getFromNamespace(.from,.package)
    }else{
      #The string should here be the name of a track

      if(.from %in% emuR::list_ssffTrackDefinitions(.handle)$name){
        logger::log_debug("We got a function in the .from argument")
        .f <- readtrack
        dotArgs[".field"] <- .from
      }else{
        #In this case, the specified track does not exist
        logger::log_error(paste0("A track named '",.from,"' does not exist in the database."))
      }
    }
  }

  innerMapFunction <- function(.session,.bundle,...){

    p(message=sprintf("Processing session %s (%s)",.session,.bundle))
    #dotdotdot <- as.list(...)

    #logger::log_debug(sprintf("Using settings %s when applying the function to session %s ( bundle %s).",as.character(jsonlite::toJSON(dotdotdot)),.session,.bundle))
    result <- .f(...)

    return(result)
  }

  idCols <- names(.metadata.defaults)

  handlers(global = TRUE)
  old_handlers <- handlers(c("progress"))
  on.exit(handlers(old_handlers), add = TRUE)


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
    tidyr::replace_na(.metadata.defaults) %>%
    dplyr::mutate(dotArgsRT) %>%
    dplyr::mutate(Age=as.integer(round(Age,digits = 0)))


  #This variable is used for filling in missing values in arguments explicitly set in metadata
  precomputedVariableNames <- intersect(names(meta),names(dsp))
  dspColumnsToAdd <- setdiff(names(dsp),names(meta))

  filledMeta <- meta %>%
    dplyr::rows_update(y=dsp %>%
                         dplyr::select(all_of(precomputedVariableNames)),
                       by=idCols,
                       unmatched="ignore")

  # Thsi block will add settings that were set in DSP defaults only
  completedStoredDSPSettings <- filledMeta %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      dplyr::select(dsp,c(idCols,dspColumnsToAdd))
      ,by=idCols) %>%
    tidyr::replace_na(functionDefaults) %>%
    dplyr::mutate(toFile=FALSE)  #Make sure an object is returned

  # We need to choose whether to guide a formant tracker by number of extracted formants in a fixed 0-5000 Hz frequency range
  # or if the 5000 Hz ceiling is instead increased and the default number of formants extracted is kept constant

  if(.by_maxFormantHz && "numFormants" %in% names(completedStoredDSPSettings)){
    completedStoredDSPSettings <- completedStoredDSPSettings %>%
      dplyr::select(-numFormants)
  }

  # What we now need is an 'listOfFiles' to supply to the DSP function
  soundFiles <- emuR::list_files(.handle,emuR:::load_DBconfig(.handle)$mediafileExtension) %>%
      dplyr::rename(listOfFiles=absolute_file_path) %>%
      dplyr::select(-file)

  # The names of columns to keep are now the columns that are defined either in soundFiles or
  # in dspCompletedSettings, and which will be used by the DSP function
  settingsThatWillBeUsed <- intersect(formalArgsNames,
                                      union(names(completedStoredDSPSettings),
                                            names(soundFiles))
                                      ) # This will be used to remove unwanted columns

  #Here we construct the settings that we want to use when applying the specific DSP
  # function to bundles (in specific sessions)
  sessionBundleDSPSettingsDF <-  completedStoredDSPSettings %>%
      dplyr::left_join(soundFiles,by=c("session","bundle")) %>%
      dplyr::select(c("session","bundle",settingsThatWillBeUsed))

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
    dplyr::rowwise() %>%
    dplyr::rename(.session=session,.bundle=bundle) %>%
    dplyr::mutate(dplyr::across(where(is.integer), as.numeric)) %>% ## Fix for wrassp functions that expect "numeric" values, not integers
    dplyr::mutate(temp = list(pmap(cur_data(),.f=innerMapFunction))) %>%
    dplyr::select(-any_of(settingsThatWillBeUsed),-.bundle,-.session)


     resTibble <- appliedDFResultInList %>%
       tibble::rownames_to_column(var = "sl_rowIdx") %>%
       dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))%>%
       dplyr::mutate(out = list(map(temp,as_tibble))) %>%
       tidyr::unnest(out) %>%
       tidyr::unnest(out)


  outDF <- .data %>%
    tibble::rownames_to_column(var = "sl_rowIdx") %>%
    dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx)) %>%
    dplyr::left_join(resTibble, by="sl_rowIdx") %>%
    dplyr::arrange(sl_rowIdx,start_item_id,end_item_id)

  return(outDF)
}



as_tibble.AsspDataObj <- function(x,.field=1, prefix="T",na.zeros=TRUE){
  df <- data.frame(x[[.field]])

  colnames(df) <- paste0(prefix,seq(1,ncol(df)))

  times <- seq(from=attr(x,"startTime"),
               by=1/attr(x,"sampleRate"),
               length.out=nrow(df))
  out <-
    tibble(times_orig=times,
               times_rel=seq(from=0,to=(attr(x,"endRecord")-1)* 1000/attr(x,"sampleRate") ,by=1000/attr(x,"sampleRate")),
               times_norm=times_rel / max(times_rel)
               )
  if(na.zeros){
    out <- as_tibble(out) %>%
      mutate(across(tidyselect::matches("T[0-9]+"), ~ na_if(.x,0)))
  }

  return(out)

}

readtrack <- function(listOfFiles,.field=1,beginTime=0, endTime=0){
  if(! all(file.exists(listOfFiles))){
    msg <- paste0("The input files ",paste(listOfFiles,collapse = ", "), " do not exist.")
    logger::log_error(msg)
    stop(msg)
  }

  trackObj <- wrassp::read.AsspDataObj(listOfFiles)

  if(is.numeric(.field)){
    toremove <- setdiff(seq_along(names(trackObj)),.field)
  }
  if(is.character(.field)){
    toremove <- setdiff(names(trackObj),.field)
  }
  #Not NULL out not needed fields
  for(f in toremove){
    trackObj[f] <- NULL
  }

  return(trackObj)

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

#reindeer:::create_ae_db() -> ae
#reindeer:::make_dummy_metafiles(ae)

out <- ae |>
 ask_for("Phonetic = s") |>
 quantify(.from=forest,windowSize=30)

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


# out %>%
#   tibble::rownames_to_column(var = "sl_rowIdx") %>%
#   dplyr::mutate(sl_rowIdx = as.integer(sl_rowIdx))%>%
#   dplyr::mutate(out = map_dfc(temp,.f=as_tibble))




