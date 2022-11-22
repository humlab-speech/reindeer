




#' Automatic annotation of intonation using MOMEL and INTSINT labels
#'
#' This function transcribes the utterances specified in a segment list using
#' the MOMEL/INTSINT framework \insertCite{Hirst:2005jg,Hirst:2011wq}{reindeer} and insert
#' the EVENT labels into a special tier ("Intsint" by default). The function
#' will then call the Praat routines \insertCite{Hirst:2007ty}{reindeer} and the
#' external 'momel' program to make the calculations, and insert the resulting
#' levels onto the transcription tier. Please note that calling multiple
#' external programs results in some overhead, so please allow some time to
#' complete the transcription for large sets of utterances.
#'
#' As a side effect, this function also sets up two label groups on the
#' attribute definition in which INTSINT labels are inserted (default
#' "Intsint"):
#'
#' 1. Absolute_tones (T,M,B), and 2. Relative_tones (H,U,S,D,L)
#'
#' which may be used in queries. In order to use them in larger queries, the
#' user has to define links between the created level and existing transcription
#' levels using [reindeer::add_linkDefinition], and links between created labels
#' on the two tiers using [reindeer::autobuild_linkFromTimes].
#'
#' At the start of the MOMEL procedure, Pitch will computed using a wide
#' frequency search. The user may guide the search by supplying more restrictive
#' maximum and minimum fundamental frequency values (minF, maxF). By default,
#' the search considers a moderate pitch span (pitchSpan=1.5), but if very
#' expressive speech is investigated, `pitchSpan` may be increased to 2.5.
#'
#' The MOMEL / INTSINT procedure also returns the MOMEL deduced f0  target
#' associated with the INTSINT label, as well as the f0 predicted by the INTSINT
#' model. These are returned by the function but currently not inserted as
#' labels in the transcriptions.
#'
#'
#' @param emuDBhandle The Emu database handle.
#' @param seglist A segment list resulting from a [query] function call,
#'   containing utterances that should be transcribed.
#' @param windowSize The window size (in ms) used when extracting f0. One hald
#'   window size of duration will also be added to each end of all extracted
#'   utterance segments to allow f0 target points to be discovered right at the
#'   beginning and end of utterances.
#' @param minF The low end of the fundamental frequency range to consider when
#'   searching for a pitch contour.
#' @param maxF The high end of the fundamental frequency range to consider when
#'   searching for a pitch contour.
#' @param pitchSpan The maximum pitch span to consider. Defaults to 1.5 octave,
#'   but could be expanded to 2.5 for expressive speech.
#' @param maximumError The maximum error allowed by MOMEL. See the momel source
#'   code for more information.
#' @param reducWindowSize The size of the reduced analysis window used by MOMEL.
#'   See the momel source code for more information.
#' @param minimalDistance The minimal distance between INTSINT labels. See the
#'   momel source code for more information.
#' @param minimalFrequencyRatio See the momel source code for information on
#'   this parameter.
#' @param intsint.level The name of the transcription level that should be
#'   created and used for storing annotations. If this level already exists, it
#'   will be used if attribute definitions match the ones given here.
#' @param attribute.definitions The names of attribute definitions to set up for
#'   the transcription level. Currently not used.
#' @param absolute.tone.labels The INTSINT labels that should be set up as
#'   members of the 'Absolute_tones' label group.
#' @param relative.tone.labels The INTSINT labels that should be set up as
#'   members of the 'Relative_tones' label group.
#' @param force Force insertion of transcriptons. Currently not implemented.
#' @param verbose Should output be displayed for all major steps?
#' @param praat_path An explicit path to the Praat executable. Usually not
#'   required.
#'
#' @return A [tibble] containing computed INTSINT (and MOMEL) information for
#'   each bundle, with `start` time relative to the start of the bundle media
#'   file.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' reindeer:::unlink_emuRDemoDir()
#' reindeer:::create_ae_db(verbose = TRUE) -> emuDBhandle
#' add_ssffTrackDefinition(emuDBhandle,"f0",onTheFlyFunctionName = "ksvF0")
#' query(emuDBhandle,"Intonational =~ .*") -> sl
#' annotate_INTSINT_MOMEL(emuDBhandle,sl) -> momelTab
#' add_perspective(emuDBhandle, "INTSINT")
#' set_signalCanvasesOrder(emuDBhandle,"INTSINT",c("OSCI","SPEC","f0"))
#' set_levelCanvasesOrder(emuDBhandle,"INTSINT",c("Intsint"))
#' serve(emuDBhandle)
#' }

annotate_INTSINT_MOMEL <- function(emuDBhandle,
                                   seglist,
                                   windowSize=30,
                                   minF=60,
                                   maxF=750,
                                   pitchSpan=1.5,
                                   maximumError=1.04,
                                   reducWindowSize=20,
                                   minimalDistance=20,
                                   minimalFrequencyRatio=0.05,
                                   intsint.level="Intsint",
                                   attribute.definitions=c("Momel","Intsint","IntsintMomel"),
                                   absolute.tone.labels=c("T","M","B"),
                                   relative.tone.labels=c("H","U","S","D","L"),
                                   force=FALSE,
                                   verbose=FALSE,
                                   praat_path=NULL){

  if(! superassp::have_praat(praat_path)){
    stop("Could not find praat. Please specify a full path.")
  }

  if(!verbose){
    logger::log_threshold(logger::WARN)
  }
  lvls <- list_levelDefinitions(emuDBhandle)

  if(! intsint.level %in% lvls$name){
    logger::log_info("Creating a new level '",intsint.level,"'")
    add_levelDefinition(emuDBhandle = emuDBhandle, name=intsint.level,type="EVENT",verbose = verbose,rewriteAllAnnots = TRUE)
    #Enable later when inserting labels into different attributes works ok.
    # for(ad in setdiff(attribute.definitions,intsint.level)){
    #    logger::log_info("Creating a new attribute '",ad,"' for level '",intsint.level,"'")
    #    add_attributeDefinition(emuDBhandle = emuDBhandle,levelName=intsint.level,name=ad,type="STRING",rewriteAllAnnots = TRUE,verbose = verbose)
    # }
    set_legalLabels(emuDBhandle = emuDBhandle,
                    levelName=intsint.level,
                    attributeDefinitionName=intsint.level,
                    legalLabels=c(absolute.tone.labels,relative.tone.labels))

    add_attrDefLabelGroup(emuDBhandle = emuDBhandle,
                          levelName=intsint.level,
                          attributeDefinitionName=intsint.level,
                          labelGroupName="Absolute_tones",
                          labelGroupValues = absolute.tone.labels)

    add_attrDefLabelGroup(emuDBhandle = emuDBhandle,
                          levelName=intsint.level,
                          attributeDefinitionName=intsint.level,
                          labelGroupName="Relative_tones",
                          labelGroupValues = relative.tone.labels)
  }else{
    #We should check existing definitions
    ads <- list_attributeDefinitions(emuDBhandle = emuDBhandle,levelName=intsint.level)$name
    if(! all(ads %in% attribute.definitions) && all(attribute.definitions %in% ads)){

      stop("The attribute definitions set for the level '",intsint.level,"' conflict with the ones given as argument to the function.\n You need to remove it first using the 'remove_levelDefinition' function, or fix the conflict.\n Removing the level definition will of course discard all information in it, so please make sure you want to overwrite it.")
    }
  }



  dsp_directory <- superassp:::make_dsp_environment()

  emuR:::load_DBconfig(emuDBhandle) -> dbConfig

  praat_script <- ifelse(PRAAT_DEVEL== TRUE,
                         file.path("inst","praat","Momel-Intsint","processINTSINTMOMEL.praat"),
                         file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","Momel-Intsint","processINTSINTMOMEL.praat")
  )

  fl <- list_files(emuDBhandle,fileExtension=dbConfig$mediafileExtension)
  seglist <- seglist %>%
    dplyr::left_join(fl,by=c("session","bundle")) #Augment the data by segment list data with file information


  plugin_script_path <- ifelse(PRAAT_DEVEL== TRUE,
                               file.path("inst","praat","Momel-Intsint","plugin_momel-intsint"),
                               file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","Momel-Intsint","plugin_momel-intsint"))

  #Copy additional files
  copied <- file.copy(from=plugin_script_path,to = dsp_directory,overwrite = TRUE,recursive = TRUE)


  intsintmomel <- superassp::cs_wrap_praat_script(praat_location = superassp::get_praat(),
                                             script_code_to_run = readLines(praat_script),
                                             directory=dsp_directory,
                                             return="last-argument")


  outputFileName <- file.path(dsp_directory,"MOMELINTSINT.csv")


  for(r in 1:nrow(seglist)){
    #Just to get sample rate
    inWav <- wrassp::read.AsspDataObj(fname = seglist[[r,"absolute_file_path"]],
                                      begin=0,
                                      end=1,
                                      samples=TRUE)
    sr <- attr(inWav,"sampleRate")
    # Now add an analysis window size to the extracted signal
    halfWindow <- windowSize / 1000 * sr /2
    inWav <- wrassp::read.AsspDataObj(fname = seglist[[r,"absolute_file_path"]],
                                     begin = seglist[[r,"sample_start"]] - halfWindow,
                                     end = seglist[[r,"sample_end"]] + halfWindow,
                                     samples = TRUE)
    outFileName <- file.path(dsp_directory,paste0(r,".wav"))
    wrassp::write.AsspDataObj(file = outFileName,dobj = inWav)
  }

  logFileName <- file.path(dsp_directory,"MOMELINTSINT.log")
  # sentence Input_Directory /Users/frkkan96/Desktop/INT/
  #   #sentence Momel_parameters 30 60 750 1.04 20 5 0.05
  #   integer Window_length_(ms) 30
  # integer Minimum_f0_(Hz) 60
  # integer Maximum_f0_(Hz) 750
  # real Pitch_span 1.5 (=normal, 2.5=expressive speech)
  # real Maximum_error 1.04
  # integer Reduced_window_length_(ms) 20
  # integer Minimal_distance_(ms) 20
  # real Minimal_frequency_ratio 0.05
  # sentence Output_file /Users/frkkan96/Desktop/INT/MOMELINTSINT.csv


  #sink(file = logFileName,type = c("output", "message"))
  csvFile <- intsintmomel(dsp_directory,
                       windowSize,
                       minF,
                       maxF,
                       pitchSpan,
                       maximumError,
                       reducWindowSize,
                       minimalDistance,
                       minimalFrequencyRatio,
                       outputFileName)
  #sink()

  momelTab <- read.delim(csvFile,sep = ";",na.strings = c("undefined","NA",""),strip.white = TRUE) %>%
    dplyr::transmute(sl_rowIdx=as.character(file),level="Intsint",attribute=tier,time=tmin*1000,`labels`=text)



  outTab <- seglist %>%
    tibble::rownames_to_column(var = "sl_rowIdx")  %>%
    dplyr::select(sl_rowIdx,session,bundle,start) %>%
    dplyr::left_join(momelTab,by ="sl_rowIdx") %>%
    dplyr::mutate(start=(start + time + windowSize /2 )) %>%
    dplyr::select(session, bundle,level,attribute,start,`labels`) %>%
    dplyr::arrange(session, bundle,level,start,attribute,`labels`)

  # explicit removal of pitch level attributes, since multiple attributes are not supported
  # by the emuR::create_itemsInLevel function

  outTab <- outTab %>%
    dplyr::filter(attribute == "Intsint")

  out <- create_itemsInLevel(emuDBhandle,itemsToCreate = outTab  ,verbose =verbose,rewriteAllAnnots = TRUE)

  superassp:::clear_dsp_environment(dsp_directory)
  return(outTab)

}




#
# # sentence SoundDirectory ../../../tests/signalfiles/DDK
# # real BeginTime 0.0
# # real EndTime 0.0
# # real Time_step 0.005
# # real Minimum_pitch 100.0
# # real Silence_threshold_(dB) -9.0
# # real Minimum_silent_interval_duration_(s) 0.05
# # real	Minimum_sounding_interval_duration_(s) 0.025
# # text Consonant_label C
# # text Vowel_label V
# # #text Sequence_label DDK
# # real Sequence_silence_threshold -25.0
# # real Sequence_minimum_duration 0.100
# # text Mediafile_extension wav
#
# annotate_DDK <- function(emuDBhandle,
#                          seglist,
#                          windowSize = 30,
#                          minF = 60,
#                          cvThreshold= -9.0,
#                          sequenceThreshold=-25.0,
#                          minConsonantDuration=0.050,
#                          minVowelDuration=0.025,
#                          minSequenceDuration=0.100,
#                          consonantLabel="C",
#                          vowelLabel="V",
#                          ddkSequenceLabel="DDK",
#                          ddkSyllablesLabel="DDK Syllables",
#                          ddkCVLabel="DDK CV",
#                          return.wide = TRUE,
#                          force = FALSE,
#                          verbose = FALSE,
#                          praat_path = NULL) {
#
#
#   if(! superassp::have_praat(praat_path)){
#     stop("Could not find praat. Please specify a full path.")
#   }
#
#   if(!verbose){
#     logger::log_threshold(logger::WARN)
#   }
#   lvls <- list_levelDefinitions(emuDBhandle)
#
#   if(! ddkSequenceLabel %in% lvls$name){
#     logger::log_info("Creating a new level '",ddkSequenceLabel,"'")
#     add_levelDefinition(emuDBhandle = emuDBhandle, name=ddkSequenceLabel,type="SEGMENT",verbose = verbose,rewriteAllAnnots = TRUE)
#     #Add different kints of sequence labels later
#     # set_legalLabels(emuDBhandle = emuDBhandle,
#     #                 levelName=ddkSequenceLabel,
#     #                 attributeDefinitionName=ddkSequenceLabel,
#     #                 legalLabels=c(ddkSequenceLabel))
#
#     # add_attrDefLabelGroup(emuDBhandle = emuDBhandle,
#     #                       levelName=intsint.level,
#     #                       attributeDefinitionName=intsint.level,
#     #                       labelGroupName="Absolute_tones",
#     #                       labelGroupValues = absolute.tone.labels)
#     #
#     # add_attrDefLabelGroup(emuDBhandle = emuDBhandle,
#     #                       levelName=intsint.level,
#     #                       attributeDefinitionName=intsint.level,
#     #                       labelGroupName="Relative_tones",
#     #                       labelGroupValues = relative.tone.labels)
#   }else{
#     #We should check existing definitions
#     ads <- list_attributeDefinitions(emuDBhandle = emuDBhandle,levelName=intsint.level)$name
#     if(! all(ads %in% attribute.definitions) && all(attribute.definitions %in% ads)){
#
#       stop("The attribute definitions set for the level '",intsint.level,"' conflict with the ones given as argument to the function.\n You need to remove it first using the 'remove_levelDefinition' function, or fix the conflict.\n Removing the level definition will of course discard all information in it, so please make sure you want to overwrite it.")
#     }
#   }
#
#   emuR:::load_DBconfig(emuDBhandle) -> dbConfig
#
#   praat_script <- ifelse(PRAAT_DEVEL== TRUE,
#                          file.path("inst","praat","Momel-Intsint","processINTSINTMOMEL.praat"),
#                          file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","Momel-Intsint","processINTSINTMOMEL.praat")
#   )
#
#   fl <- list_files(emuDBhandle,fileExtension=dbConfig$mediafileExtension)
#   seglist <- seglist %>%
#     dplyr::left_join(fl,by=c("session","bundle")) #Augment the data by segment list data with file information
#   intsintmomel <- tjm.praat::wrap_praat_script(praat_location = get_praat(),
#                                                script_code_to_run = readLines(praat_script)
#                                                ,return="last-argument")
#   inDir <- file.path(tempdir(check=TRUE),"INT")
#   unlink(inDir,force=TRUE,recursive=TRUE)
#   dir.create(inDir)
#
#   script_path <- ifelse(PRAAT_DEVEL== TRUE,
#                         file.path("inst","praat","Momel-Intsint","plugin_momel-intsint"),
#                         file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","Momel-Intsint","plugin_momel-intsint"))
#
#
#
#   intsintmomel <- tjm.praat::wrap_praat_script(praat_location = superassp::get_praat(),
#                                                script_code_to_run = readLines(praat_script)
#                                                ,return="last-argument")
#   #Copy additional files
#   copied <- file.copy(from=script_path,to = tempdir(),overwrite = TRUE,recursive = TRUE)
#
#
#   outFile <- file.path(inDir,"MOMELINTSINT.csv")
#
#   for(r in 1:nrow(seglist)){
#     #Just to get sample rate
#     inWav <- wrassp::read.AsspDataObj(fname = seglist[[r,"absolute_file_path"]],
#                                       begin=0,
#                                       end=1,
#                                       samples=TRUE)
#     sr <- attr(inWav,"sampleRate")
#     # Now add an analysis window size to the extracted signal
#     halfWindow <- windowSize / 1000 * sr /2
#     inWav <- wrassp::read.AsspDataObj(fname = seglist[[r,"absolute_file_path"]],
#                                       begin = seglist[[r,"sample_start"]] - halfWindow,
#                                       end = seglist[[r,"sample_end"]] + halfWindow,
#                                       samples = TRUE)
#     outFileName <- file.path(inDir,paste0(r,".wav"))
#     wrassp::write.AsspDataObj(file = outFileName,dobj = inWav)
#   }
#   outputFileName <- file.path(inDir,"MOMELINTSINT.csv")
#   logFileName <- file.path(inDir,"MOMELINTSINT.log")
#   # sentence Input_Directory /Users/frkkan96/Desktop/INT/
#   #   #sentence Momel_parameters 30 60 750 1.04 20 5 0.05
#   #   integer Window_length_(ms) 30
#   # integer Minimum_f0_(Hz) 60
#   # integer Maximum_f0_(Hz) 750
#   # real Pitch_span 1.5 (=normal, 2.5=expressive speech)
#   # real Maximum_error 1.04
#   # integer Reduced_window_length_(ms) 20
#   # integer Minimal_distance_(ms) 20
#   # real Minimal_frequency_ratio 0.05
#   # sentence Output_file /Users/frkkan96/Desktop/INT/MOMELINTSINT.csv
#
#
#   #sink(file = logFileName,type = c("output", "message"))
#   csvFile <- intsintmomel(inDir,
#                           windowSize,
#                           minF,
#                           maxF,
#                           pitchSpan,
#                           maximumError,
#                           reducWindowSize,
#                           minimalDistance,
#                           minimalFrequencyRatio,
#                           outputFileName)
#   #sink()
#
#   momelTab <- read.delim(csvFile,sep = ";",na.strings = c("undefined","NA",""),strip.white = TRUE) %>%
#     dplyr::transmute(sl_rowIdx=as.character(file),level="Intsint",attribute=tier,time=tmin*1000,`labels`=text)
#
#
#
#   outTab <- seglist %>%
#     tibble::rownames_to_column(var = "sl_rowIdx")  %>%
#     dplyr::select(sl_rowIdx,session,bundle,start) %>%
#     dplyr::left_join(momelTab,by ="sl_rowIdx") %>%
#     dplyr::mutate(start=(start + time + windowSize /2 )) %>%
#     dplyr::select(session, bundle,level,attribute,start,`labels`) %>%
#     dplyr::arrange(session, bundle,level,start,attribute,`labels`)
#
#
#   out <- create_itemsInLevel(emuDBhandle,itemsToCreate = outTab %>%
#                                dplyr::filter(attribute=="Intsint") ,verbose =verbose,rewriteAllAnnots = TRUE)
#
#   if(return.wide){
#     outTab <- outTab %>%
#       tidyr::pivot_wider(names_from="attribute",values_from = "labels") %>%
#       dplyr::mutate(across(IntsintMomel:Momel, utils::type.convert))
#   }
#   return(outTab)
#
# }



#' Perform voice activity detection across a database
#'
#' Voice activity detection is applied to a database to find portions of speech
#' signals where spoken communication is likely to have occured, to make
#' transcription work more efficient in databases with silences or many small
#' utterances that should be disrecarded. The segmentation is intended to be
#' used for indexing and easy navigation of the database only, and should not
#' inserted into a hierarchy of levels. The intended use of this function is
#' instead to supply the result of a "VAD == SPEECH" [reindeer::query] call to a
#' [reindeer::serve] or [reindeer:write_bundleList] so that the annotations can
#' be used for efficient navigation of a database. If not helpful in the
#' recording settings used, the user can rerun this function and with more
#' applicable thresholds, overwriting previously generated labels.
#'
#' @details Sections thought to contain speech will be marked in the `levelname`
#'   level by a SEGMENT with the label SPEECH. The `levelname` level will be
#'   cleared before inserting labels if this function is applied again to the
#'   database. The speech segmentation model of the pyannote-audio framework
#'   is used in speech segementation \insertCite{Bredin.2019,Bredin.2021}{reindeer}
#'
#' @param emuDBhandle An [emuR] database handle.
#' @param levelname The name of fhe segmentation level (and attribute) to create
#'   to hold the annotations of speech.
#' @param speech_probability_threshold  The probability threshold above which
#'   the model will percieve the signal to contain speech.
#' @param nospeech_probability_threshold The probability threshold below which
#'   the model will percieve the signal to contain non-speech.
#' @param minimum_speech_duration The minimum duration of a section of speech to
#'   consider (in seconds).
#' @param minimum_nonspeech_duration The minimum duration of a portion that
#'   could be non-speech (in seconds).
#'
#' @return A tibble
#' @export
#' @references
#' \insertAllCited{}
#'
annotate_voiceactivity <- function(emuDBhandle,
                                   levelname="VAD",
                                   speech_probability_threshold = 0.6,
                                   nospeech_probability_threshold = 0.4,
                                   minimum_speech_duration = 0.2,
                                   minimum_nonspeech_duration = 0.1){

  fl <- list_files(emuDBhandle,
                   emuR:::load_DBconfig(emuDBhandle)$mediafileExtension) %>%
    dplyr::select(-file) %>%
    dplyr::rename(listOfFiles = absolute_file_path)

  if(! levelname %in% list_levelDefinitions(emuDBhandle)$name ){
    emuR::remove_levelDefinition(emuDBhandle,name=levelname,force = TRUE)
    emuR::add_levelDefinition(emuDBhandle,name=levelname,type="SEGMENT",verbose = FALSE)
  }

  for(f in 1:nrow(fl)){
    soundFile <- fl[[f,"listOfFiles"]]
    py$soundFile <- reticulate::r_to_py(soundFile)
    py$speech_probability_threshold <- reticulate::r_to_py(speech_probability_threshold)
    py$nospeech_probability_threshold <- reticulate::r_to_py(nospeech_probability_threshold)
    py$minimum_speech_duration <- reticulate::r_to_py(minimum_speech_duration)
    py$minimum_nonspeech_duration <- reticulate::r_to_py(minimum_nonspeech_duration)

    reticulate::py_run_string("from pyannote.audio.pipelines import VoiceActivityDetection\
pipeline = VoiceActivityDetection(segmentation=\"pyannote/segmentation\")\
HYPER_PARAMETERS = {\
  \"onset\": speech_probability_threshold, \
  \"offset\": nospeech_probability_threshold,\
  \"min_duration_on\": minimum_speech_duration,\
  \"min_duration_off\": minimum_nonspeech_duration\
}\
pipeline = pipeline.instantiate(HYPER_PARAMETERS)\
vad = pipeline(soundFile)")
    currSegmentation <- readr::read_delim(py$vad$to_lab(),delim = " ",col_names = FALSE,col_types="ddc") %>%
      dplyr::rename(start= X1, end=X2, label=X3) %>%
      mutate(listOfFiles = soundFile)

    if(f == 1){
      segmentation <- currSegmentation
    }else{
      segmentation <- segmentation %>%
        bind_rows(currSegmentation)
    }
  }
  annotations <- fl %>%
    dplyr::left_join(segmentation,by="listOfFiles") %>%
    dplyr::select(-listOfFiles) %>%
    dplyr::rename(labels= label) %>%
    dplyr::mutate(start=start *1000, end=end * 1000,level = levelname,attribute = levelname,start_item_seq_idx=1)

  emuR::create_itemsInLevel(emuDBhandle,itemsToCreate = annotations)
  return(annotations)
}

#annotate_PRAATDET <- function(filePath){

#   form File info
#   comment Full path to EGG files
#   text directory /Users/jkirby/Projects/egg/praatdet/examples/
#     comment Full path to TextGrids
#   text textgrids /Users/jkirby/Projects/egg/praatdet/examples/grids/
#     comment Name of output file (written to same path as EGG files)
#   word outfile egg_out.txt
#   comment Extension for audio file (.wav, .egg, etc.)
#   word extension .wav
#   comment Channel of audio file containing EGG signal
#   integer eggChan 1
#   comment Start from a particular token?
#     integer startFile 1
#   comment Tier of interest (if irrelevant, leave as default)
#   integer intervalTier 3
#   comment Label of interval of interest (blank for none/all)
#   word intervalLabel v
#   comment Number of interval of interest (0 for none/all)
#   integer intervalNum 0
#   comment Separator (-, _ ...) when parsing token names
#   word separator _
#   comment Minimum and maximum f0 thresholds
#   integer minF0 75
#   integer maxF0 600
#   comment k: Smoothing window size parameter (points on each side)
#   integer k 10
#   comment Threshold for Howard's method
#     real    threshold 3/7
#     comment Filter frequency cutoff
#     integer passFrequency 40
#     comment Filter cutoff smoothing
#     integer smoothHz 20
#     comment Manually edit points and periods?
#     boolean manualCheck 0
#     comment Use existing PointProcess files, if available?
#     boolean useExistingPP 0
#     comment Invert signal (if your EGG has closed=down for some reason)
#     boolean invertSignal 0
# endform
#}




#' Mark periods as events
#'
#' This function uses Praat \insertCite{Praatdoingphoneti:2022}{reindeer} to
#' extract a periodicity contour, and the associated intensity. The estimated
#' timing of each glottal cycle is marked at the maximum peak, and the timing
#' and intensity of that peak is then marked as an event with associated label in the
#' `levelname` transcription level.
#'
#' @details
#' If an intensity level could not be determined at the peak, the label of the
#' EVENT will be empty.
#'
#'
#' @param emuDBhandle A emuR database handle.
#' @inheritParams superassp::praat_pitch
#' @param interpolation The method used for finding intensity associated with the pitch period event (ont of "nearest", or "linear", "cubic", "sinc70" or "sinc700" interpolation).
#' @param RelativeWidth The relative with of the window used for extracting part of the signal for analysis.
#' @param levelname The name of the (EVENT) annotation level to insert into the database.
#' @param clear Should the `levename` level be cleared before inserting annotations?
#'
#' @return A [tibble::tibble] containing information on the labels inserted (session, bundle, start, level, attribute, labels)
#' @export
#'
#' @references \insertAllCited{}

annotate_periods <- function(emuDBhandle,
                             beginTime=0.0,
                             endTime=0.0,
                             windowShift=5,
                             minF=75.0,
                             maxF=600.0,
                             windowShape="Gaussian1",
                             interpolation="cubic",
                             RelativeWidth=1.0,
                             levelname="AUTO_PERIODS",
                             clear=TRUE
                             ){


  fl <- list_files(emuDBhandle,emuR:::load_DBconfig(emuDBhandle)$mediafileExtension) %>%
    dplyr::rename(listOfFiles = absolute_file_path)


  if(levelname %in%  emuR::list_levelDefinitions(emuDBhandle)$name ){
    if(clear){
      emuR::remove_levelDefinition(emuDBhandle,levelname,force=TRUE,verbose=FALSE)

    }else{
      stop("There is already a level named '",levelname,"' in the database. Please ")
    }
  }
  emuR::add_levelDefinition(emuDBhandle,levelname,"EVENT",rewriteAllAnnots = TRUE,verbose = FALSE)



  for(f in 1:nrow(fl)){
    soundFile <-  fl %>%
      dplyr::slice(f) %>%
      purrr::pluck("listOfFiles")

    soundFile <- normalizePath(soundFile)

    dsp_directory <- superassp:::make_dsp_environment()




    praat_script <- ifelse(PRAAT_DEVEL== TRUE,
                           file.path("inst","praat","praat_periods.praat"),
                           file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","praat_periods.praat")
    )



    periods <- superassp::cs_wrap_praat_script(praat_location = superassp::get_praat(),
                                               script_code_to_run = readLines(praat_script),
                                               directory=dsp_directory,
                                               return="last-argument")

    wavFileName <- tempfile(tmpdir = dsp_directory,fileext = ".wav")
    outTabFilename <- paste0(tools::file_path_sans_ext(wavFileName),".csv")

    R.utils::createLink(link=wavFileName, target=soundFile)


    # form Compute
    # sentence SoundFile /Users/frkkan96/Desktop/kaa_yw_pb.wav
    # real BeginTime 0.0
    # real EndTime 0.0
    # real Time_step 0.005
    # real Minimum_f0 75.0
    # real Maximum_f0 600.0
    # word WindowShape Gaussian1
    # word Interpolation cubic
    # real RelativeWidth 1.0
    # sentence TrackOut /Users/frkkan96/Desktop/kaa_yw_pb.per
    # endform

    outTab <- periods(wavFileName,
                      beginTime,
                      endTime,
                      windowShift /1000, #Praat needs seconds
                      minF,
                      maxF,
                      windowShape,
                      interpolation,
                      RelativeWidth,
                      outTabFilename

    )
    periodTab <- readr::read_csv(outTab,col_types = "dd",na=c("--undefined--","NA",""))

    fl[[f,"periodTab"]] <- list(periodTab)

    superassp:::clear_dsp_environment(dsp_directory)

  }
  fl <- fl %>%
    tidyr::unnest(periodTab) %>%
    dplyr::rename(start=Time,labels=Intensity) %>%
    dplyr::mutate(start= start * 1000,
                  level="AUTO_PERIODS",attribute="AUTO_PERIODS",labels=as.character(round(labels,digits = 2))) %>%
    dplyr::select(-listOfFiles,-file)

  emuR::create_itemsInLevel(emuDBhandle,itemsToCreate = fl,rewriteAllAnnots = TRUE,verbose = FALSE)

  return(fl)
}

#   text directory /Users/jkirby/Projects/egg/praatdet/examples/
#   word outfile egg_out.txt
#   word extension .wav
#   integer eggChan 1
#     integer startFile 1
#   integer intervalTier 3
#   word intervalLabel v
#   integer intervalNum 0
#   word separator _
#   integer minF0 75
#   integer maxF0 600
#   integer k 10
#     real    threshold 3/7
#     integer passFrequency 40
#     integer smoothHz 20
#     boolean manualCheck 0
#     boolean useExistingPP 0
#     boolean invertSignal 0


annotate_peakdet <- function(eggFile="/Users/frkkan96/Desktop/eggtest/dâlem_iso_1_mis.egg",
                             extension=".egg",
                             eggChan=1,
                             minF=75,
                             maxF=600,
                             k=10,
                             threshold=3/7,
                             passFrequency=40,
                             smoothHz=20,
                             invertSignal=FALSE
                             ){

  eggFile <- normalizePath(eggFile)

  dsp_directory <- superassp:::make_dsp_environment()

  list.files("/Users/frkkan96/Documents/src/reindeer/inst/praat/praatdet",pattern  = ".praat")


  praat_script <- ifelse(PRAAT_DEVEL== TRUE,
                         file.path("inst","praat","praatdet","shelldet.praat"),
                         file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","praatdet","shelldet.praat")
  )



  praatdet <- superassp::cs_wrap_praat_script(praat_location = superassp::get_praat(),
                                             script_code_to_run = readLines(praat_script),
                                             directory=dsp_directory,
                                             return="info-window")
  # We need  to copy some additional praat files
  auxpraatfiles <- list.files(ifelse(PRAAT_DEVEL== TRUE,
                                     file.path("inst","praat","praatdet"),
                                     file.path(system.file(package = "reindeer",mustWork = TRUE),"praat","praatdet")),pattern  = ".praat",full.names=TRUE)


  file.copy(auxpraatfiles,dsp_directory)

  eggFileName <- tempfile(tmpdir = dsp_directory,fileext = extension)
  outTabFilename <- paste0(tools::file_path_sans_ext(eggFileName),".csv")

  R.utils::createLink(link=eggFileName, target=eggFile)

  #   text directory /Users/jkirby/Projects/egg/praatdet/examples/
  #   word outfile egg_out.txt
  #   word extension .wav
  #   integer eggChan 1
  #     integer startFile 1
  #   integer intervalTier 3
  #   word intervalLabel v
  #   integer intervalNum 0
  #   word separator _
  #   integer minF0 75
  #   integer maxF0 600
  #   integer k 10
  #     real    threshold 3/7
  #     integer passFrequency 40
  #     integer smoothHz 20
  #     boolean manualCheck 0
  #     boolean useExistingPP 0
  #     boolean invertSignal 0

  res <- praatdet(paste0(dsp_directory,"/"),
                  paste0(dsp_directory,"/"),
                  "egg_out.txt",
                  extension,
                  eggChan,
                  1,
                  3,
                  "v",
                  0,
                  "___",
                  minF,
                  maxF,
                  k,
                  threshold,
                  passFrequency,
                  smoothHz,
                  0,
                  0,
                  ifelse(invertSignal,1,0)
  )

  res <- (res == "All done.")

  outTab <- file.path(dsp_directory,"egg_out.txt")
  eggsTab <- readr::read_csv(outTab,col_types = "ccciddddd",na=c("--undefined--","NA",""))



}

# ### For interactive testing
# PRAAT_DEVEL=TRUE
# unlink_eggDemoDir()
# create_egg_db() -> egg
# add_levelDefinition(egg,"EGG","SEGMENT")
#
#
# annotate_peakdet() -> out
#remove_levelDefinition(emuDBhandle,"AUTO_PERIODS",force=TRUE,verbose=FALSE)
#add_levelDefinition(emuDBhandle,"AUTO_PERIODS","EVENT")
#annotate_periods(emuDBhandle) -> out


#add_perspective(emuDBhandle,"Periods")
#set_levelCanvasesOrder(emuDBhandle,"Periods",c("AUTO_PERIODS"))
#set_signalCanvasesOrder(emuDBhandle,"Periods",c("OSCI"))



