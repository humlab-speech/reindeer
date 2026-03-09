

#' Set an SSFF track as an overlay on the spectrogram or oscillograms
#'
#' The Emu transcription web application allow SSFF tracks to be overlaid on the oscillogram or the spectrogram. Only one track may be set as an overlay
#'
#' @param emuDBhandle The database handle.
#' @param perspective The name of the perspective in which the overlay should be shown. The perspective should already be defined in the database.
#' @param trackname The name of the SSFF track to be set as an overlay on either the oscillogram [set_osciOverlay] or spectrogram [set_specOverlay]
#'
#' @export
#' @rdname set_overlays
#'
#' @examplesIf interactive()
#' reindeer:::create_ae_db() -> ae
#' #Set up some suitable tracks for overlaying on an oscillogram or spectrogram
#' remove_ssffTrackDefinition(ae,name="fm",deleteFiles = FALSE)
#' add_ssffTrackDefinition(ae,name="FORMANTS","fm","fms")
#' add_ssffTrackDefinition(ae,"rms","rms",onTheFlyFunctionName = "rmsana")
#'
#' #Set the formants as overlays on the Spectrogram
#' set_specOverlay(ae,"default","FORMANTS")
#' #Set the RMS amplitude track as overlays on the waveform (oscillogram)
#' set_osciOverlay(ae,"default","rms")
#' serve(ae)
#'
set_specOverlay <- function(emuDBhandle,perspective,trackname){
  perspectiveNames <- .list_perspectives(emuDBhandle)$name
  trackNames <- .list_ssffTrackDefinitions(emuDBhandle)$name

  #Stop processing if the perspective is not defined in the database
  if(! perspective %in% perspectiveNames) {
    cli::cli_abort("Perspective {.val {perspective}} is not defined in database {.val {emuDBhandle$dbName}}")
  }

  #Stop processing if a track is not defined in the database
  if(! trackname %in% trackNames) {
    cli::cli_abort("Track {.val {trackname}} must be defined in the database before setting as overlay")
  }


  which(grepl(perspective,perspectiveNames)) -> perspid
  dbConfig = load_DBconfig(emuDBhandle)

  toAssign <- list("signalCanvasName"="SPEC","ssffTrackName"=trackname)
  whereToAssign <- 1
  if(length(dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign) > 0){
    if(toupper(dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign[[1]]$signalCanvasName) =="OSCI"){
      #We allow only overlays on OSCI and SPEC overlays. If the first one is OSCI, then set the overlay to the second position.
      whereToAssign <- 2
    }
  }
  dbConfig$EMUwebAppConfig$perspectives[[1]]$signalCanvases$assign[[whereToAssign]] <- toAssign
  store_DBconfig(emuDBhandle, dbConfig)

}

#'
#' @rdname set_overlays
#' @export

set_osciOverlay <- function(emuDBhandle,perspective,trackname){
  perspectiveNames <- .list_perspectives(emuDBhandle)$name
  trackNames <- .list_ssffTrackDefinitions(emuDBhandle)$name

  #Stop processing if the perspective is not defined in the database
  if(! perspective %in% perspectiveNames) {
    cli::cli_abort("Perspective {.val {perspective}} is not defined in database {.val {emuDBhandle$dbName}}")
  }

  #Stop processing if a track is not defined in the database
  if(! trackname %in% trackNames) {
    cli::cli_abort("Track {.val {trackname}} must be defined in the database before setting as overlay")
  }

  which(grepl(perspective,perspectiveNames)) -> perspid
  dbConfig = load_DBconfig(emuDBhandle)

  toAssign <- list("signalCanvasName"="OSCI","ssffTrackName"=trackname)
  whereToAssign <- 1
  if(length(dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign) > 0){
    if(toupper(dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign[[1]]$signalCanvasName) =="SPEC"){
      #We allow only overlays on OSCI and SPEC overlays. If the first one is SPEC, then set the overlay to the second position.
      whereToAssign <- 2
    }
  }
  dbConfig$EMUwebAppConfig$perspectives[[1]]$signalCanvases$assign[[whereToAssign]] <- toAssign
  store_DBconfig(emuDBhandle, dbConfig)
}


## For interactive testing
# library(emuR)
# reindeer:::create_ae_db() -> ae
# remove_ssffTrackDefinition(ae,name="fm",deleteFiles = FALSE)
# add_ssffTrackDefinition(ae,name="FORMANTS","fm","fms")
# add_ssffTrackDefinition(ae,"rms","rms",onTheFlyFunctionName = "rmsana")
#
# set_specOverlay(ae,"default","dft")
# rstudioapi::navigateToFile(file.path(ae$basePath,"ae_DBconfig.json"),336)
# set_specOverlay(ae,"default","FORMANTS")
# rstudioapi::navigateToFile(file.path(ae$basePath,"ae_DBconfig.json"),336)
#
# set_osciOverlay(ae,"default","rms")
# rstudioapi::navigateToFile(file.path(ae$basePath,"ae_DBconfig.json"),336)
