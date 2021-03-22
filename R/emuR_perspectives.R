
enable_formantOverlay <- function(emuDBhandle,perspective){
  perspectiveNames <- list_perspectives(emuDBhandle)$name
  trackNames <- list_ssffTrackDefinitions(emuDBhandle)$name

  #Stop processing if the perspective is not defined in the database
  if(! perspective %in% perspectiveNames) {stop("The perspective  ",perspective," is not defined in the database ", emuDBhandle$dbName,"!")}

  #Stop processing if a track FORMANTS is not defined in the database
  if(! "FORMANTS" %in% trackNames) {stop("In order to enable formant overlays, a track named 'FORMANTS' must be defined in the database !")}

  which(grepl(perspective,perspectiveNames)) -> perspid
  dbConfig = emuR:::load_DBconfig(emuDBhandle)

  dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign[[1]] <- list("signalCanvasName"="SPEC","ssffTrackName"="FORMANTS")
  res <- emuR:::store_DBconfig(emuDBhandle,dbConfig = dbConfig)
  return(res)
}

set_specOverlay <- function(emuDBhandle,perspective,trackname){
  perspectiveNames <- list_perspectives(emuDBhandle)$name
  trackNames <- list_ssffTrackDefinitions(emuDBhandle)$name

  #Stop processing if the perspective is not defined in the database
  if(! perspective %in% perspectiveNames) {stop("The perspective  ",perspective," is not defined in the database ", emuDBhandle$dbName,"!")}

  #Stop processing if a track FORMANTS is not defined in the database
  if(! trackname %in% trackNames) {stop("In order to enable an overlay on the spectrogram, the track must already be defined in the database.")}

  which(grepl(perspective,perspectiveNames)) -> perspid
  dbConfig = emuR:::load_DBconfig(emuDBhandle)

  dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign[[1]] <- list("signalCanvasName"="SPEC","ssffTrackName"=trackname)
  res <- emuR:::store_DBconfig(emuDBhandle,dbConfig = dbConfig)
  return(res)
}

set_osciOverlay <- function(emuDBhandle,perspective,trackname){
  perspectiveNames <- list_perspectives(emuDBhandle)$name
  trackNames <- list_ssffTrackDefinitions(emuDBhandle)$name

  #Stop processing if the perspective is not defined in the database
  if(! perspective %in% perspectiveNames) {stop("The perspective  ",perspective," is not defined in the database ", emuDBhandle$dbName,"!")}

  #Stop processing if a track FORMANTS is not defined in the database
  if(! trackname %in% trackNames) {stop("In order to enable an overlay on the oscillogram the track must be defined in the database !")}

  which(grepl(perspective,perspectiveNames)) -> perspid
  dbConfig = emuR:::load_DBconfig(ae)

  dbConfig$EMUwebAppConfig$perspectives[[perspid]]$signalCanvases$assign[[1]] <- list("signalCanvasName"="OSCI","ssffTrackName"=trackname)
  res <- emuR:::store_DBconfig(emuDBhandle,dbConfig = dbConfig)
  return(res)
}

