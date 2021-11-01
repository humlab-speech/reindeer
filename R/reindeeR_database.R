
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

import_recording <- function (emuDBhandle, dir, targetSessionName = "0000", speech.channel=1,egg.channel=NULL, verbose = TRUE)
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


### For interactive testing
#
#
# library(wrassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db() -> emuDBhandle
# import_mediaFiles(emuDBhandle,dir="~/Desktop/egg",egg.channel = 2)
