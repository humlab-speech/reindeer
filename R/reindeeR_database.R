
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
save_snapshot <- function(emuDBhandle,push.changes=TRUE,remote.name="origin",remote.ref="master"){

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

#' Batch rename bundles
#'
#' This function simplifies renaming of bundles.
#'
#' The user it expected to supply a data frame and tibble with "session" and
#' "name" columns, and a vector of new bundle names with the length as the
#' number of rows in the `from` data.frame. The function will then rename all
#' bundle directories and all files in the bundle directories to the new name
#' one by one.
#'
#' @details Bundle renaming is a sensitive process and many things can go wrong.
#' Therefore the function will by default simulate the file renaming and return
#' a summary of the plan for the user to review. The user may then enter the
#' same arguments again and set `simylate=TRUE` to actually perform the renaming.
#'
#' If the database is part of a git repository, the changes will be committed and pushed
#'
#' @param emuDBhandle An emuR database handle
#' @param from A tibble or data.frame with columns "session" and "name",
#'   identifying the bundle to rename. The usual procedure is to first invoke
#'   [reindeer::list_bundles] and then filter out the bundles the user wants to
#'   rename.
#' @param to A vector of strings containing new names of the bundles in `from`.
#'   The vector has to be of the same length as the number of rows in `from`.
#' @param simulate Boolean; Should the bundle renaming just be simulated, or
#'   actually performed?
#'
#' @return Either the updated bundle listing, or a tibble showing the file
#'   renaming plan.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#' reindeer:::create_ae_db() -> emuDBhandle
#' r <- list_bundles(emuDBhandle) %>%
#'   dplyr::mutate(to=toupper(name)) %>%
#'   dplyr::mutate(to=stringr::str_replace(to,"([A-Z]+)([0-9]+)","\\1-\\2")) %>%
#'   dplyr::mutate(to=tolower(to))
#'
#'  print(rename_bundles(emuDBhandle,r[1:2,c("session","name")],r[1:2,"to"],simulate=TRUE))
#' }
#'
rename_bundles <- function(emuDBhandle,from,to,simulate=TRUE){

  if(nrow(from) != nrow(as.data.frame(to))) {
    cli::cli_abort(c("Wrong number of bundles",
                     "i"="The number of bundles in the from argument {nrow(from)}",
                     "i"="The number of bundles in the to argument {nrow(to)}",
                     "x"="For every bundle in the from argument there must be an output bundle name in the to argument"))
  }

  if(! setequal( names(from), c("session","name"))){
    cli::cli_abort(c("The structure of the from tibble is wrong",
                     "i"="The 'from' has column names {names(from)}",
                     "x"="The 'from' argument needs to have the same columns as the output of `list_bundles`, which is 'session' and 'name'."))
  }

  fl <- emuR::list_files(emuDBhandle)   %>%
    dplyr::filter(session %in% from$session, bundle %in% from$name ) %>%
    dplyr::rename(inputPath=absolute_file_path) %>%
    dplyr::mutate(outputPath=stringr::str_replace_all(
      inputPath,
      purrr::set_names(to[[1]],nm=from$name)
    )) %>%
    dplyr::mutate(inputDirPath=dirname(inputPath),
                  outputDirPath=dirname(outputPath),
                  newInputPath=file.path(outputDirPath,basename(inputPath)))

  #return(fl)
  inRepo <- git2r::in_repository(emuDBhandle$basePath)



  if(!simulate){

    dirs <- fl %>%
      dplyr::select(inputDirPath,outputDirPath) %>%
      dplyr::distinct()

    file.rename(dirs[["inputDirPath"]],dirs[["outputDirPath"]])
    file.rename(fl[["newInputPath"]],fl[["outputPath"]])

    if(inRepo){
      git2r::add(repo=emuDBhandle$basePath, path=fl[["outputPath"]])
      git2r::add(repo=emuDBhandle$basePath, path=fl[["outputDirPath"]])
      git2r::commit(repo=emuDBhandle$basePath,message=paste("Renamed bundles",paste0(from$name,collapse=", ")))
    }

    return(emuR::list_bundles(emuDBhandle))
  }else{
    return(fl %>%
             dplyr::rename(session=session, original_path=inputPath,new_path=outputPath) %>%
             dplyr::transmute(session=session,
                           original_path=stringr::str_replace(original_path,paste0(emuDBhandle$basePath,"/"),""),
                           new_path=stringr::str_replace(new_path,paste0(emuDBhandle$basePath,"/"),"")
                                                            )
           )
  }


}

#reindeer:::unlink_emuRDemoDir()
#reindeer:::create_ae_db() -> emuDBhandle
# r <- list_bundles(emuDBhandle) %>%
#   mutate(to=toupper(name)) %>%
#   mutate(to=stringr::str_replace(to,"([A-Z]+)([0-9]+)","\\1-\\2")) %>%
#   mutate(to=tolower(to))
#
# print(rename_bundles(emuDBhandle,r[1:2,c("session","name")],r[1:2,"to"],simulate=TRUE) -> out)

### For interactive testing
#
#
# library(wrassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db() -> emuDBhandle
# import_mediaFiles(emuDBhandle,dir="~/Desktop/egg",egg.channel = 2)
