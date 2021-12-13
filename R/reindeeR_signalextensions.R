#' Provides the user with speaker dependent signal processing parameters
#'
#' The source of the default signal processing parameters may be a spreadsheet
#' file in the OOXML Workbook ISO/IEC 29500:2008 standard format.The simplest
#' way to obtain such a file is to provide this function with a file name that
#' does not exist. This function will then write the [DSPP] set to the file, and
#' the user may then edit the file by hand using Microsoft Excel, Libreoffice
#' Calc or some other standard compliant software. Once edited, this function
#' may be used to read in the spreadsheet file to a tibble that may be used for
#' signal processing purposes.
#'
#' Alternatively, the user may use this function to just get the defaults stored
#' in [DSPP] returned directly by not supplying a file name.
#'
#' @importFrom "logger" WARN
#'
#' @param file If the file exists, the settings stored in the file are read in and returned. If it does not exist, one will be created and the [DSPP] parameters inserted into it for the user to edit. If \code{NULL}, the [DSPP] structure will be returned directly
#'
#' @return A [tibble:tibble] containing some mandatory columns
#'  \item{Gender}{Either "Male", "Female", or \code{NA}. \code{NA} parameters will be used in cases where the gender of a speaker is not known}
#'  \item{Parameter}{The name of the parameter written exactly as expected by the called function}
#'  \item{Default}{The value that should be given to functions that take the parameter indicated in the `Parameter` column. Most often, this will be a number, but in rare cases a single character is also possible. The whole column is therefore formated as a `character` list, although the value will likely be converted to a numeric before use.}
#'
#'
#' @export
#'
#' @examples
#' xlsf <- tempfile(fileext = ".xlsx")
#' #Use the function to write out the default parameters DSPP to the temp file
#' get_parameters(xlsf)
#'
#' #Now the user may modify the default settings to their liking using a spreadsheet program
#' # and then read the settings in again to a data frame using the same command again, and get a
#' # data frame that may be used by functions in this package.
#' #Just get the DSPP structure directly
#' data(DSPP)
#' newDSPP2 <- get_parameters()
#'
get_parameters <- function(file=NULL){

  if(is.null(file)){
    if(!exists("DSPP")) data(DSPP)

    return(DSPP)
  }
  if(!file.exists(file)){
    openxlsx::write.xlsx(DSPP,file = file)
  }

  openxlsx::read.xlsx(file,na.strings = "NA") %>%
    dplyr::distinct() -> f

  #Check manditory columns
  if(! c("Gender","Parameter","Default") %in% names(f)){
    warning("Malformed settings file. The Gender, Parameter, and Default columns are mandatory. Returning the default DSPP collection of settings instead.")
    return(DSPP)
  }else{
    return(f)

  }
}

#' Call any function to compute an EmuR SSFF track definition.
#'
#'
#' This function behaves like [emuR::add_ssffTrackDefinition], but
#' can also take a well defined function (see below) not defined in the
#' \code{wrassp} package and apply it to speech signal files in an Emu database.
#'
#' @details  This will be a new section
#'
#' What is a well defined function
#'
#'   The \code{add_trackDefinition} needs to know
#'
#'   1) what SSFF tracks there will be in the output of the function, and
#'   2) what file extension is prefered for the track.
#'
#'   The function will attempt to get these pieces of information from one of
#'   two sources (in order)
#'
#'   1. the [wrassp::wrasspOutputInfos] structure of lists, so that
#'   the functions defined in \code{wrassp} is handled transparently, or
#'   2. the contents of the attributes "ext" and "tracks" set on the function.
#'
#' If found, the [add_trackDefinition] function will apply the signal generating function to all media files, and record the new tracks as
#' SSFF track definitions in the database using [emuR::add_ssffTrackDefinition] in order to ensure compatibility
#'
#' @param emuDBhandle The database handle.
#' @param name The name of the SSFF track to list in the database.
#' @param columnName The name of the column in the SSFF file to associate with the track.
#' @param fileExtension The file extension of the created or already existing SSFF file.
#' @param onTheFlyFunctionName The name of the function to apply to the input signal to produce the output track. This function must have attributes "ext" and "tracks" defined to give information on what output may be expected from using them. Alternatively, the function may be defined in the package [wrassp] and therefore well known. The function also has state, via an attribute `outputType`, that it will create an SSFF track.
#' @param onTheFlyParams An optional list of arguments to the `onTheFlyFunctionName` function. Default arguments will be derived from Age and Gender metadata too, so this parameter should mainly be used for arguments that should be applied identically to all input files.
#' @param onTheFlyOptLogFilePath The logging output directory.
#' @param inputTrackExtension The file extension of the input file. If `NULL`, the '"mediafileExtension' set in the database template file (defaults to "wav") will be used.
#' @param metadata.defaults A list of default values for named columns. Since values will always be set for these columns, the user may also rely on them being set when deducing which default parameters to use when computing SSFF tracks using the `onTheFlyFunctionName` function.
#' @param overwriteFiles If set to `TRUE`, the function will calculate SSFF track files for ALL bundles and write them into the database, overwriting existing files. The default is `FALSE` which means that only only bundles which do not have an track file with the indicated output extension will be written.
#' @param verbose Determines wheter the function should display output to the user. If `FALSE`, the function will run completely silent and only report error messages back to the user.
#' @param package The name of the package in which tbe funciton `onTheFlyFunctionName` is defined.
#'
#' @importFrom "dplyr" "%>%"
#
#' @export
#'
#' @examples
#' require(superassp)
#' reindeer:::create_ae_db() -> ae
#' emuR::list_ssffTrackDefinitions(ae)
#' ?praat_formant_burg
#' #This calls the praat_formant_burg function with default parameters
#' add_trackDefinition(ae,name="FORMANTS",columnName = "fm",fileExtension = "pfm",onTheFlyFunctionName = "praat_formant_burg")
#' #Compute formants with non-default parameters using Praat.
#' add_trackDefinition(ae,name="pfm",columnName = "fm",fileExtension = "pfm",onTheFlyFunctionName = "praat_formant_burg",onTheFlyParams=list(numFormants=3,window="hamming",maxhzformant=4000))
#' emuR::list_ssffTrackDefinitions(ae)
#' #Use the standard wrassp::forest function to compute the formants, using the defaults for female speakers
#' add_trackDefinition(ae,name="ffm",columnName = "fm",fileExtension = "ffm",onTheFlyFunctionName = "forest",onTheFlyParams=list(numFormants=3,gender="f"))
#' #And, you can attach another track definition to the same Praat-genereated file if you like.
#' add_trackDefinition(ae,name="bw",columnName = "bw",fileExtension = "pfm")
#' emuR::list_ssffTrackDefinitions(ae)
#' reindeer:::detach_demo_db(ae)
#'
#'
#'
add_trackDefinition <- function(
  emuDBhandle,
  name,
  columnName = NULL,
  fileExtension = NULL,
  onTheFlyFunctionName = NULL,
  onTheFlyParams = list(),
  onTheFlyOptLogFilePath = NULL,
  inputTrackExtension=NULL,
  metadata.defaults=list(Gender=NA,Age=35),
  overwriteFiles=FALSE,
  verbose=TRUE,
  package="superassp"){

  existingDefExists = FALSE
  #Check if the track has not already been defined
  existingDefinitions <- list_ssffTrackDefinitions(emuDBhandle)
  if(! is.null(existingDefinitions) && name %in% existingDefinitions$name){
    # In this case, we have an existing definition and need to make sure that it agrees
    # with what the user specified
    currDef <- existingDefinitions[existingDefinitions$name == name , ]
    if(! is.null(columnName) && currDef[[1,"columnName"]][[1]] != columnName){
      stop("A track '",name,"' is already defined in the database but with a different column name specification ('",currDef$columnName,"') than the specified ('",columnName,"'). Please correct the column name, or use NULL.")
    }

    if(! is.null(fileExtension) && currDef[[1,"fileExtension"]] != fileExtension){
      stop("A track '",name,"' is already defined in the database but with a different file extension specification ('",currDef$fileExtension,"') than the specified ('",fileExtension,"'). Please correct the file extension, or use NULL.")
    }
    #Amend the specifications from stored definition
    fileExtension <- currDef$fileExtension
    columnName <- currDef$columnName
    existingDefExists <- TRUE
  }

  if(!is.null(onTheFlyFunctionName)){
    # From here we deduce how to apply the function -----------------------

    defTracks <- superassp::get_definedtracks(onTheFlyFunctionName)
    #Set the default file extension to the one set as an attribute, if missing in the arguments
    fileExtension <- ifelse(!is.null(fileExtension),
                            fileExtension,
                            superassp::get_extension(onTheFlyFunctionName))


    columnName <- ifelse(!is.null(columnName) & length(defTracks) > 0 ,
                         columnName,
                         defTracks[[1]])

    if(! columnName %in% defTracks ) {
      stop("The track ",columnName, " is not a defined output track name of the function ",onTheFlyFunctionName)
    }


    if(is.null(inputTrackExtension)){
      #We need to get the default media file extension from the database definition if it not defined
      dbConfig = emuR:::load_DBconfig(emuDBhandle)
      inputTrackExtension <- dbConfig$mediafileExtension
    }



    if(!is.null(onTheFlyOptLogFilePath) ) {
       if(!dir.exists(onTheFlyOptLogFilePath)){
         stop("The logging directory '",onTheFlyOptLogFilePath,"'does not exists. Please create it first")
       }else{
         logName <- file.path(onTheFlyOptLogFilePath,paste0(onTheFlyFunctionName,".log"))
         logger::log_appender(logger::appender_file(logName))
         logger::log_threshold(logger::INFO)
         logger::log_formatter(logger::formatter_json)
       }
    }else{
      logger::log_threshold(logger::WARN)
    }

    meta_settings <- match_parameters(emuDBhandle = emuDBhandle,
                                         onTheFlyFunctionName = onTheFlyFunctionName,
                                         metadata.defaults = metadata.defaults,
                                         package = package)

    fl <- list_files(emuDBhandle = emuDBhandle,fileExtension = inputTrackExtension) %>%
      dplyr::select(-file)

    fl_meta_settings <- fl %>%
      dplyr::left_join(meta_settings,by = c("session","bundle")) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(session,bundle)

    #We have already made per file grouping of the tibble, so we may use that to extract file information
    ng <- dplyr::n_groups(fl_meta_settings)
    ngi <- as.integer(as.vector(ng))

    assertthat::assert_that(nrow(fl) == ngi, msg=paste0("Not all sounds files were assigned metadata ", nrow(fl)," ==> ",ng))


    currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
    funcFormals = as.list(formals(currFunc))
    names(funcFormals) -> fp

    #Copy arguments given to this function over the list of general arguments given to the called function
    if("optLogFilePath" %in% fp ){
      onTheFlyParams$optLogFilePath = onTheFlyOptLogFilePath
    }
    if("explicitExt" %in% fp ){
      onTheFlyParams$explicitExt = fileExtension
    }

    fl_meta_settings %>%
      dplyr::filter(!is.na(Parameter),!is.na(Setting)) %>%
      dplyr::group_map( ~ setNames(.x$Setting,nm=.x$Parameter)) -> dspParList

    if(verbose){
      if(overwriteFiles){
        message(paste0("Applying the function '",onTheFlyFunctionName, "' to all input tracks (.",inputTrackExtension,").\n"))
      }else {
        message(paste0("Applying the function '",onTheFlyFunctionName, "' to all .",inputTrackExtension," files for which a signal track file (.",fileExtension,") does not exist.\n"))
      }

      pb <- utils::txtProgressBar(min=0, max=ngi, style = 3)
    }

    for(currFileGroup in 1:ngi){

      if(verbose){
        setTxtProgressBar(pb, currFileGroup)
      }

      currSession <- unique(dplyr::group_split(fl_meta_settings)[[currFileGroup]]$session)
      currBundle <- unique(dplyr::group_split(fl_meta_settings)[[currFileGroup]]$bundle)
      outFile <- file.path(emuDBhandle$basePath,
                           paste0(currSession,emuR:::session.suffix),
                           paste0(currBundle,emuR:::bundle.dir.suffix),
                           paste0(currBundle,".",fileExtension))

      if(overwriteFiles || ! file.exists(outFile)){
        purrr::flatten(utils::modifyList(dspParList[currFileGroup][1],onTheFlyParams)) -> argLst
        #Since Settings have to be strings (character) in the DSPP table due to the "gender" argument being one
        #we need to convert strings like "11" to proper 11 values.
        argLst <- lapply(argLst,
                         utils::type.convert,as.is=TRUE)

        argLst$listOfFiles <- unique(dplyr::group_split(fl_meta_settings)[[currFileGroup]]$absolute_file_path)

        # Fix values of 'integer' class, since the wrassp functions expect 'numeric'
        if(length(argLst) > 0 ){
          for(an in names(argLst)){
            argLst[an] = ifelse(class(argLst[[an]]) =="integer",as.numeric(argLst[[an]]),argLst[[an]])
          }
        }

        #logger::log_info(toLog)
        toLog <- utils::modifyList(list("function"=onTheFlyFunctionName),argLst)
        #logger::log_info(logger::deparse_to_one_line(toLog))
        logger::log_info(toLog)

        #Now actually make the function call
        do.call(currFunc, argLst)

      }

    }
    if(verbose){
      close(pb)
    }

  }
  #Only attempt to commit if the git2r package can be loaded.
  #This is very explicitly checked by making sure that the shared library file exists, and is readable
  # * This is a hack aimed at resolving https://github.com/humlab-speech/visible-speech-deployment/issues/81
  gitLibFile <- system.file(package="git2r","libs",paste0("git2r",.Platform$dynlib.ext))
  if(file.exists(gitLibFile) &&  file.access(gitLibFile,4)){
    #Commit created files if the database is a repository
    if(git2r::in_repository(emuDBhandle$basePath)){
      created_files <- emuR::list_files(emuDBhandle,ext)
      git2r::add(repo=emuDBhandle$basePath,path = created_files$absolute_file_path)
      git2r::commit(repo=emuDBhandle$basePath,message=paste0("Adding signal files with an '",ext,"' extension"))
    }
  }


  if(! existingDefExists){

    emuR::add_ssffTrackDefinition(emuDBhandle,name=name,columnName = columnName,fileExtension = fileExtension)
  }


}


#' Get the columns / fields defined in SSFF tracks with a given extension
#'
#' This function allows the user to specify an EmuR database handle and a
#' file extension and get which columns or tracks are defined in the SSFF files.
#'
#' @param emuDBhandle The EmuR database handle.
#' @param extension The file extension of the SSFF files to investigated.
#'
#' @return A vector containing SSFF field / column names within the SSFF file.
#' @export
#'
#' @examples
#' \dontrun{
#'  ae <- reindeer:::create_ae_db()
#'  reindeer::add_trackDefinition(ae,"A","A",onTheFlyFunctionName = "praat_sauce")
#'  print(reindeer::get_trackColumns(ae,"psa"))
#' }
get_trackColumns <- function(emuDBhandle, extension=NULL){
  if(! "emuDBhandle" %in% class(emuDBhandle)){
    stop("The 'emuDBhandle' argument is not of class \"emuDBhandle\"")
  }

  if(is.null(extension)){stop("Please provide an 'extension' argument")}

  files <- emuR::list_files(emuDBhandle,extension)$absolute_file_path
  if(length(files) == 0 ) stop("SSFF tracks with the extension ",extension," are not defined in the database.")
  tocheck <- head(files,1)

  if(file.exists(tocheck)){
    curr <- tryCatch({
      wrassp::read.AsspDataObj(tocheck)
    },error = function(e) {stop("Unable to read file ",tocheck, " as an SSFF file.")})

  }
  out <- wrassp::tracks.AsspDataObj(curr)
  return(out)
}

#' @aliases  get_trackColumns
#' @export
get_trackFields <- get_trackColumns


#' Allows the user to get an SSFF object directly from an Emu database
#'
#' The user gives a database handle, a file extension of files that are present
#' in the Emu database directory (i.e. have been generated using
#' [emuR::add_ssffTrackDefinition] or [reindeer::add_trackDefinition]) calls),
#' an index and get the 'n'th track file with the file extension in the database
#' as an SSFF object.
#'
#' @param emuDBhandle An Emu database handle.
#' @param extension A file extension.
#' @param n The index of the track file to be returned as an SSFF object.
#'
#' @return An SSFF track object
#' @export
#
get_ssffObject <- function(emuDBhandle, extension, n ){
  files <- emuR::list_files(emuDBhandle,extension)$absolute_file_path
  if(length(files) == 0 ) stop("SSFF tracks with the extension ",extension," are not defined in the database.")
  if(length(files) < n ) stop("The database contains only ",length(files), " track files with extension ",extension,"!" )
  if(n < 0  ) stop("Please provide a positive index (n > 0) ." )
  tocheck <- files[n]

  if(file.exists(tocheck)){
    curr <- tryCatch({
      wrassp::read.AsspDataObj(tocheck)
    },error = function(e) {stop("Unable to read file ",tocheck, " as an SSFF file.")})

  }
  return(curr)
}



#' Metadata-aware selection of DSP parameters
#'
#' This function deduces the appropriate parameters
#' for a speech signal processing function based on the
#' speaker Age and Gender metadata set for a bundle.
#'
#' @param emuDBhandle The database handle
#' @param onTheFlyFunctionName The name of the function for which appropriate parameters should be deduced.
#' @param metadata.defaults Default settings for metadata. This argument is primarily used for setting default Age and Gender for bundles with no such metadata already set.
#' @param package The signal processing package where the `onTheFlyFunctionName` function may be found. Defaults to [superassp].
#'
#' @return
#' A data frame with columns `session`, `bundle`, `Parameter`, and `Setting`, grouped by `session` and `bundle`.
#'
#' @examples
#' \dontrun{
#' reindeer:::unlink_emuRDemoDir()
#' reindeer:::create_ae_db() -> emuDBhandle
#' reindeer:::make_dummy_metafiles(emuDBhandle)
#' print(match_parameters(emuDBhandle,onTheFlyFunctionName = "forest") )
#' }


match_parameters <- function(emuDBhandle,onTheFlyFunctionName,metadata.defaults=list(Gender=NA,Age=35),package="superassp"){

  meta <- get_metadata(emuDBhandle,manditory=names(metadata.defaults))
  dsp <- get_parameters()


  currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
  funcFormals = as.list(formals(currFunc))
  names(funcFormals) -> fp
  #IF metadata.defaults are mandatory, they should be in the output
  assertthat::assert_that(all(names(metadata.defaults) %in% names(meta)))

  meta %>%
    tidyr::replace_na(replace=metadata.defaults)  -> out
  #return(out)
  out %>%
    dplyr::left_join(dsp %>%
                       dplyr::filter(Parameter %in% fp),na_matches = "na",by=c("Gender"))  %>%
    dplyr::filter(!is.na(bundle),!is.na(session)) %>%
    dplyr::mutate(Age_lower=ifelse(is.na(Age_lower),0,Age_lower),
                  Age_upper=ifelse(is.na(Age_upper),200,Age_upper)) %>%
    dplyr::filter( Age <= Age_upper , Age >= Age_lower  ) %>%
    dplyr::mutate(AgeRange=Age_upper-Age_lower) %>%
    dplyr::arrange(session,bundle,Parameter,AgeRange) %>%
    dplyr::group_by(session,bundle,Parameter) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(session, bundle)  -> meta_settings

  #Overwrite default by manually specified parameters when present
  # We do this after matching with DSPP defaults, as they would otherwise have to be matched by file early but applied last
  if(any(fp %in% names(meta_settings))) {
    manualParameters <- base::intersect(fp,names(meta_settings))
    for(currParam in manualParameters){
      #We can now set up two logical vectors, which
      # 1) identify which rows identify a parameter for which there is a column
      paramRow <- meta_settings$Parameter == currParam
      # 2) and which rows where a default value has been set
      manualValueSet <- !is.na(meta_settings[,currParam])
      # Get the row which correspond both to a parameter setting set by DSPP and by a value set in a corresponding column
      rowToSwap <- paramRow & manualValueSet
      meta_settings[rowToSwap,"Setting"] <- as.character(meta_settings[rowToSwap,currParam]) # The conversion is required due to some settings being character
    }
  }


  #Do some cleanup
  meta_settings <- meta_settings %>%
    dplyr::select(session,bundle,Parameter,Setting) %>%
    tidyr::pivot_wider(names_from = "Parameter",values_from = "Setting") %>% #To make replace_na work
    tidyr::replace_na(replace=as.list(funcFormals)) %>%
    tidyr::pivot_longer(! c(session,bundle),names_to = "Parameter",values_to="Setting" ) %>%
    dplyr::group_by(session,bundle)


  return(meta_settings)

}






#' A utility function that builds a list of parameters to use in function call based on metadata
#'
#' This function takes a function and explicit argument list and builds a list of parameters for a function call considering the metadata of a bundle (in a session).
#'
#'
#' @inheritParams match_parameters
#' @param session Only consider this session.
#' @param bundle  The bundle in the session to build an argument list for.
#'
#' @return A list which may be supplied to a `do.call` call.
#'
#' @seealso match_parameters
#' @seealso do.call
#'

get_metaFuncFormals <- function(emuDBhandle,session,bundle,onTheFlyFunctionName,onTheFlyParams=list(),metadata.defaults=list(Gender=NA,Age=35),package="superassp"){

  currBundl <- bundle
  currSess <- session
  currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
  funcFormals = as.list(formals(currFunc))
  names(funcFormals) -> fp

  dspParList <- match_parameters(emuDBhandle,onTheFlyFunctionName,metadata.defaults,package) %>%
    dplyr::filter(!is.na(Parameter),!is.na(Setting) ) %>%
    dplyr::filter(bundle == currBundl && session == currSess) %>%
    dplyr::group_map( ~ setNames(.x$Setting,nm=.x$Parameter))

  #return(dspParList)
    purrr::flatten(utils::modifyList(purrr::flatten(dspParList),onTheFlyParams)) -> argLst
  #Since Settings have to be strings (character) in the DSPP table due to the "gender" argument being one
  #we need to convert strings like "11" to proper 11 values.
  argLst <- lapply(argLst,
                   utils::type.convert,as.is=TRUE)

  # Fix values of 'integer' class, since the wrassp functions expect 'numeric'
  if(length(argLst) > 0 ){
    for(an in names(argLst)){
      argLst[an] = ifelse(class(argLst[[an]]) =="integer",as.numeric(argLst[[an]]),argLst[[an]])
    }
  }

  return(argLst)
}



getSamples <- function(sample_start, sample_end,cut,npoints=1, endOfTrack=NULL){


  samples <- sample_end - sample_start +1
  lpoint <- ifelse(is.null(npoints),0,base::ceiling(npoints/2) -1 )
  rpoint <- ifelse(is.null(npoints),0,base::floor(npoints/2))

  mid_sample <- round(samples * cut,digits = 0) + sample_start -1
  start <- mid_sample - lpoint
  end <-  mid_sample + rpoint

  #The special case where we are running a risk of going out of bounds
  if(start <1 ){
    start <- 1
    end <- npoints
  }
  if(!is.null(endOfTrack) && end > endOfTrack ){
    end <- endOfTrack
    start <- endOfTrack - npoints + 1
  }

  return(c(start,end))
}



readTrackData <- function(filename, sample_start=NULL, sample_end=NULL,cut=NULL,npoints=NULL,ssffTrackName="audio",...){

  if(! file.exists(filename)){
    stop("The file ",filename," does not exists!")
  }

  if( !is.null(npoints) && base::trunc(npoints) != npoints){
    stop("The 'npoints' argument need to be an interger value (or NULL). Truncating.")
    npoints <- base::trunc(npoints)
  }

  if(!is.null(cut) && ( cut >1 || cut < 0) ){
    stop("The parameter 'cut' has to be between 0.0 and 1.0.")
  }

  if(! is.null(cut) && is.null(npoints)){
    npoints <- 1
  }

  selectRows <- TRUE

  if(is.null(sample_start) || is.null(sample_end)){
    # in this case, get info from the input file
    outSSFF <- wrassp::read.AsspDataObj(fname=filename,begin=0,end=0,sample=TRUE)

    if(!is.null(cut)){
      sample_start <- attr(outSSFF, "startRecord")
      sample_end <- attr(outSSFF, "endRecord")
      samples <- getSamples(sample_start=sample_start,sample_end=sample_end,cut=cut,npoints=npoints,endOfTrack=sample_end)

      selectRows <- seq(samples[1],samples[2],1)
    }
  }else {

    if(sample_start == sample_end){
      if(! is.null(cut)){
        warning("The argument 'cut' has no reasonable interpretation when extracting data for an EVENT. Ignoring the argument.")
      }
      #Ignore cut values in the case of EVENT segments (for which 'cut' has no reasonable interpretation
      cut <- 0

    }
    points <- ifelse(is.null(npoints),1, floor(npoints /2))

    samples <- getSamples(sample_start,sample_end,cut,npoints)
    start <- samples[1]
    end <- samples[2]

    outSSFF <- wrassp::read.AsspDataObj(fname=filename,begin=start,end=end,sample=TRUE)

  }

  if(! ssffTrackName %in% names(outSSFF) ){
    stop("The track ",ssffTrackName," does not exists in the track file ", filename)

  }

  track <- as.data.frame(outSSFF[[ ssffTrackName]])

  sr <- attr(outSSFF,"sampleRate")
  nSamples <- nrow(track)
  startRecord <- attr(outSSFF,"startRecord")
  endRecord <- attr(outSSFF,"endRecord")
  times <- startRecord:endRecord / sr

  nTracks <- ifelse(is.null(ncol(track)),1,ncol(track))
  names(track) <- paste0("T",1:nTracks)
  track <- dplyr::as_tibble(
    cbind(data.frame(time=times), track)[selectRows, ]
    )

  return(track)

}

computeTrackData <- function(fun,filename, start, end, arguments,ssffTrackName=NULL){

 if(!is.function(fun)){
   stop("The argument 'fun' needs to be a function.")
 }
 type <- superassp::get_outputType(fun)

 if("toFile" %in% names(formals(fun)) ){
   arguments <- utils::modifyList(arguments,list(toFile=FALSE))
 }

 arguments <- utils::modifyList(arguments,list(beginTime=start,endTime=end))

 if(toupper(type) == "SSFF"){

   fNameCandidates <- c("filename","listOfFiles")

   formalFileArg <- fNameCandidates[fNameCandidates %in% names(formals(fun))][[1]]
   arguments[formalFileArg] <- filename

   if(is.null(ssffTrackName)){
     ssffTrackName <- superassp::get_definedtracks(fun)[[1]]
     logger::log_warn("A track name was not specified. Selecting the first track ('",ssffTrackName,"').")
   }

   outSSFF <- do.call(fun,arguments)

   track <- as.data.frame(outSSFF[ssffTrackName])
   sr <- attr(outSSFF,"sampleRate")
   nSamples <- nrow(track)
   startRecord <- attr(outSSFF,"startRecord")
   endRecord <- attr(outSSFF,"endRecord")
   times <- startRecord:endRecord / sr

   nTracks <- ifelse(is.null(ncol(track)),1,ncol(track))
   names(track) <- paste0("T",1:nTracks)
   track <- dplyr::as_tibble(cbind(data.frame(time=times), track))

 }else{
   if(tolower(type) == "list"){
     # We want to make sure that the same calling convention may be used for
     #SSFF tracks and list output (slice) functions
     # The wrassp functions use listOfFiles as the file name input
     # Slice functions use "filename", as it makes no sense to fool the user into thinking that multiple
     # file names can be submitted.


    arguments$filename <- filename
    arguments$beginTime <- start
    arguments$endTime <- end

     outLIST <- do.call(fun,arguments)
     track <- dplyr::as_tibble(outLIST)

   }else{
     stop("The supplied function is not defined correctly. Please use only functions with an 'outputType' attribute set.")
   }
 }

 return(track)

}


#' Produce a table of sample rates of all media and SSFF track files
#'
#' This function will access all defined SSFF track track files mentioned in an
#' SSFF track definition as well as the speech signal files, and extract and
#' list all sample rates. This function works from the actual track files and
#' not cached information, and may be slow on large databases with many SSFF
#' tracks defined, if stored on a media with slower file access speeds.
#'
#' @param emuDBhandle An Emu database handle.
#'
#' @seealso reindeer::load_emuDB, emuR::load_emuDB
#' @return a tibble with columns "session" and "bundle", and then one column for each SSFF track defined.
#'
#' @export
#'
#' @examples
#' reindeer:::create_ae_db(verbose = TRUE) -> emuDBhandle
#' samplerates(emuDBhandle)
#' reindeer:::unlink_emuRDemoDir()
#'
sampleRates <- function(emuDBhandle){
 dbConf <- emuR:::load_DBconfig(emuDBhandle)

 if(is.null(dbConf$mediafileExtension) ){
   warning("No media file extension set in the database. Using the 'wav' extension insted")
   dbConf$mediafileExtension <- "wav"
 }

 fl  <- list_files(emuDBhandle,dbConf$mediafileExtension)
 ssffDefs <- rbind(data.frame(name=dbConf$mediafileExtension ,columnName="audio",fileExtension=dbConf$mediafileExtension ),
                   list_ssffTrackDefinitions(emuDBhandle))

 for(f in 1:nrow(fl)){
   for(s in 1:nrow(ssffDefs)){
     ext <- ssffDefs[s,"fileExtension"]
     name <- ssffDefs[s,"fileExtension"]
     fileName <- paste0(tools::file_path_sans_ext(fl[f,"absolute_file_path"]),".",ext)

     inSSFF <- wrassp::read.AsspDataObj(fname = fileName,begin = 0,end=1,samples = TRUE)
     sr <- attr(inSSFF,"sampleRate")
     fl[f,name] <- sr
   }

 }
 return(fl[,-c(4,3)])
}

get_trackdata2 <- function (emuDBhandle, seglist = NULL, ssffTrackName = NULL,
                            cut = NULL, npoints = NULL, onTheFlyFunctionName = NULL,
                            onTheFlyParams = list(), onTheFlyOptLogFilePath = NULL, use.metadata=TRUE, package="superassp",verbose = TRUE)
{

  if(is.null(emuDBhandle)){
    stop("You have to specify an Emu database handle.")
  }

  if(is.null(seglist)){
    stop("You have to specify an segment list.")
  }

  if (!is.null(cut)) {
    if (cut < 0 || cut > 1) {
      stop("Bad value given for cut argument. Cut can only be a value between 0 and 1!")
    }
    if (sum(seglist$end) == 0) {
      stop("Cut value should not be set if sum(seglist$end) == 0!")
    }
  }
}


### For interactive testing
#
#
# library(superassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db(verbose = TRUE) -> emuDBhandle
# reindeer:::make_dummy_metafiles(emuDBhandle)
# query(emuDBhandle,"Phonetic = s") -> sl
# sl <- sl[1:3,]
#
# readTrackData(filename="~/Desktop/aaa.wav") -> out
# computeTrackData(forest,filename="~/Desktop/aaa.wav",start=0.0,end=1.0,arguments=list()) -> out1
# computeTrackData(praat_voice_report,filename="~/Desktop/aaa.wav",start=0.0,end=1.0,arguments=list()) -> out2
# emuR::get_trackdata(emuDBhandle,seglist = sl,ssffTrackName = "fm") -> sld13

# out <- get_metaFuncFormals(emuDBhandle,session="0000",bundle="msajc010",onTheFlyFunctionName = "forest")
# print(get_metadata(emuDBhandle))
# print(match_parameters(emuDBhandle,onTheFlyFunctionName = "forest")-> out)
#
# # # git2r::init(emuDBhandle$basePath)
# add_trackDefinition(emuDBhandle,"f02","pitch",onTheFlyFunctionName = "mhsF0")
# add_trackDefinition(emuDBhandle, name = "FORMANTS", onTheFlyFunctionName = "forest")
#add_trackDefinition(emuDBhandle, name = "F0", onTheFlyFunctionName = "ksvF0",onTheFlyOptLogFilePath="~/Desktop/out/")



