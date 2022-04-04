#' Provides the user with speaker dependent signal processing parameters
#'
#' This function returns age and gender specific speech signal processing
#' parameters. Normally, the function just returns the stored /pre-computed
#' `DSPP` tibble, which contains the output of this when applied previously.
#'
#'
#' Recomputation may be required if more data has been added to the file
#' `file.path(system.file(package = "reindeer",mustWork =
#' TRUE),"default_parameters.xlsx")`. If re-computation has been requested, this
#' function then expands the age ranges in the spreadsheet file and computes a
#' Gender stratified locally-weighted polynomial regression with age as the
#' independent variable and the parameter value as the dependent variable.
#' Separate models are computed for Male and Female speakers. Additionally, a
#' separate model of all parameters are computed across all data for the case
#' where the gender of a speaker is unknown (`Gender==NA`). Parameters that have
#' previously been estimated using the function are are stored in the `DSPP`
#' data frame.
#'
#' @importFrom "logger" WARN
#'
#' @param recompute If `TRUE`, this function will estimate age appropriate
#'   settings from the collection of empirical defaults stored in this package.
#'   If `FALSE`, this function will just return the stored `DSPP` data.
#'
#' @return A [tibble::tibble] with one row per age and gender combination, and
#'   with one column per parameter for which an age dependent setting was
#'   identified in the set of parameters.
#'
#'
#'
#'
get_metadataParameters <- function(recompute=FALSE){

  if(!recompute){
    return(DSPP)
  }

  ff <- function(Age_lower,Age_upper,... ) {
    out <- data.frame(Age=seq(Age_lower,Age_upper,1))
    return(out)
  }

  openxlsx::read.xlsx(file.path(system.file(package = "reindeer",mustWork = TRUE),"default_parameters.xlsx"),sep.names = " ") -> defaults

  defaults %>%
    dplyr::filter( Gender %in% c("Male","Female")) %>%
    dplyr::select(Gender:Age_upper,Parameter,Setting,`Study participants`,`Study identifier`) %>%
    dplyr::rowwise() %>%
    # divide df into a list of data.frames based on supplied grouping variables
    dplyr::group_split() %>%
    #Gender,Parameter,Setting,`Study participants`,`Study identifier`
    # for each element in the list, apply this function
    purrr::map_dfr(function(df.x) {
      with(df.x,
           # get the data.frame your function returns
           ff(Age_lower,Age_upper) %>%
             # add your grouping variables back-in (stripped by ff)
             dplyr::mutate(Gender = Gender[1],Parameter=Parameter[1],Setting=Setting[1],`Study participants`=`Study participants`[1],`Study identifier`=`Study identifier`[1])
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age=ceiling(Age)) %>%
    dplyr::group_split(Gender,Parameter) %>%
    purrr::map_dfr(function(df.x) {
      with(df.x,
           # get the data.frame your function returns
           data.frame(
             Setting=predict(loess(Setting ~ Age, weights=`Study participants` , data=.,span=0.5),
                             data.frame(Age = seq(min(Age), max(Age), 1)),surface = "direct",statistics = "approximate"),
             Age = seq(min(Age), max(Age), 1)) %>%
             # add your grouping variables back-in (stripped by ff)
             dplyr::mutate(Gender=Gender[1],Parameter=Parameter[1]))
    }) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from="Parameter",values_from = "Setting",id_cols = c("Gender","Age"))%>%
    dplyr::mutate(windowSize = ifelse(is.na(windowSize),ceiling(2*1*1000/minF),windowSize )) %>%
    dplyr::mutate(nominalF2 = ifelse(is.na(nominalF3),ceiling(nominalF1*3),nominalF2 )) %>%
    dplyr::mutate(nominalF3 = ifelse(is.na(nominalF3),ceiling(nominalF1*5),nominalF3 ))  %>%
    mutate(across(where(is.numeric), ~round(.,digits = 0 ))) %>%
    dplyr::arrange(Gender,Age) -> DSPP_mf


  defaults %>%
    #dplyr::select(Age_lower:Age_upper,Parameter,Setting,`Study participants`,`Study identifier`) %>%
    dplyr::rowwise() %>%
    # divide df into a list of data.frames based on supplied grouping variables
    dplyr::group_split() %>%
    #Gender,Parameter,Setting,`Study participants`,`Study identifier`
    # for each element in the list, apply this function
    purrr::map_dfr(function(df.x) {
      with(df.x,
           # get the data.frame your function returns
           ff(Age_lower,Age_upper) %>%
             # add your grouping variables back-in (stripped by ff)
             dplyr::mutate(Parameter=Parameter[1],Setting=Setting[1],`Study participants`=`Study participants`[1],`Study identifier`=`Study identifier`[1])
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age=ceiling(Age)) %>%
    dplyr::group_split(Parameter) %>%
    purrr::map_dfr(function(df.x) {
      with(df.x,
           # get the data.frame your function returns
           data.frame(
             Setting=predict(loess(Setting ~ Age, weights=`Study participants` , data=.,span=0.5),
                             data.frame(Age = seq(min(Age), max(Age), 1)),surface = "direct",statistics = "approximate"),
             Age = seq(min(Age), max(Age), 1)) %>%
             # add your grouping variables back-in (stripped by ff)
             dplyr::mutate(Parameter=Parameter[1]))
    }) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from="Parameter",values_from = "Setting",id_cols = c("Age"))%>%
    dplyr::mutate(windowSize = ifelse(is.na(windowSize),ceiling(2*1*1000/minF),windowSize )) %>%
    dplyr::mutate(nominalF2 = ifelse(is.na(nominalF3),ceiling(nominalF1*3),nominalF2 )) %>%
    dplyr::mutate(nominalF3 = ifelse(is.na(nominalF3),ceiling(nominalF1*5),nominalF3 ))  %>%
    mutate(across(where(is.numeric), ~round(.,digits = 0 ))) %>%
    dplyr::arrange(Age) %>%
    dplyr::mutate(Gender=NA) ->  DSPP_na

    DSPP_mf %>%
      dplyr::bind_rows(DSPP_na)%>%
      dplyr::mutate(dplyr::across(where(is.numeric), as.integer)) -> DSPP

  return(DSPP)
}

#' Select superassp DSP function parameters based on metadata
#'
#' This function takes an Emu database handle and a function defined in
#' [superassp] or a named package, and outputs a [tibble::tibble] containing
#' function paramers to use for each bundle. The function parameters are derived
#' from the database of age and gender specific settings obtained from in the
#' literature based Gender and Age metadata information set for the bundle.
#'
#' @param emuDBhandle The Emu database handle.
#' @param onTheFlyFunctionName The name of the digital speech processing function
#' @param onTheFlyParams If default parameters are given, they are applied to all bundles / files, overwriting parameters set based on metadata.
#' @param metadata.defaults Default values for metadata. Usually used for setting Age and Gender for bundles missing that information.
#' @param recompute If `TRUE` the Age and Gender specific DSP settings will be recomputed from the database. Please see [get_metadataParameters] for more information.
#' @param package
#'
#' @return A [tibble::tibble] with one row per bundle / session and with extra columns containing parameters to apply.
#'
#' @examples
#' retuire(superassp)
#' reindeer:::create_ae_db(verbose = TRUE) -> emuDBhandle
#' reindeer:::add_dummy_metadata(emuDBhandle)
#'  mp <- match_parameters(emuDBhandle,
#'  onTheFlyFunctionName = "forest",onTheFlyParams = list(nominalF1=200),metadata.defaults = list(Age=35,Gender=NA)))
#' print(mp)
#'
match_parameters <- function(emuDBhandle,onTheFlyFunctionName,onTheFlyParams=NULL,metadata.defaults=list(Gender=NA,Age=35),recompute=FALSE,package="superassp"){

  meta <- get_metadata(emuDBhandle,manditory=names(metadata.defaults))
  dsp <- get_metadataParameters(recompute=recompute)
  #Get information on what function is called, and what parameters it may take

  currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
  funcFormals = as.list(formals(currFunc))
  names(funcFormals) -> fp
  toSetFp <- intersect(fp,names(dsp)) # Which metadata derived settings will be used by the function
  functionDefaults <- funcFormals[toSetFp]

  meta_settings <- meta %>%
    dplyr::left_join(dsp,na_matches = "na") %>%
    dplyr::select(session, bundle, all_of(toSetFp)) %>%
    tidyr::replace_na(functionDefaults) %>%
    dplyr::group_by(session,bundle)

  if(! is.null(onTheFlyParams) && length(onTheFlyParams) > 0){
    #Now overwrite explicitly provided arguments
    meta_settings <- meta_settings %>%
      dplyr::mutate(as.data.frame(onTheFlyParams))
  }

  return(meta_settings)

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
#'
#' @return A boolean value indicating whether creation signal files were sucessful or not.
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
    existingDefExists <- TRUE

    currDef <- existingDefinitions[existingDefinitions$name == name,c("columnName","fileExtension")]
    if(is.null(columnName)){
      columnName <- currDef$columnName
    } else {
      #In this case we need to check that the specifications agree for the track
      if(currDef$columnName != columnName){
        #Wrong definition
        stop("A track '",name,"' is already defined in the database but with a different column name specification ('",currDef$columnName,"') than what you specified ('",columnName,"').\n Please use the same column name, or use NULL.")
      }
    }

    if(is.null(fileExtension)){
      fileExtension <- currDef$fileExtension
    } else {
      #In this case we need to check that the specifications agree for the track
      if(currDef$fileExtension != fileExtension){
        #Wrong definition
        stop("A track '",name,"' is already defined in the database but with a different file extension specification ('",currDef$fileExtension,"') than the specified ('",fileExtension,"').\n Please use the same file extension name, or use NULL.")
      }
    }
  }

  if(is.null(inputTrackExtension)){
    #We need to get the default media file extension from the database definition if it not defined
    dbConfig = emuR:::load_DBconfig(emuDBhandle)
    inputTrackExtension <- dbConfig$mediafileExtension
  }


  if(!is.null(onTheFlyFunctionName)){
    # From here we deduce how to apply the function -----------------------

    #Initialize a return list
    created <- list(created=c(),failed=c())

    ## Fill in some defaults



    # Fill defaults from the attributes of the applied function
    currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
    defTracks <- superassp::get_definedtracks(currFunc)

    #Make sure we have a track definition
    if(is.null(columnName) && length(defTracks) > 0 ){
      columnName <- defTracks[1]
    }


    if(! columnName %in% defTracks ) {
      stop("The track ",columnName, " is not a defined output track name of the function ",onTheFlyFunctionName,". Valid values are ",paste(defTracks,collapse = ","))
    }


    #Set the default file extension to the one set as an attribute, if missing in the arguments
    fileExtension <- ifelse(!is.null(fileExtension),
                            fileExtension,
                            superassp::get_extension(onTheFlyFunctionName))

    if("explicitExt" %in% formalArgs(currFunc)){
      onTheFlyParams$explicitExt <- fileExtension
    }
    ## Set up logging

    if(!is.null(onTheFlyOptLogFilePath) ) {
       if(!dir.exists(onTheFlyOptLogFilePath)){
         stop("The logging directory '",onTheFlyOptLogFilePath,"'does not exists. Please create it first")
       }else{
         logName <- file.path(onTheFlyOptLogFilePath,paste0(onTheFlyFunctionName,".log"))
         logger::log_appender(logger::appender_file(logName))
         logger::log_threshold(logger::INFO)
         logger::log_formatter(logger::formatter_json)
       }
      onTheFlyParams$optLogFilePath = onTheFlyOptLogFilePath
    }else{
      logger::log_threshold(logger::WARN)
    }

    meta_settings <- metadata_parameters(emuDBhandle = emuDBhandle,
                                         onTheFlyFunctionName = onTheFlyFunctionName,
                                         onTheFlyParams = onTheFlyParams,
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


    fl_meta_settings %>%
      dplyr::rename(listOfFiles=absolute_file_path) %>%
      dplyr::group_map( as.list) -> dspParList

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

      outFile <- paste(tools::file_path_sans_ext(dspParList[[currFileGroup]][["listOfFiles"]]),fileExtension,sep=".")

      if(overwriteFiles || ! file.exists(outFile)){

        dspParList[[currFileGroup]] -> currArgLst


        #logger::log_info(toLog)
        toLog <- utils::modifyList(list("function"=onTheFlyFunctionName),currArgLst)
        #logger::log_info(logger::deparse_to_one_line(toLog))
        logger::log_info(toLog)

        #Now actually make the function call
        do.call(currFunc, currArgLst)

        #Now check that the output file was actually created
        if(!file.exists(outFile)){
          logger::log_warn("Output file ",outFile," was not created")
        }
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


    return(
      name %in% emuR::list_ssffTrackDefinitions(emuDBhandle)$name &&
        length(list_files(emuDBhandle,fileExtension)) == length(list_files(emuDBhandle,fileExtension))
    )
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




#' A utility function that builds a list of parameters to use in function call based on metadata
#'
#' This function takes a function and explicit argument list and builds a list of parameters for a function call considering the metadata of a bundle (in a session).
#'
#'
#' @inheritParams metadata_parameters
#' @param session Only consider this session.
#' @param bundle  The bundle in the session to build an argument list for.
#'
#' @return A list which may be supplied to a `do.call` call.
#'
#' @seealso metadata_parameters
#' @seealso do.call
#'

get_metaFuncFormals <- function(emuDBhandle,session,bundle,onTheFlyFunctionName,onTheFlyParams=list(),metadata.defaults=list(Gender=NA,Age=35),package="superassp"){

  currBundl <- bundle
  currSess <- session

  currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
  funcFormals = as.list(formals(currFunc))
  names(funcFormals) -> fp

  dspParList <- metadata_parameters(emuDBhandle,metadata.defaults = metadata.defaults, wide.format=FALSE) %>%
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

# library(superassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db(verbose = TRUE) -> emuDBhandle
# reindeer:::add_dummy_metadata(emuDBhandle)
#
# print(match_parameters(emuDBhandle,onTheFlyFunctionName = "forest",onTheFlyParams = list(nominalF1=200),metadata.defaults = list(Age=35,Gender=NA)))

# add_trackDefinition(emuDBhandle,name="fms",onTheFlyFunctionName = "forest")

# add_trackDefinition(emuDBhandle,"zcr",onTheFlyFunctionName = "zcrana")
#  get_metadata(emuDBhandle) -> md
# reindeer:::add_dummy_metadata(emuDBhandle)
# get_metadata(emuDBhandle) -> md
# print(metadata_parameters(emuDBhandle,onTheFlyFunctionName = "forest") -> mp)

# print(metadata_parameters(emuDBhandle,onTheFlyFunctionName = "forest",onTheFlyParams = list(te=2,ett=3,numFormants=9)) -> mp2)
# query(emuDBhandle,"Phonetic = s") -> sl
# sl <- sl[1:3,]
#
# readTrackData(filename="~/Desktop/aaa.wav") -> out
# computeTrackData(forest,filename="~/Desktop/aaa.wav",start=0.0,end=1.0,arguments=list()) -> out1
# computeTrackData(praat_voice_report,filename="~/Desktop/aaa.wav",start=0.0,end=1.0,arguments=list()) -> out2

# out <- get_metaFuncFormals(emuDBhandle,session="0000",bundle="msajc010",onTheFlyFunctionName = "forest")
# print(get_metadata(emuDBhandle))
# print(metadata_parameters(emuDBhandle,onTheFlyFunctionName = "forest")-> out)
#
# # # git2r::init(emuDBhandle$basePath)
#dd_trackDefinition(emuDBhandle,"f02","pitch",onTheFlyFunctionName = "mhsF0")
# add_trackDefinition(emuDBhandle, name = "FORMANTS", onTheFlyFunctionName = "forest")
#add_trackDefinition(emuDBhandle, name = "F0", onTheFlyFunctionName = "ksvF0",onTheFlyOptLogFilePath="~/Desktop/out/")



