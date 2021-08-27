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
#' @param defaultAge The default age to use when the user has not set a speaker "Age" metadata for the bundle or session. The user is *strongly* encouraged to set the age of the speaker explicitly as metadata, and not to rely on this default setting.
#' @param overwriteFiles If set to `TRUE`, the function will calculate SSFF track files for ALL bundles and write them into the database, overwriting existing files. The default is `FALSE` which means that only only bundles which do not have an track file with the indicated output extension will be written.
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
add_trackDefinition <- function(
  emuDBhandle,
  name,
  columnName = NULL,
  fileExtension = NULL,
  onTheFlyFunctionName = NULL,
  onTheFlyParams = list(),
  onTheFlyOptLogFilePath = NULL,
  inputTrackExtension="wav",
  defaultAge=35,
  overwriteFiles=FALSE,
  package="superassp"){

  existingDefExists = FALSE
  #Check if the track has not already been defined
  if( name %in% list_ssffTrackDefinitions(emuDBhandle )$name){
    existingDefExists = TRUE
    # This is ok if the columnName and fileExtension is also identical
    r <- which(list_ssffTrackDefinitions(emuDBhandle )$name == name)
    if(!all(list_ssffTrackDefinitions(emuDBhandle )[r,] == c(name,columnName,fileExtension))){
      stop("A track named '",name,"' is already defined, but with a differend columnName and fileExtension, and this function can therefore not define the SSFF track you as requested.")
    }
  }

  # Just make sure that "ext" is always defined (while it may be reset later in the following section)
  ext <- fileExtension

  if(!is.null(onTheFlyFunctionName)){
    # --------- From here we deduce how to apply the function -----------------------

    defTracks <- superassp::get_definedtracks(onTheFlyFunctionName)
    defExt <- superassp::get_extension(onTheFlyFunctionName)

    columnName <- ifelse(!is.null(columnName) & length(defTracks) > 0 ,columnName,defTracks[[1]])

    if(! columnName %in% defTracks ) {
      stop("The track ",columnName, " is not a defined output track name of the function ",onTheFlyFunctionName)
    }
    #Set the default file extension to the one set as an attribute, if missing in the arguments
    ext <- ifelse(!is.null(fileExtension),fileExtension,defExt)

    if(is.null(inputTrackExtension)){
      #We need to get the default media file extension from the database definition if it not defined
      dbConfig = emuR:::load_DBconfig(emuDBhandle)
      inputTrackExtension <- dbConfig$mediafileExtension
    }



    if(!is.null(onTheFlyOptLogFilePath) ) {
       if(!dir.exists(onTheFlyOptLogFilePath)){
         stop("The logging directory '",onTheFlyOptLogFilePath,"'does not exists. Please create it first")
       }else{
         #we have a logging output directory
         #logName <- file.path(onTheFlyOptLogFilePath,
         #  paste0(paste(onTheFlyFunctionName,format(Sys.time(), "%Y-%m-%d_%H:%M:%S"),sep="_"),".log")
         #)
         #logCon <- file(logName,open="at")
         logName <- file.path(onTheFlyOptLogFilePath,paste0(onTheFlyFunctionName,".log"))
         logger::log_appender(logger::appender_file(logName))
         logger::log_threshold(logger::INFO)
       }
    }else{
      logger::log_threshold(logger::WARN)
    }

    fl = emuR::list_files(emuDBhandle, inputTrackExtension)
    meta <- get_metadata(emuDBhandle)
    dsp <- get_parameters()


    currFunc <- utils::getFromNamespace(onTheFlyFunctionName,package)
    funcFormals = formals(currFunc)
    #Compute which formal arguments we also have a Gender and Age aware default setting for
    names(funcFormals) -> fp
    unique(dsp$Parameter) -> pp
    intersect(pp,fp) -> fparam

    assertthat::assert_that(all(c("Age","Gender") %in% names(meta)))

    #This is the real meat of this function. Here we get default values for parameters required by the
    # signal processing functions from the DSPP set. The best match is determined to be the match with is applicable
    # to the smallest age span.

    fl %>%
      dplyr::left_join(meta,na_matches = "na",by=c("session","bundle")) %>%
      dplyr::mutate(Age=ifelse(is.na(Age),defaultAge,Age) )  %>%
      dplyr::left_join(dsp %>%
                         dplyr::filter(Parameter %in% fparam),na_matches = "na",by=c("Gender"))  %>%
      dplyr::filter(!is.na(bundle),!is.na(session)) %>%
      dplyr::mutate(Age_lower=ifelse(is.na(Age_lower),0,Age_lower),
                    Age_upper=ifelse(is.na(Age_upper),200,Age_upper)) %>%
      dplyr::filter( Age <= Age_upper , Age >= Age_lower  ) %>%
      dplyr::mutate(AgeRange=Age_upper-Age_lower) %>%
      dplyr::arrange(session,bundle,file,absolute_file_path,Parameter,AgeRange) %>%
      dplyr::group_by(session,bundle,file,absolute_file_path,Parameter) %>%
      dplyr::slice_head(n=1) %>%
      dplyr::ungroup() %>%

      dplyr::group_by(session, bundle, file, absolute_file_path) -> fl_meta_settings

    #Overwrite default by manually specified parameters when present
    # We do this after matching with DSPP defaults, as they would otherwise have to be matched by file early but applied last
    if(any(fparam %in% names(fl_meta_settings))) {
      manualParameters <- base::intersect(fparam,names(fl_meta_settings))
      for(currParam in manualParameters){
        #We can now set up two logical vectors, which
        # 1) identify which rows identify a parameter for which there is a column
        paramRow <- fl_meta_settings$Parameter == currParam
        # 2) and which rows where a default value has been set
        manualValueSet <- !is.na(fl_meta_settings[,currParam])
        # Get the row which correspond both to a parameter setting set by DSPP and by a value set in a corresponding column
        rowToSwap <- paramRow & manualValueSet
        fl_meta_settings[rowToSwap,"Setting"] <- as.character(fl_meta_settings[rowToSwap,currParam]) # The conversion is required due to some settings being character
      }
    }


    #Do some cleanup
    fl_meta_settings <- fl_meta_settings %>%
      dplyr::select(session,bundle,file ,absolute_file_path, Parameter,Setting)

    #return(fl_meta_settings)

    #We have already made per file grouping of the tibble, so we may use that to extract file information
    ng <- dplyr::n_groups(fl_meta_settings)
    ngi <- as.integer(as.vector(ng))

    assertthat::assert_that(nrow(fl) == ngi, msg=paste0("Not all sounds files were assigned metadata ", nrow(fl)," ==> ",ng))


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


    if(overwriteFiles){
      message(paste0("Applying the function '",onTheFlyFunctionName, "' to all input tracks (.",inputTrackExtension,").\n"))
    }else {
      message(paste0("Applying the function '",onTheFlyFunctionName, "' to all .",inputTrackExtension," files for which a signal track file (.",fileExtension,") does not exist.\n"))
    }




    pb <- utils::txtProgressBar(min=0, max=ngi, style = 3)

    for(currFileGroup in 1:ngi){

      setTxtProgressBar(pb, currFileGroup)

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

        #If we want to create a log of what is going on
        toLog <- sub(")$","",
                         sub("list[(]","",paste(onTheFlyFunctionName,"args:",deparse(argLst))
                             )
                         )

        logger::log_info(toLog)

        do.call(currFunc, argLst)

      }

    }
    close(pb)
  }
  #Only attempt to commit if the git2r package can be loaded.
  if(require("git2r")){
    #Commit created files if the database is a repository
    if(git2r::in_repository(emuDBhandle$basePath)){
      created_files <- emuR::list_files(emuDBhandle,ext)
      git2r::add(repo=emuDBhandle$basePath,path = created_files$absolute_file_path)
      git2r::commit(repo=emuDBhandle$basePath,message=paste0("Adding signal files with an '",ext,"' extension"))
    }
  }


  if(! existingDefExists){

    emuR::add_ssffTrackDefinition(emuDBhandle,name=name,columnName = columnName,fileExtension = ext)
  }


}





### For interactive testing
#
#
# library(wrassp)
# library(reindeer)
# reindeer:::unlink_emuRDemoDir()
# reindeer:::create_ae_db() -> emuDBhandle
# reindeer:::make_dummy_metafiles(emuDBhandle)
# git2r::init(emuDBhandle$basePath)
# add_trackDefinition(emuDBhandle,"f02","pitch",onTheFlyFunctionName = "mhsF0",onTheFlyOptLogFilePath = "~/Desktop/test/")



