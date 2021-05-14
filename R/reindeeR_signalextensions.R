

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
#' SSFF track definitions in the database using [emuR::add_ssffTrackDefinition] in order to ensure compitability
#'
#'
#' @inheritParams emuR::add_ssffTrackDefinition

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
  inputTrackExtension=NULL,
  verbose = TRUE,
  interactive = TRUE){

  if(!is.null(onTheFlyFunctionName)){
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

    if(!is.null(onTheFlyFunctionName)){


      fl = list_files(emuDBhandle, inputTrackExtension)
      meta <- get_metadata(emuDBhandle)
      dsp <- get_parameters()

      # if(is.null(package)){
      #   currFunc <- get(onTheFlyFunctionName)
      # }else{
      #   currFunc <- getFromNamespace(onTheFlyFunctionName,package)
      # }
      funcFormals = formals(onTheFlyFunctionName)
      #Compute which formal arguments we also have a Gender and Age aware default setting for
      names(funcFormals) -> fp
      unique(dsp$Parameter) -> pp
      intersect(pp,fp) -> fparam


      fl %>%
        dplyr::left_join(meta,na_matches = "na",by=c("session","bundle")) %>%
        dplyr::full_join(dsp,na_matches = "na",by=c("Gender")) %>%
        dplyr::filter(!is.na(bundle),Parameter %in% fparam) %>%
        dplyr::filter(Age <= Age_upper | is.na(Age_upper) ,Age >= Age_lower | is.na(Age_lower) ) %>%
        dplyr::mutate(AgeFac=(Age - Age_lower)^2 + (Age - Age_upper)^2) %>%
        dplyr::arrange(session,bundle,Parameter,AgeFac) %>%
        dplyr::group_by(session,bundle,Parameter) %>%
        dplyr::slice_head(n=1) %>%
        dplyr::ungroup() %>%
        dplyr::select(session,bundle,file ,absolute_file_path, Parameter,Setting) %>%
        dplyr::group_by(session, bundle, file, absolute_file_path) -> fl_meta_settings

      #We have already made per file grouping of the tibble, so we may use that to extract file information
      ng <- n_groups(fl_meta_settings)

      #Copy arguments given to this function over the list of general arguments given to the called function
      onTheFlyParams$optLogFilePath = onTheFlyOptLogFilePath
      onTheFlyParams$explicitExt = fileExtension
      onTheFlyParams$verbose = verbose

      #Now we are ready to do call the onTheFlyFunctionName function for each media file
      for(currFileGroup in 1:ng){
        fl_meta_settings %>%
          group_map( ~ setNames(.x$Setting,nm=.x$Parameter)) -> dspParList

        purrr::flatten(utils::modifyList(dspParList[currFileGroup][1],onTheFlyParams)) -> argLst
        do.call(onTheFlyFunctionName, argLst)
      }

  }
  #emuR::add_ssffTrackDefinition(emuDBhandle,name=name,columnName = columnName,fileExtension = ext)

}






#' Supplies the user with speaker dependent signal processing parameters
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
#'
#'
#' @param file If the file exists, the settings stored in the file are read in and returned. If it does not exist, one will be created and teh [DSPP] parameters inserted into it for the user to edit. If \code{NULL}, the [DSPP] structure will be returned directly
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

### For interactive testing
#
#reindeer:::create_ae_db() -> emuDBhandle ; reindeer:::make_dummy_metafiles(emuDBhandle)
#fl = list_files(emuDBhandle,"wav")

#unlink(list_files(emuDBhandle,"meta_json")[2,"absolute_file_path"][[1]])



# %>%
#     group_by(session,bundle,file, absolute_file_path,parameter) %>%
#     summarise(setting=first(setting), .groups = "keep") %>%
#   ungroup(parameter) %>%
#   summarise(gpar=setNames(setting,nm = parameter)) %>%
#   arrange(session,bundle,file) -> settings
#

