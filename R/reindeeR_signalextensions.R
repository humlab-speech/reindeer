

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
#' require(emuR)
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
  onTheFlyParams = NULL,
  onTheFlyOptLogFilePath = NULL,
  verbose = TRUE,
  interactive = TRUE){

  # If a function name is not provided , or the function extists in wrassp, just
  # call emuR::add_ssffTrackDefinition like normal
  if( is.null(onTheFlyFunctionName) || !is.null(wrassp::wrasspOutputInfos[[onTheFlyFunctionName]])){
    emuR::add_ssffTrackDefinition(emuDBhandle=emuDBhandle,
                                  name=name,
                                  columnName = columnName,
                                  fileExtension = fileExtension,
                                  onTheFlyFunctionName = onTheFlyFunctionName,
                                  onTheFlyParams = onTheFlyParams,
                                  onTheFlyOptLogFilePath = onTheFlyOptLogFilePath,
                                  verbose = verbose,
                                  interactive = interactive)

  }else{

    # Check that the function extists
    if(exists(onTheFlyFunctionName) & is.function(get(onTheFlyFunctionName))){

      fun <- get(onTheFlyFunctionName)
      #Check that the function has been prepared for use with this function by
      # giving it the the required additional attributes "ext" and "tracks"
      if(!is.null(attr(fun,"ext")) & !is.null(attr(fun,"tracks")) ){
        #Set the default file extension to the one set as an attribute, if missing in the arguments
        ext <- ifelse(!is.null(fileExtension),fileExtension,attr(fun,"ext"))
        if(!columnName %in% attr(fun,"tracks") ) stop("The track ",columnName, " is not a defined output track name of the function ",onTheFlyFunctionName)
        columnName <- ifelse(!is.null(columnName),columnName,attr(fun,"tracks")[[1]])

      }else{
        stop("The function ",onTheFlyFunctionName," is not defined correctly. Please provide it with the attributes \"ext\" and \"tracks\".\n See ?attr for details, as well as attributes(praat_formant_burg) for an example." )
      }
      dbConfig = emuR:::load_DBconfig(emuDBhandle)
      funcFormals = formals(onTheFlyFunctionName)
      funcFormals[names(onTheFlyParams)] = onTheFlyParams
      funcFormals$optLogFilePath = onTheFlyOptLogFilePath
      fp = emuR::list_files(emuDBhandle, dbConfig$mediafileExtension)
      funcFormals$listOfFiles = paste(emuDBhandle$basePath, paste0(fp$session, emuR:::session.suffix), paste0(fp$bundle, emuR:::bundle.dir.suffix), fp$file, sep = .Platform$file.sep)
      funcFormals$explicitExt = fileExtension
      funcFormals$verbose = verbose
      do.call(onTheFlyFunctionName, funcFormals)
      #add the definition
      emuR::add_ssffTrackDefinition(emuDBhandle,name=name,columnName = columnName,fileExtension = ext)
    }else{

      stop("Could not find a definition of the function ",onTheFlyFunctionName,"." )
    }
  }
}
