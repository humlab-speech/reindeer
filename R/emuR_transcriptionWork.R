



#' Find bundles in a database that have not been transcribed yet.
#'
#' @author Fredrik Karlsson
#'
#' @param emuDBhandle The database handle of an [emuR] database.
#'
#' @return A vector of bundle names
#' @export
#'
#' @examples
#' \dontrun{
#' reindeer:::create_ae_db() -> ae
#' # Make a test case for this particular function
#' unlink(file.path(ae$basePath,"0000_ses/msajc012_bndl/msajc012_annot.json"))
#' print(list_untranscribedBundles(ae))
#' reindeer:::detach_demo_db(ae)
#' }
#'
list_untranscribedBundles <- function(emuDBhandle){
  wavs <- emuR::list_files(emuDBhandle,fileExtension = "[.]wav$")$bundle
  transcriptions <- emuR::list_files(emuDBhandle,".*[.]json")$bundle
  untranscribed <- setdiff(wavs,transcriptions)
  return(untranscribed)
}

