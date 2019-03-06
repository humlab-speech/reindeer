
#' This function gathers metata for bundles in an emuR database.
#'
#' Metadata of a file is stored in 'meta_json' files. This function goes through all
#' bundles, parses the JSON data and collects everything into a data fram which is
#' returned. The structure of the metadata does not have to be consistent across
#' meta_json files. New columns are added to the data.frame as new fields are detected.
#' And the used may specify addidional columns that should be added to the data frame
#' regardless of them being present in any fo the 'meta_json' files.
#'
#' The user may also give the name of an Excel file to which the metadata table should be
#' exported. The session and bundle columns of this file will be locked to make it
#' easier to edit the metadata. The two file columns are also hidden.
#'
#' @param dbhandle The database handle of an emuR database.
#' @param Excelfile The full path and file name of the Excel file that the metadata should be written to. The function will not overwrite this file, unless \code{overwrite} is set to \code{TRUE}.
#' @param add.metadata A vector of column names that the function should make sure
#' that they are present in the output.
#' @param overwrite The default behaviour is that an Excel file should not be
#' overwritten if it is present already. If this parameter is \code{TRUE} then the file will be overwritten.
#' @param session A session pattern. Used for editing only the metadata of bundles in a
#' specific  session.
#'
#'
#' @return A data frame containing inforamtion about the 'meta_json' files found
#' \describe{
#'   \item{session}{The name of the session.}
#'   \item{bundle}{The bundle name}
#'   \item{file}{The file name of the meta_json file found in the database}
#'   \item{absolute_file_path}{The full absolute path to the 'meta_json' file.}
#' }
#' In addition, the \code{\link[base]{data.frame}} will contain one column for every
#' type of information given in any of the 'meta_json' files.
#' @export
#'
#' @examples
#' \donotrun{
#' create_ae_db() -> ae
#' make_dummy_metafiles(ae)
#' get_metadata(ae)
#' ## Some cleanup code
#' unlink_emuRDemoDir()
#' rm(ae)
#' }
#'
get_metadata <- function(dbhandle,Excelfile=NULL,add.metadata=c("Session.DateTime","Speaker.ID"),overwrite=FALSE,session=".*"){
  #Start with checking consistency regarding output file
  if(! overwrite && !is.null(Excelfile) && file.exists(Excelfile)){
    stop("Could not write output file ",Excelfile,": File exists but should not be overwritten.")
  }
  emuR:::check_emuDBhandle(dbhandle)
  bundles <- list_bundles(dbhandle,session=session)
  metafiles <- list_files(dbhandle,fileExtension = "meta_json",sessionPattern=session)
  #Use the bundle list as a scaffold for a data fram to hold the content of all metadata files
#  metacontent <- metafiles[c("bundle","absolute_file_path")]
  for(currFile in na.omit(metafiles$absolute_file_path)){
    jsonmeta <- jsonlite::read_json(currFile,simplifyVector = TRUE)
    if( any(add.metadata %in% names(jsonmeta)) ){
      stop("Cannot add metadata columns that already has a value in a metadata file.")
    }
    # See if we should add some columns
    toappend <- setdiff(add.metadata,names(jsonmeta))
    if(!is.null(toappend) | length(toappend ) > 0){
      jsonmeta[,toappend] <- NA
    }
    # Now start inserting data from the metafiles
    for(col in names(jsonmeta)){
      metafiles[metafiles$absolute_file_path == currFile,col] <- jsonmeta[[col]]
     }
  }
  if(!is.null(Excelfile)){
    wb <- openxlsx::createWorkbook(paste(dbhandle$dbName,"metadata"))
    openxlsx::addWorksheet(wb,"metadata")
    openxlsx::writeDataTable(wb,"metadata",x=metafiles,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"metadata",firstActiveCol = 5)
    openxlsx::setColWidths(wb,"metadata",cols=3:4,hidden=TRUE)
    openxlsx::setColWidths(wb,"metadata",cols=5:30,widths = 18)
    openxlsx::saveWorkbook(wb,file=Excelfile,overwrite=overwrite)
  }

  return(metafiles)

}

#' A function for importing metadata from an Excel file
#'
#' This function takes an Excel file (in .xlsx format) and extracts metadata from it.
#' The file should begin with the folowing columns:
#' \itemize{
#' \item session
#' \item bundle
#' \item file
#' \item absolute_file_path
#' }
#' and then go on to have some columns which contains the meta data. Each row in the
#' data contains the information and metadata for a bundle (in a specific session).
#' The simples way to get such a file is to create one from a database using the
#' \code{\link{get_metadata}} function. The fields \code{file} and
#' \code{absolute_file_path} will be hidden in this file to fascilitate editing of the
#' metadata.
#'
#' Please be aware that bundles that are speficied in the Excel file will have
#' their metadata files (ending with '.meta_json') overwritten when using the
#' \code{import_metadata}. So, please make sure to remove rows of bundles that should
#' not be altered from the Excel file before importing the metadata from it.
#'
#'
#' @param dbhandle The emuR database handle of the database.
#' @param Excelfile The path to a properly formated Excel (.xlsx) file.
#' @param ignore.columns Columns in the Excel file (other than \code{session},\code{bundle},
#' \code{file} and \code{absolute_file_path}) that should not be considered to contain metadata.
#'
#' @return A vector of 'meta_json' files updated by the call. The path for each file is given relative to the base of the emuR database.
#' @export
#'
import_metadata <- function(dbhandle,Excelfile,ignore.columns=NULL){
  if(!file.exists(Excelfile)){
    stop("Unable to open the metadata Excel file.\nThe file ",filename," does not exist!")
  }
  openxlsx::read.xlsx(Excelfile) -> meta
  if(!is.null(ignore.columns) && ! ignore.columns %in% names(meta)){
    miss <- setdiff(ignore.columns,names(meta))
    warning("The columns ",paste(miss,collapse = ",")," are not present in the Excel file.")
  }

  #Here we just remove the columns named by the user which exists in the metadata file
  if(!is.null(ignore.columns)){
    meta <- meta %>% select(setdiff(names(meta),ignore.columns))
  }
  #Make sure we have an output file
  meta <- meta %>%
    select(-absolute_file_path,-file) %>%
    mutate(metadatafile=file.path(dbhandle$basePath,
                                  paste0(session,"_ses"),
                                  paste0(bundle,"_bndl"),paste0(bundle,".meta_json"))) %>%
    select(-session,-bundle)
  #Now to the main business of the function
  jsondat <- meta %>%
    select(-metadatafile)
  json <- jsondat %>%
    mutate(json=jsonlite::toJSON(.,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")) %>%
    select(json)

  towrite <- meta %>%
    bind_cols(json) %>%
    select(metadatafile,json)
  #Write the  data
  for(r in 1:nrow(towrite)){
    fileConn <- file(towrite[r,"metadatafile"])
    writeLines(towrite[r,"json"], fileConn)
    close(fileConn)
  }
  out <- gsub(paste0(dbhandle$basePath,"/"),"",towrite[["metadatafile"]])
  return(out)
}

#' Add identifying information based on the content of the wave file to the metadata information for the bundle.
#'
#' This function will extract information from the wav file associated with a bundle, and add it to the set of metadata
#' for the bundle. This information can later be used to verify that the file has not been altered later on, or to deidentify
#' wav files in a reversable manner for use outside of the emuR framework. Deidentified files are sometimes useful for blinded randomized
#' perceptual testing, and the ability to reverse the procedure is then essential to link the results of the evaluation back to the original
#' recording extracted from the emuR data base.
#'
#' @param dbhandle The handle for the emuR database.
#' @param sessionPattern A regexp pattern that allows you to limit which sessions should be affected by the manibpulation.
#' @param bundlePattern A regexp pattern that allows you to limit which bundles to include.
#' @param algorithm The name of the hashing algorithm, according to the \code{\link[digest]{digest}} function.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_ae_db() -> ae
#' add_digests(ae)
#' get_metadata(ae,Excelfile = NULL) -> res
#' print(res)
#' unlink_emuRDemoDir()
#' }
#'
add_digests <- function(dbhandle,sessionPattern=".*",bundlePattern=".*",algorithm="sha1"){
  wavs <- list_files(dbhandle,fileExtension = "*.wav",sessionPattern=sessionPattern,bundlePattern=bundlePattern)
  for(inFile in wavs[["absolute_file_path"]]){

    wrassp::read.AsspDataObj(inFile) -> w
    options(digits=15)
    attr(w,"sampleRate") -> sr
    attr(w,"endRecord") - attr(w,"startRecord") +1 -> samples
    samples / sr *1000 -> duration
    rm(w)
    digest::digest(inFile,file=TRUE,algo=algorithm) -> checksum
    #Now insert the information into the meta_json file
    outFile <- gsub(".wav$",".meta_json",inFile)
    if(file.exists(outFile)){
      json <- jsonlite::read_json(outFile,simplifyVector = TRUE)
      json["Bundle.Duration"] <- duration
    }else{
      #just create a JSON structure then
      json <- data.frame("Bundle.Duration"=duration)
    }
    json[paste0("Bundle.",algorithm,"_checksum")] <- checksum

    #Convert back to a JSON string
    outJson <- jsonlite::toJSON(json,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")
    #Write to file
    fileConn <- file(outFile)
    writeLines(outJson, fileConn)
    close(fileConn)
  }
}
