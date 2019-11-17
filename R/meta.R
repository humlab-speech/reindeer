
## Some constants
metadata.extension = "meta_json"


#' This function gathers metata for bundles in an emuR database.
#'
#' Metadata of a recording is stored in 'meta_json' files. Metadata may be set at the database, session and bundle level.
#' This function goes through the database metadata file, session metadata files and metadata files associated with each bundle,
#' parses the JSON data and collects everything into an Excel file which is written to a specificed file.
#' The data is also returned as a tibble. The structure of the metadata does not have to be consistent across
#' meta_json files. New columns are added to the as new fields are detected, and the user is then
#' expected to make sure that the Excel file output is edited manually to fix inconsistencies.
#' The user may additionally specify additional columns that should be added to the data frame
#' regardless of them being present in any of the 'meta_json' files.
#'
#' The Excel file output will contain "session" and "bundle" columns of this file will be locked and hidden to make it
#' easier to edit the metadata without making it difficult to use effectivelly afterwards.
#' The user should refrain from editing or removing these hidden columns.
#'
#' @param dbhandle The database handle of an emuR database.
#' @param Excelfile The full path and file name of the Excel file that the metadata should be written to. The function will not overwrite this file, unless \code{overwrite} is set to \code{TRUE}.
#' @param add.metadata A vector of column names that the function should make sure
#' that they are present in the output.
#' @param overwrite The default behaviour is that an Excel file should not be
#' overwritten if it exists already. If this parameter is \code{TRUE} then the file will be overwritten.
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
#'
#'
#' @examples
#' \donotrun{
#' create_ae_db() -> ae
#' make_dummy_metafiles(ae)
#' export_metadata(ae)
#' ## Some cleanup code
#' unlink_emuRDemoDir()
#' rm(ae)
#' }
#'
export_metadata <- function(dbhandle,Excelfile=NULL,add.metadata=c("Session.DateTime","Speaker.ID"),overwrite=FALSE,session=".*"){
  #Start with checking consistency regarding output file
  if(! overwrite && !is.null(Excelfile) && file.exists(Excelfile)){
    stop("Could not write output file ",Excelfile,": File exists but should not be overwritten.")
  }
  emuR:::check_emuDBhandle(dbhandle)
  bundles <- list_bundles(dbhandle,session=session)
  metafiles <- list_files(dbhandle,fileExtension = metadata.extension,sessionPattern=session)
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

  #Prepare an Excel workbook, if one should be written
  if(!is.null(Excelfile)){
    wb <- openxlsx::createWorkbook(paste(dbhandle$dbName,"bundle"))
    openxlsx::addWorksheet(wb,"bundles")
    openxlsx::writeDataTable(wb,"bundles",x=metafiles,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"bundles",firstActiveCol = 5)
    openxlsx::setColWidths(wb,"bundles",cols=3:4,hidden=TRUE)
    openxlsx::setColWidths(wb,"bundles",cols=5:30,widths = 18)
  }




  # Include the possibility of having default meta data for a sessions (in a _ses folder)
   sessJSONFiles <- list.files(file.path(dbhandle$basePath),pattern=paste0(".*.",metadata.extension),recursive = TRUE,full.names = FALSE)
   # Remove meta files associated with bundles
   sessJSONFiles <- sessJSONFiles[! grepl(emuR:::bundle.dir.suffix,sessJSONFiles) & grepl(emuR:::session.suffix,sessJSONFiles)]
   # Run only if there are session metadata files
   if(length(sessJSONFiles) > 0){
     sessJSONFilesDF <- data.frame(stringr::str_split(sessJSONFiles,pattern = .Platform$file.sep,simplify = TRUE),stringsAsFactors=FALSE)
     names(sessJSONFilesDF) <- c("session","session_metadata_file")
     # The session needs to be without suffix so that metadata may be joinded by session later
     sessJSONFilesDF$session <- gsub(paste0(emuR:::session.suffix,"$"),"",sessJSONFilesDF$session)
     sessJSONFilesDF <- na.omit(sessJSONFilesDF)


     for(row in nrow(sessJSONFilesDF)){
       currFile <- as.vector(sessJSONFilesDF[[row,"session_metadata_file"]])
       currSession <- as.vector(sessJSONFilesDF[[row,"session"]])
       currSessionDir <- paste0(currSession,emuR:::session.suffix)

       jsonmeta <- jsonlite::read_json(file.path(dbhandle$basePath,currSessionDir,currFile),simplifyVector = TRUE)

       # Now start inserting data from the session metadata file
       for(col in names(jsonmeta)){
         sessJSONFilesDF[sessJSONFilesDF$session == currSession,col] <- jsonmeta[[col]]
       }
     }

     #Add session meta data to the workbook
     if(!is.null(Excelfile)){
       openxlsx::addWorksheet(wb,"sessions")
       openxlsx::writeDataTable(wb,"sessions",x=sessJSONFilesDF,keepNA = FALSE,withFilter=FALSE)
       openxlsx::freezePane(wb,"sessions",firstActiveCol = 3)
       openxlsx::setColWidths(wb,"sessions",cols=2,hidden=TRUE)
       openxlsx::setColWidths(wb,"sessions",cols=3:30,widths = 18)
     }

     metafiles %>%
       left_join(sessJSONFilesDF,by="session",suffix=c("","_sessionmetadatafile")) %>%
       select(-session_metadata_file) -> metafiles

     # Now, it could be that the Session metadata and bundle metadata contain the same
     # pieces of information. Then, Session metadata should overwrite bundle metadata only
     # when bundle metadata is NA.
     duplicates <- grep("_sessionmetadatafile",names(metafiles),value=TRUE)
     for(dupl in duplicates){
       bundleoriginal <- gsub("_sessionmetadatafile","",dupl)
       #metafiles[is.na(metafiles[bundleoriginal]),bundleoriginal] <- metafiles[is.na(metafiles[bundleoriginal]),dupl]
       metafiles[bundleoriginal] <- ifelse(is.na(metafiles[[dupl]]),
                                                 metafiles[[bundleoriginal]],
                                                 metafiles[[dupl]])
       metafiles[dupl] <- NULL
     }

   }

   # We do not need to check owrwriting here as that is handled by saveWorkbook
   if(!is.null(Excelfile)){
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
#' \code{\link{export_metadata}} function. The fields \code{file} and
#' \code{absolute_file_path} will be hidden in this file to fascilitate editing of the
#' metadata without the user breaking anything.
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
#' This function will extract information (lenght of recording and a checksum) from the wav file associated with a bundle, and add it to the set of metadata
#' for the bundle. This information can later be used to verify that the file has not been altered later on, or to deidentify
#' wav files in a reversable manner for use outside of the emuR framework. Deidentified files are sometimes useful for blinded randomized
#' perceptual testing, and the ability to reverse the procedure is then essential to link the results of the evaluation back to the original
#' recording extracted from the emuR data base. The user may create checksums by multiple algorithms by running the function again with different \code{algorithm} arguments.
#'
#' @param dbhandle The handle for the emuR database.
#' @param sessionPattern A regexp pattern that allows the user to limit which sessions should be affected by the manipulation.
#' @param bundlePattern A regexp pattern that allows the user to limit which bundles to include.
#' @param algorithm The name of the hashing algorithm, according to the \code{\link[digest]{digest}} function.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_ae_db() -> ae
#' add_digests(ae)
#' export_metadata(ae,Excelfile = NULL) -> res
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



#' Create a biography of the labels in a list of segments in a tidy manner
#'
#' @param segs_tbl The \code[dplyr]{\link{tibble}} that is the result \code{\link[emuR]{query}} call.
#' @param emudb_hdl A \code{\link{emuR}} database handle.
#' @param compute_digests Should information that describes the recorded sound files be computed so that is is definitelly part of the
#' added metadata information.
#' @param algorithm The checksum algorithm that should be used when computing sound file information.
#'
#' @return a \code{\link[dplyr]{tibble}}
#' @export
#'
#' @examples
#' \notrun{
#' ## This code just sets up a new emuR database and inserts some
#' ## fake metadata into it.
#' create_ae_db() -> ae_test
#' make_dummy_metafiles(ae_test)
#' # Get all the 'n' segments in the database
#' query(ae_test,"Phonetic = n",resultType = "tibble") -> ae_nt
#' # Add information related to the nature the recording sessions
#' # e.g. the speaker ID, the date of the recording
#' ae_nt %>% biographize(ae_test)
#' # This code does the same as the above, but it will also compute new
#' # information that is strictly  aimed at identifying the recording
#' # (length of recording and a a sha1 digest of the wav file).
#' ae_nt %>% biographize(ae_test,compute_digests=TRUE,algorithm="sha1")
#' }
#'
biographize <- function(segs_tbl,emudb_hdl,compute_digests=FALSE,algorithm="sha1") {
  #make sure that the first argument is a segment list, and that
  # it contains "session" and "bundle" columns.
  if(! is.data.frame(segs_tbl) || !c("session", "bundle") %in% names(segs_tbl)){
    out <- paste("The input to the",match.call()[[1]], "has to be a 'tibble' or a 'data.frame'.")
    stop(out)
  }
  if(compute_digests==TRUE){
    add_digests(emudb_hdl,algorithm = algorithm)
  }
  #Here we use the special mode of export_medatata to get a data structure rather than an Excel file.
   mdata <- export_metadata(emudb_hdl,session = ".*",Excelfile=NULL,overwrite = FALSE)
   out <- segs_tbl %>% left_join(mdata,by = c("session", "bundle"))
   return(out)
}
