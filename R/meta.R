
## Some constants
metadata.extension = "meta_json"

## I will need this function until 0.9.0 of dplyr is released,
#possibly fixing the issue with all NA columns supplied to coalesce
# The implementation comes from https://stackoverflow.com/a/19254510
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}


#' Functions for gathering metata specified for recordings in an emuR database.
#'
#' Metadata of a recording is stored in 'meta_json' files. Metadata may be set at the database, session and bundle level.
#' The functions goes through the database metadata file, session metadata files and metadata files associated with
#' each bundle, parses the JSON data and collects everything into a returned \code{\link[dplyr]{tibble}}. Database
#' default values are supressed by metadata set in a session metadata file, and session level data are in turn
#' surpressed by data given at the bundle level. The structure of the metadata does not have to be consistent across
#' meta_json files. New columns are added to the as new fields are detected.
#'
#' The function \code{export_metadata} outputs metadata supplied at the bundle, session and database level in separate
#' tabs of an Excel file. The metadata in the Excel file do not include information inherinted from session or database
#' defaults.The user may additionally specify additional columns that should be added to the data frame
#' regardless of them being present in any of the 'meta_json' files.
#'
#' The user is expected to use the functions \code{export_metadata} and \code{import_metadata} to fix
#' accedental inconsistencies in the metadata speficications across sessions and bundles by exporting all
#' information to an Excel file using \code{export_metadata},  edit columns and values (including moving inconsistently
#' spelled metadata items into a single column with the intended name) using Excel or another editor that complies with
#' the OOXML Workbook ISO/IEC 29500:2008 standard.
#'
#' The user should note that the Excel file result of \code{export_metadata} will contain hidden colums that are
#' stored to fascilitate re-importation of the data.
#' The user should refrain from editing or removing these hidden columns
#'
#' @param dbhandle The database handle of an emuR database.
#' @param Excelfile The full path and file name of the Excel file that the metadata should be written to. The function will not overwrite this file, unless \code{overwrite} is set to \code{TRUE}.
#' @param add.metadata A vector of column names that the function should make sure
#' that they are present in the output.
#' @param overwrite The default behaviour is that an Excel file should not be
#' overwritten if it exists already. If this parameter is \code{TRUE} then the file will be overwritten.
#'
#' @rdname export_metadata
#' @export
#'
#' @return A data frame containing inforamtion about the 'meta_json' files found
#' \describe{
#'   \item{session}{The name of the session.}
#'   \item{bundle}{The bundle name}
#'   \item{file}{The file name of the meta_json file found in the database}
#'   \item{absolute_file_path}{The full absolute path to the 'meta_json' file.}
#' }
#' In addition, the \code{\link[dplyr]{tibble}} will contain one column for every type of information given in any of the 'meta_json' files.
#'
#'
#' @examples
#' \dontrun{
#' create_ae_db() -> ae
#' make_dummy_metafiles(ae)
#' get_metadata(ae)
#' ## Some cleanup code
#' unlink_emuRDemoDir()
#' rm(ae)
#' }
#'

get_metadata <- function(dbhandle,add.metadata=c("Session.Date","Participant.ID"),overwrite=FALSE,session=".*"){
  res <- export_metadata(dbhandle=dbhandle,add.metadata=add.metadata,overwrite=overwrite)
  return(res)
}


#'
#' @rdname export_metadata
#' @export
#'

export_metadata <- function(dbhandle,Excelfile=NULL,add.metadata=c("Session.Date","Participant.ID"),overwrite=FALSE){
  #Start with checking consistency regarding output file
  if(! overwrite && !is.null(Excelfile) && file.exists(Excelfile)){
    stop("Could not write output file ",Excelfile,": File exists but should not be overwritten.")
  }
  emuR:::check_emuDBhandle(dbhandle)
  bundles <- list_bundles(dbhandle) %>%
    rename(bundle=name)
  metafiles <- list_files(dbhandle,fileExtension = metadata.extension)
  #Use the bundle list as a scaffold for a data fram to hold the content of all metadata files
  #  metacontent <- metafiles[c("bundle","absolute_file_path")]
  for(currFile in na.omit(metafiles$absolute_file_path)){
    jsonmeta <- jsonlite::read_json(currFile,simplifyVector = TRUE)
    # if( any(add.metadata %in% names(jsonmeta)) ){
    #   stop("Cannot add metadata columns that already has a value in a metadata file.")
    # }
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
  # Now make sure that all bundles have a row
  metafiles <- bundles %>%
    left_join(metafiles,by=c("session","bundle")) %>%
    select(-file,-absolute_file_path)


  # Include the possibility of having default meta data for a sessions (in a _ses folder)
  sessJSONFiles <- list.files(file.path(dbhandle$basePath),pattern=paste0(".*.",metadata.extension),recursive = TRUE,full.names = FALSE)

  # Remove meta files associated with bundles
  sessJSONFiles <- sessJSONFiles[! grepl(emuR:::bundle.dir.suffix,sessJSONFiles) & grepl(emuR:::session.suffix,sessJSONFiles)]
  sessions <- list_sessions(dbhandle) %>%
    rename(session=name)

  # Run only if there are session metadata files
  if(length(sessJSONFiles) > 0){
    sessJSONFilesDF <- data.frame(stringr::str_split(sessJSONFiles,pattern = .Platform$file.sep,simplify = TRUE),stringsAsFactors=FALSE)
    names(sessJSONFilesDF) <- c("session","session_metadata_file")
    # The session needs to be without suffix so that metadata may be joinded by session later
    sessJSONFilesDF$session <- gsub(paste0(emuR:::session.suffix,"$"),"",sessJSONFilesDF$session)
    sessJSONFilesDF <- na.omit(sessJSONFilesDF)


    for(row in 1:nrow(sessJSONFilesDF)){
      currFile <- as.vector(sessJSONFilesDF[[row,"session_metadata_file"]])
      currSession <- as.vector(sessJSONFilesDF[[row,"session"]])
      currSessionDir <- paste0(currSession,emuR:::session.suffix)

      jsonmeta <- jsonlite::read_json(file.path(dbhandle$basePath,currSessionDir,currFile),simplifyVector = TRUE)

      # Now start inserting data from the session metadata file
      for(col in names(jsonmeta)){
        sessJSONFilesDF[sessJSONFilesDF$session == currSession,col] <- jsonmeta[[col]]
      }
    }

    #Add session meta data to the workbook,
    #or just empty sessions speficiations if there are no session metadata files

    sessJSONFilesDF <- sessions %>%
      left_join(sessJSONFilesDF,by="session")

    # Make the merger with bundle files to make the final output tibble
    metafiles %>%
      left_join(sessJSONFilesDF,by="session",suffix=c("","_sessionmetadatafile")) %>%
      select(-session_metadata_file) -> metafiles

  }

  # Now check and load metadata set at the database level
  emuR:::load_DBconfig(dbhandle) -> dbCfg
  if(is.null(dbCfg$metadataDefaults)){
    dbDefaults <- data.frame()
  }else{
    dbDefaults <- as.data.frame(dbCfg$metadataDefaults,stringsAsFactors=FALSE)
    if(length(dbDefaults) > 0){
      #This means that the field is not just empty
      # Repeat the rows so that the columns may be merged
      dbMeta <- as.data.frame(c(metafiles["bundle"],dbDefaults))  %>%
        mutate_if(is.factor,as.character)
      metafiles <- metafiles %>%
        mutate_if(is.factor,as.character)%>%
        left_join(dbMeta,by="bundle",suffix=c("","_database")) %>%
        distinct() ## This is needed since duplicate rows are introduced by the join by dbMeta

    }
  }

  #Now, there may be a metadata X column set at the bundle level, an X_sessionmetadatafile
  # column set at the session level, and an X_database column for the whole database.
  # These need to be reconsiled


  cols <- names(metafiles)
  duplicates <- grep("_(database|sessionmetadatafile)$",cols,value=TRUE)
  duplicated <- unique(gsub("_(database|sessionmetadatafile)$","",cols))


  for(bundleoriginal in duplicated){

    sessColName <- paste0(bundleoriginal,"_sessionmetadatafile")
    sessVec <- ifelse(exists(sessColName,metafiles),metafiles[,sessColName],NA)
    dbColName <- paste0(bundleoriginal,"_database")
    dbVec <- ifelse(exists(dbColName,metafiles),metafiles[,dbColName],NA)
    ## This seems odd, but it makes sure that NAs are repeated for the length of vectors.
    tempDF <- data.frame(metafiles[[bundleoriginal]],
                         sessVec,
                         dbVec,stringsAsFactors = FALSE) %>%
        mutate_if(is.factor,as.character)
    names(tempDF) <- c("bundle","session","database")
    # Here the result is the first non-NA value for each row (or NA if the row in tempDF contains only NAs)
    metafiles[bundleoriginal] <- with(tempDF,coalesce(bundle,session,database))
    # This works since you can always remove a column without an error message (even non-existing ones)
    metafiles[sessColName] <- NULL
    metafiles[dbColName] <- NULL

  }

  #Prepare an Excel workbook, if one should be written
  if(!is.null(Excelfile)){
    wb <- openxlsx::createWorkbook(paste(dbhandle$dbName,"bundle"))
    openxlsx::addWorksheet(wb,"bundles")
    openxlsx::writeDataTable(wb,"bundles",x=metafiles,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"bundles",firstActiveCol = 5)
    openxlsx::setColWidths(wb,"bundles",cols=5:30,widths = 18)

    # session information

    openxlsx::addWorksheet(wb,"sessions")
    openxlsx::writeDataTable(wb,"sessions",x=sessJSONFilesDF,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"sessions",firstActiveCol = 3)
    openxlsx::setColWidths(wb,"sessions",cols=3:30,widths = 18)
    #database defaults
    openxlsx::addWorksheet(wb,"database")
    openxlsx::writeDataTable(wb,"database",x=dbDefaults,keepNA = FALSE,withFilter=FALSE)
    # We do not need to check owrwriting here as that is handled by saveWorkbook
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
#' }
#' and then go on to have some columns which contains the meta data. Each row in the
#' data contains the information and metadata for a bundle (in a specific session).
#' The simples way to get such a file is to create one from a database using the
#' \code{\link{export_metadata}} function.
#'
#' Please be aware that bundles that are speficied in the Excel file will have
#' their metadata files (ending with '.meta_json') overwritten when using the
#' \code{import_metadata}. So, please make sure to remove rows of bundles that should
#' not be altered from the Excel file before importing the metadata from it.
#'
#'
#' @param dbhandle The emuR database handle of the database.
#' @param Excelfile The path to a properly formated Excel (.xlsx) file.
#'
#' @return A vector of 'meta_json' files updated by the call. The path for each file is given relative to the base of the emuR database.
#' @export
#'
import_metadata <- function(dbhandle,Excelfile){
  if(!file.exists(Excelfile)){
    stop("Unable to open the metadata Excel file.\nThe file ",filename," does not exist!")
  }
  openxlsx::read.xlsx(Excelfile,sheet="bundles") -> meta

  #Make sure we have an output file
  meta <- meta %>%
    mutate(metadatafile=file.path(dbhandle$basePath,
                                  paste0(session,emuR:::session.suffix),
                                  paste0(bundle,emuR:::bundle.dir.suffix),paste0(bundle,".",metadata.extension)))
  #Now to the main business of the function

  json <- meta %>%
    select(-session,-bundle,-metadatafile) %>% #These are removed as they should not me enoded in the metadata file
    mutate(json=jsonlite::toJSON(.,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")) %>%
    select(json)
  json <- c()
  for(r in 1:nrow(meta)){
    meta %>%
      slice(r) %>%
      select(-session,-bundle,-metadatafile) %>%
      select_if(function(x) !is.na(x)) -> jsondat
      currJSON <- ifelse(length(jsondat) > 0,
                         jsonlite::toJSON(jsondat,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows"),
                         "[{}]" #Just an empty JSON vector
                         )
      json <- c(json, currJSON)
  }

  towrite <- meta %>%
    bind_cols(json) %>%
    select(json)
  #Write the bundle metadata files
  for(r in 1:nrow(towrite)){
    fileConn <- file(towrite[r,"metadatafile"])
    writeLines(towrite[r,"json"], fileConn)
    close(fileConn)
  }
  bFiles <- gsub(paste0(dbhandle$basePath,"/"),"",towrite[["metadatafile"]])

  ## Now process session metadata files

  openxlsx::read.xlsx(Excelfile,sheet="sessions") -> sessionMeta
  sessjsondat <- sessionMeta %>%
    select(-session,-session_metadata_file)
  json <- sessjsondat %>%
    rowwise() %>%
    do(json=jsonlite::toJSON(.,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")) %>%
    unlist()
  return(json)

  towriteSess <- sessionMeta %>%
    bind_cols(json) %>%
    select(session,session_metadata_file,json)

  #Write the bundle metadata files
  for(r in 1:nrow(towriteSess)){
    outFile <- file.path(dbhandle$basePath,
                         paste0(towriteSess[r,"session"],emuR:::session.suffix),
                         towriteSess[r,"session_metadata_file"])
    fileConn <- file(outFile)
    writeLines(towrite[r,"json"], fileConn)
    close(fileConn)
    outFile <- gsub(paste0(dbhandle$basePath,"/"),"",outFile)
    sFiles <- ifelse(exists("sFiles"),c(sFiles,outFile),c(outFile))
  }

  # Now inject database wide metadata
  emuR:::load_DBconfig(dbhandle) -> dbCfg
  openxlsx::read.xlsx(Excelfile,sheet="database") -> dbMeta
  dbCfg$metadataDefaults <- as.list(dbMeta)
  emuR:::store_DBconfig(dbhandle,dbCfg)

  return(c(sFiles,bFiles))
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
#' @param segs_tbl The \code{\link[dplyr]{tibble}} that is the result \code{\link[emuR]{query}} call.
#' @param emudb_hdl A \code{\link{emuR}} database handle.
#' @param compute_digests Should information that describes the recorded sound files be computed so that is is definitelly part of the
#' added metadata information.
#' @param algorithm The checksum algorithm that should be used when computing sound file information.
#'
#' @return A \code{\link[dplyr]{tibble}}
#' @export
#'
#' @examples
#' \dontrun{
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
