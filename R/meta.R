



get_metadata <- function(dbhandle,add.metadata=c("Session.DateTime","Speaker.ID"),overwrite=FALSE,session=".*"){
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

  return(metafiles)


}

##jsonlite::toJSON(as.data.frame(meta),raw="base64",na="string",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")
