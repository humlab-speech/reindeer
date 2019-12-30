

create_ae_db <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    create_emuRdemoData()
  }
  db <- load_emuDB(file.path(demodir,"ae_emuDB"))
  return(db)
}


unlink_emuRDemoDir <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")
  res <- unlink(demodir,recursive = TRUE)
  binRes <- c(TRUE,FALSE)[res+1]
  return(binRes)
}


make_dummy_metafiles <- function(db,metafile=,sessionmetafile=){
  sess <- file.copy(from=file.path("tests","session.meta"),to=file.path(db$basePath,"0000_ses","0000.meta_json"))

  outMetaFiles <- list_files(db,"wav") %>%
    select(absolute_file_path) %>%
    mutate(absolute_file_path=gsub("wav$",metadata.extension,absolute_file_path)) %>%
    slice(-1) ## One file should be missing so that we may test bundle and session defaults

  res <- file.copy(from=file.path("tests","bundle.meta"),to=outMetaFiles[[1]])
  # Now make a second session, almost identical

  res <- c(sess,res)
  # store database wide default values
  emuR:::load_DBconfig(db) -> dbCfg
  dbCfg$metadataDefaults <- jsonlite::read_json(file.path("tests","db.meta"),simplifyVector = FALSE) ## Keep the list strucure since this is going directly to json
  emuR:::store_DBconfig(db,dbConfig = dbCfg)

  return(res)
}


