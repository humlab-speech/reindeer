

create_ae_db <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    emuR::create_emuRdemoData()
  }
  db <- emuR::load_emuDB(file.path(demodir,"ae_emuDB"))
  return(db)
}

detach_demo_db <- function(dbHandle){
  DBI::dbDisconnect(dbHandle$connection)
  unlink_emuRDemoDir()
  return(!dir.exists(file.path(tempdir(),"emuR_demoData")))
}

unlink_emuRDemoDir <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")
  res <- unlink(demodir,recursive = TRUE)
  binRes <- c(TRUE,FALSE)[res+1]
  return(binRes)
}


make_dummy_metafiles <- function(db,metafile="test.meta"){
  outMetaFiles <- emuR::list_files(db,"wav") %>%
    dplyr::select(absolute_file_path) %>%
    dplyr::mutate(absolute_file_path=gsub("wav$","meta_json",absolute_file_path))

  res <- file.copy(from=metafile,to=outMetaFiles[[1]])
  return(res)
}
