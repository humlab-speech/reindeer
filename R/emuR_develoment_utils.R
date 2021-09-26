

create_ae_db <- function(verbose=TRUE){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    emuR::create_emuRdemoData()
  }
  db <- emuR::load_emuDB(file.path(demodir,"ae_emuDB"),verbose=verbose)
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


make_dummy_metafiles <- function(db,session=TRUE){
  outMetaFiles <- emuR::list_files(db,"wav") %>%
    dplyr::select(absolute_file_path) %>%
    dplyr::mutate(absolute_file_path=gsub("wav$","meta_json",absolute_file_path))
  for(i in 1:(nrow(outMetaFiles)-1)){
    cat("{\"Participant_ID\":",i,",\"Gender\":\"",rep(c("Male","Female"),4)[i],"\",\"Age\":",i*10,",\"Recording_Date\":\"2019-01-01\",\"Recording_Time\":\"09:43:54\"}",sep="",
        file=outMetaFiles[[1]][i])

  }
  if(session){
    #Inject some session wide defaults
    ses <- list_sessions(db)[[1]]
    sessFile <- file.path(db$basePath,paste0(ses,emuR:::session.suffix),paste(ses,"meta_json",sep="."))
    cat('{"Age":35,"Shoe size":10,"Recording_Date":"2019-01-02"}',file=sessFile)
  }

}
