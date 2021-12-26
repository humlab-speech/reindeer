

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



create_extended_ae_db <- function(verbose=FALSE){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    emuR::create_emuRdemoData()
  }
  emuDBhandle <- emuR::load_emuDB(file.path(demodir,"ae_emuDB"),verbose=verbose)

  dir.create(file.path(emuDBhandle$basePath,"temp"))
  for(i in 1:10){
    file.copy(file.path(emuDBhandle$basePath,"0000_ses"),file.path(emuDBhandle$basePath,"temp"),recursive = TRUE)
    newName <- paste0(i,i,i,i,"_ses")
    file.rename(file.path(emuDBhandle$basePath,"temp","0000_ses"),file.path(emuDBhandle$basePath,"temp",newName))
    file.copy(file.path(emuDBhandle$basePath,"temp",newName),emuDBhandle$basePath,recursive = TRUE)
    unlink(file.path(emuDBhandle$basePath,"temp",newName),recursive = TRUE)
  }
  unlink(file.path(emuDBhandle$basePath,"temp"))
  return(emuDBhandle)
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

add_dummy_metadata <- function(emuDBhandle){

  metagrid <- expand.grid(Age=seq(1,76,3),Gender=c("Male","Female",NA)) %>%
    dplyr::arrange(Age) %>%
    dplyr::mutate(windowSize=20,minF=40,maxF=800,nominalF1=600)  %>%
    dplyr::slice(-1)

  bundles <- list_bundles(emuDBhandle)
  for(i in 1:nrow(bundles)){
      add_metadata(emuDBhandle,list(Age=metagrid[i,"Age"],
                                    Gender=metagrid[i,"Gender"],
                                    windowSize=metagrid[i,"windowSize"],
                                    minF=metagrid[i,"minF"],
                                    maxF=metagrid[i,"maxF"],
                                    nominalF1=metagrid[i,"nominalF1"]
                                    ),session=bundles[i,"session"],bundle=bundles[i,"name"])
  }

  for(i in seq(1,nrow(bundles),1)){
    if( i %% 13 == 0){
      add_metadata(emuDBhandle,list(Gender=NULL),session=bundles[i,"session"],bundle=bundles[i,"name"])
    }
    if( i %%  31  == 0){
      add_metadata(emuDBhandle,list(Age=NULL),session=bundles[i,"session"],bundle=bundles[i,"name"])
    }

  }
}

### INTERACTIVE TESTING
# unlink_emuRDemoDir()
# create_extended_ae_db() -> emuDBhandle
# add_dummy_metadata(emuDBhandle)
