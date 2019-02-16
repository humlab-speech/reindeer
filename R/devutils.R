

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


make_dummy_metafiles <- function(db,metafile="test.meta"){
  outMetaFiles <- list_files(db,"wav") %>%
    select(absolute_file_path) %>%
    mutate(absolute_file_path=gsub("wav$","meta_json",absolute_file_path))

  res <- file.copy(from=metafile,to=outMetaFiles[[1]])
  return(res)
}
