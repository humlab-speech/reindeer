unlink_emuRDemoDir <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")
  res <- unlink(demodir,recursive = TRUE)
  binRes <- c(TRUE,FALSE)[res+1]
  return(binRes)
}



unlink_emuRDemoDir()
