

emu_ae <- function(verbose=FALSE){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    utils::untar(system.file("extdata","ae.tar.xz",package="reindeer"),exdir=demodir)
  }
  db <- reindeer::corpus(file.path(demodir,"ae_emuDB"),verbose=verbose)
  return(db)
}

ae <- function(verbose=FALSE){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    utils::untar(system.file("extdata","aeflac.tar.xz",package="reindeer"),exdir=demodir)
  }
  db <- reindeer::corpus(file.path(demodir,"ae_emuDB"),verbose=verbose)
  return(db)
}

