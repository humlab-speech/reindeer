
#`[.emuDBhandle` <- function(emuDBhandle,what="sessions"){

method(`[`, "emuDBhandle") <- function(emuDBhandle,what="sessions"){
  if(is.null(what)) what <- "sessions"
  if(! what %in% c("sessions","bundles"))
  switch(what,
         "bundles" = reindeer::list_bundles(emuDBhandle),
         reindeer::list_sessions(emuDBhandle)
         )

}

names.emuDBhande <- function(emuDBhandle){
  reindeer::list_sessions(emuDBhandle)
}
