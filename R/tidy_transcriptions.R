
suggest <- function(...) {
  UseMethod("suggest")
}

suggest.character <- function(.inside_of,.algorithm,...){
  if(missing(.inside_of)) cli::cli_abort("Missing manditory argument {.arg .inside_of}.")
  if(! file.info(path.expand(.inside_of))[["isdir"]] || ! dir.exists(path.expand(.inside_of))){
    cli::cli_abort(c("The {.arg .inside_of} is not a valid path.",
                     "i"="The database path searched was {.path {path.expand(.inside_of)}}"))
  }
  if(! stringr::str_ends(basename(path.expand(.inside_of)), emuR:::emuDB.suffix)){
    cli::cli_abort(c("The {.arg .inside_of} does not seem to point to the location of a database",
                     "x"="The database directory should end with {.val {emuR:::emuDB.suffix}}.",
                     "i"="The database path searched was {.path {path.expand(.inside_of)}}"))
  }
  utils::capture.output(
    .inside_of <- emuR::load_emuDB(attr(.inside_of,"basePath"),verbose = FALSE)
  ) -> dbload.info
  logger::log_info(paste(dbload.info,collapse = "\n"))
  #Now of class emuDBhandle
  suggest(.inside_of,.algorithm,...)
}

suggest.emuDBhandle <- function(.inside_of,.algorithm, ... ){

  # Check inputs ------------------------------------------------------------

  if(missing(.algorithm)) {
    cli::cli_abort(c("Missing annotation algoritm",
                     "i"="You need to state a function that computes the suggested transcriptions.",
                     "x"="The argument {.var .algorithm} is missing"))
  }


}


suggest.data.frame <- function(.what,...,.inside_of){
  mandatory <- c("labels", "start", "end", "db_uuid", "session", "bundle", "start_item_id",
                 "end_item_id", "level", "attribute", "start_item_seq_idx", "end_item_seq_idx",
                 "type", "sample_start", "sample_end", "sample_rate")
  if(! base::setequal(mandatory,names(.what))) {
    cli::cli_abort(c("The {.arg .what} does not contain the columns required to make a {.cls segmentlist}.",
                     "i"="Missing field{?s} {.val {base::setdiff(mandatory,names(.what))}}."))
  }
  attr(.what,"basePath") <- .inside_of$basePath
  class(.what) <- c("segmentlist",class(.what) )
  suggest(.what,...)
}
