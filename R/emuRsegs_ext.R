as.emuRsegs.tibble <- function(tbl){
  requiredNames <- c("labels", "start", "end", "utts", "db_uuid", "session", "bundle",
                     "start_item_id", "end_item_id", "level", "start_item_seq_idx",
                     "end_item_seq_idx", "type", "sample_start", "sample_end", "sample_rate")
  if(!all(requiredNames %in% names(tbl))){
    stop("Could not convert the tibble to an emuRsegs object, since it does not contain all needed columns: \n\"",paste(wantedNames,collapse = "\", \""),"\"")
  }
  out <- as_data_frame(patakas)
  attr(out,"query") <- attr(tbl,"query")
  attr(out,"database") <- attr(tbl,"database")
  attr(out,"type") <- attr(tbl,"type")
  class(out) <- c("emuRsegs","emusegs","data.frame")
  return(out)
}

is.emuRsegs <- function(segs){
  classesOK <- all(class(segs) %in% c("emuRsegs","emusegs","data.frame") )
  namesOK <- (!is.null(names(segs))) & all(names(segs) %in% c("labels", "start", "end", "utts", "db_uuid", "session", "bundle",
                                                              "start_item_id", "end_item_id", "level", "start_item_seq_idx",
                                                              "end_item_seq_idx", "type", "sample_start", "sample_end", "sample_rate"))
  attrOK <- (!is.null(attr(segs,"database"))) & (!is.null(attr(segs,"type"))) & (!is.null(attr(segs,"query")))
  return( classesOK & namesOK & attrOK)
}


#' Add additional information to the (eumRsegs) results of a query
#'
#' This function may be used to attach additional information to an \code{\link{emuRsegs}}
#' object. The additional information comes from a second \code{\link{emuRsegs}}
#' containing annotations that are ancestors of labels in the first \code{\link{emuRsegs}} object. That is, the ancestors should span the decendent labels in time. The labels
#' that add information could come for instance from a \code{\link{requery_hier}} call
#' but could also be the result of an entierly separate query.
#'
#' It is important to note that the information in the two \code{\link{emuRsegs}}
#' objects are merged by start and stop time, and not by hierarchy.
#'
#'
#' @param dbHandle
#' @param segs
#' @param parentSegs
#' @param resultType
#'
#' @return
#' @export
#'
#' @examples
#'
augment.emuRsegs <- function(dbHandle,segs,parentSegs,resultType="tibble"){
  if(is.emuRsegs(segs)){
    # object segs is not a tibble yet, but is an emuRsegs object
    segs <- emuR:::convert_queryEmuRsegsToTibble(dbHandle,segs)
  }
  if(is.emuRsegs(parentSegs)){
    # object contaning the parent segments is not a tibble yet, but is an emuRsegs object
    parentSegs <- emuR:::convert_queryEmuRsegsToTibble(dbHandle,parentSegs)
  }
  out <- sqldf("select s.*, p.labels as parent_labels, p.start_item_id as parent_start_item_id, p.level as parent_level, p. attribute as parent_attribute, p.start_item_seq_idx as parent_start_item_seq_idx, p.end_item_seq_idx as parent_end_item_seq_idx, p.type as parent_type, p.sample_start as parent_sample_start, p.sample_end as parent_sample_end from segs as s,parentSegs as p where p.db_uuid == s.db_uuid and s.session == p.session and s.bundle== p.bundle and s.sample_start >= p.sample_start and s.sample_end <= p.sample_end")

  return(out)
}
