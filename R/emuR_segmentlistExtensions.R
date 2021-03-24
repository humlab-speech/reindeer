#' Add information from one segment list to another
#'
#' This function may be used to attach additional information to a segment list.
#' The additional information comes from a second segment list
#' containing annotations that are ancestors of labels in the first segment list.
#' That is, the ancestors should span the decendent labels in time. The labels
#' that add information could come for instance from a \code{\link{requery_hier}} call
#' but could also be the result of an entirely separate query.
#'
#' It is important to note that the information in the two segment lists
#'are merged by start and stop time, and not by hierarchy.
#'
#'
#' @param dbHandle An \code{emuR} database handle.
#' @param segs An \code{\link[dplyr]{tibble}} resulting from a \code{\link[emuR]{query}} call.
#' @param parentSegs An additional  \code{\link[dplyr]{tibble}} (or a legacy \code{\link[emuR]{emuRsegs}}) the result of a \code{\link[emuR]{query}}, and contains additional information that should be added to the \code{segs} argument object.

#' @param add.metadata Boolean: Should metadata associated with the bundle from which \code{segs} was returned be attached to the output "tibble" as columns? #TODO
#'
#' @return A "tibble" that has the columns of the \code{segs}, but with the
#'   columns of \code{parentSegs} appended to the right. A parent is matched
#'   with its decendant by the part of the time line of the signal that they
#'   share. That is, a label A will be on the same row as the parent P if, for
#'   instance, sample_start > parent_sample_start and sample_end <
#'   parent_sampe_end in the output. The user may therefore use this function to
#'   combine data without restructuring the entire database, or add additional
#'   data not from the database.
#'
#'   A prefix "parent_" is added to all the columns
#'   in \code{parentSegs}.
#'
#' @importFrom dplyr "%>%"
#'

#'
#' @examples
#' reindeer:::create_ae_db() -> ae
#' emuR::query(ae,"Word =~ .*") -> wd
#' emuR::query(ae,"Utterance =~ .*") -> wd
#' augment(ae,wd,utt) %>% glimpse()
#' reindeer:::detach_demo_db()
#'
#'
augment <- function(dbHandle,segs,parentSegs,add.metadata=FALSE){
  if("emusegs" %in% class(segs)){
    # object segs is not a tibble yet, but is an emuRsegs object
    segs <- emuR:::convert_queryEmuRsegsToTibble(dbHandle,segs)
  }
  if("emusegs" %in% class(parentSegs)){
    # object contaning the parent segments is not a tibble yet, but is an emuRsegs object
    parentSegs <- emuR:::convert_queryEmuRsegsToTibble(dbHandle,parentSegs)
  }
  out <- sqldf::sqldf("select s.*, p.labels as parent_labels, p.start_item_id as parent_start_item_id, p.level as parent_level, p. attribute as parent_attribute, p.start_item_seq_idx as parent_start_item_seq_idx, p.end_item_seq_idx as parent_end_item_seq_idx, p.type as parent_type, p.sample_start as parent_sample_start, p.sample_end as parent_sample_end from segs as s,parentSegs as p where p.db_uuid == s.db_uuid and s.session == p.session and s.bundle== p.bundle and s.sample_start >= p.sample_start and s.sample_end <= p.sample_end")
  out <- tibble::as_tibble(out)
  return(out)
}
