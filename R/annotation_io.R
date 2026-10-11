# ==============================================================================
# NATIVE ANNOTATION I/O
# ==============================================================================
#
# Internal functions for reading/writing bundle annotations and bundle lists
# directly from/to the emuDBcache.sqlite and JSON files. These replace the
# emuR annotation I/O functions used in serve().
#

#' Load bundle annotation data frames from SQLite
#'
#' Retrieves items, labels, and links for a specific bundle from the
#' emuDBcache.sqlite database.
#'
#' @param con DBI connection to emuDBcache.sqlite
#' @param session Session name
#' @param bundle Bundle name
#' @return List with items, labels, links data frames
#' @keywords internal
#' @noRd
.load_bundle_annot <- function(con, session, bundle) {
  items <- DBI::dbGetQuery(con,
    "SELECT * FROM items WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  labels <- DBI::dbGetQuery(con,
    "SELECT * FROM labels WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  links <- DBI::dbGetQuery(con,
    "SELECT * FROM links WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  list(items = items, labels = labels, links = links)
}
