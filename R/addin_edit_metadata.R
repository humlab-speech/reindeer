# ==============================================================================
# RStudio addin: metadata editor gadget
# ==============================================================================
#
# A miniUI gadget that exposes session- or bundle-level metadata as an
# editable DT::datatable. On accept, the diff is computed against the
# pre-edit snapshot and round-tripped through add_metadata() so the
# METADATA.json files on disk stay authoritative.
#
# .metadata_diff() is extracted as a non-gadget helper so its logic is
# testable without spawning a shiny app.

# Internal: snapshot a corpus' metadata at one hierarchy level as a tibble.
# Each row is one (session, bundle, field, value) tuple. Session-level rows
# leave `bundle = NA`; database-level rows leave both NA.
.metadata_snapshot <- function(corpus, level = c("bundle", "session",
                                                  "database")) {
  level <- match.arg(level)
  con <- get_corpus_connection(corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  table <- switch(level,
    database = "metadata_database",
    session  = "metadata_session",
    bundle   = "metadata_bundle"
  )
  rows <- tryCatch(
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", table)),
    error = function(e) data.frame()
  )
  tibble::as_tibble(rows)
}

# Internal: diff two snapshots. Returns a tibble of (level, session, bundle,
# field, old_value, new_value) for rows that changed, were added, or were
# removed. Used by the gadget on accept; also unit-testable in isolation.
.metadata_diff <- function(before, after,
                            level = c("bundle", "session", "database")) {
  level <- match.arg(level)
  empty <- tibble::tibble(
    level = character(), session = character(), bundle = character(),
    field = character(), old_value = character(), new_value = character()
  )
  if (NROW(before) == 0L && NROW(after) == 0L) return(empty)

  key_cols <- switch(level,
    database = "field_name",
    session  = c("session", "field_name"),
    bundle   = c("session", "bundle", "field_name")
  )

  norm <- function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    for (k in c(key_cols, "field_value")) {
      if (!k %in% names(df)) df[[k]] <- character(NROW(df))
    }
    df$.key <- do.call(paste, c(df[key_cols], sep = "||"))
    df[, c(".key", key_cols, "field_value"), drop = FALSE]
  }
  b <- norm(before)
  a <- norm(after)

  m <- merge(b, a, by = c(".key", key_cols),
              all = TRUE, suffixes = c(".old", ".new"))

  fv_old <- m$field_value.old
  fv_new <- m$field_value.new
  changed_mask <- !(
    (!is.na(fv_old) & !is.na(fv_new) & fv_old == fv_new) |
      (is.na(fv_old) & is.na(fv_new))
  )
  changed <- m[changed_mask, , drop = FALSE]

  sess <- if ("session" %in% key_cols) changed$session else NA_character_
  bndl <- if ("bundle"  %in% key_cols) changed$bundle  else NA_character_

  tibble::tibble(
    level = rep(level, nrow(changed)),
    session = sess,
    bundle = bndl,
    field = changed$field_name,
    old_value = changed$field_value.old,
    new_value = changed$field_value.new
  )
}

#' Edit session- or bundle-level metadata interactively (RStudio addin)
#'
#' Spawns a miniUI gadget with an editable `DT::datatable` showing the
#' selected metadata level. Edits are applied via [add_metadata()] when
#' the user clicks Done, so `METADATA.json` files on disk remain
#' authoritative. A diff summary is printed via [cli::cli_dl()].
#'
#' @param corpus A `corpus` object. When NULL, the gadget searches the
#'   global environment for a `corpus`.
#' @param level Either `"session"` or `"bundle"`.
#' @param filter Optional regex filter on the joined session/bundle keys.
#' @return The (possibly updated) corpus, invisibly.
#' @export
edit_metadata_gadget <- function(corpus = NULL,
                                  level = c("session", "bundle"),
                                  filter = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .companion_abort("shiny", purpose = "RStudio gadget")
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    .companion_abort("miniUI", purpose = "RStudio gadget")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    .companion_abort("DT", purpose = "interactive metadata editor")
  }
  level <- match.arg(level)
  corp <- .resolve_corpus_for_gadget(corpus)

  before <- .metadata_snapshot(corp, level = level)
  if (!is.null(filter) && nrow(before) > 0L) {
    key <- if (level == "session") before$session else
            paste(before$session, before$bundle, sep = "/")
    before <- before[grepl(filter, key), , drop = FALSE]
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(paste0("Edit metadata: ", level)),
    miniUI::miniContentPanel(
      DT::dataTableOutput("tbl")
    )
  )
  server <- function(input, output, session) {
    output$tbl <- DT::renderDataTable(
      DT::datatable(before, editable = TRUE, rownames = FALSE,
                     options = list(pageLength = 20))
    )
    edited <- shiny::reactiveVal(before)
    shiny::observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      df <- edited()
      df[info$row, info$col + 1L] <- info$value
      edited(df)
    })
    shiny::observeEvent(input$done, {
      d <- .metadata_diff(before, edited(), level = level)
      if (nrow(d) == 0L) {
        cli::cli_alert_info("No metadata changes to apply.")
      } else {
        cli::cli_alert_success("Applying {nrow(d)} metadata change{?s}.")
        for (i in seq_len(nrow(d))) {
          val <- list()
          val[[d$field[i]]] <- d$new_value[i]
          add_metadata(corp, val,
                        session = if (!is.na(d$session[i])) d$session[i] else NULL,
                        bundle  = if (!is.na(d$bundle[i]))  d$bundle[i]  else NULL)
        }
      }
      shiny::stopApp(invisible(corp))
    })
    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(corp)))
  }

  shiny::runGadget(ui, server,
                    viewer = shiny::dialogViewer("Edit metadata",
                                                   height = 600))
}
