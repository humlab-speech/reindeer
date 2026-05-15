# ==============================================================================
# RStudio addin: corpus browser gadget
# ==============================================================================
#
# A miniUI gadget that shows a tree of sessions and bundles plus a preview
# pane with corpus summary stats. shiny + miniUI are in Suggests; the gadget
# aborts with reindeer_missing_companion_error when either is missing.

# Internal: resolve a corpus from the active selection or function argument.
# When the caller invokes the addin from RStudio, the user may not have
# passed a corpus explicitly; in that case we look for a `corpus` object in
# the global environment.
.resolve_corpus_for_gadget <- function(corpus = NULL) {
  if (!is.null(corpus)) return(corpus)
  ns <- ls(envir = globalenv())
  hits <- vapply(ns, function(n) {
    obj <- tryCatch(get(n, envir = globalenv()), error = function(e) NULL)
    S7::S7_inherits(obj, reindeer::corpus)
  }, logical(1))
  if (!any(hits)) {
    cli::cli_abort(c(
      "No corpus object found in the global environment.",
      "i" = "Create one with {.code corp <- corpus(\"path/to/db_emuDB\")} first."
    ))
  }
  get(ns[which(hits)[1]], envir = globalenv())
}

#' Browse a corpus interactively (RStudio addin)
#'
#' A miniUI gadget that displays a tree of sessions and bundles plus a
#' preview pane showing the corpus summary
#' (`collect_corpus_summary()`). Returns the corpus invisibly so the
#' addin call composes with `|>` pipelines.
#'
#' @param corpus An optional `corpus` object. When NULL, the gadget
#'   searches the global environment for the first `corpus`-class
#'   object.
#' @param height Gadget window height in pixels.
#' @return The input `corpus`, invisibly.
#' @export
browse_corpus_gadget <- function(corpus = NULL, height = 600) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    .companion_abort("shiny", purpose = "RStudio gadget")
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    .companion_abort("miniUI", purpose = "RStudio gadget")
  }
  corp <- .resolve_corpus_for_gadget(corpus)
  summary <- collect_corpus_summary(corp, verbose = FALSE)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(paste0("Browse Corpus: ", summary$name)),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(4,
          shiny::h4("Sessions / Bundles"),
          shiny::selectInput("session", "Session",
                              choices = unique(summary$bundle_list$session)),
          shiny::uiOutput("bundle_picker")
        ),
        shiny::column(8,
          shiny::h4("Corpus summary"),
          shiny::verbatimTextOutput("preview"),
          shiny::h4("Selected bundle"),
          shiny::verbatimTextOutput("bundle_info")
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$bundle_picker <- shiny::renderUI({
      bdf <- summary$bundle_list
      bs <- bdf$bundle[bdf$session == input$session]
      shiny::selectInput("bundle", "Bundle", choices = bs)
    })
    output$preview <- shiny::renderPrint({
      cat("Corpus:    ", summary$name, "\n")
      cat("UUID:      ", summary$uuid, "\n")
      cat("Sessions:  ", summary$n_sessions, "\n")
      cat("Bundles:   ", summary$n_bundles, "\n")
      cat("Levels:    ", summary$n_levels, "\n")
      cat("Tracks:    ", summary$n_ssff_tracks, "\n")
      cat("Duration:  ", summary$total_duration_hms %||% "(unknown)", "\n")
    })
    output$bundle_info <- shiny::renderPrint({
      shiny::req(input$session, input$bundle)
      meta <- tryCatch(
        get_metadata(corp, session = input$session, bundle = input$bundle),
        error = function(e) list()
      )
      if (length(meta) == 0L) {
        cat("(no bundle-level metadata)\n")
      } else {
        for (nm in names(meta)) {
          cat(sprintf("%-20s %s\n", paste0(nm, ":"),
                       paste(meta[[nm]], collapse = ", ")))
        }
      }
    })
    shiny::observeEvent(input$done, shiny::stopApp(invisible(corp)))
    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(corp)))
  }

  shiny::runGadget(ui, server,
                    viewer = shiny::dialogViewer("Browse Corpus", height = height))
}
