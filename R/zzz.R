# ==============================================================================
# PACKAGE INITIALIZATION
# ==============================================================================

.onLoad <- function(libname, pkgname) {
  # Register S7 methods for corpus class
  # Must be done in .onLoad to ensure they're available after package installation

  # Import method assignment operator from S7
  S7::method(print, corpus) <- .print_corpus
  S7::method(summary, corpus) <- .summary_corpus

  invisible()
}
