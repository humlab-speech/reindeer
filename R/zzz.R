# ==============================================================================
# PACKAGE INITIALIZATION
# ==============================================================================

.onLoad <- function(libname, pkgname) {
  # Register S7 methods for corpus class
  # Must be done in .onLoad to ensure they're available after package installation

  # Import method assignment operator from S7
  S7::method(print, corpus) <- .print_corpus
  S7::method(summary, corpus) <- .summary_corpus
  S7::method(`[`, corpus) <- .subset_corpus

  # Register S3 print/summary methods for simulation classes

  # devtools::load_all() doesn't always register S3 methods in the correct

  # method table for base generics — explicit registration ensures dispatch
  registerS3method("print", "simulation_results", print.simulation_results, envir = asNamespace(pkgname))
  registerS3method("print", "simulation_tracks", print.simulation_tracks, envir = asNamespace(pkgname))
  registerS3method("summary", "simulation_results", summary.simulation_results, envir = asNamespace(pkgname))

  invisible()
}
