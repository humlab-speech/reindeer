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

  # vctrs hooks — needed so vec_slice/dplyr operations don't reject S7 objects
  # as "scalars". tibble (Imports) brings vctrs as a transitive dep.
  if (requireNamespace("vctrs", quietly = TRUE)) {
    registerS3method("vec_proxy", "reindeer::segment_list",
                     .vec_proxy_segment_list, envir = asNamespace("vctrs"))
    registerS3method("vec_restore", "reindeer::segment_list",
                     .vec_restore_segment_list, envir = asNamespace("vctrs"))
    registerS3method("vec_proxy", "reindeer::extended_segment_list",
                     .vec_proxy_extended_segment_list, envir = asNamespace("vctrs"))
    registerS3method("vec_restore", "reindeer::extended_segment_list",
                     .vec_restore_extended_segment_list, envir = asNamespace("vctrs"))
  }

  # Bracket subsetting — enforces required-cols downcast for select-like ops
  registerS3method("[", "reindeer::segment_list",
                   .bracket_segment_list, envir = asNamespace(pkgname))
  registerS3method("[", "reindeer::extended_segment_list",
                   .bracket_extended_segment_list, envir = asNamespace(pkgname))

  # dplyr is in Suggests; register reconstruct hooks only if available so
  # filter()/mutate()/select()/arrange() preserve segment_list class + props.
  if (requireNamespace("dplyr", quietly = TRUE)) {
    registerS3method("dplyr_reconstruct", "reindeer::segment_list",
                     .dplyr_reconstruct_segment_list, envir = asNamespace("dplyr"))
    registerS3method("dplyr_reconstruct", "reindeer::extended_segment_list",
                     .dplyr_reconstruct_extended_segment_list, envir = asNamespace("dplyr"))
  }

  invisible()
}
