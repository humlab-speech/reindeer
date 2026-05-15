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

  # Simulation S3 methods moved to the erodex companion package; no registration here.

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

  # Auto-collect methods for lazy_segment_list: any data-bearing access
  # triggers materialisation and delegates to the resulting segment_list.
  ns <- asNamespace(pkgname)
  registerS3method("dim",    "reindeer::lazy_segment_list", .lazy_dim,           envir = ns)
  registerS3method("length", "reindeer::lazy_segment_list", .lazy_length,        envir = ns)
  registerS3method("names",  "reindeer::lazy_segment_list", .lazy_names,         envir = ns)
  registerS3method("[",      "reindeer::lazy_segment_list", .lazy_bracket,       envir = ns)
  registerS3method("[[",     "reindeer::lazy_segment_list", .lazy_double_bracket,envir = ns)
  registerS3method("$",      "reindeer::lazy_segment_list", .lazy_dollar,        envir = ns)
  registerS3method("head",   "reindeer::lazy_segment_list", .lazy_head,          envir = ns)
  registerS3method("tail",   "reindeer::lazy_segment_list", .lazy_tail,          envir = ns)
  registerS3method("as.data.frame", "reindeer::lazy_segment_list",
                   .lazy_as_data_frame, envir = ns)
  if (requireNamespace("tibble", quietly = TRUE)) {
    registerS3method("as_tibble", "reindeer::lazy_segment_list",
                     .lazy_as_tibble, envir = asNamespace("tibble"))
  }

  # dplyr verbs on lazy_segment_list: collect-and-delegate
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dn <- asNamespace("dplyr")
    cls <- "reindeer::lazy_segment_list"
    registerS3method("filter",     cls, .lazy_dplyr_filter,    envir = dn)
    registerS3method("mutate",     cls, .lazy_dplyr_mutate,    envir = dn)
    registerS3method("select",     cls, .lazy_dplyr_select,    envir = dn)
    registerS3method("arrange",    cls, .lazy_dplyr_arrange,   envir = dn)
    registerS3method("slice",      cls, .lazy_dplyr_slice,     envir = dn)
    registerS3method("rename",     cls, .lazy_dplyr_rename,    envir = dn)
    registerS3method("distinct",   cls, .lazy_dplyr_distinct,  envir = dn)
    registerS3method("transmute",  cls, .lazy_dplyr_transmute, envir = dn)
    registerS3method("group_by",   cls, .lazy_dplyr_group_by,  envir = dn)
    registerS3method("ungroup",    cls, .lazy_dplyr_ungroup,   envir = dn)
    registerS3method("summarise",  cls, .lazy_dplyr_summarise, envir = dn)
    registerS3method("summarize",  cls, .lazy_dplyr_summarise, envir = dn)
    registerS3method("count",      cls, .lazy_dplyr_count,     envir = dn)
    registerS3method("tally",      cls, .lazy_dplyr_tally,     envir = dn)
    registerS3method("left_join",  cls, .lazy_dplyr_left_join,  envir = dn)
    registerS3method("right_join", cls, .lazy_dplyr_right_join, envir = dn)
    registerS3method("inner_join", cls, .lazy_dplyr_inner_join, envir = dn)
    registerS3method("full_join",  cls, .lazy_dplyr_full_join,  envir = dn)
    registerS3method("anti_join",  cls, .lazy_dplyr_anti_join,  envir = dn)
    registerS3method("semi_join",  cls, .lazy_dplyr_semi_join,  envir = dn)
  }

  # ggplot2 is in Suggests; register autoplot S3 methods only if installed.
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gn <- asNamespace("ggplot2")
    registerS3method("autoplot", "reindeer::segment_list",
                     .autoplot_segment_list, envir = gn)
    registerS3method("autoplot", "reindeer::extended_segment_list",
                     .autoplot_extended_segment_list, envir = gn)
  }

  # dplyr is in Suggests; register reconstruct hooks only if available so
  # filter()/mutate()/select()/arrange() preserve segment_list class + props.
  if (requireNamespace("dplyr", quietly = TRUE)) {
    registerS3method("dplyr_reconstruct", "reindeer::segment_list",
                     .dplyr_reconstruct_segment_list, envir = asNamespace("dplyr"))
    registerS3method("dplyr_reconstruct", "reindeer::extended_segment_list",
                     .dplyr_reconstruct_extended_segment_list, envir = asNamespace("dplyr"))

    # *_join methods that record a join-specific provenance step (verb name
    # in {left,right,inner,full,anti,semi}_join) and fire the loss warning
    # so silent row-drops during joins become visible.
    dn <- asNamespace("dplyr")
    cls <- "reindeer::segment_list"
    registerS3method("left_join",  cls, .left_join_segment_list,  envir = dn)
    registerS3method("right_join", cls, .right_join_segment_list, envir = dn)
    registerS3method("inner_join", cls, .inner_join_segment_list, envir = dn)
    registerS3method("full_join",  cls, .full_join_segment_list,  envir = dn)
    registerS3method("anti_join",  cls, .anti_join_segment_list,  envir = dn)
    registerS3method("semi_join",  cls, .semi_join_segment_list,  envir = dn)
  }

  invisible()
}
