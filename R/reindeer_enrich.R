#' @include segment_list_classes.R reindeer_lazy_segment_list.R corpus_class.R
NULL

#' Add DSP tracks or metadata to a corpus or segment list
#'
#' One verb that does the right thing depending on what you pass in.
#' Give it a `corpus` and it runs the supplied DSP function over every
#' signal file, picking age- and gender-aware parameters from the
#' speaker metadata; the corpus comes back invisibly with the new
#' track files written. Give it a `segment_list` and `with = "metadata"`
#' (the default) and it joins the corpus metadata onto the segments;
#' pass `.using = fn` to extract acoustic measurements via [quantify()].
#'
#' @param object A `corpus` (run DSP corpus-wide) or `segment_list` /
#'   `lazy_segment_list` / `extended_segment_list` (join metadata or
#'   extract DSP per segment).
#' @param .using A DSP function (typically `superassp::forest`,
#'   `superassp::ksvF0`, etc.). Required for the corpus method.
#' @param ... Extra arguments forwarded to the DSP function (e.g.
#'   `minF`, `maxF`, `nominalF1`). User values override the
#'   metadata-derived defaults.
#' @param .metadata_fields Metadata fields fed to [dsp_parameters()].
#'   Default `c("Gender", "Age")`.
#' @param .force Recompute every signal file, bypassing the persistent
#'   cache, even when a cached result exists.
#' @param .parallel,.workers Use a multi-session future plan with this
#'   many workers. Default: parallel, `detectCores() - 1`.
#' @param .use_cache,.cache_dir,.cache_format Skip signal files whose
#'   cached result (keyed on file mtime + DSP params) already exists.
#'   Default `TRUE`. See [inspect_cache()].
#' @param .signal_extension Override the corpus' `mediafileExtension`.
#' @param .verbose Print progress. Default `TRUE`.
#' @return The corpus invisibly (corpus method), or for the segment
#'   methods:
#'   * **Metadata join** (default, `with = "metadata"` or passing a
#'     corpus): the input `segment_list` with one extra column for every
#'     metadata field defined anywhere in the corpus, named after the
#'     field. Inheritance is resolved (bundle > session > database)
#'     before the join — see [get_metadata()] for the field set.
#'   * **DSP** (`.using = fn`): an [extended_segment_list] equivalent to
#'     [quantify()]'s output — input columns plus one column per DSP
#'     output (e.g. `F1`, `F2`, `F3` for `superassp::forest`).
#' @family signal
#' @seealso [quantify()], [get_metadata()], [dsp_parameters()],
#'   [inspect_cache()]
#' @examplesIf interactive()
#' corp <- demo_corpus()
#'
#' # Corpus-wide formants, age/gender-aware parameters
#' enrich(corp, .using = superassp::forest)
#'
#' # Per-segment metadata join
#' segs <- query(corp, "Phonetic =~ [aeiou]", lazy = FALSE)
#' enrich(segs, corp)                              # metadata join
#' enrich(segs, .using = superassp::forest)        # delegates to quantify()
#' @usage
#' enrich(object, .using = NULL, ..., .metadata_fields = NULL, .force = FALSE,
#'   .parallel = TRUE, .workers = NULL, .use_cache = TRUE, .cache_dir = NULL,
#'   .cache_format = c("auto", "qs", "rds"), .signal_extension = NULL,
#'   .verbose = FALSE)
#' @export
enrich <- S7::new_generic("enrich", "object")

#' Enrich corpus method - corpus-level DSP processing
#'
#' Apply a DSP function across every signal file in the corpus, using
#' age/gender-aware parameters derived from per-bundle metadata.
#'
#' @param object A corpus object.
#' @param .using A DSP function from superassp (e.g. forest, ksvF0).
#' @param ... Forwarded to the DSP function as user-supplied parameters.
#' @param .metadata_fields Metadata fields fed to [derive_dsp_parameters()].
#' @param .signal_extension Override the corpus' mediafileExtension.
#' @param .force Force recomputation, bypassing the persistent cache.
#' @param .verbose,.parallel,.workers,.use_cache,.cache_dir,.cache_format See `?enrich`.
#' @return The corpus, invisibly.
#' @name enrich.corpus
S7::method(enrich, corpus) <- function(object, .using, ...,
                                       .metadata_fields = c("Gender", "Age"),
                                       .signal_extension = NULL,
                                       .force = FALSE,
                                       .verbose = TRUE,
                                       .parallel = TRUE,
                                       .workers = NULL,
                                       .use_cache = TRUE,
                                       .cache_dir = NULL,
                                       .cache_format = c("auto", "qs", "rds")) {
  dsp_fun_name <- deparse(substitute(.using))
  .enrich_corpus_impl(
    corpus_obj = object,
    .using = .using,
    dsp_fun_name = dsp_fun_name,
    user_params = list(...),
    .metadata_fields = .metadata_fields,
    .signal_extension = .signal_extension,
    .force = .force,
    .verbose = .verbose,
    .parallel = .parallel,
    .workers = .workers,
    .use_cache = .use_cache,
    .cache_dir = .cache_dir,
    .cache_format = .cache_format
  )
}

#' Enrich segment_list method - join metadata or delegate DSP to quantify
#'
#' `enrich(segs, corp, with = "metadata")` joins corpus metadata onto the
#' segment_list (folded in from the former `biographize()` entry point).
#' `enrich(segs, .using = fn)` delegates to [quantify()] for segment-level
#' DSP extraction so users only need to learn one verb.
#'
#' @param object A segment_list (or extended_segment_list).
#' @param corpus_obj The corpus to pull metadata from (required when
#'   `with = "metadata"`).
#' @param ... Forwarded to [quantify()] when `.using` is supplied.
#' @param with One of `"metadata"` (default) or `NULL`. Set to `NULL` when
#'   only running DSP via `.using`.
#' @param .using Optional DSP function; if supplied, delegates to [quantify()].
#' @return A segment_list (metadata-joined) or extended_segment_list (DSP).
#' @name enrich.segment_list
S7::method(enrich, segment_list) <- function(object, corpus_obj = NULL, ...,
                                              with = "metadata",
                                              .using = NULL) {
  if (!is.null(.using)) {
    return(quantify(object, .using, ...))
  }
  if (!identical(with, "metadata")) {
    cli::cli_abort(c(
      "Cannot enrich a segment_list without {.arg with = \"metadata\"}",
      "i" = "For DSP enrichment use {.code enrich(corpus, .using = ...)} or pass {.arg .using}."
    ))
  }
  if (is.null(corpus_obj)) {
    cli::cli_abort("{.arg corpus_obj} is required when {.code with = \"metadata\"}.")
  }
  biographize(object, corpus_obj)
}

#' Enrich method for extended_segment_list
#'
#' Same semantics as the segment_list method: metadata join by default,
#' DSP delegation via `.using`.
#'
#' @inheritParams enrich.segment_list
#' @return A segment_list or extended_segment_list.
#' @name enrich.extended_segment_list
S7::method(enrich, extended_segment_list) <- function(object, corpus_obj = NULL, ...,
                                                       with = "metadata",
                                                       .using = NULL) {
  if (!is.null(.using)) {
    return(quantify(object, .using, ...))
  }
  if (!identical(with, "metadata")) {
    cli::cli_abort(c(
      "Cannot enrich an extended_segment_list without {.arg with = \"metadata\"}",
      "i" = "For DSP enrichment pass {.arg .using}."
    ))
  }
  if (is.null(corpus_obj)) {
    cli::cli_abort("{.arg corpus_obj} is required when {.code with = \"metadata\"}.")
  }
  biographize(object, corpus_obj)
}

#' Enrich method for lazy_segment_list — stay lazy until collect()
#'
#' Defers rather than materialising: the DSP path reuses the deferring
#' [quantify()] lazy method, and the metadata path defers via
#' [biographize()]. Either way the chain remains a `lazy_segment_list`
#' until [collect()] is called.
#'
#' @param object A lazy_segment_list.
#' @param corpus_obj Corpus to pull metadata from (metadata path).
#' @param ... Forwarded to [quantify()] on the DSP path.
#' @param with One of `"metadata"` (default) or `NULL`.
#' @param .using Optional DSP function; delegates to [quantify()].
#' @return The same `lazy_segment_list` with a deferred step appended.
#' @name enrich.lazy_segment_list
S7::method(enrich, lazy_segment_list) <- function(object, corpus_obj = NULL, ...,
                                                  with = "metadata",
                                                  .using = NULL) {
  if (!is.null(.using)) {
    return(quantify(object, .using, ...))
  }
  if (!identical(with, "metadata")) {
    cli::cli_abort(c(
      "Cannot enrich a lazy_segment_list without {.arg with = \"metadata\"}",
      "i" = "For DSP enrichment pass {.arg .using}."
    ))
  }
  if (is.null(corpus_obj)) {
    cli::cli_abort("{.arg corpus_obj} is required when {.code with = \"metadata\"}.")
  }
  biographize(object, corpus_obj)
}

# Internal implementation of corpus-level DSP enrichment.
# Split out so the S7 method body stays small and the implementation can
# be unit-tested independently of the dispatcher.
.enrich_corpus_impl <- function(corpus_obj, .using, dsp_fun_name, user_params,
                                .metadata_fields, .signal_extension,
                                .force, .verbose, .parallel, .workers,
                                .use_cache, .cache_dir, .cache_format) {
  .cache_format <- match.arg(.cache_format,
                             choices = c("auto", "qs", "rds"))

  if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
    cli::cli_abort("{.arg corpus_obj} must be a corpus object")
  }

  if (is.function(.using)) {
    dsp_fun <- .using
  } else {
    cli::cli_abort("{.arg .using} must be a function")
  }
  
  # Determine signal file extension
  if (is.null(.signal_extension)) {
    .signal_extension <- corpus_obj@config$mediafileExtension
    if (is.null(.signal_extension)) {
      .signal_extension <- "wav"
      if (.verbose) {
        cli::cli_alert_info("Using default extension: {.val wav}")
      }
    }
  }
  
  if (.verbose) {
    cli::cli_h2("Enriching corpus with {.fn {dsp_fun_name}}")
    cli::cli_alert_info("Processing {.val {.signal_extension}} files")
  }
  
  # Get all signal files
  signal_files <- peek_signals(corpus_obj)
  signal_files <- signal_files[signal_files$extension == .signal_extension, ]
  
  if (nrow(signal_files) == 0) {
    cli::cli_alert_warning("No signal files found with extension {.val {.signal_extension}}")
    return(invisible(corpus_obj))
  }
  
  if (.verbose) {
    cli::cli_alert_success("Found {nrow(signal_files)} signal file{?s}")
  }
  
  # Get metadata for all bundles - optimized query to only fetch needed bundles
  con <- get_corpus_connection(corpus_obj)
  db_uuid <- corpus_obj@.uuid
  
  # Only fetch metadata for bundles we're actually processing
  needed_bundles <- unique(paste(signal_files$session, signal_files$bundle, sep = "||"))
  
  # More efficient: query only the metadata we need using parameterized IN clause
  placeholders <- paste(rep("?", length(needed_bundles)), collapse = ", ")
  bundle_metadata_query <- sprintf(
    "SELECT session, bundle, field_name, field_value FROM metadata_bundle WHERE db_uuid = ? AND (session || '||' || bundle) IN (%s)",
    placeholders
  )
  
  # Fallback to simpler approach if query fails
  bundle_metadata_long <- tryCatch({
    DBI::dbGetQuery(con, bundle_metadata_query, params = c(list(db_uuid), as.list(needed_bundles)))
  }, error = function(e) {
    # Fallback: get all and filter in R
    all_meta <- DBI::dbReadTable(con, "metadata_bundle")
    all_meta <- all_meta[all_meta$db_uuid == db_uuid, ]
    sf_keys <- paste(signal_files$session, signal_files$bundle, sep = "||")
    meta_keys <- paste(all_meta$session, all_meta$bundle, sep = "||")
    all_meta[meta_keys %in% sf_keys, c("session", "bundle", "field_name", "field_value")]
  })
  
  # Pivot long-form metadata to wide
  if (nrow(bundle_metadata_long) > 0) {
    dt_meta <- data.table::as.data.table(bundle_metadata_long)
    bundle_metadata <- data.table::dcast(dt_meta, session + bundle ~ field_name,
                                         value.var = "field_value")
    bundle_metadata <- as.data.frame(bundle_metadata, stringsAsFactors = FALSE)
  } else {
    bundle_metadata <- data.frame(session = character(), bundle = character(),
                                  stringsAsFactors = FALSE)
  }

  # Pre-join metadata with signal files for efficiency
  signal_files_with_meta <- merge(signal_files, bundle_metadata, by = c("session", "bundle"), all.x = TRUE)
  
  # Determine number of workers
  if (.parallel) {
    if (is.null(.workers)) {
      .workers <- max(1, parallel::detectCores() - 1)
    }
    
    if (.verbose) {
      cli::cli_alert_info("Using parallel processing with {.workers} worker{?s}")
    }
    
    # Set up parallel processing
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = .workers)
  }
  
  # Process bundles
  if (.verbose) {
    cli::cli_progress_bar("Processing bundles", total = nrow(signal_files_with_meta))
  }
  
  # Setup persistent cache if requested
  cache_conn <- NULL
  if (.use_cache) {
    resolved_cache_dir <- .cache_dir %||% corpus_obj@.cache_dir
    cache_conn <- .get_persistent_cache_connection(resolved_cache_dir, verbose = .verbose)
  }

  # Define processing function
  process_bundle <- function(i, signal_files_with_meta, dsp_fun,
                             metadata_fields, user_params, verbose = FALSE,
                             cache_conn = NULL, cache_format = "auto") {
    bundle_row <- signal_files_with_meta[i, ]

    # Derive DSP parameters from metadata
    dsp_params <- derive_dsp_parameters(
      dsp_fun = dsp_fun,
      metadata = bundle_row,
      metadata_fields = metadata_fields,
      user_params = user_params
    )

    # Check cache if enabled. `.force` bypasses the read so every bundle is
    # recomputed; the recomputed result still overwrites the cached entry.
    if (!is.null(cache_conn)) {
      cache_key <- digest::digest(list(
        bundle_row$full_path,
        file.info(bundle_row$full_path)$mtime,
        dsp_params
      ))
      if (!.force) {
        cached <- .get_persistent_cache(cache_key, cache_conn)
        if (!is.null(cached)) {
          return(list(success = TRUE, bundle = bundle_row$bundle,
                      session = bundle_row$session, cached = TRUE))
        }
      }
    }

    # Apply DSP function
    tryCatch({
      result <- do.call(dsp_fun, c(
        list(listOfFiles = bundle_row$full_path),
        dsp_params,
        list(toFile = TRUE, verbose = FALSE)
      ))

      # Store in cache if enabled
      if (!is.null(cache_conn)) {
        .set_persistent_cache(cache_key, TRUE, cache_conn, format = cache_format)
      }

      list(success = TRUE, bundle = bundle_row$bundle, session = bundle_row$session)
    }, error = function(e) {
      list(success = FALSE, bundle = bundle_row$bundle, session = bundle_row$session,
           error = e$message)
    })
  }
  
  # Execute processing (parallel or sequential)
  if (.parallel) {
    results <- furrr::future_map(
      seq_len(nrow(signal_files_with_meta)),
      process_bundle,
      signal_files_with_meta = signal_files_with_meta,
      dsp_fun = dsp_fun,
      metadata_fields = .metadata_fields,
      user_params = user_params,
      verbose = FALSE,
      cache_conn = cache_conn,
      cache_format = .cache_format,
      .progress = .verbose,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    results <- list()
    for (i in seq_len(nrow(signal_files_with_meta))) {
      results[[i]] <- process_bundle(
        i, signal_files_with_meta, dsp_fun,
        .metadata_fields, user_params, FALSE,
        cache_conn = cache_conn, cache_format = .cache_format
      )
      if (.verbose) {
        cli::cli_progress_update()
      }
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
    
    # Report any errors
    errors <- Filter(function(x) !x$success, results)
    if (length(errors) > 0) {
      cli::cli_alert_warning("{length(errors)} bundle{?s} failed processing")
      for (err in errors) {
        cli::cli_alert_info("{err$session}/{err$bundle}: {err$error}")
      }
    }
    
    cli::cli_alert_success("Enrichment complete")
  }
  
  invisible(corpus_obj)
}

#' Derive DSP parameters from bundle metadata
#'
#' Internal helper. End users should call [dsp_parameters()] instead;
#' kept exported for companion packages (`erodex`) that share the
#' age/gender derivation logic.
#'
#' Age/Gender are resolved to literature-derived, LOESS-smoothed norms
#' from the internal `DSPP` table (via `dspp_metadataParameters_dt()`),
#' matched to the DSP function's formal arguments. This is the same
#' lookup [dsp_parameters()] previews, so the preview equals what is
#' actually applied.
#'
#' @param dsp_fun A DSP function whose formal arguments are matched against
#'   the resolved norms.
#' @param metadata Named list of bundle metadata (expects `Age`, `Gender`).
#' @param metadata_fields Character vector of extra metadata field names to
#'   map straight onto matching formals of `dsp_fun`.
#' @param user_params Named list of user overrides (win over derived norms).
#' @return A named list of DSP parameters to pass to `dsp_fun`.
#' @keywords internal
#' @export
derive_dsp_parameters <- function(dsp_fun, metadata, metadata_fields, user_params) {

  # Get formal arguments of DSP function
  fun_formals <- names(formals(dsp_fun))

  # Start with empty parameter list
  params <- list()

  # Extract metadata values
  meta_list <- as.list(metadata)

  # Age/Gender -> literature-derived DSP norms from the DSPP table.
  # dsp_parameters() previews exactly this; both route through the same
  # .lookup_dspp_row()/.normalize_gender() helpers so they cannot diverge.
  if ("Gender" %in% names(meta_list) && "Age" %in% names(meta_list)) {
    age    <- suppressWarnings(as.numeric(meta_list$Age))
    gender <- .normalize_gender(meta_list$Gender)

    if (!is.na(age) && !is.na(gender)) {
      dspp <- tibble::as_tibble(dspp_metadataParameters_dt())
      row  <- .lookup_dspp_row(dspp, age, gender)

      if (nrow(row) == 1L) {
        # Pull every DSPP norm column the DSP function actually accepts.
        norm_cols <- setdiff(intersect(names(row), fun_formals), c("Age", "Gender"))
        for (col in norm_cols) {
          if (!is.na(row[[col]])) params[[col]] <- row[[col]]
        }
      } else {
        cli::cli_warn(
          c("No DSPP norm row for Age {age}, Gender {gender}; using DSP defaults.",
            i = 'Preview with {.code dsp_parameters(age = {age}, gender = "{gender}")}.'),
          class = c("reindeer_metadata_warning", "reindeer_warning"))
      }
    }
  }

  # Any remaining requested metadata fields map straight onto matching formals.
  for (field in metadata_fields) {
    if (field %in% names(meta_list) && field %in% fun_formals) {
      params[[field]] <- meta_list[[field]]
    }
  }

  # Merge with user params (user params override)
  utils::modifyList(params, user_params)
}

#' Derive DSP params per bundle
#'
#' Applies [derive_dsp_parameters()] one bundle at a time. The underlying
#' function has a single-row contract; passing a whole metadata table
#' recycles vectorised `Age`/`Gender` against the DSPP table and returns
#' an arbitrary row. This wrapper resolves one params list per
#' (session, bundle) and returns a tibble with a `dsp_params` list-column.
#'
#' @param metadata A tibble with `session`, `bundle`, `Age`, `Gender`
#'   columns (one row per bundle).
#' @return Tibble with `session`, `bundle`, and a `dsp_params` list-column
#'   whose entries are the fully merged parameter lists (user overrides
#'   already applied).
#' @noRd
.derive_dsp_params_per_bundle <- function(dsp_fun, metadata, metadata_fields,
                                          user_params) {
  n <- nrow(metadata)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- derive_dsp_parameters(
      dsp_fun = dsp_fun,
      metadata = metadata[i, , drop = FALSE],
      metadata_fields = metadata_fields,
      user_params = user_params
    )
  }
  tibble::tibble(
    session = metadata$session,
    bundle = metadata$bundle,
    dsp_params = out
  )
}

