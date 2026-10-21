#' @include corpus_class.R segment_list_quantify.R reindeer_corpus_config.R dsp_parameter_derivation.R
NULL

# ==============================================================================
# quantify.corpus: corpus-wide DSP materialization + ssffTrackDefinitions
# provenance. See specs/2026-09-24-quantify-unification-design.md.
# ==============================================================================

#' Resolve a DSP function's identity (name/package/version) for provenance
#'
#' `dsp_fun_name` is the caller's deparsed expression (e.g.
#' `"superassp::trk_formant_forest"` or `"trk_formant_forest"` if the
#' package was attached). A qualified expression is parsed directly;
#' a bare name falls back to the function's own defining namespace.
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @return List with `function`, `package` (`NA` if unresolvable),
#'   `version` (`NA` if `package` is `NA` or has no installed version).
#' @noRd
.resolve_dsp_identity <- function(dsp_fun, dsp_fun_name) {
  qualified <- regmatches(
    dsp_fun_name,
    regexec("^([[:alnum:].]+)::([[:alnum:]._]+)$", dsp_fun_name)
  )[[1]]

  if (length(qualified) == 3) {
    pkg <- qualified[2]
    fn_name <- qualified[3]
  } else {
    fn_name <- dsp_fun_name
    pkg <- tryCatch({
      env_name <- environmentName(topenv(environment(dsp_fun)))
      # Under full-suite `devtools::test()` loading (unlike a lone
      # `testthat::test_file()` run), a closure defined inline in a test
      # has this package's own namespace reachable in its environment
      # chain, so `topenv()` resolves to the package itself rather than
      # "R_GlobalEnv" - exclude it too, computed from this very function's
      # home so it stays correct if the package is ever renamed.
      own_pkg <- environmentName(topenv(environment(.resolve_dsp_identity)))
      if (nzchar(env_name) && !env_name %in% c("R_GlobalEnv", "base", own_pkg)) {
        env_name
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    if (!is.na(pkg)) {
      real_name <- tryCatch({
        exported <- getNamespaceExports(pkg)
        Find(function(nm) identical(get(nm, envir = asNamespace(pkg)), dsp_fun), exported)
      }, error = function(e) NULL)
      if (!is.null(real_name)) fn_name <- real_name
    }
  }

  version <- if (!is.na(pkg)) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  list(`function` = fn_name, package = pkg, version = version)
}

#' Build the `generator` block recorded on an `ssffTrackDefinitions` entry
#'
#' `args` contains only what the caller explicitly passed to `quantify()` - 
#' never metadata-derived or per-bundle-resolved values (those have no home
#' in a single global track definition; see the design spec's "Non-goals").
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @param user_params Named list of explicit caller args (`list(...)` at the
#'   `quantify.corpus` method boundary).
#' @return A `generator` list ready to embed in an `ssffTrackDefinitions[i]`.
#' @noRd
.build_generator_block <- function(dsp_fun, dsp_fun_name, user_params) {
  identity <- .resolve_dsp_identity(dsp_fun, dsp_fun_name)
  list(
    `function` = identity$`function`,
    package = identity$package,
    version = identity$version,
    args = if (length(user_params)) user_params else stats::setNames(list(), character(0)),
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' Upsert one entry into a corpus' ssffTrackDefinitions
#'
#' @param corpus_obj A corpus object.
#' @param track_def A list with at least `name`; typically also
#'   `columnName`, `fileExtension`, and optionally `generator`/`from`/`index`.
#' @param overwrite If `FALSE` (default) and an entry with the same `name`
#'   already exists, aborts. If `TRUE`, replaces it in place.
#' @return The corpus, invisibly.
#' @noRd
.upsert_ssff_track_definition <- function(corpus_obj, track_def, overwrite = FALSE) {
  dbConfig <- load_DBconfig(corpus_obj)
  existing <- dbConfig$ssffTrackDefinitions %or_empty% list()
  existing_names <- vapply(existing, function(t) t$name %or_empty% "", character(1))

  idx <- match(track_def$name, existing_names)
  if (!is.na(idx)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort(c(
        "SSFF track {.val {track_def$name}} is already registered.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ), class = c("reindeer_track_collision_error", "reindeer_error"))
    }
    existing[[idx]] <- track_def
  } else {
    existing[[length(existing) + 1L]] <- track_def
  }

  dbConfig$ssffTrackDefinitions <- existing
  store_DBconfig(corpus_obj, dbConfig)
  invisible(corpus_obj)
}

#' Discover the on-disk SSFF column name(s) for a just-written track
#'
#' `attr(dsp_fun, "tracks")` gives the in-memory display name (e.g.
#' `"RMS[dB]"`), which is not always the name superassp writes into the
#' SSFF binary header. Reading one just-written file back with
#' `superassp::read_track()` gets the ground-truth column name(s) instead
#' of guessing from the display label.
#'
#' @param written_file Path to one SSFF file this call just wrote.
#' @param display_tracks Character vector from `attr(dsp_fun, "tracks")`,
#'   used as a fallback (sanitized) if the file can't be read back.
#' @return Character vector, same length/order as `display_tracks`.
#' @noRd
.discover_written_column_names <- function(written_file, display_tracks) {
  sanitized <- gsub("\\[.*\\]$", "", display_tracks)
  if (is.null(written_file) || !file.exists(written_file)) {
    return(sanitized)
  }
  read_back <- superassp::read_track(written_file)
  if (!is.list(read_back) || length(read_back) != length(display_tracks)) {
    return(sanitized)
  }
  on_disk_names <- names(read_back)
  if (is.null(on_disk_names) || any(!nzchar(on_disk_names))) {
    return(sanitized)
  }
  on_disk_names
}

# Internal implementation of corpus-level DSP compute-and-register.
# (Was `.enrich_corpus_impl()` in the removed R/reindeer_enrich.R; the
# bundle loop, metadata-derived params, caching, and parallelism are
# unchanged. New: track/column inference + ssffTrackDefinitions upsert.)
.quantify_corpus_compute <- function(corpus_obj, .using, dsp_fun_name, user_params,
                                     name, columnName, fileExtension, overwrite,
                                     sessionPattern, bundlePattern,
                                     .metadata_fields, .signal_extension,
                                     .force, .verbose,
                                     .use_cache, .cache_dir, .cache_format) {
  .cache_format <- match.arg(.cache_format, choices = c("auto", "qs", "rds"))

  if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
    cli::cli_abort("{.arg object} must be a corpus object")
  }
  if (!is.function(.using)) {
    cli::cli_abort("{.arg .using} must be a function")
  }

  display_tracks <- attr(.using, "tracks")
  if (is.null(display_tracks)) display_tracks <- dsp_fun_name
  dsp_ext <- attr(.using, "ext") %or_empty% fileExtension
  if (is.null(dsp_ext)) {
    cli::cli_abort(c(
      "Cannot determine a file extension for {.fn {dsp_fun_name}}.",
      "i" = "Pass {.arg fileExtension} explicitly."
    ))
  }

  if (!is.null(name) && length(name) != length(display_tracks)) {
    cli::cli_abort(c(
      "{.arg name} has {length(name)} element{?s} but {.fn {dsp_fun_name}} produces {length(display_tracks)} track{?s}.",
      "i" = "Pass one name per output track, in order, or omit {.arg name} to auto-derive."
    ))
  }

  if (is.null(.signal_extension)) {
    .signal_extension <- corpus_obj@config$mediafileExtension
    if (is.null(.signal_extension)) {
      .signal_extension <- "wav"
      if (.verbose) cli::cli_alert_info("Using default extension: {.val wav}")
    }
  }

  if (.verbose) {
    cli::cli_h2("Quantifying corpus with {.fn {dsp_fun_name}}")
    cli::cli_alert_info("Processing {.val {(.signal_extension)}} files")
  }

  signal_files <- peek_signals(corpus_obj)
  signal_files <- signal_files[signal_files$extension == .signal_extension, ]
  signal_files <- signal_files[grepl(sessionPattern, signal_files$session) &
                                grepl(bundlePattern, signal_files$bundle), ]

  if (nrow(signal_files) == 0) {
    cli::cli_alert_warning("No signal files found with extension {.val {(.signal_extension)}}")
    return(invisible(corpus_obj))
  }

  if (.verbose) cli::cli_alert_success("Found {nrow(signal_files)} signal file{?s}")

  bundle_metadata <- as.data.frame(
    .speaker_metadata(corpus_obj, signal_files[, c("session", "bundle")],
                      union(.metadata_fields, c("Age", "Gender"))),
    stringsAsFactors = FALSE)

  signal_files_with_meta <- merge(signal_files, bundle_metadata, by = c("session", "bundle"), all.x = TRUE)

  cache_conn <- NULL
  if (.use_cache) {
    resolved_cache_dir <- .cache_dir %or_empty% corpus_obj@.cache_dir
    cache_conn <- .get_persistent_cache_connection(resolved_cache_dir, verbose = .verbose)
  }

  # Main process: derive parameters, build cache keys and look up the cache.
  # Workers only compute, so no connection ever crosses a process boundary.
  n_bundles <- nrow(signal_files_with_meta)
  jobs <- lapply(seq_len(n_bundles), function(i) {
    bundle_row <- signal_files_with_meta[i, ]
    dsp_params <- derive_dsp_parameters(
      dsp_fun = .using, metadata = bundle_row,
      metadata_fields = .metadata_fields, user_params = user_params
    )
    out_file <- file.path(
      dirname(bundle_row$full_path),
      paste0(tools::file_path_sans_ext(basename(bundle_row$full_path)), ".", dsp_ext))
    cache_key <- NA_character_
    cached <- FALSE
    if (!is.null(cache_conn)) {
      info <- file.info(bundle_row$full_path)
      cache_key <- digest::digest(list(
        bundle_row$full_path, info$mtime, info$size,
        dsp_params, .fn_identity(.using), dsp_ext, .cache_schema
      ))
      # A cache hit only counts while the output file it stands for exists.
      cached <- !.force && file.exists(out_file) &&
        !is.null(.get_persistent_cache(cache_key, cache_conn))
    }
    list(bundle = bundle_row$bundle, session = bundle_row$session,
         full_path = bundle_row$full_path, out_file = out_file,
         dsp_params = dsp_params, cache_key = cache_key, cached = cached)
  })

  # A bundle counts as done only when the routine wrote its output file. An
  # existing file is moved aside first (so a stale one cannot pass for new
  # output) and restored if the routine fails.
  run_job <- function(job, dsp_fun) {
    if (job$cached) {
      return(list(success = TRUE, bundle = job$bundle, session = job$session,
                  cached = TRUE, out_file = job$out_file))
    }
    backup <- paste0(job$out_file, ".reindeer-bak")
    had_file <- file.exists(job$out_file) && file.rename(job$out_file, backup)
    err <- tryCatch({
      do.call(dsp_fun, c(
        list(listOfFiles = job$full_path), job$dsp_params,
        list(toFile = TRUE, verbose = FALSE)
      ))
      NULL
    }, error = conditionMessage)
    if (is.null(err) && !file.exists(job$out_file)) {
      err <- paste0("no ", basename(job$out_file), " was written")
    }
    if (had_file) {
      if (is.null(err)) unlink(backup) else file.rename(backup, job$out_file)
    }
    list(success = is.null(err), bundle = job$bundle, session = job$session,
         cache_key = job$cache_key, out_file = job$out_file, error = err)
  }

  # The caller's future::plan() decides whether this runs in parallel.
  results <- future.apply::future_lapply(jobs, run_job, dsp_fun = .using,
                                         future.seed = TRUE)

  if (!is.null(cache_conn)) {
    miss_keys <- unique(vapply(
      Filter(function(x) isTRUE(x$success) && !isTRUE(x$cached), results),
      function(x) x$cache_key %or_empty% NA_character_, character(1)))
    miss_keys <- miss_keys[!is.na(miss_keys)]
    if (length(miss_keys) > 0) {
      .set_persistent_cache_batch(
        lapply(miss_keys, function(k) list(cache_key = k, result = TRUE)),
        cache_conn, format = .cache_format)
    }
  }

  successes <- Filter(function(x) isTRUE(x$success), results)
  errors <- Filter(function(x) !isTRUE(x$success), results)
  if (length(successes) == 0) {
    cli::cli_abort(c(
      "{.fn {dsp_fun_name}} wrote no {.file .{dsp_ext}} file; no track was registered.",
      "x" = "{errors[[1]]$session}/{errors[[1]]$bundle}: {errors[[1]]$error}"
    ), class = c("reindeer_dsp_error", "reindeer_error"))
  }
  if (length(errors) > 0) {
    cli::cli_warn(c(
      "{.fn {dsp_fun_name}} failed on {length(errors)} bundle{?s}; they have no {.file .{dsp_ext}} file.",
      stats::setNames(vapply(errors, function(e) paste0(e$session, "/", e$bundle, ": ", e$error),
                             character(1)), rep("x", length(errors)))
    ), class = c("reindeer_dsp_warning", "reindeer_warning"))
  }

  column_names <- .discover_written_column_names(successes[[1]]$out_file, display_tracks)

  for (i in seq_along(display_tracks)) {
    track_name <- if (!is.null(name)) name[i] else gsub("\\[.*\\]$", "", display_tracks[i])
    track_def <- list(
      name = track_name,
      columnName = if (!is.null(columnName)) columnName[i] else column_names[i],
      fileExtension = dsp_ext,
      generator = .build_generator_block(.using, dsp_fun_name, user_params)
    )
    .upsert_ssff_track_definition(corpus_obj, track_def, overwrite = overwrite)
  }

  if (.verbose) cli::cli_alert_success("Quantification complete")
  invisible(corpus_obj)
}

#' @rdname quantify
#' @section Method arguments - corpus:
#' \describe{
#'   \item{`.using`}{DSP function to compute and register a new track.
#'     Omit to connect to an already-existing SSFF track instead (see
#'     `from`/`index`).}
#'   \item{`.metadata_fields`}{Speaker metadata fields used to derive DSP
#'     parameters. Default `c("Gender", "Age")`.}
#'   \item{`.signal_extension`}{Media file extension to read signal files
#'     from. Defaults to the database's configured media extension.}
#'   \item{`.force`}{Recompute and overwrite cached results. Default `FALSE`.}
#'   \item{`name`, `columnName`, `fileExtension`}{Track identity for the
#'     corpus method. In compute-and-register mode (`.using` given),
#'     auto-derived from the DSP function when omitted - one entry per
#'     output track group (e.g. formants register `F` and `B`
#'     separately). In connect-existing mode (`.using` omitted), `name`
#'     is required.}
#'   \item{`from`, `index`}{Connect-existing mode only: `from` is the
#'     on-disk SSFF column label within the underlying file (e.g.
#'     `"F[Hz]"` for a formant track) - the same string you'd pass to
#'     `superassp::read_track(tracks = )` - not the registered track's
#'     `name`. `index` (1-based) then picks one column of that (for a
#'     column that itself holds more than one value per frame).}
#'   \item{`overwrite`}{Replace an existing same-`name` track definition
#'     instead of erroring. Default `FALSE`.}
#'   \item{`session_pattern`, `bundle_pattern`}{Regex filters over which
#'     bundles to process. Default `".*"` (all).}
#'   \item{`.verbose`, `.parallel`, `.workers`, `.use_cache`,
#'     `.cache_dir`, `.cache_format`}{As for the segment_list method
#'     (see above), applied per bundle.}
#' }
#' @usage NULL
#' @name quantify.corpus
S7::method(quantify, corpus) <- function(object, .using = NULL, ...,
                                         name = NULL,
                                         column_name = NULL,
                                         file_extension = NULL,
                                         from = NULL, index = NULL,
                                         overwrite = FALSE,
                                         session_pattern = ".*",
                                         bundle_pattern = ".*",
                                         .metadata_fields = c("Gender", "Age"),
                                         .signal_extension = NULL,
                                         .force = FALSE,
                                         .verbose = TRUE,
                                         .parallel = NULL,
                                         .workers = NULL,
                                         .use_cache = TRUE,
                                         .cache_dir = NULL,
                                         .cache_format = c("auto", "qs", "rds")) {
  .warn_ignored_parallel_args(.parallel = .parallel, .workers = .workers)
  # Old camelCase spellings land in `...`; map them instead of passing them on
  # to the DSP routine as parameters.
  dots <- .rename_args(list(...), c(sessionPattern = "session_pattern",
                                    bundlePattern = "bundle_pattern",
                                    columnName = "column_name",
                                    fileExtension = "file_extension"), "quantify")
  for (nm in intersect(names(dots), c("session_pattern", "bundle_pattern",
                                      "column_name", "file_extension"))) {
    assign(nm, dots[[nm]])
    dots[[nm]] <- NULL
  }
  if (!is.null(.using) && (!is.null(from) || !is.null(index))) {
    cli::cli_abort(c(
      "Cannot supply both {.arg .using} and {.arg from}/{.arg index}.",
      "i" = "Compute mode ({.arg .using}) registers every column the DSP produces.",
      "i" = "{.arg from}/{.arg index} only make sense when connecting to something that already exists."
    ))
  }

  if (!is.null(.using)) {
    dsp_fun_name <- deparse(substitute(.using))
    .quantify_corpus_compute(
      corpus_obj = object, .using = .using, dsp_fun_name = dsp_fun_name,
      user_params = dots, name = name, columnName = column_name,
      fileExtension = file_extension, overwrite = overwrite,
      sessionPattern = session_pattern, bundlePattern = bundle_pattern,
      .metadata_fields = .metadata_fields, .signal_extension = .signal_extension,
      .force = .force, .verbose = .verbose,
      .use_cache = .use_cache, .cache_dir = .cache_dir,
      .cache_format = .cache_format
    )
  } else {
    .quantify_corpus_connect(
      corpus_obj = object, name = name, columnName = column_name,
      fileExtension = file_extension, from = from, index = index,
      overwrite = overwrite, dots = dots
    )
  }
}

#' Connect-existing mode for quantify.corpus (no DSP call, no file write)
#' @noRd
.quantify_corpus_connect <- function(corpus_obj, name, columnName, fileExtension,
                                     from, index, overwrite, dots) {
  if (is.null(name)) {
    cli::cli_abort(c(
      "{.arg name} is required when connecting an existing track (no {.arg .using}).",
      "i" = "Pass {.arg name} plus either {.arg fileExtension} or {.arg from}/{.arg index}."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  if (length(name) != 1L) {
    cli::cli_abort("{.arg name} must be a single string in connect-existing mode.",
                   class = c("reindeer_track_validation_error", "reindeer_error"))
  }

  has_ext <- !is.null(fileExtension)
  has_sub <- !is.null(from) || !is.null(index)
  if (has_sub && (is.null(from) || is.null(index))) {
    cli::cli_abort(c(
      "{.arg from} and {.arg index} must both be supplied together.",
      "i" = "{.arg from} names the already-registered multi-column track; {.arg index} (1-based) picks a column."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  if (!has_ext && !has_sub) {
    cli::cli_abort(c(
      "Connect-existing mode needs either {.arg fileExtension} (adopt a whole file) ",
      "or {.arg from}/{.arg index} (name one column of a registered track)."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  # Note: fileExtension and from/index are NOT mutually exclusive here - a
  # from/index sub-column reference may also carry the fileExtension of the
  # underlying multi-column file (see the "(from/index) addresses a
  # sub-column" test, which supplies both together).

  track_def <- list(name = name)
  if (!is.null(columnName)) track_def$columnName <- columnName
  if (has_ext) track_def$fileExtension <- fileExtension
  if (has_sub) {
    track_def$from <- from
    track_def$index <- as.integer(index)
  }
  if (!is.null(dots$generator)) {
    track_def$generator <- dots$generator
  }

  .upsert_ssff_track_definition(corpus_obj, track_def, overwrite = overwrite)
  invisible(corpus_obj)
}
