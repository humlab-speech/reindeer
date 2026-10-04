# ==============================================================================
# ARTIC SESSION ENGINE
# ==============================================================================
#
# One HTTP + WebSocket server implementing the EMU-webApp protocol
# (EMU-webApp-websocket-protocol 0.0.2), used by annotate() and review().
# The HTTP server also serves the Artic static build and range-aware media;
# the WebSocket server answers the protocol commands for DBconfig, bundle
# lists (including review time anchors), bundles, and save-back.

#' @importFrom httpuv startServer stopAllServers
.artic_session <- function(corpus,
                           bundle_entries = NULL,
                           dbconfig_overlay = NULL,
                           sessionPattern = ".*",
                           bundlePattern = ".*",
                           bundleListName = NULL,
                           host = "127.0.0.1",
                           port = 17890,
                           autoOpenURL = "http://127.0.0.1:17890/?autoConnect=true",
                           browser = getOption("browser"),
                           useViewer = TRUE,
                           debug = FALSE,
                           debugLevel = 0,
                           appDir = NULL,
                           require_artic = character()) {

  # Set debug level
  if (debug && debugLevel == 0) {
    debugLevel <- 2
  }

  # Get emuDBhandle for compatibility with emuR functions
  emuDBhandle <- get_emuDBhandle(corpus)

  # Load database configuration and apply the in-memory overlay (editing
  # permissions, renderable perspectives, review track overlays). The corpus
  # _DBconfig.json on disk is never modified.
  DBconfig <- load_DBconfig(emuDBhandle)
  if (!is.null(dbconfig_overlay)) {
    DBconfig <- dbconfig_overlay(DBconfig)
  }

  # Bundle scope: a review playlist (pre-built entries with time anchors), a
  # saved bundle list, or the corpus bundle table filtered by patterns.
  entries <- if (!is.null(bundle_entries)) {
    if (!is.null(bundleListName)) {
      .artic_abort("{.arg bundle_entries} and {.arg bundleListName} are mutually exclusive.")
    }
    bundle_entries
  } else {
    bl <- if (!is.null(bundleListName)) {
      .read_bundle_list(emuDBhandle$basePath, bundleListName)
    } else {
      .list_bundles(emuDBhandle)
    }
    if (!is.null(sessionPattern) && sessionPattern != ".*") {
      bl <- bl[reindeer_regexprl(sessionPattern, bl[["session"]]), , drop = FALSE]
    }
    if (!is.null(bundlePattern) && bundlePattern != ".*") {
      bl <- bl[reindeer_regexprl(bundlePattern, bl[["name"]]), , drop = FALSE]
    }
    if (nrow(bl) == 0L) {
      .artic_abort("No bundles to serve (check the session/bundle patterns).")
    }
    lapply(seq_len(nrow(bl)), function(i) {
      e <- list(
        session = as.character(bl$session[[i]]),
        name = as.character(bl$name[[i]])
      )
      if ("comment" %in% names(bl)) e$comment <- as.character(bl$comment[[i]])
      if ("finishedEditing" %in% names(bl)) e$finishedEditing <- as.logical(bl$finishedEditing[[i]])
      e
    })
  }
  if (length(entries) == 0L) {
    .artic_abort("No bundles to serve.")
  }

  # Resolve the Artic dist once per session (the fallback ladder does
  # filesystem checks, so it should not run per request).
  webAppDir <- find_artic(appDir = appDir, require = require_artic)

  # Define HTTP request handler
  httpRequest <- function(req) {
    if (req$REQUEST_METHOD == "GET") {
      queryStr <- shiny::parseQueryString(req$QUERY_STRING)

      # Handle media file requests
      if (!is.null(queryStr$session) && !is.null(queryStr$bundle)) {
        mediaFilePath <- file.path(
          emuDBhandle$basePath,
          paste0(queryStr$session, get_session_suffix()),
          paste0(queryStr$bundle, get_bundle_dir_suffix()),
          paste0(queryStr$bundle, ".", queryStr$fileExtension)
        )

        audioFileData <- NULL
        res <- .serve_file_response(mediaFilePath, "audio/x-wav", req$HTTP_RANGE)
        res$headers$`Access-Control-Allow-Origin` <- "*"
        return(res)
      } else {
        # Handle static file requests from EMU-webApp
        path <- httpuv::decodeURIComponent(req$PATH_INFO)
        Encoding(path) <- "UTF-8"

        # Prevent path traversal attacks - reject paths with ../ or absolute paths
        if (grepl("\\.\\.", path, fixed = TRUE) || startsWith(path, "/")) {
          return(list(
            status = 403L,
            headers = list(`Content-Type` = "text/plain"),
            body = "Forbidden: Invalid path\r\n"
          ))
        }

        status <- 200L

        # Use revised EMU-webApp directory (resolved once per serve() call)
        path <- file.path(webAppDir, path)

        # Additional validation: ensure resolved path is within webAppDir
        # Use normalizePath with mustWork=FALSE to avoid errors on non-existent paths
        normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
        normalized_webAppDir <- normalizePath(webAppDir, winslash = "/", mustWork = TRUE)

        # Check if the normalized path starts with the webapp directory
        if (!startsWith(normalized_path, normalized_webAppDir)) {
          return(list(
            status = 403L,
            headers = list(`Content-Type` = "text/plain"),
            body = "Forbidden: Path outside webapp directory\r\n"
          ))
        }

        if (utils::file_test("-d", path)) {
          # Directory listing for the webApp root
          if (file.exists(idx <- file.path(path, "index.html"))) {
            body <- readLines(idx, warn = FALSE)
          } else {
            d <- file.info(list.files(path, all.files = TRUE, full.names = TRUE))
            title <- utils::URLencode(path, reserved = TRUE)
            body <- c("<!DOCTYPE html>", "<html>", "<head>",
              sprintf("<title>%s</title>", title), "</head>",
              "<body>",
              c(sprintf("<h1>Index of %s</h1>", title),
                # Note: Using simplified directory listing instead of emuR:::fileinfo_table
                paste0("<ul>", paste0("<li>", names(d), "</li>", collapse = ""), "</ul>")),
              "</body>", "</html>")
          }
          if (is.character(body) && length(body) > 1) {
            body <- paste(body, collapse = "\n")
          }
          return(list(
            status = 200L,
            body = body,
            headers = list(`Content-Type` = "text/html")
          ))
        }

        # Files go through the one range-aware implementation. The inlined copy
        # that used to live here mis-parsed open-ended ranges (`bytes=100-` made
        # `b3 == 0` evaluate to NA, an error inside `if`), and it duplicated
        # .serve_file_response().
        return(.serve_file_response(path, guess_mime_type(path), req$HTTP_RANGE))
      }
    }
  }

  # Define WebSocket handlers
  onHeaders <- function(req) {
    # Currently unused
  }

  serverEstablished <- function(ws) {
    cli::cli_alert_success("reindeer websocket service established")

    serverClosed <- function(ws) {
      cli::cli_alert_info("reindeer websocket service closed")
    }

    sendError <- function(ws, errMsg, callbackID) {
      status <- list(type = "ERROR", details = errMsg)
      response <- list(callbackID = callbackID, status)
      responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
      result <- ws$send(responseJSON)
    }

    serverReceive <- function(isBinary, DATA) {
      if (debugLevel >= 4) {
        cli::cli_alert_info("onMessage() call, binary: {isBinary} data: {DATA}")
      }

      # Parse message
      D <- if (is.raw(DATA)) rawToChar(DATA) else DATA
      D <- enc2utf8(D)
      jr <- jsonlite::fromJSON(D, simplifyVector = FALSE)

      if (debugLevel >= 2) {
        cli::cli_alert_info("Received command from EMU-webApp: {jr[['type']]}")
        if (debugLevel >= 3) {
          jrNms <- names(jr)
          for (jrNm in jrNms) {
            value <- jr[[jrNm]]
            if (inherits(value, "character")) {
              cli::cli_alert_info("param: {jrNm}: {value}")
            } else {
              cli::cli_alert_info("param: {jrNm}")
            }
          }
        }
      }

      # Handle different message types
      if (jr$type == "GETPROTOCOL") {
        protocolData <- list(
          protocol = "EMU-webApp-websocket-protocol",
          version = "0.0.2"
        )
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          data = protocolData
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) cli::cli_alert_success("Sent protocol.")

      } else if (jr$type == "GETDOUSERMANAGEMENT") {
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          data = "NO"
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) cli::cli_alert_success("Sent user management: no.")

      } else if (jr$type == "GETGLOBALDBCONFIG") {
        if (debugLevel >= 4) {
          cli::cli_alert_info("Send config: {as.character(DBconfig)}")
        }
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          data = DBconfig
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) {
          if (debugLevel >= 4) cli::cli_alert_info("{responseJSON}")
          cli::cli_alert_success("Sent config.")
        }

      } else if (jr$type == "GETBUNDLELIST") {
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          dataType = "bundleList",
          data = entries
        )

        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        if (debugLevel >= 5) cli::cli_alert_info("{responseJSON}")
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) {
          cli::cli_alert_success("Sent bundle list with length: {length(entries)}")
        }
      } else if (jr$type == "GETBUNDLE") {
        bundleName <- jr[["name"]]
        bundleSess <- jr[["session"]]
        if (debugLevel > 2) {
          cli::cli_alert_info("Requested bundle: {bundleName}, session: {bundleSess}")
        }

        err <- NULL
        if (debugLevel > 3) {
          cli::cli_alert_info("Convert bundle to S3 format: {bundleName}")
        }

        # Load annotation
        annotFilePath <- normalizePath(file.path(
          emuDBhandle$basePath,
          paste0(bundleSess, get_session_suffix()),
          paste0(bundleName, get_bundle_dir_suffix()),
          paste0(bundleName, get_annotation_suffix(), ".json")
        ))
        b <- jsonlite::fromJSON(annotFilePath, simplifyVector = FALSE)

        if (is.null(b)) {
          err <- simpleError(paste("Could not load bundle", bundleName, "of session", bundleSess))
        }

        # Create media file URL
        if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
          translateFunction <- rstudioapi::translateLocalUrl
        } else {
          translateFunction <- paste0
        }

        mediaFile <- list(
          encoding = "GETURL",
          data = paste0(
            translateFunction(paste0("http://", ws$request$HTTP_HOST)),
            "?session=", utils::URLencode(bundleSess, reserved = TRUE),
            "&bundle=", utils::URLencode(bundleName, reserved = TRUE),
            "&fileExtension=", utils::URLencode(DBconfig$mediafileExtension, reserved = TRUE)
          )
        )

        # Load SSFF files
        if (is.null(err)) {
          ssffTracksInUse <- DBconfig$ssffTrackDefinitions
          ssffTrackNmsInUse <- .get_ssff_tracks_in_use(DBconfig)

          if (debugLevel >= 4) {
            cli::cli_alert_info("{length(ssffTrackNmsInUse)} track definitions in use: {paste(ssffTrackNmsInUse, collapse = ' ')}")
          }

          ssffFiles <- list()
          ssffFilesHash <- character(0)

          for (ssffTr in DBconfig$ssffTrackDefinitions) {
            if (ssffTr[["name"]] %in% ssffTrackNmsInUse) {
              fe <- ssffTr[["fileExtension"]]
              ssffFilesHash[fe] <- normalizePath(file.path(
                emuDBhandle$basePath,
                paste0(bundleSess, get_session_suffix()),
                paste0(bundleName, get_bundle_dir_suffix()),
                paste0(bundleName, ".", fe)
              ))
            }
          }

          ssffFileExts <- names(ssffFilesHash)
          for (ssffFileExt in ssffFileExts) {
            ssffFilePath <- ssffFilesHash[ssffFileExt]
            mf <- tryCatch(file(ssffFilePath, "rb"), error = function(e) NULL)
            if (is.null(mf)) {
              # A registered track with no file on disk must not block the whole
              # bundle; skip it so the rest of the annotation still loads.
              cli::cli_warn("Missing SSFF file for {bundleSess}/{bundleName}: {.path {ssffFilePath}}; skipping.")
              next
            }
            mfData <- tryCatch(
              readBin(mf, raw(), n = file.info(ssffFilePath)$size),
              error = function(e) NULL
            )
            close(mf)
            if (is.null(mfData)) {
              cli::cli_warn("Could not read SSFF file {.path {ssffFilePath}}; skipping.")
              next
            }
            ssffFiles[[length(ssffFiles) + 1]] <- list(
              encoding = "BASE64",
              data = base64enc::base64encode(mfData),
              fileExtension = ssffFileExt
            )
          }

          if (is.null(err)) {
            data <- list(
              mediaFile = mediaFile,
              ssffFiles = ssffFiles,
              annotation = b
            )
          }
        }

        # Send response
        if (is.null(err)) {
          responseBundle <- list(
            status = list(type = "SUCCESS"),
            callbackID = jr$callbackID,
            responseContent = "bundle",
            contentType = "text/json",
            data = data
          )
        } else {
          errMsg <- err[["message"]]
          cli::cli_alert_danger("Error: {errMsg}")
          responseBundle <- list(
            status = list(type = "ERROR", message = errMsg),
            callbackID = jr[["callbackID"]],
            responseContent = "status",
            contentType = "text/json"
          )
        }

        responseBundleJSON <- jsonlite::toJSON(responseBundle, auto_unbox = TRUE, force = TRUE, pretty = FALSE)
        result <- ws$send(responseBundleJSON)

        if (is.null(err) & debugLevel >= 2) {
          if (debugLevel >= 8) cli::cli_alert_info("{responseBundleJSON}")
          cli::cli_alert_success("Sent bundle containing {length(ssffFiles)} SSFF files")
        }
        err <- NULL

      } else if (jr[["type"]] == "SAVEBUNDLE") {
        jrData <- jr[["data"]]
        jrAnnotation <- jrData[["annotation"]]
        bundleSession <- jrData[["session"]]
        bundleName <- jrData[["annotation"]][["name"]]

        if (debugLevel > 3) {
          cli::cli_alert_info("Save bundle {bundleName} from session {bundleSession}")
        }

        err <- NULL
        ssffFiles <- jr[["data"]][["ssffFiles"]]
        oldBundleAnnotDFs <- .load_bundle_annot(emuDBhandle$connection, bundleSession, bundleName)

        warnOptionSave <- getOption("warn")
        options(warn = 2)
        on.exit(options(warn = warnOptionSave))

        responseBundle <- NULL

        if (is.null(oldBundleAnnotDFs)) {
          err <- simpleError(paste("Could not load bundle", bundleSession, bundleName))
        } else {
          # Save SSFF files
          for (ssffFile in ssffFiles) {
            sp <- normalizePath(file.path(
              emuDBhandle$basePath,
              paste0(bundleSession, get_session_suffix()),
              paste0(bundleName, get_bundle_dir_suffix()),
              paste0(bundleName, ".", ssffFile$fileExtension)
            ))

            if (is.null(sp)) {
              errMsg <- paste0("SSFF track definition for file extension '",
                              ssffFile[["fileExtension"]], "' not found!")
              err <- simpleError(errMsg)
            } else {
              if (debugLevel > 3) {
                cli::cli_alert_info("Writing SSFF track to file: {sp}")
              }
              ssffTrackBin <- base64enc::base64decode(ssffFile[["data"]])
              ssffCon <- tryCatch(file(sp, "wb"), error = function(e) {
                err <<- e
              })

              if (is.null(err)) {
                res <- tryCatch(writeBin(ssffTrackBin, ssffCon))
                close(ssffCon)
                if (inherits(res, "error")) {
                  err <- res
                  break
                }
              }
            }
          }

          # Save annotation
          bundleData <- jr[["data"]][["annotation"]]
          if (is.null(err)) {
            annotFilePath <- file.path(
              emuDBhandle$basePath,
              paste0(bundleSession, get_session_suffix()),
              paste0(bundleName, get_bundle_dir_suffix()),
              paste0(bundleName, get_annotation_suffix(), ".json")
            )
            json <- jsonlite::toJSON(bundleData, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
            res <- tryCatch(writeLines(json, annotFilePath, useBytes = TRUE), error = function(e) e)

            if (inherits(res, "error")) {
              err <- res
            }

            # Update database
            DBI::dbBegin(emuDBhandle$connection)
            .remove_bundle_from_db(emuDBhandle$connection, bundleSession, bundleName)

            newMD5annotJSON <- tools::md5sum(annotFilePath)
            names(newMD5annotJSON) <- NULL

            bundleAnnotDFs <- .parse_annot_json(as.character(json))
            # Fill in db_uuid/session/bundle for parsed annotation DFs
            for (tbl_name in c("items", "labels", "links")) {
              if (nrow(bundleAnnotDFs[[tbl_name]]) > 0) {
                bundleAnnotDFs[[tbl_name]]$db_uuid <- emuDBhandle$UUID
                bundleAnnotDFs[[tbl_name]]$session <- bundleSession
                bundleAnnotDFs[[tbl_name]]$bundle <- bundleName
              }
            }
            .add_bundle_to_db(emuDBhandle$connection,
                              emuDBhandle$UUID,
                              bundleSession,
                              bundleName,
                              bundleAnnotDFs$annotates,
                              bundleAnnotDFs$sampleRate,
                              newMD5annotJSON)
            .store_bundle_annot(emuDBhandle$connection,
                                bundleAnnotDFs,
                                bundleSession,
                                bundleName)
            DBI::dbCommit(emuDBhandle$connection)

            # Update bundle list if specified
            if (!is.null(bundleListName)) {
              bl <- .read_bundle_list(emuDBhandle$basePath, bundleListName)
              bl[bl$session == bundleSession & bl$name == bundleName, ]$comment <- jr[["data"]][["comment"]]
              bl[bl$session == bundleSession & bl$name == bundleName, ]$finishedEditing <- jr[["data"]][["finishedEditing"]]
              .write_bundle_list(emuDBhandle$basePath, bundleListName, bl)
            }
          }
        }

        # Send response
        if (is.null(err)) {
          responseBundle <- list(
            status = list(type = "SUCCESS"),
            callbackID = jr$callbackID,
            responseContent = "status",
            contentType = "text/json"
          )
        } else {
          m <- err[["message"]]
          cli::cli_alert_danger("Error: {m}")
          responseBundle <- list(
            status = list(type = "ERROR", message = m),
            callbackID = jr[["callbackID"]],
            responseContent = "status",
            contentType = "text/json"
          )
        }

        responseBundleJSON <- jsonlite::toJSON(responseBundle, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseBundleJSON)
        err <- NULL

      } else if (jr[["type"]] == "DISCONNECTWARNING") {
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr[["callbackID"]],
          responseContent = "status",
          contentType = "text/json"
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        ws$close()
        cli::cli_alert_info("reindeer websocket service closed by EMU-webApp")
      }
    }

    ws$onMessage(serverReceive)
    ws$onClose(serverClosed)
  }

  # Stop a previously-started reindeer server (scoped to this package) so a
  # restart binds cleanly, without killing unrelated httpuv servers.
  prev_handle <- getOption("reindeer.serve_handle")
  if (!is.null(prev_handle)) {
    try(httpuv::stopServer(prev_handle), silent = TRUE)
  }

  # Print server info
  cli::cli_h2("Starting reindeer Artic server")
  cli::cli_alert_info("Navigate your browser to: {.url http://localhost:{port}}")
  cli::cli_alert_info("Server connection URL: {.url ws://localhost:{port}}")
  cli::cli_alert_info("To stop the server:")
  cli::cli_ul(c(
    "Press the 'clear' button in Artic",
    "Close/reload the webApp in your browser",
    "Call {.code httpuv::stopServer(getOption('reindeer.serve_handle'))} in R"
  ))

  # Create server app
  app <- list(
    call = httpRequest,
    onHeaders = onHeaders,
    onWSOpen = serverEstablished
  )

  # Start server and retain the handle so it can be stopped scoped later.
  server <- tryCatch(
    httpuv::startServer(host = host, port = port, app = app),
    error = function(e) {
      .artic_abort(c(
        "Could not start the Artic server on {host}:{port}.",
        "i" = "The port may be in use; try {.code port = httpuv::randomPort()}.",
        "x" = conditionMessage(e)
      ))
    }
  )
  options(reindeer.serve_handle = server)

  # Auto-open browser
  if (length(autoOpenURL) != 0 && autoOpenURL != "") {
    viewer <- getOption("viewer")

    if (useViewer & requireNamespace("rstudioapi", quietly = TRUE) & rstudioapi::isAvailable()) {
      # Artic is served from its dist root, so absolute asset URLs resolve
      # against the server root; the RStudio Viewer can load the URL directly.

      # Open in viewer or browser
      if (!is.null(viewer)) {
        viewer(paste0(
          "http://127.0.0.1:", port, "/?autoConnect=true",
          "&serverUrl=", sub(
            "http", "ws",
            rstudioapi::translateLocalUrl(paste0("http://127.0.0.1:", port), absolute = TRUE)
          )
        ))
      } else {
        utils::browseURL(
          paste0("http://127.0.0.1:", port, "/?autoConnect=true",
                "&serverUrl=ws://127.0.0.1:", port),
          browser = browser
        )
      }
    } else {
      utils::browseURL(autoOpenURL, browser = browser)
      cli::cli_alert_info("Unable to detect RStudio. Opening online version.")
    }
  }

  return(invisible(TRUE))
}


# Read a file for an HTTP response, honouring an optional byte-range header.
# Returns list(status, headers, body). Serves full content (200) when no
# range or an open-ended "bytes=0-" is requested, 416 on malformed ranges,
# and 206 partial content otherwise. Keeps large media files out of memory.
.serve_file_response <- function(path, content_type, range = NULL) {
  if (!file.exists(path)) {
    return(list(
      status = 404L,
      headers = list(`Content-Type` = "text/plain"),
      body = "Not found\r\n"
    ))
  }
  file_size <- file.info(path)$size
  if (is.null(range) || identical(range, "bytes=0-")) {
    return(list(
      status = 200L,
      headers = list(`Content-Type` = content_type),
      body = readBin(path, "raw", file_size)
    ))
  }

  rng <- strsplit(range, split = "(=|-)")[[1]]
  if (length(rng) < 2 || rng[1] != "bytes") {
    return(.serve_range_unsatisfiable())
  }
  b2 <- suppressWarnings(as.numeric(rng[2]))
  b3 <- if (length(rng) >= 3 && nzchar(rng[3])) {
    suppressWarnings(as.numeric(rng[3]))
  } else {
    file_size - 1L
  }
  if (is.na(b2) || is.na(b3) || b2 < 0 || b3 < b2 || b2 >= file_size) {
    return(.serve_range_unsatisfiable())
  }
  b3 <- min(b3, file_size - 1L)

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, where = b2, origin = "start")
  list(
    status = 206L,
    headers = list(
      `Content-Type` = content_type,
      `Content-Range` = sprintf("bytes %d-%d/%d", b2, b3, file_size)
    ),
    body = readBin(con, "raw", b3 - b2 + 1L)
  )
}

.serve_range_unsatisfiable <- function() {
  list(
    status = 416L,
    headers = list(`Content-Type` = "text/plain"),
    body = "Requested range not satisfiable\r\n"
  )
}

# ==============================================================================
# LOCAL HELPER FUNCTIONS (replacing emuR internal dependencies)
# ==============================================================================
# These functions replicate minimal behavior from emuR internal functions
# to avoid fragile ::: dependencies. If emuR exports these in the future,
# consider switching to the exported versions.

#' Local regex match function
#'
#' Replicates emuR:::emuR_regexprl behavior for pattern matching.
#' Returns logical vector indicating which elements match the pattern.
#'
#' @param pattern Regular expression pattern
#' @param x Character vector to match against
#' @return Logical vector of matches
#' @keywords internal
#' @note Replaces emuR:::emuR_regexprl to avoid internal dependency
#' @noRd
reindeer_regexprl <- function(pattern, x) {
  grepl(pattern, x, perl = TRUE)
}

#' EMU file suffixes
#'
#' Constants for EMU database file naming conventions.
#' These replicate get_session_suffix(), get_bundle_dir_suffix(), etc.
#'
#' @keywords internal
#' @note Replaces emuR internal constants to avoid ::: dependency
#' @noRd
.emu_suffixes <- list(
  session = "_ses",
  bundle_dir = "_bndl",
  annotation = "_annot"
)

#' Get session suffix
#' @keywords internal
#' @noRd
get_session_suffix <- function() .emu_suffixes$session

#' Get bundle directory suffix
#' @keywords internal
#' @noRd
get_bundle_dir_suffix <- function() .emu_suffixes$bundle_dir

#' Get annotation suffix
#' @keywords internal
#' @noRd
get_annotation_suffix <- function() .emu_suffixes$annotation

#' Guess MIME type from file extension
#'
#' Replicates emuR:::guess_type for common file types used in EMU.
#'
#' @param path File path
#' @return MIME type string
#' @keywords internal
#' @note Replaces emuR:::guess_type to avoid internal dependency
#' @noRd
guess_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "html" = "text/html",
    "css" = "text/css",
    "js" = "application/javascript",
    "json" = "application/json",
    "wav" = "audio/wav",
    "mp3" = "audio/mpeg",
    "png" = "image/png",
    "jpg" = , "jpeg" = "image/jpeg",
    "gif" = "image/gif",
    "svg" = "image/svg+xml",
    "txt" = "text/plain",
    "mjs" = "text/javascript",
    "map" = "application/json",
    "wasm" = "application/wasm",
    "woff" = "font/woff",
    "woff2" = "font/woff2",
    "ttf" = "font/ttf",
    "otf" = "font/otf",
    "eot" = "application/vnd.ms-fontobject",
    "ico" = "image/x-icon",
    "webp" = "image/webp",
    "m4a" = "audio/mp4",
    "aac" = "audio/aac",
    "flac" = "audio/flac",
    "ogg" = , "oga" = "audio/ogg",
    "wma" = "audio/x-ms-wma",
    "webm" = "video/webm",
    "mp4" = "video/mp4",
    "mov" = "video/quicktime",
    "application/octet-stream"  # default
  )
}

# ==============================================================================

#' Get emuDBhandle from corpus
#'
#' Converts a reindeer corpus object to an emuDBhandle for compatibility
#' with emuR functions.
#'
#' @param corpus A reindeer corpus object
#' @return An emuDBhandle object
#' @keywords internal
#' @noRd
get_emuDBhandle <- function(corpus) {
  # Get connection from corpus
  conn <- get_connection(corpus)

  # Create emuDBhandle structure
  handle <- list(
    dbName = corpus@dbName,
    basePath = corpus@basePath,
    connection = conn,
    UUID = corpus@.uuid
  )

  class(handle) <- "emuDBhandle"
  return(handle)
}
