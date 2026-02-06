# ==============================================================================
# SERVE EMU-WEBAPP FOR REINDEER CORPUS
# ==============================================================================

#' Serve a corpus in the EMU-webApp annotation interface
#'
#' Launches a local web server that serves a modified EMU-webApp for annotating
#' and visualizing speech data. This function is adapted from `emuR::serve()` but
#' serves a revised version of the annotation application from `../EMU-webApp/dist/`.
#'
#' @param corpus A reindeer corpus object created with `corpus()`
#' @param sessionPattern Regular expression pattern to filter sessions (default: ".*" for all)
#' @param bundlePattern Regular expression pattern to filter bundles (default: ".*" for all)
#' @param seglist Optional segment_list to restrict which bundles are served
#' @param bundleListName Optional name of a bundle list to serve
#' @param host Host address to bind the server to (default: "127.0.0.1")
#' @param port Port number for the server (default: 17890)
#' @param autoOpenURL URL to open automatically. Set to "" to disable auto-open.
#'   Default opens the revised EMU-webApp with autoConnect=true
#' @param browser Browser to use (uses `getOption("browser")` by default)
#' @param useViewer Use RStudio viewer pane if available (default: TRUE)
#' @param debug Enable debug output (default: FALSE)
#' @param debugLevel Debug verbosity level 0-8 (default: 0, or 2 if debug=TRUE)
#'
#' @details
#' This function creates an HTTP server with WebSocket support to enable
#' real-time communication between the browser-based EMU-webApp and R. It
#' serves the revised EMU-webApp from the `EMU-webApp` directory located
#' at `../EMU-webApp/` relative to the reindeer package.
#'
#' The function implements the EMU-webApp websocket protocol v0.0.2, handling:
#' - Database configuration retrieval
#' - Bundle list provision
#' - Bundle data (annotations, signal files, media) delivery
#' - Annotation saving and updates
#'
#' To stop the server:
#' - Press the 'clear' button in the EMU-webApp
#' - Close or reload the webApp in your browser
#' - Call `httpuv::stopAllServers()` in R
#'
#' @return Invisible TRUE
#'
#' @section WebApp Location:
#' The function looks for the revised EMU-webApp in this order:
#' 1. R option: `getOption("reindeer.emuWebApp.dir")`
#' 2. Environment variable: `EMU_WEBAPP_DIR`
#' 3. Package installation: `system.file("EMU-webApp/dist", package = "reindeer")`
#' 4. Default fallback: `../EMU-webApp/dist` relative to package location
#'
#' To set a custom path:
#' ```r
#' # Using R options (persistent within session)
#' options(reindeer.emuWebApp.dir = "/path/to/EMU-webApp/dist")
#'
#' # Using environment variable (persistent across sessions)
#' Sys.setenv(EMU_WEBAPP_DIR = "/path/to/EMU-webApp/dist")
#' ```
#'
#' @examples
#' \dontrun{
#' # Serve entire corpus
#' corp <- corpus("path/to/mydb_emuDB")
#' serve(corp)
#'
#' # Serve specific sessions
#' serve(corp, sessionPattern = "Session.*")
#'
#' # Serve bundles matching a pattern
#' serve(corp, bundlePattern = "msajc.*")
#'
#' # Serve only bundles from a query result
#' segments <- ask_for(corp, "Phonetic == t")
#' serve(corp, seglist = segments)
#'
#' # Serve on a different port
#' serve(corp, port = 8080)
#'
#' # Stop the server
#' httpuv::stopAllServers()
#' }
#'
#' @export
serve <- S7::new_generic("serve", "corpus")

#' @export
S7::method(serve, corpus) <- function(corpus,
                                      sessionPattern = ".*",
                                      bundlePattern = ".*",
                                      seglist = NULL,
                                      bundleListName = NULL,
                                      host = "127.0.0.1",
                                      port = 17890,
                                      autoOpenURL = "http://127.0.0.1:17890/?autoConnect=true",
                                      browser = getOption("browser"),
                                      useViewer = TRUE,
                                      debug = FALSE,
                                      debugLevel = 0) {

  # Set debug level
  if (debug && debugLevel == 0) {
    debugLevel <- 2
  }

  # Get emuDBhandle for compatibility with emuR functions
  emuDBhandle <- get_emuDBhandle(corpus)

  # Load database configuration
  DBconfig <- load_DBconfig(emuDBhandle)

  # Get bundle list
  if (is.null(seglist)) {
    allBundlesDf <- .list_bundles(emuDBhandle)
  } else {
    # Check if seglist is valid
    if (!inherits(seglist, "segment_list") && !is.data.frame(seglist)) {
      cli::cli_abort("seglist must be a segment_list or data.frame with required columns")
    }

    # Validate required columns
    required_cols <- c("session", "bundle")
    optional_cols <- c("start", "end", "sample_rate")  # Used by some functions but not required here
    missing_cols <- setdiff(required_cols, names(seglist))

    if (length(missing_cols) > 0) {
      cli::cli_abort(c(
        "seglist is missing required columns: {.field {missing_cols}}",
        "i" = "Required columns are: {.field {required_cols}}"
      ))
    }

    # Validate column types for required columns
    if (!is.character(seglist$session) && !is.factor(seglist$session)) {
      cli::cli_abort("seglist column {.field session} must be character or factor, not {.cls {class(seglist$session)}}")
    }
    if (!is.character(seglist$bundle) && !is.factor(seglist$bundle)) {
      cli::cli_abort("seglist column {.field bundle} must be character or factor, not {.cls {class(seglist$bundle)}}")
    }

    # Validate optional columns if present
    if ("start" %in% names(seglist) && !is.numeric(seglist$start)) {
      cli::cli_abort("seglist column {.field start} must be numeric, not {.cls {class(seglist$start)}}")
    }
    if ("end" %in% names(seglist) && !is.numeric(seglist$end)) {
      cli::cli_abort("seglist column {.field end} must be numeric, not {.cls {class(seglist$end)}}")
    }
    if ("sample_rate" %in% names(seglist) && !is.numeric(seglist$sample_rate)) {
      cli::cli_abort("seglist column {.field sample_rate} must be numeric, not {.cls {class(seglist$sample_rate)}}")
    }

    tmp <- data.frame(
      session = as.character(seglist$session),
      bundle = as.character(seglist$bundle),
      stringsAsFactors = FALSE
    )
    allBundlesDf <- unique(tmp)
  }

  bundlesDf <- allBundlesDf

  # Handle bundle list
  if (!is.null(bundleListName)) {
    if (!is.null(seglist)) {
      cli::cli_abort("both seglist & bundleListName can't be set at the same time!")
    }
    bundlesDf <- .read_bundle_list(emuDBhandle$basePath, bundleListName)

    # Ensure restrictions are set for bundle comments/editing
    if (is.null(DBconfig$EMUwebAppConfig$restrictions$bundleComments) ||
        is.null(DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing)) {
      DBconfig$EMUwebAppConfig$restrictions$bundleComments <- TRUE
      DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing <- TRUE
      store_DBconfig(emuDBhandle, DBconfig)
    }
  }

  # Warning about bundle comments/editing without bundle list
  if (!is.null(DBconfig$EMUwebAppConfig$restrictions$bundleComments) ||
      !is.null(DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing)) {
    if (is.null(bundleListName)) {
      cli::cli_warn(paste0(
        "'bundleComments' and/or 'bundleFinishedEditing' are set to true ",
        "in the DBconfig and the bundleListName parameter wasn't set! Any changes made ",
        "to those fields in the bundleListSideBar in the EMU-webApp won't be saved as ",
        "those values are stored in the bundleLists!"
      ))
    }
  }

  # Filter by session pattern (only if bundleListName is not set, since it was already loaded)
  if (is.null(bundleListName) && !is.null(sessionPattern) && sessionPattern != ".*") {
    ssl <- reindeer_regexprl(sessionPattern, bundlesDf[["session"]])
    bundlesDf <- bundlesDf[ssl, ]
  }

  # Filter by bundle pattern (only if bundleListName is not set)
  if (is.null(bundleListName) && !is.null(bundlePattern) && bundlePattern != ".*") {
    bsl <- reindeer_regexprl(bundlePattern, bundlesDf[["name"]])
    bundlesDf <- bundlesDf[bsl, ]
  }

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

        audioFile <- file(mediaFilePath, "rb")
        audioFileData <- readBin(audioFile, raw(), n = file.info(mediaFilePath)$size)
        close(audioFile)

        res <- list(
          status = 200L,
          headers = list(
            `Content-Type` = "audio/x-wav",
            `Access-Control-Allow-Origin` = "*"
          ),
          body = audioFileData
        )
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

        # Use revised EMU-webApp directory
        webAppDir <- get_webapp_dir()
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

        body <- if (utils::file_test("-d", path)) {
          type <- "text/html"
          if (file.exists(idx <- file.path(path, "index.html"))) {
            readLines(idx, warn = FALSE)
          } else {
            # Directory listing
            d <- file.info(list.files(path, all.files = TRUE, full.names = TRUE))
            title <- utils::URLencode(path, reserved = TRUE)
            c("<!DOCTYPE html>", "<html>", "<head>",
              sprintf("<title>%s</title>", title), "</head>",
              "<body>",
              c(sprintf("<h1>Index of %s</h1>", title),
                # Note: Using simplified directory listing instead of emuR:::fileinfo_table
                paste0("<ul>", paste0("<li>", names(d), "</li>", collapse = ""), "</ul>")),
              "</body>", "</html>")
          }
        } else {
          type <- guess_mime_type(path)
          range <- req$HTTP_RANGE

          if (is.null(range) || identical(range, "bytes=0-")) {
            readBin(path, "raw", file.info(path)[, "size"])
          } else {
            # Handle range requests for large media files
            range <- strsplit(range, split = "(=|-)")[[1]]
            b2 <- as.numeric(range[2])
            b3 <- as.numeric(range[3])

            if (length(range) < 3 || (range[1] != "bytes") || (b2 >= b3) || (b3 == 0)) {
              return(list(
                status = 416L,
                headers = list(`Content-Type` = "text/plain"),
                body = "Requested range not satisfiable\r\n"
              ))
            }

            status <- 206L
            con <- file(path, open = "rb", raw = TRUE)
            on.exit(close(con))
            seek(con, where = b2, origin = "start")
            readBin(con, "raw", b3 - b2 + 1)
          }
        }

        if (is.character(body) && length(body) > 1) {
          body <- paste(body, collapse = "\n")
        }

        res <- list(
          status = status,
          body = body,
          headers = c(
            list(`Content-Type` = type),
            if (status == 206L) list(`Content-Range` = paste(
              sub("=", " ", req$HTTP_RANGE),
              file.info(path)[, "size"],
              sep = "/"
            ))
          )
        )
        return(res)
      }
    }
  }

  # Define WebSocket handlers
  onHeaders <- function(req) {
    # Currently unused
  }

  serverEstablished <- function(ws) {
    cat("reindeer websocket service established\n")

    serverClosed <- function(ws) {
      cat("reindeer websocket service closed\n")
    }

    sendError <- function(ws, errMsg, callbackID) {
      status <- list(type = "ERROR", details = errMsg)
      response <- list(callbackID = callbackID, status)
      responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
      result <- ws$send(responseJSON)
    }

    serverReceive <- function(isBinary, DATA) {
      if (debugLevel >= 4) {
        cat("onMessage() call, binary:", isBinary, " data: ", DATA, "\n")
      }

      # Parse message
      D <- if (is.raw(DATA)) rawToChar(DATA) else DATA
      D <- readr::parse_character(D)
      jr <- jsonlite::fromJSON(D, simplifyVector = FALSE)

      if (debugLevel >= 2) {
        cat("Received command from EMU-webApp: ", jr[["type"]], "\n")
        if (debugLevel >= 3) {
          jrNms <- names(jr)
          for (jrNm in jrNms) {
            value <- jr[[jrNm]]
            cat("param: ", jrNm)
            if (inherits(value, "character")) {
              cat(": ", jr[[jrNm]])
            }
            cat("\n")
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
        if (debugLevel >= 2) cat("Sent protocol. \n")

      } else if (jr$type == "GETDOUSERMANAGEMENT") {
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          data = "NO"
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) cat("Sent user management: no. \n")

      } else if (jr$type == "GETGLOBALDBCONFIG") {
        if (debugLevel >= 4) {
          cat("Send config: ", as.character(DBconfig), "\n")
        }
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          data = DBconfig
        )
        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) {
          if (debugLevel >= 4) cat(responseJSON, "\n")
          cat("Sent config. \n")
        }

      } else if (jr$type == "GETBUNDLELIST") {
        response <- list(
          status = list(type = "SUCCESS"),
          callbackID = jr$callbackID,
          dataType = "uttList",
          data = bundlesDf
        )

        # Add time anchors if seglist provided
        if (!is.null(seglist)) {
          dataWithTimeAnchors <- list()
          for (i in 1:nrow(response$data)) {
            sesBool <- response$data[i, ]$session == seglist$session
            bndlBool <- response$data[i, ]$bundle == seglist$bundle

            start_sample_vals <- round(((seglist[sesBool & bndlBool, ]$start / 1000) +
                                         0.5 / seglist[sesBool & bndlBool, ]$sample_rate) *
                                        seglist[sesBool & bndlBool, ]$sample_rate)
            end_sample_vals <- round(((seglist[sesBool & bndlBool, ]$end / 1000) +
                                       0.5 / seglist[sesBool & bndlBool, ]$sample_rate) *
                                      seglist[sesBool & bndlBool, ]$sample_rate)

            dataWithTimeAnchors[[i]] <- list(
              session = response$data[i, ]$session,
              name = response$data[i, ]$bundle,
              timeAnchors = data.frame(
                sample_start = start_sample_vals,
                sample_end = end_sample_vals
              )
            )
          }
          response$data <- dataWithTimeAnchors
        }

        responseJSON <- jsonlite::toJSON(response, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
        if (debugLevel >= 5) cat(responseJSON, "\n")
        result <- ws$send(responseJSON)
        if (debugLevel >= 2) {
          cat("Sent utterance list with length: ", nrow(bundlesDf), " \n")
        }

      } else if (jr$type == "GETBUNDLE") {
        bundleName <- jr[["name"]]
        bundleSess <- jr[["session"]]
        if (debugLevel > 2) {
          cat("Requested bundle:", bundleName, ",session:", bundleSess, "\n")
        }

        err <- NULL
        if (debugLevel > 3) {
          cat("Convert bundle to S3 format", bundleName, "\n")
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
        if (rstudioapi::isAvailable()) {
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
            cat(length(ssffTrackNmsInUse), " track definitions in use:\n")
            for (sfInU in ssffTrackNmsInUse) {
              cat(sfInU, " ")
            }
            cat("\n")
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
            mf <- tryCatch(file(ssffFilePath, "rb"), error = function(e) {
              err <<- e
            })

            if (is.null(err)) {
              mfData <- readBin(mf, raw(), n = file.info(ssffFilePath)$size)
              if (inherits(mfData, "error")) {
                err <- mfData
                break
              }
            } else {
              break
            }

            mfDataBase64 <- base64enc::base64encode(mfData)
            encoding <- "BASE64"
            ssffDatObj <- list(
              encoding = encoding,
              data = mfDataBase64,
              fileExtension = ssffFileExt
            )
            ssffFiles[[length(ssffFiles) + 1]] <- ssffDatObj
            close(mf)
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
          cat("Error: ", errMsg, "\n")
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
          if (debugLevel >= 8) cat(responseBundleJSON, "\n")
          cat("Sent bundle containing", length(ssffFiles), "SSFF files\n")
        }
        err <- NULL

      } else if (jr[["type"]] == "SAVEBUNDLE") {
        jrData <- jr[["data"]]
        jrAnnotation <- jrData[["annotation"]]
        bundleSession <- jrData[["session"]]
        bundleName <- jrData[["annotation"]][["name"]]

        if (debugLevel > 3) {
          cat("Save bundle ", bundleName, " from session ", bundleSession, "\n")
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
                cat("Writing SSFF track to file: ", sp, "\n")
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
          cat("Error: ", m, "\n")
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
        cat("reindeer websocket service closed by EMU-webApp\n")
      }
    }

    ws$onMessage(serverReceive)
    ws$onClose(serverClosed)
  }

  # Stop any existing servers
  httpuv::stopAllServers()

  # Print server info
  cli::cli_h2("Starting reindeer EMU-webApp server")
  cli::cli_alert_info("Navigate your browser to: {.url http://localhost:{port}}")
  cli::cli_alert_info("Server connection URL: {.url ws://localhost:{port}}")
  cli::cli_alert_info("To stop the server:")
  cli::cli_ul(c(
    "Press the 'clear' button in the EMU-webApp",
    "Close/reload the webApp in your browser",
    "Call {.code httpuv::stopAllServers()} in R"
  ))

  # Create server app
  app <- list(
    call = httpRequest,
    onHeaders = onHeaders,
    onWSOpen = serverEstablished
  )

  # Start server
  httpuv::startServer(host = host, port = port, app = app)

  # Auto-open browser
  if (length(autoOpenURL) != 0 && autoOpenURL != "") {
    viewer <- getOption("viewer")

    if (useViewer & rstudioapi::isAvailable()) {
      webApp_path <- get_webapp_dir()

      if (!dir.exists(webApp_path)) {
        cli::cli_abort(c(
          "EMU-webApp directory not found at: {.path {webApp_path}}",
          "i" = "Please ensure the EMU-webApp is available at the expected location"
        ))
      }

      # Prepare base path for RStudio
      base_path <- "/"
      if (rstudioapi::isAvailable()) {
        if (rstudioapi::translateLocalUrl(paste0("http://localhost:", port, "/")) !=
            paste0("http://localhost:", port, "/")) {
          base_path <- paste0("/", rstudioapi::translateLocalUrl(paste0("http://localhost:", port, "/")))
        }
      }

      # Modify index.html for local serving
      index_html <- readr::read_file(file.path(webApp_path, "index.html"))
      index_html_new <- stringr::str_replace(
        index_html,
        pattern = "<base href=\"/EMU-webApp/\">",
        replacement = paste0("<base href=\"", base_path, "\">")
      )
      index_html_new <- stringr::str_replace(
        index_html_new,
        pattern = "manifest=\"manifest.appcache\"",
        replacement = ""
      )

      # Write modified index.html to temp location
      temp_index <- tempfile(fileext = ".html")
      readr::write_file(x = index_html_new, file = temp_index)

      # Open in viewer or browser
      if (!is.null(viewer)) {
        viewer(paste0(
          "http://127.0.0.1:", port, "/?autoConnect=true",
          "&serverUrl=", stringr::str_replace(
            rstudioapi::translateLocalUrl(paste0("http://127.0.0.1:", port), absolute = TRUE),
            "http", "ws"
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

#' Get EMU-webApp directory path
#'
#' Returns the path to the revised EMU-webApp distribution directory.
#' The function looks for the EMU-webApp in this order:
#' 1. Option: getOption("reindeer.emuWebApp.dir")
#' 2. Environment variable: EMU_WEBAPP_DIR
#' 3. Package installation: system.file("EMU-webApp/dist", package = "reindeer")
#' 4. Default fallback: ../EMU-webApp/dist relative to package location
#'
#' @return Path to EMU-webApp dist directory
#' @keywords internal
get_webapp_dir <- function() {
  # Check option first
  webapp_path <- getOption("reindeer.emuWebApp.dir")

  # Fall back to environment variable
  if (is.null(webapp_path) || webapp_path == "") {
    webapp_path <- Sys.getenv("EMU_WEBAPP_DIR", unset = "")
  }

  # Fall back to package-installed version
  if (webapp_path == "") {
    webapp_path <- system.file("EMU-webApp/dist", package = "reindeer")
  }

  # Final fallback: relative to package location
  if (webapp_path == "" || !dir.exists(webapp_path)) {
    pkg_path <- system.file(package = "reindeer")
    webapp_path <- file.path(dirname(pkg_path), "EMU-webApp", "dist")
  }

  # Validate that the path exists
  if (!dir.exists(webapp_path)) {
    cli::cli_abort(c(
      "EMU-webApp directory not found.",
      "i" = "Tried: {.path {webapp_path}}",
      "i" = "Set a custom path via {.code options(reindeer.emuWebApp.dir = '/path/to/EMU-webApp/dist')}",
      "i" = "Or set environment variable: {.code Sys.setenv(EMU_WEBAPP_DIR = '/path/to/EMU-webApp/dist')}"
    ))
  }

  return(webapp_path)
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
.emu_suffixes <- list(
  session = "_ses",
  bundle_dir = "_bndl",
  annotation = "_annot"
)

#' Get session suffix
#' @keywords internal
get_session_suffix <- function() .emu_suffixes$session

#' Get bundle directory suffix
#' @keywords internal
get_bundle_dir_suffix <- function() .emu_suffixes$bundle_dir

#' Get annotation suffix
#' @keywords internal
get_annotation_suffix <- function() .emu_suffixes$annotation

#' Guess MIME type from file extension
#'
#' Replicates emuR:::guess_type for common file types used in EMU.
#'
#' @param path File path
#' @return MIME type string
#' @keywords internal
#' @note Replaces emuR:::guess_type to avoid internal dependency
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
