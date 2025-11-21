# serve() Function Summary

## Overview

The `serve()` function launches a local web server that serves a revised version of the EMU-webApp annotation interface for interactive visualization and editing of speech corpora managed by reindeer.

## Implementation

**File**: `R/reindeer_serve.R`

**Created**: 2025-11-19

**Based on**: `emuR::serve()` v2.0.2+

## Key Features

1. **S7 Generic Method**: Implements serve as an S7 generic with a method for the `corpus` class
2. **Revised EMU-webApp**: Serves from `/Users/frkkan96/Documents/src/EMU-webApp/dist/` instead of the standard emuR webApp
3. **Full Protocol Support**: Implements EMU-webApp websocket protocol v0.0.2
4. **Bundle Filtering**: Supports session/bundle pattern filtering and segment list restrictions
5. **RStudio Integration**: Automatically detects and uses RStudio Viewer pane when available

## Architecture

```
User calls serve(corpus)
        ↓
HTTP Server (httpuv) starts on port 17890
        ↓
    ┌─────────────────────────────────────┐
    │  HTTP Request Handler               │
    │  - Serves static EMU-webApp files   │
    │  - Serves media files from bundles  │
    │  - Handles range requests (streaming)│
    └─────────────────────────────────────┘
        ↓
    ┌─────────────────────────────────────┐
    │  WebSocket Server                   │
    │  - GETPROTOCOL                      │
    │  - GETDOUSERMANAGEMENT              │
    │  - GETGLOBALDBCONFIG                │
    │  - GETBUNDLELIST                    │
    │  - GETBUNDLE                        │
    │  - SAVEBUNDLE                       │
    │  - DISCONNECTWARNING                │
    └─────────────────────────────────────┘
        ↓
Browser opens EMU-webApp → WebSocket connection → Real-time annotation
```

## Function Signature

```r
serve(corpus,
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
      debugLevel = 0)
```

## Parameters

- **corpus**: A reindeer corpus object (S7 class)
- **sessionPattern**: Regex to filter sessions (default: all)
- **bundlePattern**: Regex to filter bundles (default: all)
- **seglist**: Optional segment_list to restrict bundles
- **bundleListName**: Optional bundle list name
- **host**: Server host address (default: "127.0.0.1")
- **port**: Server port (default: 17890)
- **autoOpenURL**: URL to auto-open (default: local with autoConnect)
- **browser**: Browser command (from `getOption("browser")`)
- **useViewer**: Use RStudio viewer if available (default: TRUE)
- **debug**: Enable debug output (default: FALSE)
- **debugLevel**: Debug verbosity 0-8 (default: 0)

## Usage Examples

### Basic Usage

```r
library(reindeer)

# Load corpus
corp <- corpus("path/to/database_emuDB")

# Start server
serve(corp)
```

### Filtered Serving

```r
# Serve specific sessions
serve(corp, sessionPattern = "Session0.*")

# Serve specific bundles
serve(corp, bundlePattern = "msajc.*")

# Serve query results only
segments <- ask_for(corp, "Phonetic == t")
serve(corp, seglist = segments)
```

### Custom Configuration

```r
# Different port
serve(corp, port = 8080)

# Disable auto-open
serve(corp, autoOpenURL = "")

# Debug mode
serve(corp, debug = TRUE, debugLevel = 2)
```

## Key Differences from emuR::serve()

1. **Input Type**: Takes `corpus` S7 object instead of `emuDBhandle`
2. **WebApp Location**: Serves from custom location (`/Users/frkkan96/Documents/src/EMU-webApp/dist/`)
3. **Internal Conversion**: Automatically converts `corpus` to `emuDBhandle` for compatibility with emuR functions
4. **CLI Integration**: Uses `cli` package for user-friendly messages
5. **Error Handling**: More informative error messages with cli alerts

## Dependencies

- **httpuv**: HTTP/WebSocket server
- **jsonlite**: JSON serialization for protocol messages
- **base64enc**: Encoding SSFF track data
- **shiny**: Query string parsing
- **readr**: Reading/writing modified index.html
- **stringr**: String manipulation for URL construction
- **rstudioapi**: RStudio Viewer integration
- **emuR**: Core EMU-SDMS functionality (list_bundles, load_bundleAnnotDFsDBI, etc.)

## Helper Functions

### `get_webapp_dir()`

Returns the path to the revised EMU-webApp distribution directory.

**Current implementation**: Hardcoded to `/Users/frkkan96/Documents/src/EMU-webApp/dist/`

**Future enhancement**: Could be made configurable via `options(reindeer.emuWebApp.dir = "path")`

### `get_emuDBhandle(corpus)`

Converts a reindeer `corpus` S7 object to an `emuDBhandle` for compatibility with emuR internal functions.

**Structure**:
```r
list(
  dbName = corpus@dbName,
  basePath = corpus@basePath,
  connection = get_connection(corpus),
  UUID = corpus@.uuid
)
class: "emuDBhandle"
```

## WebSocket Protocol Messages

The function handles these EMU-webApp protocol messages:

1. **GETPROTOCOL**: Returns protocol version "0.0.2"
2. **GETDOUSERMANAGEMENT**: Returns "NO" (no user auth)
3. **GETGLOBALDBCONFIG**: Returns database configuration JSON
4. **GETBUNDLELIST**: Returns list of bundles (optionally with time anchors from seglist)
5. **GETBUNDLE**: Returns bundle data (annotations + SSFF tracks + media URL)
6. **SAVEBUNDLE**: Saves modified annotations and SSFF tracks back to database
7. **DISCONNECTWARNING**: Closes WebSocket connection gracefully

## Data Flow

```
EMU-webApp (Browser)
        ↓ WebSocket
GETBUNDLELIST request
        ↓
R: Query bundlesDf (filtered by patterns/seglist)
        ↓
R: Send JSON response with bundle list
        ↓
EMU-webApp displays bundle list
        ↓ User selects bundle
GETBUNDLE request
        ↓
R: Load annotation JSON from disk
R: Load SSFF files (formants, pitch, etc.)
R: Base64-encode SSFF data
R: Create media file URL
        ↓
R: Send JSON response with bundle data
        ↓
EMU-webApp renders waveform, spectrogram, annotations
        ↓ User edits annotation
SAVEBUNDLE request
        ↓
R: Decode SSFF data from base64
R: Write SSFF files to disk
R: Write annotation JSON to disk
R: Update SQLite cache (DBI transactions)
R: Update bundle list if specified
        ↓
R: Send SUCCESS response
        ↓
EMU-webApp confirms save
```

## Error Handling

The function includes comprehensive error handling:

- **Missing EMU-webApp directory**: Aborts with clear error message
- **Bundle loading errors**: Caught and sent to webApp as ERROR status
- **SSFF file errors**: Individual file errors don't crash the server
- **Annotation save errors**: Rolled back via DBI transactions
- **Invalid seglist**: Validates segment_list structure

## Testing

**Example script**: `inst/examples/serve_example.R`

**Manual testing**:
```r
devtools::load_all()
corp <- ae()  # Load demo database
serve(corp)   # Start server and open browser
# Interact with webApp
httpuv::stopAllServers()  # Stop server
```

**Automated testing**: Not yet implemented (requires headless browser testing)

## Future Enhancements

1. **Configurable WebApp Path**: Make EMU-webApp location configurable via options
2. **Bundle List Support**: Full testing of bundleListName parameter
3. **Authentication**: Optional user management for multi-user scenarios
4. **HTTPS Support**: SSL/TLS for secure connections
5. **Remote Access**: Support for serving to non-localhost addresses
6. **Progress Indicators**: Show bundle loading progress for large corpora
7. **Session Management**: Multiple simultaneous webApp connections
8. **Auto-save**: Periodic auto-save of annotations

## Documentation

- **Roxygen2 docs**: Exported to `man/serve.Rd`
- **Usage examples**: `inst/examples/serve_example.R`
- **This summary**: `SERVE_FUNCTION_SUMMARY.md`

## Known Limitations

1. **Single Connection**: Only one webApp connection at a time
2. **Hardcoded WebApp Path**: Not yet configurable
3. **No Authentication**: No user management system
4. **Local Only**: Default host is 127.0.0.1 (localhost)
5. **Blocking Operation**: Server runs in foreground (expected behavior)

## Compatibility

- **R Version**: Requires R >= 4.1 (S7 dependency)
- **emuR Version**: Compatible with emuR >= 2.0.2
- **EMU-webApp**: Protocol v0.0.2
- **Operating Systems**: Tested on macOS, should work on Linux/Windows

## References

- emuR serve() implementation: `/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/emuR/R/emuR`
- EMU-webApp protocol: `/Users/frkkan96/Documents/src/EMU-webApp/dist/manual/EMU-webAppWebsocketProtocol/`
- httpuv documentation: https://github.com/rstudio/httpuv

## Notes for Maintenance

1. **Keep protocol in sync**: If EMU-webApp protocol changes, update message handlers
2. **Test with emuR updates**: Ensure compatibility when emuR internals change
3. **Monitor httpuv changes**: WebSocket API may evolve
4. **Update EMU-webApp path**: Make configurable when standardizing deployment

## Session Info (Development)

```
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin23.4.0
reindeer version: 0.1.3
emuR version: 2.5.0
httpuv version: 1.6.15
```
