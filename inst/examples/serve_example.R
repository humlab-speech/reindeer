#!/usr/bin/env Rscript

# ==============================================================================
# EXAMPLE: Using reindeer::serve() to launch EMU-webApp
# ==============================================================================

# This example demonstrates how to use the serve() function to launch the
# EMU-webApp annotation interface with a reindeer corpus

library(reindeer)

# ------------------------------------------------------------------------------
# Option 1: Serve a demo corpus
# ------------------------------------------------------------------------------

# Load the demo ae corpus
corp <- ae()

# Launch the EMU-webApp server
# This will:
# 1. Start an HTTP server on http://localhost:17890
# 2. Start a WebSocket server for real-time communication
# 3. Automatically open the EMU-webApp in your browser
# 4. Connect the webApp to your R session

serve(corp)

# The browser should automatically open to the EMU-webApp interface.
# You can now:
# - Browse bundles
# - View and edit annotations
# - Play audio
# - View signal tracks (formants, pitch, etc.)
# - Save changes back to the database

# To stop the server:
# - Press the 'clear' button in the EMU-webApp, OR
# - Close the browser tab, OR
# - Run: httpuv::stopAllServers()

# ------------------------------------------------------------------------------
# Option 2: Serve your own corpus
# ------------------------------------------------------------------------------

# Create a corpus from your database
# corp <- corpus("path/to/your_database_emuDB")
# serve(corp)

# ------------------------------------------------------------------------------
# Option 3: Serve with filters
# ------------------------------------------------------------------------------

# Serve only specific sessions
# serve(corp, sessionPattern = "Session.*")

# Serve only specific bundles
# serve(corp, bundlePattern = "msajc.*")

# ------------------------------------------------------------------------------
# Option 4: Serve query results
# ------------------------------------------------------------------------------

# Find all /t/ phonemes and serve only those bundles
# segments <- ask_for(corp, "Phonetic == t")
# serve(corp, seglist = segments)
# # The webApp will automatically navigate to the /t/ segments

# ------------------------------------------------------------------------------
# Option 5: Custom server settings
# ------------------------------------------------------------------------------

# Use a different port
# serve(corp, port = 8080)

# Disable auto-opening browser
# serve(corp, autoOpenURL = "")

# Enable debug output
# serve(corp, debug = TRUE, debugLevel = 2)

# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# 1. The serve() function looks for EMU-webApp in this order:
#    - R option: getOption("reindeer.emuWebApp.dir")
#    - Environment variable: EMU_WEBAPP_DIR
#    - Package installation directory
#    - Default: ../EMU-webApp/dist relative to package
#    Set custom path: options(reindeer.emuWebApp.dir = "/path/to/EMU-webApp/dist")
#
# 2. Changes made in the webApp are saved directly to the database files
#
# 3. The function implements the EMU-webApp websocket protocol v0.0.2
#
# 4. In RStudio, the webApp will open in the Viewer pane by default.
#    Set useViewer=FALSE to open in an external browser instead.
#
# 5. The server runs in the foreground. To stop it and continue working in R,
#    you need to stop the server first (see methods above).
