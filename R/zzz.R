# ==============================================================================
# PACKAGE INITIALIZATION
# ==============================================================================

.onLoad <- function(libname, pkgname) {
  # S7 automatically handles print/summary via S3 dispatch
  # No special registration needed
  invisible()
}
