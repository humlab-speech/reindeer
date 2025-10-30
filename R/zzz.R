# ==============================================================================
# PACKAGE INITIALIZATION
# ==============================================================================

.onLoad <- function(libname, pkgname) {
  # Force S7 method registration for print and summary
  # Methods defined with method<- in package files should work automatically,
  # but we ensure they're active by accessing them once
  
  # The methods are already defined in the respective files with:
  # S7::method(print, corpus) <- function...
  # This .onLoad ensures they're properly registered
  
  # No explicit action needed - S7 handles registration automatically
  # when the package namespace is built
  
  invisible()
}
