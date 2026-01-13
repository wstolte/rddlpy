
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Try to import ddlpy at load; if missing, provide message
  have_reticulate <- requireNamespace("reticulate", quietly = TRUE)
  if (!have_reticulate) {
    packageStartupMessage("Package 'reticulate' is required. Please install it.")
    return(invisible())
  }
  try({
    reticulate::py_config() # triggers python discovery
  }, silent = TRUE)
  # Try import once, but don't error hard at load
  try({
    reticulate::import("ddlpy")
  }, silent = TRUE)
}
