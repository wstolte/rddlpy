
#' Installeer de Python package 'ddlpy' in de actieve reticulate-omgeving
#'
#' Deze functie gebruikt `reticulate::py_install()` om `rws-ddlpy` te installeren.
#' Eventueel kun je extra's zoals 'netcdf' en 'examples' meegeven.
#'
#' @param envname Optionele naam van een conda- of virtualenv-omgeving.
#' @param extras Vector van extra componenten, standaard c("netcdf","examples").
#' @param method Installatiemethode ("auto", "virtualenv", "conda").
#' @return Onzichtbaar `NULL`.
#' @examples
#' \dontrun{
#' install_ddlpy()
#' }
#' @export
install_ddlpy <- function(envname = NULL, extras = c("netcdf","examples"), method = "auto") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is vereist.")
  }
  pkg <- paste0("rws-ddlpy", if (length(extras) > 0) paste0("[", paste(extras, collapse=","), "]") else "")
  reticulate::py_install(packages = pkg, envname = envname, method = method)
  invisible(NULL)
}
