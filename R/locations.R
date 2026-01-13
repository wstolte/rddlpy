
#' Haal DDL locaties op via ddlpy
#'
#' Wrapper rond `ddlpy.locations()` die een R `data.frame` teruggeeft.
#'
#' @param catalog_filter Optionele lijst (named list) met filters conform ddlpy.
#' @param use_cache Logisch; indien beschikbaar gebruikt ddlpy een cache van de catalogus.
#' @param convert Logisch; convert naar R data.frame (TRUE) of python pandas (FALSE).
#' @return Een `data.frame` met locaties en beschikbare parameters.
#' @examples
#' \dontrun{
#' df <- ddl_locations()
#' head(df)
#' }
#' @export
ddl_locations <- function(catalog_filter = NULL, use_cache = TRUE, convert = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("'reticulate' is vereist")
  ddlpy <- reticulate::import("ddlpy")
  # probeer met parameters; val terug op default signatuur als dat faalt
  ret <- tryCatch({
    ddlpy$locations(catalog_filter = catalog_filter, use_cache = use_cache)
  }, error = function(e) {
    ddlpy$locations()
  })
  if (isTRUE(convert)) {
    reticulate::py_to_r(ret)
  } else {
    ret
  }
}
