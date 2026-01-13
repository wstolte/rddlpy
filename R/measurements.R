
#' Haal metingen op voor één locatie (via indexcode)
#'
#' @param index_code Character, bv. "HOEKVHLD"
#' @param start_date, end_date Date/POSIXct of ISO-8601 character
#' @param grootheid Character filter; default "WATHTE"
#' @param groepering Character filter; default "NVT"
#' @param hoedanigheid Character filter; default "NAP"
#' @param convert Logical; TRUE = data.frame
#' @export
ddl_measurements <- function(index_code,
                             start_date,
                             end_date,
                             grootheid = "WATHTE",
                             groepering = "NVT",
                             hoedanigheid = "NAP",
                             convert = TRUE) {
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("'reticulate' is vereist; install.packages('reticulate')", call. = FALSE)
  }
  ddl <- reticulate::import("ddlpy", delay_load = TRUE)
  
  locs <- ddl$locations()
  bool_station      <- locs$index$isin(list(index_code))
  bool_grootheid    <- locs$`__getitem__`("Grootheid.Code")$isin(list(grootheid))
  bool_groepering   <- locs$`__getitem__`("Groepering.Code")$isin(list(groepering))
  bool_hoedanigheid <- locs$`__getitem__`("Hoedanigheid.Code")$isin(list(hoedanigheid))
  
  mask     <- bool_station$`__and__`(bool_grootheid)$`__and__`(bool_groepering)$`__and__`(bool_hoedanigheid)
  selected <- locs$loc(mask)
  
  if (reticulate::py_len(selected) < 1L) {
    available <- locs$loc(locs$index$isin(list(index_code)))
    avail_r   <- tryCatch(reticulate::py_to_r(available), error = function(e) NULL)
    msg <- sprintf("Geen match voor '%s' met filters %s/%s/%s.\n", index_code, grootheid, groepering, hoedanigheid)
    if (!is.null(avail_r) && nrow(avail_r) > 0) {
      msg <- paste0(msg, "Beschikbare combinaties (eerste 5):\n",
                    paste(utils::capture.output(print(utils::head(avail_r, 5L))), collapse = "\n"))
    }
    stop(msg, call. = FALSE)
  }
  
  row     <- selected$iloc[[0L]]
  py_meas <- ddl$measurements(row,
                              start_date = reticulate::r_to_py(start_date),
                              end_date   = reticulate::r_to_py(end_date))
  
  if (isTRUE(convert)) {
    res <- reticulate::py_to_r(py_meas)
    if (!is.data.frame(res)) res <- as.data.frame(res, stringsAsFactors = FALSE)
    return(res)
  } else {
    return(py_meas)
  }
}
