
#' Haal metingen op voor één locatie (via indexcode)
#'
#' @param index_code Character, bv. "HOEKVHLD"
#' @param start_date, end_date Date/POSIXct of ISO-8601 character
#' @param grootheid Character filter; default "WATHTE"
#' @param groepering Character filter; default "NVT"
#' @param hoedanigheid Character filter; default "NAP"
#' @param convert Logical; TRUE = data.frame
#' @export

ddl_measurements <- function(locatie_naam = "Hoek van Holland",
                             start_date, end_date,
                             grootheid = "WATHTE",
                             groepering = "",       # "GETETM2", "GETETBRKD2"
                             procestype = "meting", # "verwachting", "astronomisch", 
                             hoedanigheid = "NAP",
                             convert = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("'reticulate' is vereist; install.packages('reticulate')", call. = FALSE)
  }
  ddl <- reticulate::import("ddlpy", delay_load = TRUE)
  
  # 1) Haal pandas.DataFrame en zet óók even om naar R df
  locs_py <- ddl$locations()
  locs_r  <- reticulate::py_to_r(locs_py)
  
  # In de notebooks/docs is de stationcode de index (en ook in kolom `Code`),
  # we filteren in R:
  rows <- which(
    locs_r$Naam %in% locatie_naam &
      locs_r$`Grootheid.Code`    %in% grootheid &
      locs_r$`Groepering.Code`   %in% groepering &
      locs_r$ProcesType          %in% procestype &
      locs_r$`Hoedanigheid.Code` %in% hoedanigheid
  )
  
  if (length(rows) < 1L) {
    # Toon wat er wél is voor dit station
    avail <- subset(locs_r, Naam == locatie_naam,
                    select = c("Naam","Grootheid.Code","Groepering.Code", "ProcesType", "Hoedanigheid.Code"))
    msg <- sprintf("Geen match voor '%s' met filters %s/%s/%s.\n",
                   locatie_naam, grootheid, groepering, hoedanigheid)
    if (nrow(avail) > 0) {
      msg <- paste0(
        msg, "Beschikbare combinaties (eerste 5):\n",
        paste(utils::capture.output(print(utils::head(avail, 5L))), collapse = "\n")
      )
    }
    stop(msg, call. = FALSE)
  }
  
  # 2) Pak de éérste match als pandas Series via iloc (0-based index!)
  i0  <- as.integer(rows[1] - 1L)
  row <- locs_py$iloc[[i0]]
  
  # 3) Roep ddlpy.measurements aan
  py_meas <- ddl$measurements(
    row,
    start_date = reticulate::r_to_py(start_date),
    end_date   = reticulate::r_to_py(end_date)
  )
  
  if (isTRUE(convert)) {
    res <- reticulate::py_to_r(py_meas)
    if (!is.data.frame(res)) res <- as.data.frame(res, stringsAsFactors = FALSE)
    return(res)
  } else {
    return(py_meas)
  }
}
