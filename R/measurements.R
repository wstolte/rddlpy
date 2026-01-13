


#' Haal metingen op voor ??n locatie (via indexcode)
#'
#' Deze wrapper zoekt eerst de locaties op en roept daarna `ddlpy.measurements()` aan
#' met een enkele rij (pandas Series) uit het `ddlpy.locations()`-dataframe.
#'
#' @param index_code Character, de index (zoals "HOEKVHLD") van de gewenste locatie
#'   in het ddlpy-locaties dataframe.
#' @param start_date, end_date Datums/tijden; `Date`, `POSIXct`, of character (ISO-8601).
#' @param grootheid Character filter; default "WATHTE" (waterstand). Pas aan indien nodig.
#' @param groepering Character filter; default "NVT" (tijdreeks).
#' @param hoedanigheid Character filter; default "NAP" (referentievlak).
#' @param convert Logisch; converteren naar R data.frame (TRUE) of een Python object teruggeven.
#' @return Een `data.frame` met meetwaarden (o.a. kolom `Meetwaarde.Waarde_Numeriek`), of het Python object als `convert = FALSE`.
#' @examples
#' \dontrun{
#' m <- ddl_measurements(
#'   "HOEKVHLD",
#'   start_date = as.POSIXct("2023-01-01"),
#'
#'   end_date   = as.POSIXct("2023-01-15")
#' )
#' }
#' @export
ddl_measurements <- function(index_code,
                             start_date,
                             end_date,
                             grootheid = "WATHTE",
                             groepering = "NVT",
                             hoedanigheid = "NAP",
                             convert = TRUE) {

  # Vereiste packages
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("'reticulate' is vereist; installeer met install.packages('reticulate')", call. = FALSE)
  }

  # Import ddlpy (lazy)
  ddl <- reticulate::import("ddlpy", delay_load = TRUE)

  # Haal locaties op (pandas.DataFrame)
  locs <- ddl$locations()

  # Bouw filters (pandas-style via reticulate)
  bool_station      <- locs$index$isin(list(index_code))
  bool_grootheid    <- locs$`__getitem__`("Grootheid.Code")$isin(list(grootheid))
  bool_groepering   <- locs$`__getitem__`("Groepering.Code")$isin(list(groepering))
  bool_hoedanigheid <- locs$`__getitem__`("Hoedanigheid.Code")$isin(list(hoedanigheid))

  # Combineer masks en subset
  mask      <- bool_station$`__and__`(bool_grootheid)$`__and__`(bool_groepering)$`__and__`(bool_hoedanigheid)
  selected  <- locs$loc(mask)

  # Geen match?
  if (reticulate::py_len(selected) < 1L) {
    # Tip: toon wat er WEL is voor dit station
    available <- locs$loc(locs$index$isin(list(index_code)))
    avail_r   <- tryCatch(reticulate::py_to_r(available), error = function(e) NULL)

    msg <- sprintf(
      "Geen locatie/parameter gevonden voor index_code='%s' (filters: %s/%s/%s).\n" ,
      index_code, grootheid, groepering, hoedanigheid
    )
    if (!is.null(avail_r) && nrow(avail_r) > 0) {
      msg <- paste0(
        msg,
        "Beschikbare combinaties voor dit station (eerste 5 rijen):\n",
        utils::capture.output(print(utils::head(avail_r, 5L))), collapse = "\n"
      )
    }
    stop(msg, call. = FALSE)
  }

  # Neem ??n rij (pandas Series) zoals vereist door ddlpy.measurements()
  row <- selected$iloc[[0L]]

  # Datumconversie: reticulate kan POSIXct/Date meestal automatisch, maar expliciet kan ook
  py_start <- reticulate::r_to_py(start_date)
  py_end   <- reticulate::r_to_py(end_date)

  # Aanroep naar ddlpy.measurements(row, start_date=..., end_date=...)
  py_meas <- ddl$measurements(row, start_date = py_start, end_date = py_end)

  # Converteren naar R data.frame indien gevraagd
  if (isTRUE(convert)) {
    res <- reticulate::py_to_r(py_meas)
    if (!is.data.frame(res)) {
      res <- as.data.frame(res, stringsAsFactors = FALSE)
    }
    return(res)
  } else {
    return(py_meas)
  }
}
