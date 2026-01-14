
#' Haal metingen op voor één locatie (code of naam)
#'
#' Filtert `ddlpy.locations()` op station + metadata en geeft vervolgens
#' **één rij** (pandas Series) door aan `ddlpy.measurements()` — conform ddlpy-usage
#' (subset → iloc[0] → measurements).
#'
#' @param station Character; stationcode (bv. "HOEKVHLD") of naam (bv. "Hoek van Holland").
#' @param start_date,end_date Datum/tijd; `Date`, `POSIXct` of ISO-8601 string (bijv. tz = "UTC").
#' @param grootheid Character; default "WATHTE".
#' @param groepering Character; default "NVT".
#'   Geef **""** (lege string) om expliciet op leeg veld te filteren; geef `NULL` voor geen filter.
#' @param hoedanigheid Character; default "NAP".
#' @param procestype Character; default "meting".
#' @param convert Logical; TRUE = R `data.frame`, FALSE = Python object.
#' @return `data.frame` met metingen, of Python object bij `convert = FALSE`.
#' @export
ddl_measurements <- function(station,
                             start_date,
                             end_date,
                             grootheid    = "WATHTE",
                             groepering   = "NVT",
                             hoedanigheid = "NAP",
                             procestype   = "meting",
                             convert      = TRUE) {
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("'reticulate' is vereist; voer install.packages('reticulate') uit.", call. = FALSE)
  }
  
  # 0) Gebruik convert = FALSE zodat Python-objecten Python blijven tot py_to_r()
  ddl <- reticulate::import("ddlpy", convert = FALSE)
  
  # 1) Haal locaties (pandas DataFrame)
  locs_py <- ddl$locations()
  
  # 1a) Voeg kolom 'Code' toe vanuit de index (string)
  #     Dit voorkomt het 'duplicated index' / 'rownames not set' probleem aan de R-kant.
  locs_py$`__setitem__`("Code", locs_py$index$astype("str"))
  
  # 1b) Converteer nu in één keer naar R
  locs_r <- reticulate::py_to_r(locs_py)
  
  # 1c) Stationcode of naam resolven
  code_vec <- as.character(locs_r$Code)                       # altijd aanwezig nu
  naam_vec <- if ("Naam" %in% names(locs_r)) as.character(locs_r$Naam) else rep(NA_character_, nrow(locs_r))
  
  if (station %in% code_vec) {
    station_code <- station
  } else {
    nm <- which(tolower(naam_vec) == tolower(station))
    if (length(nm) >= 1L) station_code <- code_vec[nm[1L]] else
      stop(sprintf("Station '%s' niet gevonden als code of naam.", station), call. = FALSE)
  }
  
  # 2) Procestype-kolom robuust bepalen (case-insensitive match)
  pt_candidates <- c(
    "ProcesType","Proces.Type","Proces","Proces.Code",
    "ProcesType.Code","ProcesType.Omschrijving","proces_type","ProcesTypeOmschrijving"
  )
  pt_col <- pt_candidates[pt_candidates %in% names(locs_r)]
  has_pt <- length(pt_col) >= 1L
  if (has_pt) {
    pt_col <- pt_col[[1L]]
    pt_vec <- tolower(as.character(locs_r[[pt_col]]))
    want_pt <- tolower(procestype)
  } else {
    pt_vec <- rep(NA_character_, nrow(locs_r))
    want_pt <- tolower(procestype)
  }
  
  # 3) Vector-condities (NB: géén &&)
  cond_station      <- (code_vec == station_code)
  cond_grootheid    <- if ("Grootheid.Code"    %in% names(locs_r)) locs_r[["Grootheid.Code"]]    %in% grootheid    else TRUE
  cond_hoedanigheid <- if ("Hoedanigheid.Code" %in% names(locs_r)) locs_r[["Hoedanigheid.Code"]] %in% hoedanigheid else TRUE
  
  # groepering: "" betekent expliciet filteren op lege string (of NA); NULL = geen filter
  if ("Groepering.Code" %in% names(locs_r)) {
    gcol <- as.character(locs_r[["Groepering.Code"]])
    if (is.null(groepering)) {
      cond_groepering <- TRUE
    } else {
      non_empty_vals <- groepering[nzchar(groepering)]
      wants_empty    <- any(groepering == "")
      cond_groepering <- rep(FALSE, nrow(locs_r))
      if (length(non_empty_vals)) cond_groepering <- cond_groepering | (gcol %in% non_empty_vals)
      if (wants_empty)           cond_groepering <- cond_groepering | (is.na(gcol) | gcol == "")
    }
  } else {
    cond_groepering <- TRUE
  }
  
  cond_procestype <- if (has_pt) (pt_vec == want_pt) else TRUE
  
  rows <- which(cond_station & cond_grootheid & cond_groepering & cond_hoedanigheid & cond_procestype)
  
  if (length(rows) < 1L) {
    show_cols <- intersect(c("Code","Naam","Grootheid.Code","Groepering.Code","Hoedanigheid.Code", pt_col), names(locs_r))
    avail <- try({
      if (length(show_cols)) subset(locs_r, code_vec == station_code, select = show_cols)
      else subset(locs_r, code_vec == station_code)
    }, silent = TRUE)
    
    msg <- sprintf(
      "Geen match voor station '%s' (code %s) met filters grootheid=%s, groepering=%s, hoedanigheid=%s, procestype=%s.\n",
      station, station_code,
      paste(grootheid, collapse = ","),
      if (is.null(groepering)) "<geen filter>" else paste(sprintf('"%s"', groepering), collapse = ","),
      paste(hoedanigheid, collapse = ","),
      procestype
    )
    
    if (!inherits(avail, "try-error") && is.data.frame(avail) && nrow(avail) > 0) {
      msg <- paste0(
        msg, "Beschikbare combinaties (eerste 5):\n",
        paste(utils::capture.output(print(utils::head(avail, 5L))), collapse = "\n"), "\n"
      )
      if (has_pt) {
        uniq_pt <- unique(pt_vec[code_vec == station_code])
        msg <- paste0(msg, "Gevonden procestypes: ", paste(uniq_pt, collapse = ", "), ".\n")
      } else {
        msg <- paste0(msg, "Let op: geen procestype-kolom aangetroffen; kolommen: ",
                      paste(names(locs_r), collapse = ", "), ".\n")
      }
    }
    stop(msg, call. = FALSE)
  }
  
  # 4) Neem de eerste match als pandas Series via iloc (0-based index)
  i0  <- as.integer(rows[1] - 1L)
  row <- locs_py$iloc[[i0]]  # row <- locs_py$iloc[[i0]]
  # if (reticulate::py_is_none(row)) {
  #   stop("Interne selectie leverde None i.p.v. pandas Series; controleer filters/index.", call. = FALSE)
  # }
  
  # 5) Aanroep naar ddlpy.measurements(Series, ...)
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
