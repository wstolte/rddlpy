
# helpers.py (schets)
import ddlpy
import pandas as pd

def measurements_by_index(locatie_naam, start_date, end_date,
                          grootheid="WATHTE", groepering="NVT", hoedanigheid="NAP"):
    locs = ddlpy.locations()
    sel = locs.loc[
        locs.["Naam"].isin([locatie_naam]) &
        locs["Grootheid.Code"].isin([grootheid]) &
        locs["Groepering.Code"].isin([groepering]) &
        locs["ProcesType"].isin([procestype]) &
        locs["Hoedanigheid.Code"].isin([hoedanigheid])
    ]
    if len(sel) < 1:
        raise ValueError(f"Geen match voor {locatie_naam} met filters {grootheid}/{groepering}/{procestype}/{hoedanigheid}.")
    row = sel.iloc[0]
    return ddlpy.measurements(row, start_date=start_date, end_date=end_date)
