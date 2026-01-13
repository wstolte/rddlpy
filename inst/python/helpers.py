
# helpers.py (schets)
import ddlpy
import pandas as pd

def measurements_by_index(index_code, start_date, end_date,
                          grootheid="WATHTE", groepering="NVT", hoedanigheid="NAP"):
    locs = ddlpy.locations()
    sel = locs.loc[
        locs.index.isin([index_code]) &
        locs["Grootheid.Code"].isin([grootheid]) &
        locs["Groepering.Code"].isin([groepering]) &
        locs["Hoedanigheid.Code"].isin([hoedanigheid])
    ]
    if len(sel) < 1:
        raise ValueError(f"Geen match voor {index_code} met filters {grootheid}/{groepering}/{hoedanigheid}.")
    row = sel.iloc[0]
    return ddlpy.measurements(row, start_date=start_date, end_date=end_date)
