
# Helpers to make ddlpy calls easier from R via reticulate
import ddlpy
import pandas as pd
from typing import Optional

def get_locations(catalog_filter: Optional[dict] = None, use_cache: bool = True) -> pd.DataFrame:
    return ddlpy.locations(catalog_filter=catalog_filter, use_cache=use_cache)

def measurements_by_index(index_code: str, start_date, end_date) -> pd.DataFrame:
    """Look up location row by index and call measurements."""
    locs = ddlpy.locations()
    if index_code not in locs.index:
        raise KeyError(f"index '{index_code}' not found in locations index")
    row = locs.loc[index_code]
    return ddlpy.measurements(row, start_date=start_date, end_date=end_date)
