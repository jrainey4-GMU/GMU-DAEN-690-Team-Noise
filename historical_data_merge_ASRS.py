import pandas as pd
import numpy as np
import os
from openpyxl import load_workbook
import openpyxl
import re

# loop for the ASRS data to merge all files
asrsFolder = '\\ASRSdataOriginal'
asrs = pd.DataFrame()
for entry in os.scandir(directory + asrsFolder):
    temp = pd.read_csv(entry.path)
    #removing useless datatype header and blank row
    temp.drop([0, 2], axis=0)
    asrs = asrs.append(temp)

#saves merged, unfiltered data
asrs.to_csv("DATA_ASRS_merged.csv")

asrs = asrs[['State Reference','Date','Synopsis','Narrative']]
# filtering the data records for possible drone reports to be used for cross val
asrs['Narrative'] = asrs['Narrative'].str.upper()
asrs = asrs[(asrs['Narrative'].str.contains("UAV|UAS|UNIDENTIFIED|DRONE|BIRD|UFO|BALLOONS|BALLOON")) | (asrs['Synopsis'].str.contains("UAV|UAS|UNIDENTIFIED|DRONE|BIRD|UFO|BALLOONS|BALLOON"))]
asrs = asrs[asrs['State Reference']!="US"]
asrs = asrs[asrs['State Reference']!="FO"]
asrs = asrs[asrs['State Reference']!="BC"]
asrs = asrs[asrs['State Reference']!="PR"]
asrs = asrs[asrs['State Reference']!="NS"]
asrs = asrs[asrs['State Reference'].notnull()]

#standardizing the state names into uppercase and matching states
state_lookup = pd.read_excel("DATA_standardization_lookups.xlsx", dtype=str, sheet_name="STATE")
s_keys = state_lookup['raw']
s_vals = state_lookup['lookup']
state = {s_keys[i]: s_vals[i] for i in range(len(s_keys))}
asrs['State Reference'] = asrs['State Reference'].apply(lambda x: state[x])
#renaming column name
asrs.rename(columns={'State Reference':'State'}, inplace=True)
#extrating year and month into sep. cols
asrs['Year'] = asrs['Date'].apply(lambda x: str(x)[0:4])
asrs['Month'] = asrs['Date'].apply(lambda x: str(x)[-2:])
#sorting and reindexing
asrs = asrs.sort_values(by=['Year','Month','State'], ascending=True).reset_index(drop=True)
#saving the filtered data for use in cros val
asrs.to_csv("DATA_ASRS_filtered.csv")