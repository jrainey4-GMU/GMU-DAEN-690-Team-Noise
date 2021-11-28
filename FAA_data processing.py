import pandas as pd
import numpy as np
import re
import datetime

########## import data files.  ##################################
# for the file importing to a. THe column with the raw text should be named 'Event' or
# the script below will need to be edited.
a = pd.read_csv(#file path for the merged filed output)

# this is the import used with the notified LEO data
l = pd.read_excel("DATA_standardization_lookups.xlsx", dtype=str, sheet_name="LEO")

# import the FAA AC data to be used to import/join new data to our data set
columns = ['Aircraft Type Designator', 'Manufacturer', 'Class', 'Engine Type', 'Engine Number', 'FAA Weight', 'CWT', 'Air Carrier']
ac_data = pd.read_excel("DATA_FAA_aircrafts.xlsx", sheet_name = 'Aircraft Data',
                        usecols = columns)

##############################################################################

# converts the raw text to upper case to remove any possible regex issues
a['Event'] = a['Event'].str.upper()
a['City'] = a['City'].str.strip().str.upper()
a['State'] = a['State'].str.strip().str.upper()

# extracts every unique AC designator from the FAA data file
unique_ac = ac_data['Aircraft Type Designator'].unique()

#starting variable for building the regex pattern
#B757 is not in the FAA data file
ac_pattern = r'\b(B757|'
# looping through the unique AC list to create the regex pattern string
for i in range(len(unique_ac)):
    #adds the closing ")" to the pattern instead of the or |
    #need to skip adding the FORT plane so that it is on the end ot pattern and processed last
    #locations for Military FORTs are causing false positives.
    if i == len(unique_ac) - 1:
        ac_pattern += unique_ac[i].strip() + r"|HELICOPTER|HELI|HELO|HAWK)\b"
    elif (unique_ac[i] == "FORT") or (unique_ac[i] == "HAWK"):
        pass
    else:
        ac_pattern += unique_ac[i].strip() + "|"

# creates regex pattern from a list of colors. to be use to extract color data points
color_pattern = "("
colors = ['red', 'orange', 'yellow', 'green', 'cyan', 'azure', 'blue',
          'violet', 'magenta', 'rose', 'grey', 'gray', 'black', 'tan',
          'white', 'purple', 'silver']
for i in range(len(colors)):
    if i == len(colors) - 1:
        color_pattern += colors[i].strip() + ")"
    else:
        color_pattern += colors[i].strip() + "|"

# cats is a dictionary used to hold regex search patterns for the date we want to extract
# the keys are turned into colum headers
cats = {
    ## FAA_ops removes all the possible forms of the 'FROM FAA OPS: tag in the text blob
    ## it allows for easier removal and only selecting the first one when there are multipl in a single blob
    #"FAA_ops": '[\w\s]*FAA[\s]*OPS\s*:\s*([\w\s,]*/*){2,3}([\w\s,.]*:*)',
    "Alert_For": '[UAS\s]*MOR Alert for (.*?)(?=\s{1})', #'[UAS\s]*MOR Alert for (.*?)(?=\S*:)',
    "Summary": 'Summary:\s*(.*)',
    "Notified": '\.([\s*\w*]*)(?=NOTIFIED)',
    "Advise": '(ARTCC|TRACON|ATCT|APCH|ARPT|ACFT)', #'(?<=/)([\s*\w*]*)(ARTCC|TRACON|ATCT|APCH|ARPT|ACFT){1}', #'/?([\s*\w*]*)(?=ADVISED)'#'/?([\s*\w*]*)(?=(ADVIS[E]?[ED]?|REPORTED|RECEIVED){1})'
    'time': '(\d{4})\w',
    'timezone': '\d{4}(\w)',
    'Aircraft': ac_pattern,
    'UAScolor': color_pattern,
    'altitude': "(\d{0,4},?\d{3,})\s*F[E|O]*T"
}

# using LEO data file to create a dictionary and regex pattern
# both will be used to extract a standardized name for who was notified
keys = l['Notified LEs']
vals = l['Standardized Lookup']
# creates a dictionary for the standardized LEO categories
leo = {keys[i]: vals[i] for i in range(len(keys))}
# creating the regex LEO pattern that will be used for searching and extracting from the raw text
leo_pattern = '('
for i in range(len(keys)):
    if i == len(keys) - 1:
        leo_pattern += keys[i].strip() + ")"
    else:
        leo_pattern += keys[i].strip() + "|"

################################################################
########## functions to run the regex searches in combination with pandas apply
###############################################################
def reggie(pattern, string):
    '''
    pattern = regex search or match pattern
    string = string to search with the regex
    '''
    # conditional to be used to remove the PRELIM INFO: tags or the similar variations
    if pattern == 'Summary:\s*(.*)':
        # print(string)
        # try to find Summary
        try:
            hit = re.search(pattern, string, re.IGNORECASE).group(1).strip()
            return hit
        # if summary failed, fall back to pattern 2
        except:
            try:
                check = re.search("[\w\s]*FAA[\s]*OPS\s*:\s*[\w\s,']*/(.*)", string, re.IGNORECASE).group(1)
                if len(re.findall("/", string)) < 3:
                    # hit = re.search("[\w\s]*FAA[\s]*OPS\s*:\s*([\w\s,']*/*){0,2}([\w\s,.']*)",string, re.IGNORECASE).group(2)
                    hit = re.search("[\w\s]*FAA[\s]*OPS\s*:\s*[\w\s,']*/.*/([\w\s,.'/]*)", string, re.IGNORECASE).group(
                        1)
                    return hit
                else:
                    # hit = re.search("[\w\s]*FAA[\s]*OPS\s*:\s*([\w\s,']*/*){0,3}([\w\s,.'/]*)",string, re.IGNORECASE).group(2)
                    hit = re.search("[\w\s]*FAA[\s]*OPS\s*:\s*[\w\s,']*/.*/.*/([\w\s,.'/]*)", string,
                                    re.IGNORECASE).group(1)
                    return hit
            except:
                return string

    # conditional for the MOR ALERT
    ## elif pattern == '[UAS\s]*MOR Alert for (.*?)(?=\S*:)':
    elif pattern == '[UAS\s]*MOR Alert for (.*?)(?=\s{1})':
        try:
            #removes a few of the mismatches
            hit = re.sub('[ Hazardous and/or Unauthorized UAS Activity|Other]', '',
                         re.search(pattern, string, re.IGNORECASE).group(1).strip())
            # print("try")
            return str(hit)
        except:
            return ""

    # used for the initial Notified extraction that returns the city and extra info
    elif pattern == '\.([\s*\w*]*)(?=NOTIFIED)':
        try:
            hit = re.search(pattern, string).group(1).strip()
            if (re.search('NOT|NO', hit, re.IGNORECASE)) or (re.search('UNK[N]*', hit, re.IGNORECASE)) or (
            re.search('EVASIVE', hit, re.IGNORECASE)):
                return ""
            else:
                # removes some extra text from the result
                hit = re.sub('was', '', hit, flags=re.I)
                hit = re.sub('were', '', hit, flags=re.I)
                
                # second regex search to remove the extra verbiage and only return the agency that was notified
                hit = re.search(leo_pattern, hit).group(1)
                return leo[hit]
        except:
            return ""

    # advised entity extraction
    elif pattern == '(ARTCC|TRACON|ATCT|APCH|ARPT|ACFT)':
        try:
            hit = re.search(pattern, string).group(1)
            return hit
        except:
            return ""

    # aircraft designator extraction
    elif pattern == ac_pattern:
        # attemps to find one the of Air craft names from the FAA airfcraft list
        try:
            hit = re.search("A/C: (.*?)(?=\S*:)", string).group(1)
            return re.search(pattern, hit, re.IGNORECASE).group(1)
        except:
            try:
                hit = re.search(pattern, string, re.IGNORECASE).group(1)
                # looks to see if the result is all letters. If it is all letters, it removes the letter pattern
                # from the search pattern and does a second regex search on the text to see if there is another
                # ac. If there is, it returns the second search. If there isn't, it returns the original hit.
                if re.search('[^\d\W]', hit):
                    try:
                        temp_pattern = pattern.replace(re.search('[^\d\W]', hit, re.IGNORECASE), "")
                        hit = re.search(temp_pattern, string, re.IGNORECASE).group(1)
                        return hit
                    except:
                        return hit
                # if the original hit is alphanumeric then it automatically returns the hit.
                return hit
            except:
                return ""

    # extracts the largest meaasurement provided in feet
    elif pattern == "(\d{0,4},?\d{3,})\s*F[E|O]*T":
        try:
            hit = re.findall(pattern, string)
            altitude = -1
            high = ''
            for i in hit:
                if int(i.replace(",", "")) > altitude:
                    altitude = int(i.replace(",", ""))
                    high = i
            # used to count the number of digits to handle the altitude entries for ranges that don't have a -
            # ie: 600700  instead of 600-700
            digits = 0
            if len(high) >= 5:
                # checking to see if the 6 characters is all numeric.
                # If there is a common then the regex hit can be returned as is
                for i in high:
                    if i.isdigit():
                        digits += 1
                if (digits == 5) and (int(high.replace(",", "")) > 30000):
                    return high.replace(",", "")[-3:]
                elif digits == 6:
                    return high.replace(",", "")[-3:]
                elif digits >= 7:
                    return high.replace(",", "")[-4:]

                else:
                    return high.replace(",", "")
            else:
                return high.replace(",", "")
        except:
            return ""

    # remaining regex searches that don't require specific workflows
    else:
        try:
            hit = re.search(pattern, string, re.IGNORECASE).group(1).strip()
            # print("try")
            return str(hit)
        except:
            try:
                hit = list(filter(None, re.search(pattern, string, re.IGNORECASE)))
                # print("try2")
                return str(hit)
            except:
                # print("except")
                return ""


# used to handle missing cities and states
def location_cleaner(x, dictionary):
    try:
        return dictionary[x]
    except:
        return 'Not Provided'

#### look up the additional aircraft data from the FAA_aircraft file
# function to use with with pandas apply and look up the data
def aircraft(name, col, lookup_df):
    try:
        return lookup_df[lookup_df['Aircraft Type Designator'] == name].iloc[0, col]
    except:
        return ""

# function to replace all fractions, e.g. 1/2, with its decimal format 0.5
def deci(string):
    try:
        return re.sub("(\d)/(\d*)",lambda x: str(round(int(x.group(1))/int(x.group(2)),3)), string)
    except:
        return string

##############################################################
##################################################################

# replace all fractions, e.g. 1/2, with its decimal format 0.5
a['Event'] = a['Event'].apply(lambda x: deci(x))

# applies the regex helper function to every text entry in the loaded dataframe
for k, v in cats.items():
    a[k] = a['Event'].apply(lambda x: reggie(v, x))

# using the function to add the AC data to our output
a['Class'] = a['Aircraft'].apply(lambda x: aircraft(x, 2, ac_data))
a['Engine Type'] = a['Aircraft'].apply(lambda x: aircraft(x, 3, ac_data))
a['Engine Number'] = a['Aircraft'].apply(lambda x: aircraft(x, 4, ac_data))
a['FAA Weight'] = a['Aircraft'].apply(lambda x: aircraft(x, 5, ac_data))
a['Air Carrier'] = a['Aircraft'].apply(lambda x: aircraft(x, 7, ac_data))

# correcting city and state errors
city_lookup = pd.read_excel("DATA_standardization_lookups.xlsx", dtype=str, sheet_name="CITY", converters={'np.NaN':""})
c_keys = city_lookup['raw']
c_vals = city_lookup['lookup']
city = {c_keys[i]: c_vals[i] for i in range(len(c_keys))}
a['City'] = a['City'].apply(lambda x: location_cleaner(x, dictionary = city))

state_lookup = pd.read_excel("DATA_standardization_lookups.xlsx", dtype=str, sheet_name="STATE")
s_keys = state_lookup['raw']
s_vals = state_lookup['lookup']
state = {s_keys[i]: s_vals[i] for i in range(len(s_keys))}
a['State'] = a['State'].apply(lambda x: location_cleaner(x, dictionary = state))

#sorting by date and state
a['Date'] = a['Date'].apply(lambda x: pd.to_datetime(x))
a.sort_values(by=['Date', 'State'], ascending=True, inplace=True).reset_index(drop=True)
a.drop(columns="Unnamed: 0", inplace=True)

#save to csv
a.to_csv("DATA_FAA_split.csv")
