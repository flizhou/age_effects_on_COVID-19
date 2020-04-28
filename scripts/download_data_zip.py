import requests, zipfile, io, json
import pandas as pd

def get_files(code):
    """
    A wrapper function to obtain .csv files from the world bank.
    
    Parameters:
    -----------
    code: str
        The code of the indicator.

    Returns:
    --------
    str
        downloaded .csv filename    
    """
    r = requests.get('http://api.worldbank.org/v2/en/indicator/' + code + '?downloadformat=csv', stream=True)
    with zipfile.ZipFile(io.BytesIO(r.content)) as z:
        filename = z.namelist()[1]
        z.extract(filename, '../data/raw_data')
        return filename

# main
files = {}

files['age_1564'] = get_files("SP.POP.1564.TO.ZS")
files['age_65up'] = get_files("SP.POP.65UP.TO.ZS")
files['smoking'] = get_files("SH.PRV.SMOK")
files['air_pollution'] = get_files("EN.ATM.PM25.MC.M3")
files['medical_bed'] = get_files("SH.MED.BEDS.ZS")
files['physicians'] = get_files("SH.MED.PHYS.ZS")
files['nurses_midwives'] = get_files("SH.MED.NUMW.P3")
files['population_total'] = get_files("SP.POP.TOTL")
files['population_density'] = get_files("EN.POP.DNST")

with open('../data/clean_data/filenames.json', 'w') as json_file:
    json.dump(files, json_file)