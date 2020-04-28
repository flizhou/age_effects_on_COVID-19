import requests
import pandas as pd

def get_data(code, page=318):
    """
    A wrapper function to obtain data from the world bank API.
    
    Parameters:
    -----------
    code: str
        The code of the indicator.
    page: int (default: 318)
        The number of pages to aggerate.
    
    Returns:
    --------
    pandas.DataFrame    
    """
    data_json = []
    for i in range(1, page):
        request = requests.get("https://api.worldbank.org/v2/country/all/indicator/" + 
                               indicator + "?format=json&page=" + str(i))
        data_json.extend(request.json()[1])
        
    return pd.DataFrame(data_json)

# main
df_1564 = get_data("SP.POP.1564.TO.ZS")
df_1564.to_csv('../data/clean_data/df_1564.csv', index=False)

df_64up = get_data("SP.POP.65UP.TO.ZS")
df_64up.to_csv('../data/clean_data/df_64up.csv', index=False)

df_smok = get_data("SH.PRV.SMOK")
df_smok.to_csv('../data/clean_data/smoking_prevalence_2016.csv', index=False)

df_air = get_data("EN.ATM.PM25.MC.M3")
df_air.to_csv('../data/clean_data/air_polution_2017.csv', index=False)

df_bed = get_data("SH.MED.BEDS.ZS")
df_bed.to_csv('../data/clean_data/hospital_bed_2013.csv', index=False)

df_phy = get_data("SH.MED.PHYS.ZS")
df_phy.to_csv('../data/clean_data/physicians_2015.csv', index=False)

df_nurse = get_data("SH.MED.NUMW.P3")
df_nurse.to_csv('../data/clean_data/nurse_midwivies_2015.csv', index=False)

df_pop = get_data("SP.POP.TOTL")
df_pop.to_csv('../data/clean_data/total_population_2018.csv', index=False)

df_pop_dens = get_data("EN.POP.DNST")
df_pop_dens.to_csv('../data/clean_data/population_density_2016.csv', index=False)