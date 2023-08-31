import pandas as pd
import wbgapi as wb
from geopy.geocoders import Nominatim
import numpy as np


geolocator = Nominatim(user_agent="geoapiExercises")

def coord2country(x,y):
    location = geolocator.reverse(str(x)+","+str(y))
    if location == None:
        return np.nan, np.nan, np.nan, np.nan
    address = location.raw['address']
    
    city = address.get('city', '')
    state = address.get('state', '')
    country = address.get('country', '')
    zipcode = address.get('postcode')

    return country, state, city, zipcode

def lightNameParse(name):
    satelite = name[:3]
    year = name[3:7]
    region = name[8:]

    return satelite, year, region

# Note: This function is vectorized.
def coords2dist(x1,y1,x2,y2):
    return np.sqrt(np.power(x1-x2,2) + np.power(y1-y2,2))

lights = pd.read_csv("F142000 Africa +ME.csv", header = None)
lights.columns = ['longitude', 'latitude', 'light_val']



# Processing process: 
#   1. Load in all the WDI controls
#   2. Iterate through the csv of all the names of lights data
#   3. For each iteration, load the neccesary population density file based on parsing the name of the lights csv
#   4. Match each location coord in the nighttime lights file with the closest pop density number
#   5. Either go row by row to match everything in which case also input the country level control data as well or
#      merge the pop and lights with inbuilt functions and then add in the contry level controls


# Version 1: Line by Line

#for index, row in lights.iterrows():
    
lights["city"] = np.nan

lights["state"] = np.nan

lights["country"] = np.nan

lights["zipcode"] = np.nan


for irow, row in lights.iterrows():
    print(lights.iloc[irow]['longitude'], lights.iloc[irow]['latitude'])
    country, state, city, zipcode = coord2country(lights.iloc[irow]['longitude'], lights.iloc[irow]['latitude'])
    lights.at[irow, 3] = city
    lights.at[irow, 4] = state
    lights.at[irow, 5] = country
    lights.at[irow, 6] = zipcode
    print(f'{city}, {state}, {country}, {zipcode}')
    
    print(lights.iloc[irow])
    print("-----------------------------------------------")


# Version 2: In-built functions









