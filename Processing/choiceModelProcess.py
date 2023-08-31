import numpy as np
import pandas as pd
import time 
from geopy.geocoders import Nominatim
from bs4 import BeautifulSoup
import requests as reqy


geolocator = Nominatim(user_agent="geoapiExercises")


def coord2country(x,y):
    start = time.time()

    location = geolocator.reverse(str(x)+","+str(y))
    if location == None:
        return np.nan, np.nan, np.nan, np.nan
    address = location.raw['address']
    
    city = address.get('city', '')
    state = address.get('state', '')
    country = address.get('country', '')

    end = time.time()
    print(f'takes {end - start} seconds, country is {country}, state is {state}, city is {city}')

    return np.array([country, state, city])

def issue_parse(url):
    req = reqy.get(url)
    soup = BeautifulSoup(req.content, 'html.parser')
    
    country = " ".join(soup.find_all("h1")[0].string.split()[3:])
    
    #price_span = soup.findAll(lambda tag: tag.name == "span" and 
    #                          len(tag.attrs) == 1 and tag["class"] == "first_currency")
    price_spans = soup.findAll("span", {"class": "first_currency"})
    price_lower = soup.findAll("span", {"class": "barTextLeft"})
    price_upper = soup.findAll("span", {"class": "barTextRight"})

    
    print(country)
    print(url)
    print("______________________________________________________________")
    
    one_bed_city_median = price_spans[0].string[:-2]
    one_bed_city_lower = ""
    one_bed_city_upper = ""
    if one_bed_city_median != "":
        one_bed_city_median = float(one_bed_city_median.replace(",", ""))
        

    one_bed_out_median = price_spans[1].string[:-2]
    one_bed_out_lower = ""
    one_bed_out_upper = ""
    if one_bed_out_median != "":
        one_bed_out_median = float(one_bed_out_median.replace(",", ""))
        


    three_bed_city_median = price_spans[2].string[:-2]
    three_bed_city_lower = ""
    three_bed_city_upper = ""
    if three_bed_city_median != "":
        three_bed_city_median = float(three_bed_city_median.replace(",", ""))
        
    

    three_bed_out_median = price_spans[3].string[:-2]
    three_bed_out_lower = ""
    three_bed_out_upper = ""
    if three_bed_out_median != "":
        three_bed_out_median = float(three_bed_out_median.replace(",", ""))
        
    if len(price_lower) > 4:
        one_bed_city_lower = price_lower[0].string
        if one_bed_city_lower != None:
            one_bed_city_lower = float(one_bed_city_lower.replace(",", ""))
        
        one_bed_out_lower = price_lower[1].string
        if one_bed_out_lower != None:
            one_bed_out_lower = float(one_bed_out_lower.replace(",", ""))
        
        three_bed_city_lower = price_lower[2].string
        if three_bed_city_lower != None:
            three_bed_city_lower = float(three_bed_city_lower.replace(",", ""))
        
        three_bed_out_lower = price_lower[3].string
        if three_bed_out_lower != None:
            three_bed_out_lower = float(three_bed_out_lower.replace(",", ""))
        

    if len(price_upper) > 4:
        one_bed_city_upper = price_upper[0].string
        if one_bed_city_upper != None:
            one_bed_city_upper = float(one_bed_city_upper.replace(",", ""))
        
        one_bed_out_upper = price_upper[1].string
        if one_bed_out_upper != None:
            one_bed_out_upper = float(one_bed_out_upper.replace(",", ""))
        
        three_bed_city_upper = price_upper[2].string
        if three_bed_city_upper != None:
            three_bed_city_upper = float(three_bed_city_upper.replace(",", ""))
        
        three_bed_out_upper = price_upper[3].string
        if three_bed_out_upper != None:
            three_bed_out_upper = float(three_bed_out_upper.replace(",", ""))
        


    return [country, 
            one_bed_city_median, one_bed_city_lower, one_bed_city_upper,
            three_bed_city_median, three_bed_city_lower, three_bed_city_upper,
            one_bed_out_median, one_bed_out_lower, one_bed_out_upper,
            three_bed_out_median, three_bed_out_lower, three_bed_out_upper]
        


issues_url = "https://www.numbeo.com/property-investment/"
issues_req = reqy.get(issues_url)
soup = BeautifulSoup(issues_req.content, 'html.parser')

issues_list = []
for link in soup.find_all('a'):
    cur = link.get('href')
    if "country_result.jsp?country=" in cur:
        issues_list.append("https://www.numbeo.com/property-investment/" + cur + "&displayCurrency=USD")



df = []
for url in issues_list:
    df.append(issue_parse(url))


true_df = pd.DataFrame(df, columns = ["country", 
                                      "one.bed.city.median", "one.bed.city.lower", "one.bed.city.upper", 
                                      "three.bed.city.median", "three.bed.city.lower", "three.bed.city.upper",
                                      "one.bed.out.median", "one.bed.out.lower", "one.bed.out.upper", 
                                      "three.bed.out.median", "three.bed.out.lower", "three.bed.out.upper"])

print(true_df)
true_df.to_csv("../choiceData.csv", index = False)