import requests
import re
import pandas as pd
import time
import numpy as np

def fin_index(start, string, end_char):
    end = start + 1
    while string[end] != end_char:
        end += 1
    return start, end


def sdi_parse(url):
    page = requests.get(url)
    html = page.text
    var_settle_index = re.search(r'\b(var settlement)\b', page.text)

    found_start = False
    found_end = False
    start = 0
    end = 0

    i = var_settle_index.end()

    while not found_end:
        if page.text[i] == "{" and not found_start:
            start = i
            found_start = True
            print(start)
        if page.text[i:i+2] == "};":
            end = i+1
            found_end = True
            print(end)

        i += 1

    info = page.text[start:end]
    
    values = ["_Municipality\":\"", "_geolocation\":", "_City\":\"", "_Ward\":\"", "currency\":\"", "_Bank\":\"", "Country\":\"",
              "_Mosques\":\"", "_Temples\":\"", "_Province\":\"", "_Renting\":\"", "_density\":\"", "_Hospital\":\"", "_Churches\":\"",
              "_Road_Type\":\"", "_PreSchool\":\"", "_FoodShops\":\"", "_Households\":", "_Sewer_Line\":\"", "_AidsClinic\":\"", "_Municipality\":\"",
              "_HealthClinic\":\"", "_PrimarySchool\":\"", "_SecondarySchool\":\"", "_Household_Size\":", "_CommonDisease_1\":\"", "_CommonDisease_2\":\"",
              "_CommonDisease_3\":\"", "_CommonDisease_4\":\"", "_pop_c\":\"", "_Tot_ToiletSeats\":\"", "_Area_Calculate_Acres\":\"", "_Settlement_Name_Community\":\"",
              "_Area_Calculate\":\"", "_Structures_Total\":\"", "_Working_Taps_Ratio\":\"", "violence_against_women\":\"", "violence_against_children\":\"", 
              "_Priority1\":\"", "_Priority2\":\"", "_Priority3\":\"", "_Priority4\":\"", "_Priority5\":\"", "_Area_sqmt\":"]

    row = [url]
    #print(html)
    #print(info)
    for val in values:
        cur_index = info.find(val)
        #print(val)
        cur_start, cur_end = fin_index(cur_index + len(val), info, "\"" if val[-1] == "\"" else ("," if "geolocation" not in val else "]"))
        cut = info[cur_start:cur_end]
        '''
        if "geolocation" in val:
            cut = cut[1:]
            cut = cut.split(",")
            
            lat = float(cut[0])
            long = float(cut[1])
            
            row.append(lat)
            row.append(long)
            continue
        
        if "City" in val or "Country" in val:
            regex = re.compile('[^a-zA-Z]')
            print(cut)
            print("_____")
            cut = re.sub(regex, "", cut)
            cut = cut.capitalize()
            print(cut)
        '''
        if "City" in val or "Country" in val:
            cut_split = cut.split("_")
            cut_split = cut_split[1:]
            cut = "-".join(list(map(lambda x: x.capitalize(),cut_split)))

        row.append(cut)
        
    return row

def COL_parse(link):
    page = requests.get(link)
    
    flags = ["Meal, Inexpensive Restaurant", "Water (12 oz small bottle)", "McMeal at McDonalds (or Equivalent Combo Meal)", "Average Monthly Net Salary (After Tax)",
             "Cappuccino (regular)", "Milk (regular), (1 gallon)", "Eggs (regular) (12)", "Taxi 1 mile (Normal Tariff)", "Gasoline (1 gallon)", "Basic (Electricity, Heating, Cooling, Water, Garbage) for 915 sq ft Apartment",
             "Internet (60 Mbps or More, Unlimited Data, Cable/ADSL)", "Cinema, International Release, 1 Seat", "1 Pair of Jeans (Levis 501 Or Similar)", "Apartment (1 bedroom) in City Centre", "Apartment (1 bedroom) Outside of Centre",
             "Price per Square Feet to Buy Apartment in City Centre", "Price per Square Feet to Buy Apartment Outside of Centre", "Mortgage Interest Rate in Percentages (%), Yearly, for 20 Years Fixed-Rate"]

    
    if page == "<Response [200]>":
        return [np.nan] * len(flags)
    
    html = page.text
    data = []
    for flag in flags:
        flag_index_end = html.find(flag) + len(flag)
        #print(html[flag_index_end-10:flag_index_end+10])
        found = 0
        i = flag_index_end
        while True:
            #print(html[i])
            if html[i] == ">":
                found += 1
            if found >= 3:
                break
            i += 1
        
        start = i + 1
        #print(html[start:start+10])
        while not (html[i] == "&" or html[i] == "?" or html[i] == "<"):
            i+=1
        
        end = i 
        #print(html[start:end])
        data.append(html[start:end])

    return data

        


# Still need to debug https://sdinet.org/settlement/1852/3984487

#print(sdi_parse("https://sdinet.org/settlement/1853/10801083"))
#print(sdi_parse("https://sdinet.org/settlement/1847/33842825"))


#print(COL_parse("https://www.numbeo.com/cost-of-living/in/Lusaka?displayCurrency=USD"))



links = pd.read_csv("SDI Links.csv")
df = [] 
i = 0
for link in links.Link:
    start = time.time()
    print(link)
    if i >= 975:
        row_data = sdi_parse(link)
        city = row_data[3]
        print(city)
        COL_url = f"https://www.numbeo.com/cost-of-living/in/{city}?displayCurrency=USD"
        
        row_data += COL_parse(COL_url)

        df.append(row_data)
    end = time.time()
    print(f"{i}: {end - start}")
    print("---------------------------------------------")
    i += 1

#sdi_data = pd.DataFrame(df, columns = ["URL", "Municipality", "Latitude", "Longitude", "Geolocation", "City", "Ward", "Currency", "Bank", "Country", "Mosques", "Temples", "Province",
#                                       "Renting", "Density", "Hospital", "Churches", "Road Type", "Preschool", "Foodshops", "Households", "Sewer Line", "Aids Clininc",
#                                       "Municipality", "Health Clinic", "PrimarySchool", "SecondarySchool", "Household Size", "Common Disease 1", "Common Disease 2", 
#                                       "Common Disease 3", "Common Disease 4", "Pop", "Total Toilet Seats", "Area Acres", "Settlement Name", "Area Acres", "Total Structures", 
#                                       "Working Taps Ratio", "Violence against Women", "Violence against Children", "Priority 1", "Priority 2", "Priority 3", "Priority 4"
#                                       "Priority 5", "Area (square meters)"])

sdi_data = pd.DataFrame(df, columns = ["URL", "Municipality", "Geolocation", "City", "City-State", "Ward", "Currency", "Bank", "Country", "Mosques", "Temples", "Province",
                                       "Renting", "Density", "Hospital", "Churches", "Road Type", "Preschool", "Foodshops", "Households", "Sewer Line", "Aids Clininc",
                                       "Municipality", "Health Clinic", "PrimarySchool", "SecondarySchool", "Household Size", "Common Disease 1", "Common Disease 2", 
                                       "Common Disease 3", "Common Disease 4", "Pop", "Total Toilet Seats", "Area Acres", "Settlement Name", "Area Acres", "Total Structures", 
                                       "Working Taps Ratio", "Violence against Women", "Violence against Children", "Priority 1", "Priority 2", "Priority 3", "Priority 4"
                                       "Priority 5", "Area (square meters)"])

print(sdi_data)
sdi_data.to_csv("training_data.csv", index = False)

