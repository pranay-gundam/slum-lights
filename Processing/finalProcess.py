import pandas as pd
import numpy as np


def dist(x, y, x0, y0):
    return np.sqrt(np.square(x - x0) + np.square(y - y0))


def filterHelp(colsNames, condition, df):
    return df[[any(elem) for elem in list(map(lambda i: [condition(df[name][i]) for name in colsNames], range(len(df[colsNames[0]])))) ]]

def next_helper(x, lists):
    i=0
    while x > lists[i]:
        i += 1
    return f"bin {i}"

# Feature Engineering and filtering
def mode1(df, cnum):
    print("inmode1")
    base_lightname = "lights_Kenya"
    satelites = ["F142000", "F142001", "F142002", "F142003", "F152000", "F152001", "F152002",
                 "F152003", "F152004", "F152005", "F152006", "F152007", "F162004", "F162005",
                 "F162006", "F162007", "F162008", "F162009", "F182010", "F182011", "F182012",
                 "F182013"]
    light_names = [base_lightname + sat for sat in satelites]


    popname1 = "ken_pd_"
    year = ["2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
            "2011", "2012", "2013"]
    
    pop_names = [popname1 + y for y in year]
    

    df["slum"] = [1 if df["slum"][i] else 0 for i in range(len(df))]

    df = filterHelp(light_names + pop_names, lambda x: x != 0, df)
    df = df.dropna(subset=pop_names+light_names)

    print("doing the categorical shits")
    for lname in light_names:
        quant_break1 = np.quantile(df[lname], np.linspace(0, 1, cnum))
        
        df["c"+lname] = list(map(lambda x: next_helper(x, quant_break1), df[lname]))
        #df["c"+lname] = list(map(lambda x: next(y for y, val in enumerate(quant_break) if val > x), df[lname]))

    for pname in pop_names:
        quant_break2 = np.quantile(df[pname], np.linspace(0, 1, cnum))
        
        df["c"+pname] = list(map(lambda x: next_helper(x, quant_break2), df[pname]))
        #df["c"+pname] = list(map(lambda x: next(y for y, val in enumerate(quant_break) if val > x), df[pname]))

    # still need to filter out no light no density zones
    print("doing the filtering shits")
    
    print(df.head())
    
    return df


# This is to add more slums information from an already compiled 
def mode2(df, slums):
    print("in mode2")
    slums_names = ["Working Taps Ratio", "Area (square meters)", "Priority 1", "Priority 2", "Priority 3", "Priority 4", "Priority 5",
                   "Sewer Line", "Total Toilet Seats", "Health Clinic", "Common Disease 1", "Common Disease 2", "Common Disease 3",
                   "Common Disease 4", "Total Structures", "Household Size", "Aids Clinic", "Foodshops", "Households", "Preschool",
                   "PrimarySchool", "SecondarySchool"]

    for name in slums_names:
        df[name] = np.nan


    # Working Tap Ratio, area, priority 1, priority 2, priority 3, priority 4, priority 5
    for i,row in df.iterrows():
        if df["slum"][i] == 1:
            closest_slum_index = np.argmin(dist(slums["lattitude"], slums["longitude"], df["long"][i], df["lat"][i]))
            for name in slums_names:
                df[name][i] = slums[name][closest_slum_index]
    print(df.head())
    return df
            

if __name__ == "__main__":
    slums = pd.read_csv("../slums_allTrain.csv")

    kenyaCity = pd.read_csv("../kenyaCity_modelRestricted.csv")
    kenyaState = pd.read_csv("../kenyaState_modelRestricted.csv")

    kenyaCity = mode1(kenyaCity, 10)
    kenyaState = mode1(kenyaState, 10)

    kenyaCity = mode2(kenyaCity, slums)
    kenyaState = mode2(kenyaState, slums)

    kenyaCity.drop(kenyaCity.columns[[0]], axis = 1, inplace = True)
    kenyaState.drop(kenyaState.columns[[0]], axis = 1, inplace = True)

    kenyaCity.to_csv("../kenyaCity_modelAll10.csv", index = False)
    kenyaState.to_csv("../kenyaState_modelAll10.csv", index = False)




    