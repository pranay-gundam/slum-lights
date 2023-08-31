library(mlogit)
library(tidyverse)
library(WDI)
library(stargazer)



slumPop = WDI(indicator=c("EN.POP.SLUM.UR.ZS"), country=c("all"), start=1960, end=2021)
urbanPop = WDI(indicator=c("SP.URB.TOTL"), country=c("all"), start=1960, end=2021)

setwd("E:/Senior Thesis/# Final Work/Modeling")
df = read_csv("../choiceData.csv")

slumPop_country = slumPop %>%
  filter(country %in% df$country) %>%
  left_join(urbanPop, by = c("country", "year")) %>%
  na.omit() %>%
  mutate(year_weight = year - min(year)+1,
         EN.POP.SLUM.UR.ZS = EN.POP.SLUM.UR.ZS/100) %>%
  group_by(country) %>%
  summarise(weighted_percent = sum(year_weight*EN.POP.SLUM.UR.ZS)/sum(year_weight),
            weighted_pop = sum(year_weight*SP.URB.TOTL)/sum(year_weight),
            slum_count = weighted_pop*weighted_percent,
            city_count = (1 - weighted_percent)*weighted_pop)
  



findf = slumPop_country %>%
  left_join(df, by = "country") %>%
  mutate(city.one.diff = one.bed.city.upper - one.bed.city.lower,
         out.one.diff = one.bed.out.upper - one.bed.city.lower,
         city.three.diff = three.bed.city.upper - three.bed.city.lower,
         out.three.diff = three.bed.out.upper - three.bed.city.lower) %>%
  na.omit()

head(findf)

model0 = glm(data = findf, formula = weighted_percent ~ one.bed.city.median + 
                                                        three.bed.city.median + 
                                                        one.bed.out.median + 
                                                        three.bed.out.median + 
               city.one.diff + out.one.diff + city.three.diff + out.three.diff, family="binomial")
summary(model0)
stargazer(model0)

model1 = glm(data = findf, formula = weighted_percent ~ city.three.diff + out.three.diff, family="binomial")
summary(model1)
stargazer(model1)

model1 = glm(data = findf, formula = weighted_percent ~ city.three.diff, family="binomial")
summary(model1)
stargazer(model1)




