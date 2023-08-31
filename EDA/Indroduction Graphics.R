library(WDI)
library(tidyverse)
library(wesanderson)


scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


urbanPop.percent = WDI(indicator=c("SP.URB.TOTL.IN.ZS"), country=c("all"), start=1962, end=2020, extra = TRUE, cache = NULL)
head(urbanPop.percent)
urbanPop.percent1 = filter(urbanPop.percent, region != 'Aggregates')
urbanPop.percent2 = filter(urbanPop.percent, region == 'Aggregates')
urbanPop.percent3 = filter(urbanPop.percent, region == 'Aggregates')

countries = c("IND", "CHN", "BRA", "USA", "KEN", "ZAF", "FRA", "GBR", "GER", 
              "ZWE")

unique(urbanPop.percent$region)

regions1 = c("East Asia & Pacific", "Europe & Central Asia", "Middle East & North Africa", 
            "South Asia", "North America", "Latin America & Caribbean (excluding high income)",
            "Sub-Saharan Africa (excluding high income)")

regions2 = c("Least developed countries: UN classification", "Low & middle income",
             "Low income", "Lower middle income", "Middle income", "Upper middle income",
             "High income")

urbanPop.percent1 = filter(urbanPop.percent1, iso3c %in% countries)
urbanPop.percent2 = filter(urbanPop.percent2, country %in% regions1)
urbanPop.percent3 = filter(urbanPop.percent3, country %in% regions2)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = urbanPop.percent1)+
  geom_line(aes(x = year, y = SP.URB.TOTL.IN.ZS, color = country), size = 1.05)+
  labs(x = "Year", y = "Urban population as percent of total population",
       caption = "Source: WDI", title = "Timeseries of percent urban population")+
  scale_color_brewer(palette="RdGy")+
  theme_bw()+
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))

ggplot(data = urbanPop.percent2)+
  geom_line(aes(x = year, y = SP.URB.TOTL.IN.ZS, color = country), size = 1.05)+
  labs(x = "Year", y = "Urban population as percent of total population",
       caption = "Source: WDI", title = "Timeseries of percent urban population")+
  theme_bw()+
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))


p3 = subset(urbanPop.percent3, country %in% c("High income", "Low income", "Middle income") & year >= 2000)
p3$growth = (p3$SP.URB.TOTL.IN.ZS / lead(p3$SP.URB.TOTL.IN.ZS) - 1)*100


urbanPop.growth = WDI(indicator=c("SP.URB.GROW"), country=c("all"), start=1962, end=2020, extra = TRUE, cache = NULL)
regions_slum = c("Sub-Saharan Africa",'Europe & Central Asia',"South Asia","Latin America & Caribbean", "Middle East & North Africa", "East Asia & Pacific")
regions4 = c("Low income", "Lower middle income", "Middle income", "Upper middle income",
             "High income")
urbanPop.growth2 = filter(urbanPop.growth, country %in% regions4)

urbanPop.growthregions = urbanPop.growth %>%
  filter(region %in% regions_slum) %>%
  group_by(region, year) %>%
  summarise(growth = mean(SP.URB.GROW, na.rm = TRUE)) %>%
  arrange(region, year) %>%
  pivot_longer(cols = c("growth"),
             names_to = "type",
             values_to = "value")

ggplot(data = urbanPop.growthregions)+
  geom_line(aes(x = year, y = value), size = 0.8, color = "#386cb0")+
  scale_colour_Publication()+
  facet_wrap(~region)+
  labs(x = "Year", y = "Percentage (%)",
       caption = "Source: WDI", title = "")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))



ggplot(data = urbanPop.growth2)+
  geom_line(aes(x = year, y = SP.URB.GROW, color = country), size = 0.8)+
  scale_color_manual(values = c("Darkblue", "darkred", "darkgreen", "orange",
                                "black"))+
  labs(x = "Year", y = "Percentage (%)",
       caption = "Source: WDI", title = "Urban population growth ")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))


slumPop = WDI(indicator=c("EN.POP.SLUM.UR.ZS"), country=c("all"), start=1960, end=2020, extra = TRUE, cache = NULL)
slumPop = na.omit(slumPop)
unique(slumPop$region)
regions_slum = c("Sub-Saharan Africa",'Europe & Central Asia',"South Asia","Latin America & Caribbean", "Middle East & North Africa", "East Asia & Pacific")

slumPopf = slumPop %>%
  filter(region %in% regions_slum) %>%
  group_by(region, year) %>%
  summarize(avg = mean(EN.POP.SLUM.UR.ZS))

slumPopf

ggplot(data = slumPopf)+
  geom_line(aes(x = year, y = avg), size = 1.001, color = "#386cb0")+
  facet_wrap(~region)+
  labs(x = "Year", y = "Percentage (%)",
       caption = "Source: WDI", title = "")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))

urbanPop.percent2lol = urbanPop.percent %>%
  na.omit() %>%
  filter(region %in% regions_slum) %>%
  group_by(region, year) %>%
  summarize(avg = mean(SP.URB.TOTL.IN.ZS))

ggplot(data = urbanPop.percent2lol)+
  geom_line(aes(x = year, y = avg), size = 1.001, color = "#386cb0")+
  facet_wrap(~region)+
  labs(x = "Year", y = "Percentage (%)",
       caption = "Source: WDI", title = "")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))

kenslumpop = na.omit(subset(slumPop, country == "Kenya"))

ggplot(data = kenslumpop)+
  geom_line(aes(x = year, y = EN.POP.SLUM.UR.ZS), size = 1.001, color = "#386cb0")+
  scale_color_manual(values = c("Darkblue", "red", "darkgreen", "orange",
                                "brown"))+
  labs(x = "Year", y = "Percentage of Urban Population(%)",
       caption = "Source: WDI", title = "")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))



indis = c("IE.PPI.ICTI.CD", "IE.PPI.ENGY.CD", "IE.PPI.WATR.CD", "IE.PPI.TRAN.CD")
infra_df = WDI(indicator = indis, 
               country=c("all"), start=1990, end=2020, extra = TRUE, cache = NULL)

cleaned_Kenya = infra_df %>%
  mutate(transport = IE.PPI.TRAN.CD/1000000,
         water = IE.PPI.WATR.CD/1000000,
         energy = IE.PPI.ENGY.CD/1000000,
         ICT = IE.PPI.ICTI.CD/1000000) %>%
  filter(iso3c == "KEN") %>%
  arrange(region, year) %>%
  pivot_longer(cols = c("transport", "energy", "water", "ICT"),
               names_to = "type",
               values_to = "value")

ggplot(data = cleaned_Kenya, aes(x = year, y = value, color = type))+
  geom_line(size = 1.001)+
  scale_color_manual(values = c("Darkblue", "red", "darkgreen", "orange",
                                "brown", "black"))+
  labs(x = "Year", y = "Millions of Dollars ($)",
       caption = "Source: WDI", title = "Average country-wide infrastructure spending per region")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.875),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))




(cleaned = infra_df %>%
    mutate(transport = IE.PPI.TRAN.CD/1000000,
           water = IE.PPI.WATR.CD/1000000,
           energy = IE.PPI.ENGY.CD/1000000,
           ICT = IE.PPI.ICTI.CD/1000000) %>%
    filter(region %in% regions_slum) %>%
    group_by(region, year) %>%
    summarise(avg_transport = mean(transport, na.rm = TRUE),
              avg_water = mean(water, na.rm = TRUE),
              avg_energy = mean(energy, na.rm = TRUE),
              avg_ICT = mean(ICT, na.rm = TRUE)) %>%
    arrange(region, year) %>%
    pivot_longer(cols = c("avg_transport", "avg_energy", "avg_water", "avg_ICT"),
                 names_to = "type",
                 values_to = "value"))

cleaned_africa = cleaned %>%
  filter(region == "Sub-Saharan Africa")


ggplot(data = cleaned, aes(x = year, y = value, color = type))+
  geom_line(size = 1.001)+
  scale_colour_Publication()+
  facet_wrap(~region)+
  labs(x = "Year", y = "Millions of Dollars ($)",
       caption = "Source: WDI", title = "")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.9,0.25),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))

