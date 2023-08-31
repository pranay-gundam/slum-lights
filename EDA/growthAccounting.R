library(WDI)
library(tidyverse)
library(ggplot2)
library(wesanderson)
library(xtable)
library(pwt10)
library(data.table)
library(stargazer)
library(vcd)

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
data("pwt10.0")

setwd("E:/Senior Thesis/# Final Work")

df = subset(pwt10.0, isocode == "KEN")




fin = df %>%
  filter(year >= 1970) %>%
  select(c("rgdpna", "pop", "labsh", "rkna", "emp", "year")) %>%
  na.omit() %>%
  mutate(cap_gdp = rgdpna/pop,
         log_cap_gdp = log(cap_gdp),
         normlog_cap_gdp = log_cap_gdp - log_cap_gdp[1],
         trend = 0.02*(year - year[1]),
         capsh = 1 - labsh,
         hours = 10400 * emp * 1000000,
         cap_hours = hours/pop,
         log_cap_hours = log(cap_hours),
         cap_term = (capsh)*log(rkna/cap_gdp)/(1-capsh),
         prod_term = log_cap_gdp - log_cap_hours - cap_term)

#plot1: normalized log gdp
fin %>%
 ggplot(aes(x = year, y = normlog_cap_gdp))+
  geom_line(size = 0.7)+
      labs(title = "Normalized Trended GDP Growth", y = "log Values (1970 = 0)", caption = "Source: Pen World Tables 10.0", x = "Year")+
      theme_bw()+
      scale_colour_Publication()+
      theme(legend.title = element_blank(),
            legend.position = c(0.2,0.2),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0), 
            text = element_text(family="serif"),
            axis.line = element_line(colour = "black"), 
            panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 0))

#plot2: Growth Accounting



#plot3: alpha
fin %>%
  ggplot(aes(x = year, y = capsh))+
  geom_line(size = 0.7)+
  labs(title = "Capital Share", y = "Proportion", caption = "Source: Pen World Tables 10.0", x = "Year")+
  theme_bw()+
  scale_colour_Publication()+
  theme(legend.title = element_blank(),
        legend.position = c(0.2,0.2),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0), 
        text = element_text(family="serif"),
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0))


#lol = df %>%
#  mutate(y_pc = log(rgdpna / emp), # GDP per worker
#         k_pc = log(rkna / emp), # Capital per worker
#         a = 1 - labsh) %>% # Capital share
#  arrange(year) %>% # Order by year
#  group_by(isocode) %>% # For each country calculate...
#  mutate(g = (y_pc - lag(y_pc)) * 100, # ...the growth rate of GDP per capita
#         dk = (k_pc - lag(k_pc)) * 100, # ...the growth rate of capital per capita
#         dsolow = g - a * dk) %>% # ...the Solow residual
#  na.omit()
#view(lol)
#view(df_clean)

#df_clean = df %>%
#  select(c("rgdpna", "pop", "labsh", "rkna", "emp", "year")) %>%
#  na.omit() %>%
#  mutate(test_trend = rgdpna[1]/pop[1]*1.02^(year - year[1]),
#         trend = 0.02*(year - year[1]),
#         hours = 10400 * emp * 1000000,
#         thours = log(hours / pop),
#         tgdp = log(rgdpna / pop) - trend,
#         tcapital = ((-mean(labsh) + 1)/(mean(labsh)) * log(rkna/(rgdpna/pop))),
#         tproduc = tgdp - trend - tcapital - thours,
#         Labor = thours - thours[1],
#         "Detrended Output" = tgdp - tgdp[1],
#         Capital = tcapital - tcapital[1],
#         "Productivity" = tproduc - tproduc[1])

#df_test = df %>%
#  mutate(gdpcap_growth = log(rgdpna/pop) - log(lag(rgdpna)/lag(pop)),
#         gdp_trend = (year - year[1])*log(1.02))

#df_test %>%
#  ggplot(aes(x = year))+
#  geom_line(aes(y = gdpcap_growth), color = "blue")+
#  geom_line(aes(y = gdp_trend), color = "red")

#df_clean %>%
#  ggplot(aes(x = year))+
#  geom_line(aes(y = rgdpna/pop), color = "blue")+
#  geom_line(aes(y = test_trend), color = "red")

#df_clean %>%
#  ggplot(aes(x = year, y = labsh))+
#  geom_line()

#head(df_clean)

#df_clean_long = df_clean %>%
#  select(c("year", "Labor", "Detrended Output", "Capital", "Productivity")) %>%
#  pivot_longer(!year, names_to = "term", values_to = "value")

#head(df_clean_long)

#(df_clean_long %>%
#    ggplot(aes(x = year, y = value, color = term, line = term))+
#    geom_line(size = 1.001)+
#    labs(title = "", y = "log Values (1954 = 0)", caption = "Source: Pen World Tables 10.0", x = "Year")+
#    theme_bw()+
#    scale_colour_Publication()+
#    theme(legend.title = element_blank(),
#          legend.position = c(0.2,0.2),
#          plot.subtitle = element_text(hjust = 0.5),
#          plot.caption = element_text(hjust = 0), 
#          text = element_text(family="serif"),
#          axis.line = element_line(colour = "black"), 
#          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
#          axis.text.x = element_text(angle = 0)))





lpdf = read_csv("kenyaCity_modelAll.csv")

aggs_light = lpdf %>%
  select(-c(1,2,4,5)) %>%
  select(1:36) %>%
  colMeans() %>%
  as.data.frame()

taggs_light = transpose(aggs_light)
colnames(taggs_light) = rownames(aggs_light)




taggs_light_long = taggs_light %>%
  pivot_longer(cols = names(taggs_light)) %>%
  mutate(year = substr(name, nchar(name) - 3, nchar(name)),
         name = substr(name, 1, 5)) %>%
  group_by(year, name) %>%
  summarise(value = mean(value)) %>%
  mutate(name = ifelse(name == "light", "lights", "popdensity"))

table(taggs_light_long$name, taggs_light_long$year)

(taggs_light_long %>%
    ggplot(aes(x = year, y = value, group = 1, color = name))+
    facet_wrap(~name, nrow = 2, scales = "free_y",
               strip.position = "left", 
               labeller = as_labeller(c(lights = "Average Light Value (/sq km)", 
                                        popdensity = "Average Pop Density (/sq km)")))+
    scale_colour_Publication()+
    geom_line(size = 1.001)+
    geom_point(color = "black")+
    labs(x = "Year", y = "",
         caption = "Source: NOAA, World Pop Hub", 
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))


gdpdf = WDI(indicator=c("NY.GDP.PCAP.CD"), country = c("KEN"), start=2000, extra = TRUE, cache = NULL)
gdpdf = gdpdf %>%
  select(c("year", "NY.GDP.PCAP.CD"))

(taggs_light_wide = taggs_light_long %>%
  pivot_wider(names_from = "name", values_from = "value"))

(gdp_lightpop_merge = merge(gdpdf, taggs_light_wide, by = "year"))
colnames(gdp_lightpop_merge)[2] = "GDP"
head(gdp_lightpop_merge)

gdp_lightpop_merge$detrendGDP = log(gdp_lightpop_merge$GDP) - 0.02


coeff = mean(gdp_lightpop_merge$GDP) / mean(gdp_lightpop_merge$lights)
coeff
(gdp_lightpop_merge %>%
    ggplot(aes(x = year))+
    geom_line(aes(y = lights), color = "#386cb0", size = 1.001)+
    geom_line(aes(y = GDP/coeff), color = "#fdb462", size = 1.001)+
    scale_y_continuous(name = "Light Values",
                       sec.axis = sec_axis(~.*coeff, name="GDP per Capita (US Dollars)"))+
  labs(x = "Year",
       caption = "Source: NOAA, WDI", 
       title = "Kenya: Average lights and GDP per Capita over time")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))

(lol = cor(gdp_lightpop_merge[c(1,2,3,4, 5)]))
stargazer(lol)
xtable(lol)



lin1 = lm(data = gdp_lightpop_merge,
          formula = GDP ~ year + lights)

lin2 = lm(data = gdp_lightpop_merge,
          formula = lights ~ year + GDP)

summary(lin1)
summary(lin2)



gdp_lightpop_merge$preds1 = predict(lin1, newdata = gdp_lightpop_merge[c(1,4)])
gdp_lightpop_merge$preds2 = predict(lin2, newdata = gdp_lightpop_merge[c(1,2)])
head(gdp_lightpop_merge)

legend_colors <- c("Lights" = "#386cb0", 
                   "GDP per Capita" = "#fdb462", 
                   "Predicted GDP Per Capita" = "#fdb462",
                   "Predicted Lights" = "#386cb0")

(gdp_lightpop_merge %>%
    ggplot(aes(x = year))+
    geom_line(aes(y = lights, color = "Lights"), size = 1.001)+
    geom_line(aes(y = preds2, color = "Predicted Lights"), size = 1.001, linetype = "dashed")+
    geom_line(aes(y = GDP/coeff, color = "GDP per Capita"), size = 1.001)+
    geom_line(aes(y = preds1/coeff, color = "Predicted GDP Per Capita"), size = 1.001, linetype = "dashed")+
    scale_color_manual(values = legend_colors) + 
    scale_y_continuous(name = "Light Values",
                       sec.axis = sec_axis(~.*coeff, name="GDP per Capita (US Dollars)"))+
    labs(x = "Year",
         caption = "Source: NOAA, WDI", 
         title = "",
         color = "Legend")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.2,0.85),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))

