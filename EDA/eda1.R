library(WDI)
library(tidyverse)
library(ggplot2)
library(wesanderson)
library(xtable)
library(ggmap)
library(RColorBrewer)
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

theme_Publication <- function(base_size=14, base_family="Serif") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

setwd("E:/Senior Thesis/# Final Work")
df = read_csv("kenyaCity_modelAll.csv")

df$`Light Levels (2013)` = df$clights_KenyaF182013
df$`Pop Density Levels (2013)` = df$cken_pd_2013

slumdf = df %>% filter(slum == 1)
head(df)
names(df)




(PL_tab = table(df$clights_KenyaF182013, df$cken_pd_2013))
mosaicplot(PL_tab, shade = TRUE,
           xlab = "Light Levels",
           ylab = "Population Density Levels",
           main = "Distribution of all Observations",
           off = 10)




PL_xtab = xtabs( ~ `Light Levels (2013)` + `Pop Density Levels (2013)`, df)
mosaic(PL_xtab,
       shading = TRUE,
       legend = TRUE,
       gp = shading_max)

(PLslum_tab = table(slumdf$clights_KenyaF182013, slumdf$cken_pd_2013))
mosaicplot(PLslum_tab, shade = TRUE,
           xlab = "Light Levels",
           ylab = "Population Density Levels",
           off = 10)


PLslum_xtab = xtabs( ~ `Light Levels (2013)` + `Pop Density Levels (2013)`, slumdf)
mosaic(PLslum_xtab,
       shading = TRUE,
       legend = TRUE,
       gp = shading_max)

(sum_stats = df %>%
  mutate(habited = ken_pd_2013 == 0) %>%
  mutate(habitation = ifelse(habited == TRUE, "Uninhabited", ifelse(slum == 1, "Slum", "Not a Slum"))) %>%
  group_by(habitation) %>%
  summarize(total_count=n(),
            avg_light = mean(lights_KenyaF182013),
            avg_pop = mean(ken_pd_2013),
            sd_light = sd(lights_KenyaF182013),
            sd_pop = sd(ken_pd_2013),
            median_light = median(lights_KenyaF182013),
            median_pop = median(ken_pd_2013)) %>%
  as.data.frame()) 

xtable(sum_stats)
stargazer(sum_stats)

(df %>%
  ggplot(aes(x = lights_KenyaF182013, y = ken_pd_2013, fill = as.factor(slum), color = as.factor(slum), group = as.factor(slum)))+
    geom_point(alpha = 0.2))

density_df = df %>%
  mutate(habited = ken_pd_2013 == 0) %>%
  mutate(habitation = ifelse(habited == TRUE, "Uninhabited", ifelse(slum == 1, "Slum", "Not a Slum"))) %>%
  filter(habitation != "Uninhabited")


Q_light = quantile(density_df$lights_KenyaF182013, probs=c(.25, .75), na.rm = FALSE)
iqr_light = IQR(density_df$lights_KenyaF182013)
up_light =  Q_light[2]+1.5*iqr_light # Upper Range  
low_light = Q_light[1]-1.5*iqr_light # Lower Range
density_df_light = subset(density_df, density_df$lights_KenyaF182013 > low_light & density_df$lights_KenyaF182013 < up_light)

library(pander)

slum_light = subset(density_df, habitation == "Slum")$lights_KenyaF182013
noslum_light = subset(density_df, habitation == "Not a Slum")$lights_KenyaF182013

(a = ks.test(slum_light, noslum_light))
(at = t.test(slum_light, noslum_light))


(b = ks.test(subset(density_df, habitation == "Slum")$ken_pd_2013,
        subset(density_df, habitation == "Not a Slum")$ken_pd_2013))


(density_df_light %>%
  ggplot(aes(x = lights_KenyaF182013, fill = habitation))+
  geom_density(alpha = 0.6)+
  scale_fill_Publication()+
    labs(x = "Light Value", y = "Density",
         caption = "Source: NOAA", title = "Density curves of lightvalues (ouliers removed)")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))


Q_pop = quantile(density_df$ken_pd_2013, probs=c(.25, .75), na.rm = FALSE)
iqr_pop = IQR(density_df$ken_pd_2013)
up_pop =  Q_pop[2]+1.5*iqr_pop # Upper Range  
low_pop = Q_pop[1]-1.5*iqr_pop # Lower Range
density_df_pop = subset(density_df, density_df$ken_pd_2013 > low_pop & density_df$ken_pd_2013 < up_pop)



(density_df_pop %>%
    ggplot(aes(x = ken_pd_2013, fill = habitation))+
    geom_density(alpha = 0.6)+
    scale_fill_Publication()+
    labs(x = "Pop Density (1 sq km)", y = "Density",
         caption = "Source: NOAA", title = "Density curves of pop density values (outliers removed)")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))

dfbins = read_csv("kenyaCity_modelAll10.csv")
dfbins$`Light Levels (2013)` = dfbins$clights_KenyaF182013
dfbins$`Pop Density Levels (2013)` = dfbins$cken_pd_2013

slumdfbins = dfbins %>% filter(slum == 1)
head(dfbins)


(PL_tab = table(dfbins$clights_KenyaF182013, dfbins$cken_pd_2013))
mosaicplot(PL_tab, shade = TRUE,
           xlab = "Light Levels",
           ylab = "Population Density Levels",
           main = "Distribution of all Observations")

PL_xtab = xtabs( ~ `Light Levels (2013)` + `Pop Density Levels (2013)`, dfbins)
mosaic(PL_xtab,
       shading = TRUE,
       legend = TRUE,
       gp = shading_max)

(PLslum_tab = table(slumdfbins$clights_KenyaF182013, slumdfbins$cken_pd_2013))
mosaicplot(PLslum_tab, shade = TRUE,
           xlab = "Light Levels",
           ylab = "Population Density Levels",
           main = "Distribution of all Slum Observations")

PLslum_xtab = xtabs( ~ `Light Levels (2013)` + `Pop Density Levels (2013)`, slumdfbins)
mosaic(PLslum_xtab,
       shading = TRUE,
       legend = TRUE,
       gp = shading_max)


pivoted_slumdfbins = slumdfbins %>%
  select(c("Pop Density Levels (2013)", "Light Levels (2013)")) %>%
  pivot_longer(cols = c("Pop Density Levels (2013)", "Light Levels (2013)"), 
               names_to = "type")
head(pivoted_slumdfbins)

(pivoted_slumdfbins %>%
    ggplot(aes(x = value, fill = type))+
    geom_bar(color = "black", alpha = 0.9)+
    scale_fill_Publication()+
    facet_wrap(~ type) +
    labs(x = "Bin", y = "Count",
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
          axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.5)))

