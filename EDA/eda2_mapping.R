library(tidyverse)
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(ggmap)
library(RColorBrewer)

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

setwd("E:/Senior Thesis/# Final Work")
df = read_csv("kenyaCity_modelAll.csv")
head(df)

coords.data = df %>%
  filter(slum == 1) %>%
  dplyr::select(c("long", "lat"))


head(coords.data)


#Kenya = getData("GADM", country="KE", level=0)
#Kenya1 = getData("GADM", country="KE", level=1)


#Kenya1_UTM<-spTransform(Kenya1, CRS("+init=EPSG:4326"))
#Kenya_UTM<-spTransform(Kenya, CRS("+init=EPSG:4326"))

#Kenya1_UTM@data
#Kenya_UTM@data
#plot(Kenya1)

map_bounds <- c(left = 33.5, bottom = -5, right = 42, top = 5)
stamenMap <- get_stamenmap(map_bounds, zoom = 7, maptype = "toner-lite")

(coords.map =  stamenMap %>%
    ggmap(extent="device", legend="none")+
    geom_point(data=coords.data,  aes(x=long, y=lat), color="darkgrey", shape=16, alpha=0.1)+ 
    labs(x = "Latitude", y = "Longitude",
         caption = "Source: Slum Dwellers International",
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))

ggsave(filename="./coords.png")


df1 = read_csv("kenyaCity_mapping.csv")

clean_df1 = df1 %>%
  filter(slum == 1 | predVanilla == 1) %>%
  dplyr::select(c("long", "lat", "slum", "predVanilla")) %>%
  setNames(c("long", "lat", "Actual", "Predicted")) %>%
  pivot_longer(cols = c("Actual", "Predicted"), values_to = "val", 
               names_to = "Type") %>%
  filter(val == 1) %>%
  dplyr::select(-c("val"))

head(clean_df1)

(coords.map =  stamenMap %>%
    ggmap(extent="device")+
    geom_point(data=clean_df1,  aes(x=long, y=lat, color = Type), shape=20, alpha=0.05)+
    guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 19)))+
    scale_color_manual(values = c("darkgrey", "#386cb0"))+ 
    labs(x = "Latitude", y = "Longitude",
         caption = "Source: Slum Dwellers International",
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))

clean_df2 = df1 %>%
  filter(slum == 1 | predBins == 1) %>%
  dplyr::select(c("long", "lat", "slum", "predBins")) %>%
  setNames(c("long", "lat", "Actual", "Predicted")) %>%
  pivot_longer(cols = c("Actual", "Predicted"), values_to = "val", 
               names_to = "Type") %>%
  filter(val == 1) %>%
  dplyr::select(-c("val"))

head(clean_df2)

(coords.map =  stamenMap %>%
    ggmap(extent="device")+
    geom_point(data=clean_df2,  aes(x=long, y=lat, color = Type), shape=20, alpha=0.05)+
    scale_color_manual(values = c("darkgrey", "#fdb462"))+ 
    guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 19)))+
    labs(x = "Latitude", y = "Longitude",
         caption = "Source: Slum Dwellers International",
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))


clean_df3 = df1 %>%
  filter(slum == 1 | predBins == 1) %>%
  dplyr::select(c("long", "lat", "slum", "weightedBins")) %>%
  setNames(c("long", "lat", "Actual", "Predicted")) %>%
  pivot_longer(cols = c("Actual", "Predicted"), values_to = "val", 
               names_to = "Type") %>%
  filter(val == 1) %>%
  dplyr::select(-c("val"))

head(clean_df3)

(coords.map =  stamenMap %>%
    ggmap(extent="device")+
    geom_point(data=clean_df3,  aes(x=long, y=lat, color = Type), shape=20, alpha=0.05)+
    scale_color_manual(values = c("darkgrey", "#7fc97f"))+ 
    guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 19)))+
    labs(x = "Latitude", y = "Longitude",
         caption = "Source: Slum Dwellers International",
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))


clean_df4 = df1 %>%
  filter(slum == 1 | predRF == 1) %>%
  dplyr::select(c("long", "lat", "slum", "weightedBins")) %>%
  setNames(c("long", "lat", "Actual", "Predicted")) %>%
  pivot_longer(cols = c("Actual", "Predicted"), values_to = "val", 
               names_to = "Type") %>%
  filter(val == 1) %>%
  dplyr::select(-c("val"))

head(clean_df4)

(coords.map =  stamenMap %>%
    ggmap(extent="device")+
    geom_point(data=clean_df4,  aes(x=long, y=lat, color = Type), shape=20, alpha=0.05)+
    scale_color_manual(values = c("darkgrey", "#ef3b2c"))+ 
    guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 19)))+
    labs(x = "Latitude", y = "Longitude",
         caption = "Source: Slum Dwellers International",
         title = "")+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.875),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0), 
          text = element_text(family="serif"),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0)))


