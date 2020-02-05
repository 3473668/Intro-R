#week 2
#Thulisile Mkhayiphi
#05/02/2020


#Data converted from asc to RData by using
#read.table("data/the route"header=TRUE)...on a console pane and it was saved as workspace 


#load libraries
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(tidyr)



#loading the file
load("data/gebco_sa_c.RData")

#tidy data
bathy_wide_tidy<-bathy_wide %>% 
  gather("7.99166666665":"39.97500006395",key ="long",value ="elevation")  
  
#convert dataframe to numeric to use for the scale

df2 = as.data.frame(sapply(df1, as.numeric)) #formular used for converting value to numeric
bathy_wide_1<-as.data.frame(apply(bathy_wide_tidy,2,as.numeric))

#load packages
library(scales)
library(maps)
#install leaflets package.
library(leaflets)


#mapping the co-ordinates------
load("data/africa_map.RData")
 
final_map <- ggplot(data = bathy_wide_1, aes(x = long, y = lat)) +
  geom_raster(data = sst, aes(fill = elevation)) +
  scale_fill_gradientn("elevation/\ndepth(m)", values = scales::rescale(c(-4000:-2000:0 :2000:4000))+
                        colour = "blue","darkgreen","red","grey", size = 0.2) +
  
  geom_polygon(colour = "black", fill = "grey", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = long, y = lat, fill = bins), 
            colour = "blue", size = 0.1) +
  
  labs(x="",y="")+
  scale_x_continuous(breaks = seq(10,35,5),
                     labels = c("10","15","20","25","30","35"),
                     position = "bottom",expand = c(0,0))+
  scale-y-continuous(expand = c(0,0))+
  theme(panel_1.grid.major=element_blank(),panel.grid=element_blank().title = element_blank(), 
        panel.background=element_blank())
        legend.text = element_text(size = 6) # Change text size in legend
        legend.title = element_text(size = 7) # Change legend title text size
        legend.justification = c(1, 0) # Change position of legend
        legend.position = c(0.54, 0.5) # Fine tune position of legend
  
final_map









