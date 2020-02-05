#week 2
#Thulisile Mkhayiphi
#05/02/2020
#

#Data converted from asc to RData by using
#read.table("data/the route"header=TRUE)

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

#mapping the co-ordinates------
  
final_map <- ggplot(data = bathy_wide_1, aes(x = long, y = lat)) +
  geom_raster(data = sst, aes(fill = elevation)) +
  scale_fill_gradientn("elevation/\ndepth(m)", values = scales::rescale(c(-4000:-2000:0 :2000:4000))+
                        colour = "blue","darkgreen","red","grey", size = 0.2) +
  
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "blue", size = 0.1) +
  
   labs(x="",y="")+
  scale_fill_gradien



  
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(expand = c(0,0))+ = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 6), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "green"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map









