#ggplot
#Mapping on day 3
#30th january
#Thulisile Mkhayiphi

#Loading packages
library(tidyverse)# Activating the packages
library(boot) #library func. activates the packages


#plotting with boot
urine <- boot:: urine


ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))
  labs(x="Osmeradulation", y= pH)
#loading in the libraries
library(tidyverse)
library (ggpubr)

#Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
# load("data/MUR_low_res.RData")

#Custom made colur pallete
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "red", aes(group = group)) # The land mask

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent


sst <- MUR

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
  colour = "white", size = 0.5) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#final map

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
  colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(100, 158), ylim = c(-55, -10), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
  legend.text = element_text(size = 7), # Change text size in legend
  legend.title = element_text(size = 7), # Change legend title text size
  legend.key.height = unit(0.3, "cm"), # Change size of legend
  legend.background = element_rect(colour = "white"), # Add legend background
  legend.justification = c(1, 0), # Change position of legend
  legend.position = c(0.55, 0.4)) # Fine tune position of legend)
final_map

####Chapter 10
# Loading libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)


# Load Africa map
load("data/africa_map.RData")
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "blue", colour = "black") +
  coord_equal(xlim = c(100, 158), ylim = c(-55, -10), expand = 0) # Force lon/lat extent
sa_1

sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30, 
           colour = "navy") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "springgreen")
sa_2

  
sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.1, y = -32.0, 
           size = 8.0, 
           angle = 30, 
           colour = "blue") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "springgreen")
sa_2

sa_3 <- sa_2 +
  # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
  #          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
  #          transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 110, x.max = 115, y.min = -33, y.max = -31, # Set location of symbol
        scale = 4, symbol = 16)
sa_3



#load africa_map
sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4


sa_final <- sa_4 +
  scale_x_continuous(breaks = seq(16, 32, 4),
                     labels = c("16°E", "20°E", "24°E", "28°E", "32°E"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(-36, -24, 4),
                     labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
                     position = "right") +
  labs(x = "", y = "")
sa_final


ggsave(plot = sa_final, filename = "figures/southern_africa_final.pdf", 
       height = 6, width = 8)


















