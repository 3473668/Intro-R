# Day 3
# Thulisile Mkhayiphi
#30_01_2020

# Load packages
library(tidyverse)
library(ggplot2)

# Load data
laminaria <- read_csv("data/laminaria.csv")

# Create a quick scatterplot
ggplot(data =laminaria, aes(x = stipe_mass, y = stipe_length)) +
geom_point(aes(colour = "sites"))


ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
geom_point(aes(colour = "sites")) +
scale_colour_gradient(colours = c(#48B4B6,#48A989,#619A5F,#79873F,#8A7332,#925E35)

ggplot(data = urine, aes(x = osmo, y = ph)) +
geom_point(aes(colour = "sites"")) +
scale_colour_distiller() # Change the continuous variable colour palette

cols11 <- c(#48B4B6,#48A989,#619A5F,#79873F,#8A7332,#925E35)

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point()

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_polygon(colour = "black", fill = "red"  # The land mask



# Loading libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)











