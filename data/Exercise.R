#summary stasts
#29th January 2020
#Thulie Mkhayiphi
########################
  
#loading packages
library(tidyverse)

#loading the data

laminaria<-read_csv("data/laminaria.csv")

laminaria %>% # Chose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>%
  summarise (avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length), # Create a summary of the sd of the total lengths
            var_stp_ln = var(total_length),
            med_stp_ln= median(total_length))

##plotting-function ggplot

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "salmon", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

# plotting
 
# Load libraries
library(tidyverse)
            
# Load data
ChickWeight <- datasets::ChickWeight
# Create a basic figure
  ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point() +
  geom_line(aes(group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")
  theme_bw()

  ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
    geom_point(aes(size = weight)) +
    geom_smooth(method = "lm", size = 1.2)

## faceting
  library(tidyverse)
  library(ggpubr)

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
    geom_point() +
    geom_smooth(method = "lm") + # Note the `+` sign here
    facet_wrap(~Diet, ncol = 4) + # This is the line that creates the facets
    labs(x = "Days", y = "Mass (g)")  

ChickLast <- ChickWeight %>% 
  filter(Time == 21)  

line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")

line_1


lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")


# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
histogram_1


# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1

ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend


    