#Crickets_exercise
#Thulisile Mkhayiphi
#13/02/2020

#The aim of my exercise is to compare different sites.A,B and C.
#I wanted to know which site that is mostly visited by the coackroaches.
#These site were being visited by crickets that are intruder and Residence
#The two crickets were differentiated by mark,residence ones were marked and Intruder were not.

# Load libraries
library(tidyverse)
library(ggplot2)


# Load the data
crickets <- read_csv("data/crickets.csv")


# Examine the data
head(crickets) 
tail(crickets) 
glimpse(crickets) # A more thorough summary
names(crickets) # THe names of the columns

# Subsetting data
crickets %>% # Tell R which dataframe to use
  select(Site, Intruder) %>% # Select site and intruder columns
  slice(8:33) # Select specific rows


# The row with the greatest length
crickets %>% # Tell R which dataset to use
  filter(Intruder == max(Intruder)) # Select row with max total length


#Summarise variables (columns) with summarise()

crickets %>% 
  summarise(mean_Residence = mean(Residence, na.rm = TRUE))#another way of calculating the mean of the residence

crickets %>% 
  summarise(mean_intuder = mean(Residence, na.rm = TRUE), #calculating the mean of the residence
                     sd_Residence = sd(Residence, na.rm = TRUE),#calculating the sd of the residence
                     min_Residence= min(Residence, na.rm = TRUE),#calculating the min of the residence
                     max_Residence= max(Residence, na.rm = TRUE)#calculating the max of the residence
)

crickets %>% 
  summarise(mean_intuder = mean(Intruder, na.rm = TRUE), #calculating the mean of the Intruder
            sd_Intruder = sd(Intruder, na.rm = TRUE),#calculating the sd of the Intruder
            min_Intruder= min(Intruder, na.rm = TRUE),#calculating the min of the Intruder
            max_Intruder= max(Intruder, na.rm = TRUE)#calculating the max of the Intruder
  )


#Visualisations using linear model to compare sites where crickets visited
#plot 1 for Residents
ggplot(data =crickets, aes(x =Trials , y = Residence, colour = Site)) + 
   geom_smooth(method = "lm", size = 1.5)

#Plot 2 for Intruders
ggplot(data =crickets, aes(x =Trials , y = Intruder, colour = Site)) +
  geom_smooth(method = "lm", size = 1.5)

#According to the first plot site C is the site that is mostly visited by Residence crickets
#site A as number of trials increases number of crickets visiting the area drops
#number of intruder visiting site A decrease as trials increases.
#In plot 2 site C have lowest number of crickets viting the area 
#the reason might be the competition is too high Resident crickets took over.
#and the resources are not enough for them to survive in the area.
#According to the results Intruder crickets prefer site B than all other sites.
