
library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)



New_dataframe<-laminaria %>% 
  mutate(total_length_half=total_length/2) %>% 
  select(site,total_length_half(na.rm=TRUE))


        
laminaria %>%
  group_by(site,blade_length)%>%
  summarise(mean_blade_length=mean(blade_length),
            min_blade_length=min(blade_length),
            max_blade_length=max(blade_length),n=n())


laminaria %>%
  group_by(site) %>%
  summarise(max_stipe_mass=max(stipe_mass))


laminaria %>%
  select(site, region, stipe_length, stipe_mass)

###
    
            