#homework_assignment
#Intro R Workshop: Data Manipulation, Anlyses and Visualisation 
#Thulisile Mkhayiphi
#10/02/2020


#load libraries
library(tidyverse)


# Load the data
BOD<-datasets::BOD

#Section 1 
#Examine the built-in dataset BOD. Which of the following is true: 
head(BOD) 
tail(BOD) 
glimpse(BOD) 
names(BOD) 

#c. BOD is tidy: each row is an observation with two values (time and demand) 


#load the data
BJsales <-datasets::BJsales

EuStockMarkets  <-datasets:: EuStockMarkets 


DNase <-datasets:: DNase 

Formaldehyde <-datasets:: Formaldehyde

Orange <-datasets:: Orange 

UCBAdmissions <-datasets::UCBAdmissions

#Which of the following built-in datasets is tidy (you can pick more than one): 
#DNase  
#Formaldehyde
#Orange

#Section 2 
#load libraries
library(dplyr)
library(dslabs) 
data(murders) 

#Examine the  data
head(murders) 
tail(murders) 
glimpse(murders)
view(murders)
names(murders) 

#Write a paragraph describing the murder datasets
#Datasets consists of 5 columns,state,abb,region,population and totals.
#It shows or state and compares number of population per state and its region.
#According to the data Texas has high number of totals 
#and it seems like number of population is highest in region South and West

murders <- mutate(murders, population_in_millions = population / 10^6) 


# Subsetting data

murders_A<-murders %>% # Tell R which dataframe we are using and saves result as a dataset
  select(state,population) # Select only specific columns


murders_A <- murders_A %>% filter(state!="Florida")

no_south <- murders %>% filter(region!="South")

#how many states are there
#34


#using%in% to filter with dplyr(craeting new data frame showing the data for New York)
A_texas <- murders %>%  filter(state%in% c("New York","Texas"))



#calculate the population size of South and West regional
South_West <- murders%>% filter(region != c("South","West"))
South_West_POP <- sum(South_West$population)
South_West_POP <-  as.data.frame(South_West_POP)

#Create a new data frame with population size of the Northeast region
NorthEast <- murders %>% filter(region ==c("Northeast")) %>% 
  select(population)

#Create two plots of your choice and explain visible trends

#load libraries
library(tidyverse)
library(ggplot2)


#2 plots of my choice
#plot 1
ggplot(data =murders, aes(x =state , y = population, colour = region)) +
  geom_point(aes(size = population)) +
  geom_smooth(method = "lm", size = 1.2)

#plot 2
ggplot(data =murders, aes(x =region , y = total, colour = region)) +
  geom_point(aes(size = population)) +
  geom_smooth(method = "lm", size = 1.2)



#Compare with population size of the South with the population size of the West
#Accoding to the plots,West region has highest number of population compared to South
#The West region at the beginning was very slowly but as time passes by it is reaching the maximam capaccity
#The South population also show a rapid growth in number of population 
#and North Central has lowest number of population compared to all other regions



#Create a new data frame where the total>20 but <100 and to exclude the value 120
above_20 <- murders %>% 
  filter(totals>20)
between_20_and_100 <- above_20 %>% 
  filter(total<100)

library(tidyverse)
library(dplyr)

#Create an object, containing from 10th to 24th row and 26th row. Hint: consider using the slice() function.
btwn_10_and_24 <- murders %>% #select specific columns
  slice(10:24,26)

#Use as_tibble to convert the murders data table into a tibble and save it in an object called murders_tibble.
murders_tibble <- as.tibble(murders) %>% #for dplyr package
   group_by(region)


# tidyverse code that is equivalent to this code:
murders_tibble <- as.tibble(murders) #for tidyverse package


#Section 3 
#load libraries
library(dplyr) 
library(dslabs)
library(tidyverse)
data(heights)


#Write a paragraph describing the heights dataset 
#The table consist 1050 rows,the height column and sex column.
#The heights btween two genders differ.
#According to the data Males has highest hight of 75.00 and 70 female with69.
#Females has lower heights compared to males.


head(heights)
tail(heights) 
glimpse(heights)
view(heights)
names(heights) 

#calculating mean,sd,min,max
heights %>%
  group_by(sex)%>%
  summarise(avg_height=mean(height),#create a summary of the mean of the males
            sd_height=sd(height),#create a summary of the sd of the males
            min_height=min(height),#create a summary of the min of the males
            max_height=max(height),#create a summary of the maximum of the males
            med_height=median(height))#create a summary of the median of the males




#section 4

#(a)Count the number of elements are missing in both x and y

DATA <- data.frame(x=c(1,6,21,19,NA,73,NA),y=c(NA,NA,3,NA,13,24,NA))
 Missing_x <- DATA %>% 
  select(everything()) %>%   #replace to your needs
  summarise_all(funs(sum(is.na(.))))
 

 #(b)Transform the code, used above (a), into a function
 new_code <- function(datasets){datasets %>%  select(everything()) %>% 
   summarise_all(funs(sum(is.na(.))))
 }  
 
 
 #(c)Create three new vectors and test the function created in (b)

own_DATA <- data.frame(x=c(1,3,6,9,NA,15,NA,NA),y=c(NA,NA,NA,8,10,12,14,16),z=c(NA,5,10,15,20,25,NA,69))

missing_values <- new_code(own_DATA)



#Section 5

 seasonal_data <- data.frame(year=c(2015,2016,2017,2018),
              winter = c(41, 39, 47, 40),   
              spring = c(41, 46, 57, 45),
              summer = c(75, 52, 85, 66),
              Autumn = c(57, 66, 52, 56))
 
 
#Using the data above,design an hypothesis,
 #The temperatures are higher in summer and Autum   # hypothesis
 #The temperatures are lower during winter and spring    # hpothesis
 
#then create two plots and write a paragraph discussing your findings 
 
 S_data1 <- seasonal_data %>% 
   gather(winter,spring,summer,Autumn,key="season",value = "temp")
 
 
 
 ##plotting_function ggplot
 #load libraries
 library(tidyverse)
 library(ggplot2)
 library(ggpubr)
 
 seasonal_data_1 <- ggplot(data = S_data1, aes(x = year, y = temp, colour = season)) +
   geom_point() +
   geom_smooth(method = "gam") +
   labs(x = "years", y = "Temperature (c)")
 seasonal_data_1
 
 # Note that we are using 'ChickLast', not 'ChickWeight'
 seasonal_data_1 <- ggplot(data = S_data1, aes(x = temp)) +
   geom_histogram(aes(fill = season), position = "dodge", binwidth = 90) +
   labs(x = "years", y = "Temperature")
 seasonal_data_1
 
 
#write a paragraph discussing your findings
 #According to the findings plot 1 and 2 illustrate that summer 
 #and Autum  has highest temperatures  over the years compared to other seasons
 #Winter has lower temperatures from 2015 to 2018
 #The temperatures are almost constant over the years.
 #and springs temperature are increasing toward 2018 while autumn drops
 

 
 cats_data<- tibble(cats = c("A", "B", "C"),
                    position = c("1-2-3", "3-1-2", "2-3-1"),
                    minutes = c(3, 3, 3),
                    
                    seconds = c(12, 44, 15))
 cats_data 
 
 #Using the seperate() function split the position column into new three columns. The new column names will be
 #first_place, second_place and third_place.
 
 cats_data_1 <- cats_data %>% 
   separate(col = position, into = c("first_place", "second_place","third_place"), sep = "-")
 
 
 #Unite the minutes and seconds column into its own column. 
 #The new column name will be total_time.
 cats_data_2 <- cats_data_1 %>% 
   unite(minutes,seconds,col="total_time",sep="-")
 
 
 
 
 
 #Section 6
 #load libraries
 library(tidyverse)

 
 # Load the data
 Seatbelts <- read_csv("data/Seatbelts.csv")
 
 
 
# Examine the data
head(  Seatbelts) # First five lines
tail(  Seatbelts) # Last two lines
glimpse(Seatbelts) # A more thorough summary
names ( Seatbelts) # THe names of the columns
 
 
 
 #Gathering
 Seatsbelts_1 <- Seatbelts %>%
   gather(front, rear, key = "Car_layout", value = "nr_cars")
 
 
 #spreading
 Seatbelts_2 <- Seatsbelts_1%>% 
   spread(key = Car_layout, value = nr_cars)
 
 
 #separate
 Seatsbelts_3 <- Seatsbelts_1 %>% 
   separate(col = Car_layout, into = c("front"))
 
 # Load the data
 Titanic <- read_csv("data/Titanic.csv")
 
 # Examine the data
 head( Titanic) # First five lines
 tail( Titanic) # Last two lines
 glimpse( Titanic) # A more thorough summary
 names (Titanic) # THe names of the columns
 
 
 #joining
Titanic_4 <- left_join("Titanic_1","Titanic_2"),by = c("Class","src", "Age"))
 
 
 #arrange
 Titanic %>% 
   arrange(Sex, Age)#arranging data into sex and age
 
 
 Titanic %>% 
   arrange(desc(Class))#arranging data into class
 
 
 # Select columns individually by name
 Titanic %>% 
   select(Sex, Age, Survived)
 
 
 # Select all columns except those stated individually
 Titanic %>% 
   select(-X1, -Survived)#selecting data ecept for x1 and survived
 
 
 
 # Group by class
Titanic_Class <- Titanic %>% 
   group_by(Class)
 
 
 
 #mutate
#load libraries
library(tidyverse)

 Titanic %>% 
   mutate(half_Freq = Freq/2)
 
 
