# week 5 homework
  #Read in both the conductivity and depth data.
  #Convert date columns appropriately
  #Round the conductivity data to the nearest 10 seconds so that it matches with the depth data
  #Join the two dataframes together (in a way where there will be no NAs... i.e. join in a way where only exact matches between the two dataframes are kept)
  #take averages of date, depth, temperature, and salinity by minute
  #Make any plot using the averaged data
  #Do the entire thing using mostly pipes (i.e. you should not have a bunch of different dataframes). Keep it clean.
  #Don't forget to comment!
  #Save the output, data, and scripts appropriately
#created by: Diana Gao
#created on: 2024-09-30

#load in libraries ------
library(tidyverse)
library(here)
library(lubridate)

#read in data --------
CondData <- read.csv(here("week5", "data", "CondData.csv"))
DepthData <- read.csv(here("week5", "data", "DepthData.csv"))

#clean data --------- 
#first Conductivity data 
is.character(CondData$date) # test if date is a character (in this case yes)

CondDataClean <- CondData %>%
  mutate(date = mdy_hms(date)) %>% 
  mutate(date = round_date(date, "10 seconds")) #round times to nearest 10 seconds

#depth data
is.character(DepthData$date) #character is yes
#mutate(as.character())
DepthDataClean <- DepthData %>%
  mutate(date = ymd_hms(date)) # just make sure date is in date format

#data analysis ------
combined <- inner_join(CondDataClean, DepthDataClean, by = "date") #keep only dates that are present in both

averaged <- combined %>%
  mutate(date = floor_date(date, unit = "minute")) %>% #round down all times to the minute
  group_by(date) %>%
  summarise(Avg_Temp = mean(Temperature, na.rm = TRUE), # get means of temp, salinity, depth
            Avg_Salinity = mean(Salinity, na.rm = TRUE), 
            Avg_Depth = mean(Depth, na.rm = TRUE)) %>%
  pivot_longer(cols = Avg_Temp:Avg_Depth, 
              names_to = "variables", 
              values_to = "values")

facet_titles <- c(Avg_Depth = "Depth", Avg_Salinity = "Salinity", Avg_Temp = "Temperature") # make list of what facet titles should be 
averaged %>%
  ggplot(aes(x = date, y = values), 
         group = variables) + #denote x and y, group by the type of measurement
  geom_point(alpha = 0.3, color = "cadetblue2") + #make scatterplot
  geom_smooth(span = .6, color = "black", linewidth = .5) + #add best fit line
  facet_wrap(~variables, scales = "free",  labeller = labeller(variables = facet_titles)) + # facet by variables, make pot axes independent, change faceted plot titles
  labs(title = "Depth, Salinity, and Temperature Over Time",
       subtitle = "Taken on 2021/1/15",
       x = "Time", 
       y = element_blank()) # changing x, removing Y labels


ggsave(here("week5", "output", "week5_HW.png"), 
       width = 7, height = 4)

