##############
#Week 4 HW script from video lecture
#Prompt - Write a script that:
#Using the chemistry data:
#1. Create a new clean script
#2. Remove all the NAs
#3. Separate the Tide_time column into appropriate columns for analysis
#4. Filter out a subset of data (your choice)
#5. use either pivot_longer or pivot_wider at least once
#6. Calculate some summary statistics (can be anything) and export the csv file into the output folder
#7. Make any kind of plot (it cannot be a boxplot) and export it into the output folder
#8. Make sure you comment your code and your data, outputs, and script are in the appropriate folders
#Created by: Diana Gao
#Created on: 2024-9-24
##############

#load libraries ------
library(tidyverse)
library(here)

#load data -------
ChemData<-read_csv(here("week4","data", "chemicaldata_maunalua.csv"))
View(ChemData)
#glimpse(ChemData)

#cleaning the data -------
ChemData_clean <- ChemData %>%
  drop_na() %>% #filters out everything that has NAs
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns - Tide and time
           sep = "_") %>% # separate by _ since og data is xx_xx
  filter(Season == "SPRING") %>% # filter out only spring data
  pivot_longer(cols = Temp_in:Salinity, # the cols you want to pivot. This says only select temp and salinity
             names_to = "Variables", # the names of the new cols with all the column names
             values_to = "Values") %>% # names of the new column with all the values
  group_by(Waypoint, Variables, Site, Zone, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>% #get mean values of ea measurement by site and time
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% # make wide again
  rename(avg_salinity = Salinity, 
         avg_temp_in = Temp_in) %>%
write_csv(here("week4","output","summary_videoHW.csv"))  # export summary data as a csv to the right folder
#plot it
facet_titles <- c(BP = "Black Point", W = "Wailupe")
ChemData_clean %>%
  ggplot(aes(x = avg_temp_in, y = avg_salinity, #denote x and y
             group = Site,
             shape = Zone,
             color = Time)) + #change color based on time
  geom_point() + #make scatterplot
  facet_wrap(~Site, 
             labeller = labeller(Site = facet_titles)) + 
  labs(title = "Temperature vs Salinity by Site",
      x = "Average Temperature (C)", 
      y = "Average Salinity" # changing x and y axis labels
      ) +
  theme_bw()
ggsave(here("week4", "output", "week4_video_HW.png"), 
       width = 7, height = 5)
