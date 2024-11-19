#load libraries ------
library(tidyverse)
library(here)
library(tidytuesdayR)
library(stringr)
library(rworldmap) # tried to use "map" and "mapdata" packages but their country names did not match the tidy tuesday ones :(
                  # the map data in this package does contain ISO codes 
library(PNWColors)
library(classInt)
#heat map with number of subdivisions? see if there is some sort of reltionship w country area and subdivision


#load data ----
tuesdata <- tidytuesdayR::tt_load(2024, week = 46)

countries <- tuesdata$countries
country_subdivisions <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries


#data cleaning
count_subdiv <- country_subdivisions %>%
  select(name, alpha_2) %>% # select only the subdiv and alphas
  rename(subdivision = name) %>% # clean subdivisions so stuff doesn't get confusing w join
  left_join(countries, join_by(alpha_2)) %>% #join on so that each subdiv has the data of the country it is a a part of
  group_by(name) %>%
  count(name)

name_subdiv <- country_subdivisions %>%
  select(name, alpha_2) %>% # select only the subdiv and alphas
  rename(subdivision = name) %>% # clean subdivisions so stuff doesn't get confusing w join
  left_join(countries, join_by(alpha_2)) %>% #join on so that each subdiv has the data of the country it is a a part of
  group_by(name) %>%
  summarise(subdivision = str_c(subdivision, collapse=", "))

clean <- name_subdiv %>%
  left_join(count_subdiv, join_by(name)) %>%
  left_join(countries, join_by(name))

map_with_data <- joinCountryData2Map(clean, 
                    joinCode = "UN",
                    nameJoinColumn = "numeric",
                    nameCountryColumn = "name") 

#plotting ------
pal<-pnw_palette("Lake", type = "continuous") # make a color palette
classInt <- classIntervals(map_with_data$n, style="jenks")
catMethod = classInt[["brks"]] # set the values for the legend breaks

# now actually make the map
fin_map <- mapCountryData(map_with_data, 
               nameColumnToPlot="n", # data plotted is count
               addLegend=FALSE, # remove default legend
               catMethod = catMethod, # set breaks method
               colourPalette = pal) # palette is set to pal defined earlier
do.call(addMapLegend, c(fin_map, legendLabels="all", legendWidth=0.5, legendIntervals="data", legendMar=2)) # make the legend nicer lol

#continuus
fin_map <- mapCountryData(map_with_data, 
                          nameColumnToPlot="n", # data plotted is count
                          addLegend=FALSE, # remove default legend
                          numCats = 100,
                          catMethod = "fixedWidth", # set breaks method
                          colourPalette = pal) # palette is set to pal defined earlier
do.call(addMapLegend, c(fin_map, legendLabels="all", legendWidth=0.5, legendMar=2)) # make the legend nicer lol
