### Functions homework
### Created by: Diana Gao
### Created on 2024-10-30

# Load packages------
library(here)
library(tidyverse)
library(janitor)
library(ggrepel)

# Load data-------
data <- read_csv(here("week9", "data", "AVONET_Raw_Data.csv")) # bird morpho measurements from AVONET
names <- read_csv(here("week9", "data", "Honeycreeper_info.csv")) # gathered honeycreeper info

# Funcitons --------
#Function to get average values of each measurement by species 
summ_mean <- function(a, x, y, z) {
  value <- {{a}} %>% 
    group_by({{z}}) %>%
    summarise(across({{x}}:{{y}}, ~mean(.x,na.rm = TRUE)))%>%
    distinct()
  return(value)
} # a is dataframe, x is first col of measurements, y is last col of measurements, z col with species names in it

#function to plot data against tarsus length 
x_vs_tarsus <- function(a, x, y, z) {
  ggplot(a, aes(x = {{x}}, y = {{y}}, label = {{z}})) + 
    geom_point() + 
    geom_text_repel(size = 2.5) + 
    theme_bw()
} # a is dataframe, x is col wwith tarsus length, y is col of measurements, z col with species names in it

# Clean the data-----------
clean <- data %>% # dataset has multiple birds of same sp
  rename("Sci name" = eBird.species.group) %>% # make col names match
  inner_join(names, by = "Sci name") %>% #keep only cols with honeycreeper sp
  select("Sci name", "Beak.Length_Culmen", "Beak.Width", "Beak.Depth", "Tarsus.Length") %>% # select out a few measurements I want
  clean_names() %>% # clean the names cuz damn
  summ_mean(beak_length_culmen, tarsus_length, sci_name)

#Plotting
clean %>%
  x_vs_tarsus(tarsus_length, beak_length_culmen, sci_name) # beak length vs tarsus length

clean %>%
  x_vs_tarsus(tarsus_length, beak_width, sci_name) # beak width vs tarsus length

clean %>%
  x_vs_tarsus(tarsus_length, beak_depth, sci_name) # beak depth vs tarsus length