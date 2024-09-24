##############
#Week 4 HW script from in-class lecture
#Prompt - Write a script that:
#1. calculates the mean and variance of body mass by species, island, and sex without any NAs
#2. filters out (i.e. excludes) male penguins, then calculates the log body mass, then selects only the columns for species, island, sex, and log body mass, then use these data to make any plot. Make sure the plot has clean and clear labels and follows best practices. Save the plot in the correct output folder.
#Created by: Diana Gao
#Created on: 2024-9-23
##############

#libraries ------- 
library(palmerpenguins)
library(tidyverse)
library(here)

#preview the data -------
#glimpse(penguins)
view (penguins)

#data analysis -------
#prompt 1: 
prompt1_by_sp <- penguins %>% # use penguin dataframe
  filter(!is.na(sex)) %>% #filter out entries where sex is NA
  group_by(species) %>% #group data by species
  summarize(mean_bm = mean(body_mass_g), 
            var_bm = var(body_mass_g)) # creates summary data table: mean, variance of body mass

prompt1_by_island <- penguins %>% # use penguin dataframe
  filter(!is.na(sex)) %>% #filter out entries where sex is NA
  group_by(island) %>% #group data by species
  summarize(mean_bm = mean(body_mass_g), 
            var_bm = var(body_mass_g)) # creates summary data table: mean, variance of body mass

prompt1_by_sex <- penguins %>% # use penguin dataframe
  filter(!is.na(sex)) %>% #filter out entries where sex is NA
  group_by(sex) %>% #group data by species
  summarize(mean_bm = mean(body_mass_g), 
            var_bm = var(body_mass_g)) # creates summary data table: mean, variance of body mass


#prompt 2:
prompt2 <- penguins %>% # use penguin data
  filter(sex != "male") %>% # filter out males (includes NAs)
  mutate(log_mass = log(body_mass_g)) %>% #calculate log body mass, put in new column log_mass
  select(species, island, sex, log_mass) # only keep columns species, island, sex, and log_mass

ggplot(data = prompt2, #use prompt2 data
       mapping = aes(x = species, y = log_mass, #for plot, x is sp and y is log_mass
       color = island) # color points by island
       ) + 
  geom_jitter() + 
  labs(x = "Species", 
       y = "Log (Body Mass (g))",
       color = "Island") + # changing x and y axis labels, capitalizing legend title
  theme_bw() + 
  theme(legend.position = "inside", # put legend inside plot
        legend.background = element_blank(), # make legend bg transparent
        legend.position.inside = c(.9, .25)) # change legend position inside plot w 2-number vector, whole plot is from 0,0 to 1,1
#export
ggsave(here("week4", "output", "week4_inclass_HW.png"), 
       width = 7, height = 5)