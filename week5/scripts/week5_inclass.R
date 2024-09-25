# week 5 in class lecture script: practicing joins using data from Becker and Silbiger (2020)
#created by: Diana Gao
#created on: 2024-09-24

#load libraries ------
library(tidyverse)
library(here)

#load raw data ------
EnviroData <- read.csv(here("week5", "data", "site.characteristics.data.csv")) # Environmental data from each site

TPCData<-read_csv(here("week5","data","Topt_data.csv")) #Thermal performance data

#clean/edit the data ------
EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%
  arrange(site.letter) # arrange the dataframe by site

FullData_left<- left_join(TPCData, EnviroData_wide) %>% # joining env and coral data, join with by = join_by(site.letter) if you don't have properly matching IDs
  relocate(where(is.numeric), .after = where(is.character)) # relocate all the numeric data after the character data

#thinkpairshare
summarydata <- FullData_left %>%
  select(-site.block) %>%
  group_by(site.letter) %>% # group by site letter
  summarise_if(is.numeric, # select only columns that have numeric data
               #E:Topt, light:substrate.cover # could also do this if using summarise_at
              funs(mean = mean , var = var), # calc mean and variance
              na.rm = TRUE) 
# another way to do it
summarydata <- FullData_left %>%
pivot_longer(cols = E:substrate.cover, 
             names_to = "Parameter", 
             values_to = "Values") %>%
  group_by(site.letter, Parameter) %>%
  summarise(mean_values = mean(values), 
            var_values = var(values))
#another another way
summarydata <- FullData_left %>%
  group_by(site.letter) %>%
  summarise(across(where(is.numeric), list(mean=  mean, var = var), na.rm = TRUE))

#making a tibble -------
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8)) 
T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9)) # make tibbles with 2 columns
left_join(T1, T2)
right_join(T1, T2) # note the site.id is 1:1 match so these two have different results
inner_join(T1, T2)
full_join(T1, T2)
semi_join(T1, T2)
anti_join(T1, T2)

#for fun :)
install.packages("cowsay")
library(cowsay)
# I want a shark to say hello
say("hello", by = "shark")
# I want a cat to say I want pets
say("I want pets", by = "cat")