### Video Lecture Notes
### Created by: Diana Gao
### Created on: 2024/10/29

# Load libraries
library(tidyverse)
library(here)

# Load data -----
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

# Start-----

# what is a factor
fruits<-factor(c("Apple", "Grape", "Banana"))
fruits

# starwars data
glimpse(starwars)
starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE)

# lump all the rare species (<3 individuals) together into an "other"
star_counts<-starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>%
  count(species)
star_counts

# Using factors to reorder a bar chart
star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n))+ # reorder the factor of species by n
  geom_col()

#when you filter data you might have to drop levels if you have a factor
starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels
mutate(species = fct_recode(species, "Humanoid" = "Human"))
# Income data
total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor

# Graph
total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+
  geom_line() 
# the quintiles are in alphabetical, change it

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+ # order income_quintile first by year and then by income_dollars_sum
  geom_line()+
  labs(color = "income quantile")

# to set the factor order manually
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1