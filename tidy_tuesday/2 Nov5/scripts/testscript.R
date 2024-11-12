#libraries -----
library(tidyverse)
library(tidytuesdayR)
library(here)
library(gganimate)

#load data -------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 45)
democracy_data <- tuesdata$democracy_data

#goal is to plot number of ea diff type of govt over time w/ animation
#data starts at 1950
#learn how to use line geom with gg animate

# clean the data ----
clean <- democracy_data %>%
  select(country_name, year, is_democracy, is_communist, is_monarchy) %>%
  group_by(year) %>%
  mutate(democracy = sum(is_democracy, na.rm = TRUE), 
            communist = sum(is_communist, na.rm = TRUE), 
            monarchy = sum(is_monarchy, na.rm = TRUE)) %>%
  select(year, democracy, communist, monarchy) %>%
  pivot_longer(!year, names_to = "regime", values_to = "count")

# plot the data
animation <- clean %>%
  ggplot(aes(x = year, y = count, 
             color = regime)) +
  geom_line() + 
  labs(title = "Number of Democratic Nations over Time", 
       caption = "Data is from tidyTuesday 2024-11-15", 
       x = "Year", 
       y = "Number of Nations") + 
  theme_bw() +
  transition_reveal(year)

animate(animation, nframes = 200, #make into 200frames
        fps = 24, #set the frame by 20 frame per seconds
)
