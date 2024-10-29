#### Test R script for good plot bad plot
#### Using cheese dataset from TidyTuesday
#### Created by: Diana Gao
#### Created on: 2021/10/25

### Loading libraries ------------
library(here)
library(tidyverse)
library(tidytuesdayR)
library(stringr)
library(ggridges)      ## ridgeline plots
library(ggpubr)
### Load in data ------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses
head(cheeses)

### Clean the data ---------------
# how important is the color and fat content to flavor note?
# grouped by hardness of cheese
#make the fat content into categories of l m h
#hardness <- c("hard", "soft", "firm") # list of strings which the "type" must contain 
test <- str_detect(cheeses$type, "hard")|str_detect(cheeses$type, "soft")|str_detect(cheeses$type, "firm")


cheeseclean <- cheeses %>%
  select(cheese, type, flavor, color, fat_content) %>% # select out these cols
  mutate(Hard =  str_detect(cheeses$type, "hard")|str_detect(cheeses$type, "firm")) %>% # make a col if it is hard or firm then true
  mutate(Soft = str_detect(cheeses$type, "soft")) %>% # make a col if it is soft then true
  drop_na(cheese, type, flavor, color, fat_content) %>% # drop NAs in these cols
  separate(type, c("type1", "type2", "type3"), sep = ",") %>% # separate type column by commas
  select(cheese, type1, flavor, color, fat_content, Hard, Soft) %>% # out type2 and type3 cols bc they are redundant, only type1 col kept
  filter(str_detect(fat_content, "^[0-9]{2}[%]") | str_detect(fat_content, "^[0-9]{2}[\\.][0-9]{1,3}[%]")) %>% # only select the rows with ONE % in the fat content
  separate_longer_delim(flavor, delim = ", ") %>% # separate all the notes into different rows with all other info repeated
  mutate(flavor = str_trim(flavor))
  
cheeseclean$fat_content <- as.numeric(gsub("%", "", cheeseclean$fat_content)) / 100 # fat content was character, turn into numeric decimal
# plotting -----------
# a little something to be able to see our means to make our cheese wheel :)
cheesesum <- cheeseclean %>% 
  group_by(flavor) %>%
  summarise(fat = mean(fat_content)) %>%
  mutate(notfat = 1-fat)


# bad plot :(
img.file <- here("goodplotbadplot", "data", "cheese.png")
img <- png::readPNG(img.file)
ggplot(cheeseclean, aes(x=flavor, y = fat_content, shape = Hard, color = color)) + 
  background_image(img) +
  geom_jitter(width = 3, height = .1) +
  labs(title = "Flavor vs Fat content",
       subtitle = "yay moldy cheese chart:)") + 
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here("goodplotbadplot","output","cheeseplot.png")) # save graph as png

# good plot
lowdata <- c("yeasty", "woody", "pungent", "mellow", "garlicky", "caramel", "butterscotch")
cheeseclean %>%
  filter(flavor != "yeasty" & flavor != "woody" & flavor != "pungent" & flavor != "mellow" & flavor != "garlicky" & flavor != "caramel" & flavor != "floral" & flavor != "butterscotch") %>% # remove notes that aren't showing up (not enough data)
  ggplot(aes(fat_content, flavor)) +
  geom_density_ridges(linewidth = 0, fill = "#FBEA74") + # ridge plot showing the fat content distribution for each flavor note
  scale_x_continuous(labels = scales::percent) + # change x axis to percent format
  labs(title = "Fat content for each flavor note", 
       x = "Fat Content", 
       y = "Flavor Note", 
       caption = "Data from cheese.com") + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())






######## graveyard ----------------


ggplot(cheeseclean, aes(x = flavor, y = fat_content,)) +
  geom_area()+
  facet_grid(rows=flavor)

  
  theme_ridges() + 
  theme(legend.position = "none") +
  coord_flip()


ggplot(cheeseclean, aes(x = fat_content, y = flavor, 
                 color = flavor, fill = flavor)) +
  ggdist::stat_halfeye(
    adjust = .33,
    width = .67, 
    color = NA,
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, size = 3
  ) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  coord_flip()
# bad plot idea, make all the flavors diff columns and jitter plot the amount of percent fats in each
# good plot idea, 
  
mutate(fat_content = replace(fat_content, fat_content >= 60, "high")) %>% # looking at the fat content distribution of dataset (~30-75) split into 15s to make 3 categories. 75-15 = 60, upper bound
  mutate(fat_content = replace(fat_content, fat_content <= 45, "low")) %>% # Split range into equal thirds (+-15). so 30+15 = 45 lower bound
  mutate(fat_content = replace(fat_content, fat_content >45 & fat_content < 60, "medium")) %>% # between 45 and 60 is medium
  
str_replace_all(pattern = "\\.", replacement = "-") %>% # replace periods with -
cheeses[17, fat_content]<- "34%" %>% # manually changing certain values that weren't in percent form, 34g/100g og
  cheeses[18, fat_content]<- "53%" %>% # manually changing certain values that weren't in percent form, 45-60% og so average is 53%
  cheeses[22, fat_content]<- "35%" %>% # manually changing certain values that weren't in percent form, 30-40% og so average is 35%
  cheeses[28, fat_content]<- "54%" %>% # manually changing certain values that weren't in percent form, 54.23/100g og
  cheeses[33, fat_content]<- "41%" %>% # manually changing certain values that weren't in percent form, 34-48% og so average is 41%
  cheeses[36, fat_content]<- "43%" %>% # manually changing certain values that weren't in percent form, 40-45% og so average is 43%
  mutate(fatcont = mean()) # fat content is mixed, some are g/g, some are % ranges. Make everything into a single % number
str_sub("[0-9]{2}$%")
a
mean()
averagae
  mutate(Firm = str_detect(cheeses$type, "firm")) %>%
  mutate(fat_content) # clean up fat content column so it is all in percent values >>

    
  str_detect(cheeses$type, "hard")|str_detect(cheeses$type, "soft")|str_detect(cheeses$type, "firm")


### plotting ------
  ggplot(cheeseclean, aes(x = type1, y = fat_content, color = milk)) +
    geom_jitter()
cheeseplot <- ggplot(x = flavor, y = fat_content)