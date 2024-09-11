#intro-------------
#Week 3 scripts for learning ggplot 
#created by: Diana Gao
#Created on: 2024/9/10


#libraries----------
library("palmerpenguins")
library("tidyverse")


#look @ data---------------
glimpse(penguins) # tells you rows, columns, with names and datatype, and glimpse of data
view(penguins) # look at penguins dataset

#analysis----------
ggplot(data = penguins, #tells you that the dataset being used is "penguins"
       mapping = aes(x = bill_depth_mm,  #bill depth is used for x
                     y = bill_length_mm, #bill length is used for y
                     color = species,  #color is determined by species
                     alpha = flipper_length_mm, #transparency is determined by flipper length
                     size = body_mass_g) #shape is determined by island
       )  +
  geom_point() + #tells ggplot to use a scatterplot
  labs(title = "Bill depth and length", #changing labels for things
       subtitle = "Dimensions for Adelie, Chinstrap, and Getoo Penguins", 
       x = "Bill depth (mm)", 
       y = "Bill length (mm)", 
       color = "Species", 
       size = "Body mass (g)",
       caption = "Source: Palmer Station LTER / palmerpenguins package", 
       ) + 
  scale_color_viridis_d()

##### for next examples, no labelling/titles but for final plots should make sure to have
#####
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point(size = 2, alpha = 0.5) # using settings to have everything be size 2 with 50% transparency

##### faceting
#facet_grid
ggplot(penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm, 
           color = species))+
  geom_point()+
  facet_grid(species~sex) + #facet_grid (x, y) each subplot is one species/sex combo
  guides(color = FALSE)

#facet_wrap
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species, ncol=2) #ncol=2 means make it two columns

