#intro-------------
#Week 3 scripts for learning ggplot with palmerpenguins data
#created by: Diana Gao
#Created on: 2024/9/16


#libraries----------
library("palmerpenguins")
library("tidyverse")
library("here")
library("beyonce")
library("ggthemes")

#look @ data---------------
glimpse(penguins) # tells you rows, columns, with names and datatype, and glimpse of data
#view(penguins) # look at penguins dataset fully

#analysis----------
penguinplot <- ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, # use bill depth for x axis
                     y = bill_length_mm, # use bil length for y axis
                     group = species, # group above data by species
                     color = species)) + # then label these species with diff colors
  geom_point()+ # scatterplot
  geom_smooth(method = "lm")+ # linear model for regression best fit line
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)" # changing x and y axis labels
  ) +
  scale_color_manual(values = beyonce_palette(20)) + # colors are from beyonce palette 10
  theme_bw() + #choose the bw theme
  theme(legend.key.height = unit(12, "points"), # change the legend key (aka the point+line+grey) height
        legend.background = element_blank(), # make legend bg transparent
        legend.position = "inside", # put legend inside plot
        legend.position.inside = c(.1, .85)) # change legend position inside plot w 2-number vector, whole plot is from 0,0 to 1,1

ggsave(here("week3", "output", "penguinplot.png"), 
       width = 7, height = 5)
