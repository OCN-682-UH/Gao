#intro-------------
#Script for creating a plot in 1 hr for homework w palmerpenguins data
#created by: Diana Gao
#Created on: 2024/9/16


#libraries----------
library("palmerpenguins")
library("tidyverse")
library("here")
library("ggthemes")

#look @ data + store it as r object---------------
#glimpse(penguins) # tells you rows, columns, with names and datatype, and glimpse of data
view(penguins) # look at penguins dataset fully
#penguins <- penguins

#analysis----------
# Calculate the mean and the standard deviation for M and F penguins' body mass
my_sum <- subset(penguins, !is.na(sex)) %>%
  group_by(species, sex) %>%
  summarise( 
    mean=mean(body_mass_g),
    sd=sd(body_mass_g)
  )

#make a body mass vs sex violin plot, with separate plots for each sp
ggplot(subset(penguins, !is.na(sex)), # remove data where sex is NA
       mapping = aes(x = sex, # use sex for x axis
                     y = body_mass_g, # use body mass for y axis
                     group = species, # group above data by species
                     color = species)) + # then label each sex diff colors
  geom_jitter() + # use jitterplot
  geom_violin(alpha = 0.5,) + # also violin plot
  #geom_errorbar(aes(x = sex, ymin=mean-sd, ymax=mean+sd)) + #also add error bars w +/-1 standard deviation, but this isnt working? 
  labs(x = "Sex", 
       y = "Body Mass (g)" # changing x and y axis labels
  ) + 
  facet_wrap(~ species, ncol=3) + # facet, ncol=3 means make it two columns
  scale_color_manual(values = c("magenta", "blue", "orange")) + # colors are from viridis
  theme_bw() + #bw theme
  theme(legend.position="none") # remove the legend

#export plot
ggsave(here("week3", "output", "penguinplot_HW.png"), 
       width = 7, height = 5)
