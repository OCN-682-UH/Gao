#### Notes for the at home lecture week 8 
#### Created by: Diana Gao
#### Created on 2024-10-22

### library -------------
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)

### trying patchwork --------
# plot 1
p1<-penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species))+
  geom_point()
p1

# plot 2
p2<-penguins %>%
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)

p1+p2 + # Brings two plots together side by side. For on top of eachother, /
  plot_layout(guides = 'collect') + # collects the legends on one side
  plot_annotation(tag_levels = 'A') # labels each plot (A and B)

### trying ggrepel----------
#View(mtcars)
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) + # extract row names and put them as labels. For labeling according to col, just put name of col
  #geom_text() + # creates a text label using label above. This makes labels but theyre all on top of eachother
  geom_text_repel() + # this makes the labels repel eachother. If you want a box around text use geom_label_repel()
  geom_point(color = 'red')

### trying gganimate ----------
plot <- penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )+
  ease_aes("sine-in-out") +
  labs(title = 'Year: {closest_state}') # the thing in brackets is the thing that changes, not in brackets is static
    animate(plot, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer()) + # have to add this smh
    anim_save(here("week8","output","mypengiungif.gif")) # this doesn't work alone

### trying magick -------------------
penguin<-image_read("https://img.freepik.com/premium-psd/african-penguin-isolated-transparent-background-png-psd_888962-1729.jpg?w=996") # read in pic of penguin using link
penguin #hehe

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()
ggsave(here("week8","output","penguinplot.png")) # save graph as png

penplot<-image_read(here("week8","output","penguinplot.png"))
penplot
out <- image_composite(penplot, penguin, offset = "+70+30") # oh my I chose a way too big pic I think
out

pengif<-image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center")
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
animation # why is my gif so massive lol


### sourdough! -------
remotes::install_github("andrewheiss/sourrr")
library(sourrr)
build_recipe(final_weight = 900, hydration = 0.75)
## 450g flour (514.3g total; 64.3g from starter)
## 321g water (385.7g total; 64.3g from starter)
## 129g starter (25%; 100% hydration)
## 10g salt (2%)
## ---
## 75% hydration
## 910g final loaf
lol