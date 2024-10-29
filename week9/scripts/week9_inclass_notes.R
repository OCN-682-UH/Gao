##### in class lecture 9: practicing funcitons
##### created by: Diana Gao
##### created on: 2024/10/22

####### intro to writing scripts
### libraries ---------
library(tidyverse)

### data -------
df <- tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df)

### cleaning data

df<- df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE))) %>% # keep column a as a, every value minus minimum value in col / diff max and min
  mutate(b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE))) %>% # keep column a as a, every value minus minimum value in col / diff max and min
  mutate(c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE))) # super easy to mess up when writing over and over :(

#so instead write a function! 
rescale01 <- function(x) { # x is the place where u can put params
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) # in the curly brackets put the function you want
  return(value) # then spit out "value" 
}

df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d)) # cool :D now can just put the col name in place of x

# make a function for changing F into C
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

fahrenheit_to_celsius (22)

# make a function changing C into K 
c_to_k <- function(temp_C) {
  temp_k <- temp_C + 273
  return(temp_k)
} #Remember Kelvin is celcius + 273.15

c_to_k # yay

###### Now diff example, penguin data. adding more pkg-----------

### libraries ----------
library(palmerpenguins)
library(PNWColors) # for the PNW color palette 

### graph------------
pal<-pnw_palette("Lake",3, type = "discrete") # create color palette of 3 discrete colors
ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and another example of how to manually change the legend title for colors
  theme_bw()

# turn above into a function
myplot<-function(){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
} # in this case didn't do return bc the graph willl already show (but not save as an object)

## but this function is too stiff - cannot be easily be used w diff data. So let's change it
myplot<-function(data, x, y){ # adding arguments 
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = x, y =y , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}

# test it
myplot(data = penguins, x = body_mass_g, y = bill_length_mm) # will return an error "object 'body_mass_g' not found"

#Even though body_mass_g exists within the penguin dataframe, there is still no individual parameter called body_mass_g in our environment and R is confused. There is a solution though from within the {rlang} package (a part of the tidyverse)!

#{rlang} uses what is literally called a"curly-curly" {{}} to help us assign variables that are column names in dataframes.

##Let's add curly-curlies to the column names and try again
myplot<-function(data, x, y){ 
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+ # note the curly curlies around x and y
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
myplot(data = penguins, x = body_mass_g, y = bill_length_mm)
# test with new functions 
myplot(data = penguins, x = body_mass_g, y = flipper_length_mm) # cool :) 


## can set defaults too
myplot<-function(data = penguins, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
# test it
myplot(x = body_mass_g, y = bill_length_mm) # Can still put in other data = arguments but will default to one in function
# can use + just like a normal ggplot 
myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

#### Adding if else statements---------
a <- 4
b <- 5
if (a > b) { # my question
  f <- 20 # if it is true give me answer 10
} else { # else give me answer 20
  f <- 10
}
# when you return f it says 10,so a < b :D
### graphing again, but have a conditional to add regression line 
myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines, it is TRUE by default
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   
      theme_bw()
  } # if argument lines == true, then add regression line, true by default
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+ 
      theme_bw()
  } # if no lines don't add regression line
}

#try it :)
myplot(x = body_mass_g, y = flipper_length_mm)
# now try remove the lines
myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)
