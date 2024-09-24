## working with penguin plot data to learn diplyr###
##  created by: Diana Gao
## Created on: 2024-9-17

#libraries------
library(palmerpenguins)
library(tidyverse)
library(here)

##load the data--------
glimpse(penguins)
view (penguins)

##filtering---------
filter(.data = penguins, year == "2008") # select data from yr 2008
filter(.data = penguins, body_mass_g > 5000)

filter(.data = penguins, sex == "females", body_mass_g > 5000) # penguins that are females wiwth body mass >5000g

filter(.data = penguins, year == "2008" | year == "2009") # yrs 2008 OR 2009
#or filter(.data = penguins, year %in% c("2008", "2009"))
nodream <- filter(.data = penguins, !island == "Dream") # not from island dream
#typical way is: filter(.data = penguins, island != "Dream")
# or select for the other two islands
AandG <- filter(.data = penguins, species %in% c("Adelie", "Gentoo"))
#or use or argument like in the 2008, 2009 one

##mutating-----------
data2 <- mutate(.data = penguins, 
                body_mass_kg = body_mass_g/1000, 
                bill_length_depth = bill_length_mm/bill_depth_mm
                )
data3<- mutate(.data = penguins,
               after_2008 = ifelse(year>2008, "yes", "no"))
data4 <- mutate(.data = penguins, 
                flipperlength_and_bodymass = flipper_length_mm + body_mass_g, 
                size = ifelse(body_mass_g>4000, "big", "small")
               ) # 2 new columns: add flip length and BM, size if bm bigger than 4000g is big and smaller is small
##using the pipe! ------
penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) #calculate log biomass

##select ------
penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass)
#renaming using select
penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(Species = species, island, sex, log_mass) #renaming using select

##summarize ------
penguins %>% # 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE)) # na.rm=true = remove NAs, calc mean flipper length

penguins %>%
  group_by(island) %>% # group data by islands
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), # calc mean of bill length by island
            max_bill_length = max(bill_length_mm, na.rm=TRUE)) # calc min of bill length by island
penguins %>%
  group_by(island, sex) %>% # group by island AND sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
## drop NAs ----
#instead of using na.rm = true ... 
penguins %>%
  drop_na(sex) # drop data if has NA in sex
penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))
## graph it! -----
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) + # remember to switch to + for the ggplot, the whole ggplot is one whole chunk
  geom_boxplot()
