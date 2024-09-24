##############
#Week 4 script from video lecture (not homework)
#Created by: Diana Gao
#Created on: 2024-9-24
##############

#load libraries ------
library(tidyverse)
library(here)

#load data -------
ChemData<-read_csv(here("week4","data", "chemicaldata_maunalua.csv"))
View(ChemData)
#glimpse(ChemData)

#practie cleaning the data -------
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that has NAs
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns - Tide and time
           sep = "_", # separate by _ since og data is xx_xx
           remove = FALSE ) %>% #keep og column
  unite(col = "Site_Zone", # the name of the NEW col
        c(Site,Zone), # the columns to unite
        sep = ".", # lets put a . in the middle
        remove = FALSE) # keep the original
head(ChemData_clean) #view data
#turn into long
ChemData_long <-ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Variables", # the names of the new cols with all the column names
               values_to = "Values") # names of the new column with all the values
view(ChemData_long)
#summarize
ChemData_long %>%
  group_by(Variables, Site) %>% # group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), # get mean
            Param_vars = var(Values, na.rm = TRUE)) # get variance

#thinkpair share example
ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>% # group by variables and site
  summarise(Param_means = mean(Values), # get mean
            Param_vars = var(Values), # get variance
            Param_stdev = sd(Values) # get standard deviation
  )

#make a boxplot
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+ 
  geom_boxplot()+ #make boxplot
  facet_wrap(~Variables, scales = "free") #"frees" x and y axes to be independent for each plot, if want to do an individual axis do free_x or free_y

#go from long to wide
ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns
              values_from = Values) # column with the values

#actually clean the data --------
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns Tide and time
           sep = "_", # separate by _
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", # the names of the new cols with all the column names 
               values_to = "Values") %>% # names of the new column with all the values 
  group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>% #get mean values of ea measurement by site and time
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% # make the summary dataframe wide
  write_csv(here("week4","output","summary_videolect.csv"))  # export summary data as a csv to the right folder
#make sure to view after each step to verify