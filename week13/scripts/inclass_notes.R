# Week 13 notes - loops
# created 2024-12-3
# created by diana gao


#load libraries ------
library(tidyverse)
library(here)

# notes -------------

print(paste("The year is", 2000)) # basic print function

# now iterate it
years<-c(2015:2021) # make a vector of years you want to print
for (i in years){ # set up the for loop where i is the index, index doesn't have to be i can be any letter
  print(paste("The year is", i)) # loop over i
}
# prints with diff years 

#Pre-allocate space for the for loop
# empty matrix that is as long as the years vector
year_data<-tibble(year =  rep(NA, length(years)),  # column name for year
                  year_name = rep(NA, length(years))) # column name for the year name
year_data

# base r 
for (i in 1:length(years)){ #  set up the for loop where i is the index # 1:length(years) changes it so that the year number doesn't name the row number; instead counts like 2015 = row1, etc
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over i
  year_data$year[i]<-years[i] # loop over year
}
year_data
# can test with i = 1 in console, to test if code is running 

####### for loops are also useful for reading in a lot of data at once --------
testdata<-read_csv(here("week13", "data", "cond_data","011521_CT316_1pcal.csv"))
glimpse(testdata)

### first, tell the computer the path to the data
CondPath<-here("week13", "data", "cond_data")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath, pattern = ".csv")
files

### next, allocate space for the data
# pre-allocate space
# make an empty dataframe that has one row for each file and 3 columns
cond_data<-tibble(filename =  rep(NA, length(files)),  # column name for year
                  mean_temp = rep(NA, length(files)), # column name for the mean temperature
                  mean_sal = rep(NA, length(files)), # column name for the mean salinity
) # column name for the year name
cond_data 

### next, write your basic commands/codes
# first, 
raw_data<-read_csv(paste0(CondPath,"/",files[1])) # the files[1] is just a test by reading in the first file and see if it works
head(raw_data)
#next, get a mean temp
mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp

### then, turn it into a loop 
# this will be the code that turns the process into a loop: 
for (i in 1:length(files)){ # loop over 1:3 the number of files
}
# so now embed the code into it
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data) # take a peak to see if the code ran right
  cond_data$filename[i]<-files[i] # then add in the filename column
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE) # next fill in the mean temp
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE) # next fill in mean salinity
  } # now you automatically have the mean temp and sal for each csv in a nice df


##### Now instad of using for loops, use purrr -------------
# Let's calculate the mean from a set of random numbers and do it 10 times
# map is similar to the apply functions in base r
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) %>% # calculate 15 random numbers based on a normal distribution in a list
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

# if you want to iterate with your own funciton
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function, in this case the function is just rnorm 
  map_dbl(mean)
# if you want to change arguments within a function 
1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function, .x changes the "i" 
  map_dbl(mean)

### can also use it to import dat
# first tell the computer where the files are like b4
CondPath<-here("week13", "data", "cond_data")
# but this time can just tell the computer to read the full names of each csv (which includes the path to each csv)
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
files
# next read in the file names using map instead of a "for" loop and filenames as a column
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
data
# this above process only works if each csv has the same column names ^^'
# but cool! did the same thing as the for loop but in fewer lines of code