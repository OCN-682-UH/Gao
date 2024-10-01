# week 5 in video lecture script: practicing lubridate
#created by: Diana Gao
#created on: 2024-09-30

#load libraries -------
library(tidyverse)
library(here)
library(lubridate)

#notes ------
ymd("2021-02-24")
mdy("02/24/2021")
mdy_hms("02/24/2021 22:22:20")

# make a character string
datetimes<-c("02/24/2021 22:22:20", 
             "02/25/2021 11:21:10", 
             "02/26/2021 8:01:52") 
# convert to datetimes
datetimes <- mdy_hms(datetimes)
#extract the month
month(datetimes, 
      label = TRUE, # version that names the month rather than just saying 2
      abbr = FALSE) #Spell it out
#extract days
day(datetimes) # day (number 1-31)
wday(datetimes, label = TRUE) # extract day of week

round_date(datetimes, "minute") # round to nearest minute

#challenge ------
#read in data
CondData <- read.csv(here("week5", "data", "CondData.csv"))
is.character(CondData$date) # test if date is a character (in this case yes)
CondDataISO <- CondData %>%
  mutate(date = mdy_hms(date))


