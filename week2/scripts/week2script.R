#script for week 2 learning how to import data
#created by: diana gao
#created on 2024/9/10
#################


#load libraries-------------------
library("here")
library("tidyverse")


#read in data---------------------
WeightData<-read.csv(here("week2", "data", "weightdata.csv")) #read in weightdata.csv


#data analysis--------------------
head(WeightData) #looks at top 6 lines of WeightData
