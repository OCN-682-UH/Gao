### Tidy tuesday monster data test script (week of oct. 29th)
### Created by: Diana Gao
### Created on: 2024-11-4
### Trying out functions to bin data, want to bin based 

#loading packages ----------
library(tidytuesdayR)
library(tidyverse)
library(here)


#loading data ---------------
tuesdata <- tidytuesdayR::tt_load('2024-10-29')
monster_movie_genres <- tuesdata$monster_movie_genres
monster_movies <- tuesdata$monster_movies

#functions -----------
# function 1 will separate the data into quartiles (4 quantiles) and then detect any data points which fall outside of it. 
# this func is nested within the remove_outlier function 
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25) # calc first quantile limit
  Quantile3 <- quantile(x, probs=.75) # calc third quantile limit
  IQR = Quantile3 - Quantile1 # caclculate interquartile range
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5) # return true if x is in the quartiles 
}

remove_outlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) { # for loop to traverse in columns vector
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]   # remove observation if it satisfies detect_outlier function
  }
  print("Remove outliers")
  print(dataframe) # return dataframe
}
#cleaning data ----------
# average rating weighted by the number of votes it got
cleaned_movie_names <- monster_movies %>%
  filter(title_type == "movie", na.rm = TRUE)

cleaned_horror_movies <- monster_movie_genres %>%
  filter(genres == "Horror", na.rm = TRUE)

quantiles <- quantile(data_vector, probs=c(0, 0.25, 0.5, 0.75, 1))

horror_movies <- cleaned_movie_names %>%
  inner_join(cleaned_horror_movies, by = "tconst") %>% # merge two tables so only movies with horror as a genre are kept
  remove_outlier(columns = "num_votes") %>% # remove outliers from dataset - seems like mostly the super popular ones (>~5k votes)
  mutate(year_bin = if_else(year > 2010, "new", "old")) # when data is evenly split along year, median year 2012 is cut in to the two diff bins. Set bins so movies released before 2010 are old and 2011 or later is new

ggplot(horror_movies, aes(x = num_votes, y = average_rating)) + 
  geom_point() +
  scale_x_continuous(trans="log10") +
  geom_smooth() + 
  facet_wrap(~year_bin)
  theme_bw()
# meh doesn't really seem like theres a strong correlation, but interestingly new movies are

remove_outlier(horror_movies, )
