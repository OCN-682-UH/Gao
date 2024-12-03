# load packages ---------
library(tidyverse)
library(tidyplots)
library(here)
# learn tidyplots! 

# plot each year in bar graphs - two bars for the two titles
# load the data ------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 48)

cbp_resp <- tuesdata$cbp_resp
cbp_state <- tuesdata$cbp_state

# clean the data
cbp_clean <- cbp_resp %>%
  select(fiscal_year, title_of_authority, encounter_count) %>%
  group_by(title_of_authority, fiscal_year) %>%
  count()

#plot the data
cbp_clean %>%
  tidyplot(x = fiscal_year, 
           y = n, 
           color = title_of_authority) %>%
  add_barstack_absolute() %>%
  adjust_title("Number of individuals encountered per fiscal year") %>%
  adjust_x_axis_title("Fiscal Year") %>%
  adjust_y_axis_title("Number of Individuals Encountered") %>%
  adjust_legend_title("Title of Authority")%>%
  save_plot(here("tidy_tuesday", "5_Nov26", "output", "titles.png"))