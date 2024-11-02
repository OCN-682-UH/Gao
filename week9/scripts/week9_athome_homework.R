### week 9 at home lecture homework
### created by: Diana Gao
### created on: 2024-11-1

# Load libraries -------
library(tidyverse)
library(here)
library(janitor)

# Load in data
data <- read_csv(here("week9", "data", "intertidaldata.csv"))
site_loc <- read_csv(here("week9", "data", "intertidaldata_latitude.csv"))

# Data cleaning/analysis
# goal is to plot the # of mussels for each site
clean_loc <- site_loc %>%
  clean_names()

clean <- data %>%
  clean_names() %>%
  group_by(site) %>%
  summarise(across(algae:stars_counts, ~sum(.x,na.rm = TRUE)))%>%
  distinct() %>%
  select(site, mussels) %>%
  left_join(clean_loc, by = "site") %>%
  mutate(site = factor(site)) %>%


# Plot data
ggplot(clean, aes(x = fct_reorder(site, latitude), y = mussels)) + 
  geom_col() + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Number of Mussels at each site", subtitle = "Highest latitude at top, lowest at bottom", x = "Site Name", y = "Mussels") +
  coord_flip() + 
  theme_classic()
ggsave(here("week9", "outputs", "Mussels_per_site.png"))
  