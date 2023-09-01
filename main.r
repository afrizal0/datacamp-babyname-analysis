library(tidyverse)
library(dplyr)
library(ggplot2)

babynames <- readRDS("dataset/babynames.rds")

# shows first 10 rows 
head(babynames, n = 10)

# shows the baby born in 1990 ordered by number of born in descending 
babynames %>%
  filter(year == 1990) %>%
  arrange(desc(number))

# find the most popular names each year
babynames %>%
  group_by(year) %>%
  slice_max(number)

# filter for only the names "Steven", "Thomas", "Matthew" and visualize with line plot
selected_names <- babynames %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"))
selected_names_viz <- ggplot(selected_names, aes(x=year, y=number, color=name)) + geom_line()
ggsave("img/selected_names_viz.png")
print(selected_names_viz)

# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  slice_max(fraction, n = 1)

babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>%
  mutate(name_total = sum(number), name_max=max(number))


names_filtered <- names_normalized %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color=name)) + geom_line()

ggsave("img/names_filtered_viz.png")

babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio of fraction between each year 
  mutate(ratio = fraction / lag(fraction))

babynames_ratios_filtered <- babynames_fraction %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(ratio = fraction / lag(fraction)) %>%
  filter(fraction >= 0.00001)


babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  slice_max(ratio) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)
