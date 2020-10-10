################################################################################
################################################################################
#
# FILE: geographical_distribution.R
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Sep 21 2020
#
# DESC: This file creates a map-plot with the geographical distribution of 
#       sample stadiums across the US.
#
################################################################################
################################################################################


################################### Libraries ##################################

library(readr)
library(dplyr)
library(ggplot2)
library(usmap)
library(stringr)
library(extrafontdb)
library(extrafont)

################################################################################


################################## Options #####################################

# Output folders
plots_folder_path <- '/Users/muser/dfolder/Research/stadiums/output/plots'

mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
)

################################################################################


################################ Main code #####################################

# Collect the stadiums dataset
stadium_coordinates <- read_csv("dfolder/Research/stadiums/data/processed/descriptive/stadium_coordinates.csv")
stadium_coordinates <- distinct(stadium_coordinates)

hockey_stadiums_summary_2018 <- read_csv("dfolder/Research/stadiums/data/processed/descriptive/hockey_stadiums_summary_2018.csv")
hockey_stadiums_summary_2018 <- hockey_stadiums_summary_2018 %>% 
  mutate(sport = 'hockey')

basketball_stadiums_summary_2018 <- read_csv("dfolder/Research/stadiums/data/processed/descriptive/basketball_stadiums_summary_2018.csv")
basketball_stadiums_summary_2018 <- basketball_stadiums_summary_2018 %>%
  mutate(sport = 'basketball')

baseball_stadiums_summary_2018 <- read_csv("dfolder/Research/stadiums/data/processed/descriptive/baseball_stadiums_summary_2018.csv")
baseball_stadiums_summary_2018 <- baseball_stadiums_summary_2018 %>%
  mutate(sport = 'baseball')

football_stadiums_summary_2018 <- read_csv("dfolder/Research/stadiums/data/processed/descriptive/football_stadiums_summary_2018.csv")
football_stadiums_summary_2018 <- football_stadiums_summary_2018 %>%
  mutate(sport = 'football')

stadiums_data <- baseball_stadiums_summary_2018 %>% 
    bind_rows(basketball_stadiums_summary_2018) %>%
    bind_rows(hockey_stadiums_summary_2018) %>% 
    bind_rows(football_stadiums_summary_2018) %>% 
    left_join(stadium_coordinates, by = c('stadium_id' = 'sname_place_id'))
stadiums_data <- stadiums_data %>% select(longitude, latitude, everything())
st_transformed <- stadiums_data %>% 
    select(longitude, latitude, stadium_id) %>% 
    usmap_transform() %>% 
    select(-longitude, -latitude)
stadiums_data <- stadiums_data %>% 
    left_join(st_transformed)

# Filter the cities to be labeled
cities <- stadium_coordinates %>% 
    group_by(city, state) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(longitude, latitude, city, state) %>%
    mutate(city = str_to_title(city), state = str_to_upper(state))
cities <- usmap_transform(cities)
cities <- cities %>% filter(city %in% str_to_title(stadiums_data$city))
cities_to_show <- c('Seattle', 'Portland', 'San Francisco', 'Los Angeles', 'San Diego',
                    'Salt Lake City', 'Denver', 'Oklahoma City', 'Dallas', 'San Antonio',
                    'Minneapolis', 'Milwakee', 'Chicago', 'Indianapolis', 'Kansas City',
                    'Cincinnati', 'Saint Louis', 'Memphis', 'Atlanta', 'Pittsburgh', 
                    'Boston', 'New York', 'Raleigh', 'Tampa', 'Orlando', 'Miami',
                    'Baltimore', 'Clevelend', 'Detroit', 'Houston', 'New Orleans', 
                    'Phoenix')
cities_select <- cities %>% filter(city %in% cities_to_show)

# Produce the geo-plot
stadiums_geo_pic <-
    plot_usmap(size = 0.025, color = 'white', fill = 'gray', alpha = 0.5) +
    geom_jitter(data = stadiums_data, aes(x = longitude.1, 
                                          y = latitude.1, 
                                          shape = str_to_title(sport), 
                                          color = str_to_title(sport), 
                                          fill = str_to_title(sport)),
                size = 2, width = 35000, height = 35000) + 
    ggrepel::geom_label_repel(data = cities_select,
                              aes(x = longitude.1, y = latitude.1, 
                                  label = paste(city, state, sep = ', ')),
                              size = 2, alpha = 0.95, family = 'Times', force = 0.5,
                              label.r = unit(0.1, "lines"), label.size = 0.25,
                              segment.color = 'black', segment.size = 0.25,
                              seed = 1002) +
    scale_shape_manual(values = c(21, 22, 23, 24)) + 
    scale_fill_manual(values =  mycolorscheme3) + 
    scale_color_manual(values =  mycolorscheme3) + 
    theme(text = element_text(family="Times"), legend.direction = 'horizontal') + 
    labs(fill = "", color = "", shape = "") + 
    theme(legend.position = c(0.95, 0.025), 
          legend.box = 'horizontal',
          legend.justification = c(1, 0),
          legend.key.size = unit(0.075, 'inches'),
          plot.margin = margin(0, 0, 0, 0, 'pt')) + 
    my_theme

ggsave(file.path(plots_folder_path, 'stadiums_geographical_distribution.pdf'), 
       plot = stadiums_geo_pic, 
       device = cairo_pdf, 
       width = 5, 
       height = 3.5)
embed_fonts(file.path(plots_folder_path, 'stadiums_geographical_distribution.pdf'))

################################################################################