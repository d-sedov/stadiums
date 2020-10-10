###############################################################################
#
# FILE: correlations_comparisons.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Aug 6 2020
#
# DESC: This code contains the code to make simple comparisons.
#
# COMMENT: 
#
###############################################################################


################################ Libraries ####################################

library(haven)
library(lfe)
library(gsubfn)
library(stargazer)
library(stringi)
library(stringr)
library(kableExtra)
library(xtable)
library(zoo)
library(scales)

###############################################################################


################################# Options ######################################

# Output folders
tables_folder_path = '/home/quser/project_dir/stadiums/output/tables'
plots_folder_path = '/home/quser/project_dir/stadiums/output/plots'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
my_theme_large <- theme(legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  plot.title = element_text(hjust = 0.5, size = 12),
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 10)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

###############################################################################


################################ Constants ####################################

year <- 2018

sports <- c('basketball', 'baseball', 'hockey', 'football', 'basketball_hockey')

industries <- c('FoodAccommodation', 
                'Retail', 'Finance', 'Professional', 'Education', 'Health')

ranges <- 5

distances <- paste(seq(0, ranges - 1), seq(1, ranges), sep = '-')
distances <- paste0(distances, ' km')

input_folder <- file.path('/home/quser/project_dir/stadiums',
                          'data/processed/analysis')

output_folder <- file.path('/home/quser/project_dir',
                           'stadiums/output/tables') 

###############################################################################


############# Function to prepare the data, estimate models ###################

prepareStadiumData <- function(sport) {
  # Function that imports and prepares the data on stadiums to compare game
  # days vs no-game days.
  
  industry <- 'Retail'
  
  if (sport == 'basketball_hockey') {
    # Get basketball data
    basketball_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                      'basketball',
                                      paste0('stadiums_', industry, '.dta')
    )
    basketball <- read_dta(basketball_file_path)
    
    # Get hockey data
    hockey_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                  'hockey',
                                  paste0('stadiums_', industry, '.dta')
    )
    hockey <- read_dta(hockey_file_path)
    
    # Concatenate data, select game-days from both sports
    # (apply 'any' aggregation function to 'game' column)
    data <- bind_rows(basketball, hockey) %>% 
      group_by(stadium_id, Date) %>% 
      arrange(desc(game)) %>%
      filter(row_number() == 1)
  } else {
    # Set path and import data
    data_file_path <- file.path(input_folder,
                                sport,
                                paste0('stadiums_', industry, '.dta')
    )
    data <- read_dta(data_file_path)
    
    # Get first date record per stadium
    data <- data %>% 
      group_by(stadium_id, Date) %>% 
      filter(row_number() == 1)
  }
  
  data$sport <- sport
  
  return(data)
}

prepareEstablishmentsData <- function(sport) {
  # Function that imports and prepares the data on establishments to compare game
  # days vs no-game days.
  
  visits_list <- list()
  
  if (sport == 'basketball_hockey') {
    for (i in industries) {
      # Get basketball data
      basketball_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                        'basketball',
                                        paste0('stadiums_', i, '.dta')
      )
      basketball <- read_dta(basketball_file_path)
      
      # Only establishments within 3 km 
      basketball$distance <- basketball$distance / 1000
      basketball <- basketball %>% 
        mutate(distance_bin = cut(distance, 
                                  seq(0, ranges), 
                                  right = FALSE,
                                  labels = 1 : ranges)) %>%
        filter(distance <= 3)
      
      # Compute total visits around basketball stadiums
      basketball <- basketball %>% 
        group_by(stadium_id, Date) %>% 
        summarise(establishment_visits = sum(visits), 
                  game = first(game))
      
      # Get hockey data
      hockey_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                    'hockey',
                                    paste0('stadiums_', i, '.dta')
      )
      hockey <- read_dta(hockey_file_path)
      
      # Only establishments within 3 km 
      hockey$distance <- hockey$distance / 1000
      hockey <- hockey %>% 
        mutate(distance_bin = cut(distance, 
                                  seq(0, ranges), 
                                  right = FALSE,
                                  labels = 1 : ranges)) %>%
        filter(distance <= 3)
      
      # Compute total visits around hockey stadiums
      hockey <- hockey %>% 
        group_by(stadium_id, Date) %>% 
        summarise(establishment_visits = sum(visits), 
                  game = first(game))
      
      # Concatenate data, select game-days from both sports
      # (apply 'any' aggregation function to 'game' column)
      data <- bind_rows(basketball, hockey) %>% 
        group_by(stadium_id, Date) %>% 
        arrange(desc(game)) %>%
        filter(row_number() == 1) %>%
        ungroup()
      
      visits_list <- c(visits_list, list(data))
    }
  } else {
    for (i in industries) {
      # Set path and import data
      data_file_path <- file.path(input_folder,
                                  sport,
                                  paste0('stadiums_', i, '.dta')
      )
      data <- read_dta(data_file_path)
      
      # Only establishments within 3 km 
      data$distance <- data$distance / 1000
      data <- data %>% 
        mutate(distance_bin = cut(distance, 
                                  seq(0, ranges), 
                                  right = FALSE,
                                  labels = 1 : ranges)) %>%
        filter(distance <= 3)
      
      # Get first date record per stadium
      data <- data %>% 
        group_by(stadium_id, Date) %>% 
        summarize(establishment_visits = sum(visits), game = first(game)) 
      
      visits_list <- c(visits_list, list(data))
      
    }
  }
  
  # Concatenate data on all establishments
  data <- bind_rows(visits_list)
  # Compare game-days vs no-game days
  data <- data %>% 
    group_by(stadium_id, Date) %>% 
    summarize(establishment_visits = sum(establishment_visits), 
              game = first(game)) %>% 
    ungroup() %>%
    group_by(game) %>%
    summarize(establishment_visits = mean(establishment_visits))
  
  return(data)
}

###############################################################################


################################## Analysis ###################################

stadiums_data <- lapply(sports, prepareStadiumData)

# Compare visits on days with games and no games across sports
stadium_game_comparisons <- lapply(stadiums_data, 
                                   function(x) {
                                     x %>% 
                                       group_by(game) %>%
                                       summarize(sport = first(sport), 
                                                 stadium_visits = mean(stadium_visits))
                                   })
stadium_game_comparisons <- bind_rows(stadium_game_comparisons)
stadium_game_comparisons <- stadium_game_comparisons %>% 
  mutate(sport = ifelse(sport == 'basketball_hockey', 'basketball &\n hockey', sport)) %>%
  filter(!sport %in% c('basketball', 'hockey'))

pic <- ggplot(stadium_game_comparisons,
              aes(fill = factor(game), y = stadium_visits, x = str_to_title(sport))) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(name = '', values = mycolorscheme3[2:1],
                    guide = 'legend', 
                    labels = c('No game', 'Game')) + 
  ylab('Mean daily visits to stadiums') + 
  theme_bw(base_family = 'Times') +
  scale_y_continuous(labels = scales::comma) +
  my_theme_large +
  theme(axis.title.x = element_blank(), legend.title = element_blank(),
        legend.justification=c(0.01,0.99),
        legend.position = c(0.01, 0.99),
        plot.margin = unit(c(0.5,0,0,0), "cm"))

ggsave(filename = file.path(plots_folder_path, 
                            'stadium_visits_game_nogame.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.0, height = 2.5)
embed_fonts(file = file.path(plots_folder_path,
                             'stadium_visits_game_nogame.pdf'))

# Plot total visits over time, game count per day
stadium_timeline <- lapply(stadiums_data, 
                           function(x) {
                             x %>% group_by(Date) %>%
                               summarize(sport = first(sport), 
                                         stadium_visits = sum(stadium_visits), 
                                         game = sum(game))
                           })
stadium_timeline <- bind_rows(stadium_timeline)
stadium_timeline <- stadium_timeline %>% 
  arrange(sport, Date) %>%
  group_by(sport) %>%
  mutate(rollmean_visits = rollmean(stadium_visits, k = 7, fill = NA),
         rollmean_games = rollmean(game, k = 7, fill = NA))
stadium_timeline <- stadium_timeline %>% 
  filter(sport != 'basketball_hockey')

pic1 <- ggplot(stadium_timeline, aes(x = Date, 
                                    group = sport,
                                    color = str_to_title(sport))) + 
  geom_line(aes(y = stadium_visits, alpha = 0.175)) + 
  geom_line(aes(y = rollmean_visits, alpha = 1)) +  
  theme_bw(base_family = 'Times') +
  my_theme_large +
  scale_color_manual(name = 'Color', values = mycolorscheme3,
                    guide = 'legend') +
  scale_y_continuous(labels = scales::comma) +
  scale_alpha_continuous(name = 'Transparency', 
                         breaks = c(0.175, 1), 
                         labels = c('Daily data', 
                                    '7-day moving average')
                         ) + 
  coord_cartesian(ylim = c(0, 25000)) + 
  ylab('Total SG visits to stadiums') +
  theme(axis.title.x = element_blank())

ggsave(filename = file.path(plots_folder_path, 
                            'stadium_visits_timeline.pdf'), 
       device = cairo_pdf, plot = pic1, width = 7, height = 4)
embed_fonts(file = file.path(plots_folder_path,
                             'stadium_visits_timeline.pdf'))

pic2 <- ggplot(stadium_timeline, aes(x = Date, 
                                    group = sport,
                                    color = str_to_title(sport))) + 
  geom_line(aes(y = game, alpha = 0.175)) + 
  geom_line(aes(y = rollmean_games, alpha = 1)) +  
  theme_bw(base_family = 'Times') +
  my_theme_large +
  scale_color_manual(name = 'Color', values = mycolorscheme3,
                     guide = 'legend') +
  scale_y_continuous(labels = scales::comma) +
  scale_alpha_continuous(name = 'Transparency', 
                         breaks = c(0.175, 1), 
                         labels = c('Daily data', 
                                    '7-day moving average')) + 
  ylab('Total number of games') +
  theme(axis.title.x = element_blank())

ggsave(filename = file.path(plots_folder_path, 
                            'stadium_games_timeline.pdf'), 
       device = cairo_pdf, plot = pic2, width = 7, height = 4)
embed_fonts(file = file.path(plots_folder_path,
                             'stadium_games_timeline.pdf'))

pic3 <- ggarrange(pic1, 
                  pic2,
                  ncol = 1, 
                  nrow = 2, 
                  common.legend = TRUE,
                  legend = 'right', 
                  align = 'hv')

ggsave(filename = file.path(plots_folder_path, 
                            'stadium_visits_games_timeline.pdf'), 
       device = cairo_pdf, plot = pic3, width = 7, height = 5)
embed_fonts(file = file.path(plots_folder_path,
                             'stadium_visits_games_timeline.pdf'))

establishment_data <- lapply(sports, prepareEstablishmentsData)
# Add sports column
establishment_data <- Map(cbind, establishment_data, sport = sports)
# Append data
establishment_data <- bind_rows(establishment_data)

# Build the visits barchart
establishment_game_comparison <- establishment_data %>% 
  mutate(sport = ifelse(sport == 'basketball_hockey', 'basketball &\n hockey', sport)) %>%
  filter(!sport %in% c('basketball', 'hockey'))

pic <- ggplot(establishment_game_comparison,
              aes(fill = factor(game), y = establishment_visits, x = str_to_title(sport))) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(name = '', values = mycolorscheme3[2:1],
                    guide = 'legend', 
                    labels = c('No game', 'Game')) + 
  ylab('Mean daily business visits near stadiums') + 
  theme_bw(base_family = 'Times') +
  scale_y_continuous(labels = scales::comma) +
  my_theme_large +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.justification=c(0.99, 0.99),
        legend.position = c(0.99, 0.99), 
        plot.margin = unit(c(0.5,0,0,0), "cm"))

ggsave(filename = file.path(plots_folder_path, 
                            'establishment_visits_game_nogame.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.0, height = 2.5)
embed_fonts(file = file.path(plots_folder_path,
                             'establishment_visits_game_nogame.pdf'))

###############################################################################