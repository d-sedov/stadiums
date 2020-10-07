###############################################################################
#
# FILE: stadium_description.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Aug 6 2020
#
# DESC: This code contains the code to produce descripritive statistics for
#       stadiums.
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

###############################################################################


################################ Constants ####################################

year <- 2018

sports <- c('basketball', 'baseball', 'hockey', 'football')

industries <- c('FoodAccommodation', 
                'Retail', 
                'Finance', 
                'Professional', 
                'Education', 
                'Health')

ranges <- 5

distances <- paste(seq(0, ranges - 1), seq(1, ranges), sep = '-')
distances <- paste0(distances, ' km')

descriptive_folder <- file.path('/home/quser/project_dir', 
                          'stadiums/data/processed/descriptive')

input_folder <- file.path('/home/quser/project_dir/stadiums',
                          'data/processed/analysis')

output_folder <- file.path('/home/quser/project_dir',
                           'stadiums/output/tables') 

###############################################################################


################################## Functions ##################################

prepareStadiumData <- function(sport) {
  # Function that imports and prepares the data on stadiums to compare game
  # days vs no-game days.
  
  industry <- 'FoodAccommodation'
  
  # Set path and import data
  data_file_path <- file.path(input_folder,
                              sport,
                              paste0('stadiums_', industry, '.dta')
  )
  data <- read_dta(data_file_path)
  
  # Get first date record per stadium
  data <- data %>% 
    group_by(stadium_id, Date) %>% 
    arrange(desc(game)) %>%
    filter(row_number() == 1)
  
  data$sport <- sport
  
  return(data)
}

###############################################################################


############################## Import data ####################################

baseball_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                paste0('baseball_stadiums_summary_', 
                                                       year, 
                                                       '.csv')
                                                )
                                      )
hockey_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                              paste0('hockey_stadiums_summary_',
                                                     year,
                                                     '.csv')
                                              )
                                    )
basketball_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                  paste0('basketball_stadiums_summary_',
                                                         year,
                                                         '.csv')
                                                  )
                                        )
football_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                paste0('football_stadiums_summary_', 
                                                       year, 
                                                       '.csv')
                                                )
                                      )

###############################################################################


############################# Produce descriptives ############################

# Assign sport and append the data
baseball_stadiums_summary$sport <- 'baseball'
hockey_stadiums_summary$sport <- 'hockey'
basketball_stadiums_summary$sport <- 'basketball'
football_stadiums_summary$sport <- 'football'
stadiums_summary <- bind_rows(baseball_stadiums_summary, 
                              hockey_stadiums_summary,
                              basketball_stadiums_summary, 
                              football_stadiums_summary)

basketball_shared_stadium_count <- stadiums_summary %>% 
  filter(sport %in% c('basketball')) %>%
  count(stadium_id) %>%
  filter(n > 1) %>%
  nrow()

football_shared_stadium_count <- stadiums_summary %>% 
  filter(sport %in% c('football')) %>%
  count(stadium_id) %>%
  filter(n > 1) %>%
  nrow()

shared_stadium_count <- stadiums_summary %>% 
  filter(sport %in% c('basketball', 'hockey')) %>%
  group_by(sport, stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  count(stadium_id) %>%
  filter(n > 1) %>% 
  nrow()

# Aggregate duplicate stadiums
stadiums_summary <- stadiums_summary %>% 
  select(-name) %>%
  group_by(sport, stadium_id) %>%
  summarize(games = sum(games), 
            area_m2 = first(area_m2), 
            capacity = first(capacity),
            attendance = sum(attendance), 
            visits = sum(stadium_visits), 
            establishment_count = first(establishment_count))

# Create the summary table:
stadiums_summary <- stadiums_summary %>% 
  group_by(sport) %>% 
  summarize(n_stadiums = n(), 
            n_games = mean(games, na.rm = T),
            mean_area = mean(area_m2), 
            mean_capacity = mean(capacity),
            mean_establishment_count = mean(establishment_count, na.rm = T),
            mean_attendance = sum(attendance) / sum(games) 
            ) 


# Get stadium visit counts on the daily level
stadiums_data <- lapply(sports, prepareStadiumData)

# Export stadium-level game-no-game average_visits
stadium_game_no_game_visits <- lapply(stadiums_data,
                                      function(x) {
                                        x %>%
                                          group_by(stadium_id, game) %>%
                                          summarize(sport = first(sport), 
                                                    stadium_visits = mean(stadium_visits))
                                      })
stadium_game_no_game_visits <- bind_rows(stadium_game_no_game_visits)
write_csv(x = stadium_game_no_game_visits, path = file.path(descriptive_folder, 
                                                            'stadium_game_no_game_visits.csv'))

# Compare visits on days with games and no games across sports
stadium_game_comparisons <- lapply(stadiums_data, 
                                   function(x) {
                                     x %>% 
                                       group_by(game) %>%
                                       summarize(sport = first(sport), 
                                                 stadium_visits = mean(stadium_visits))
                                   })
stadium_game_comparisons <- bind_rows(stadium_game_comparisons)
stadium_game_comparisons <- stadium_game_comparisons %>% pivot_wider(id_cols = sport,
                                                                     names_from = game, 
                                                                     values_from = stadium_visits)
stadium_game_comparisons <- stadium_game_comparisons %>% 
  rename(`Game day` = `1`, `No-game day` = `0`)

# Merge the two tables
stadiums_summary <- stadiums_summary %>% left_join(stadium_game_comparisons)
  
# Sport names to title 
stadiums_summary$sport <- stri_trans_totitle(stadiums_summary$sport)

# Drop official attendance column
stadiums_summary <- stadiums_summary %>% 
  select(-mean_attendance)

# Rename remaining columns:
colnames(stadiums_summary) <- c('Sport',
                                'Stadiums',
                                'Games',
                                'Area',
                                'Capacity',
                                'Bus. nearby',
                                'No-game day',
                                'Game day')


headers <- c(2, 4, 2)
names(headers) <- c(' ', 
                   'Means',
                   'Average daily SG visits'
                   )

shared_stadiums_note <- paste0(basketball_shared_stadium_count,
                               ' stadium is shared by multiple basketball teams. ',
                               football_shared_stadium_count,
                               ' stadium is shared by multiple football teams. ',
                               shared_stadium_count,
                               ' stadiums are shared by a basketball and a hockey team.')

stadiums_summary <- xtable(stadiums_summary,
                           caption = paste("Stadium sample summary statistics.", 
                                           shared_stadiums_note, 
                                           'Stadium area measured in square meters.',
                                           'Businesses in a 3 km radius defined as nearby businesses.'),
                           label = "stadiums_summary_table", 
                           digits = 1) %>%
  xtable2kable(include.rownames = FALSE,
               booktabs = TRUE,
               format.args = list(big.mark = ','), table.placement = 't') %>%
  kable_styling(latex_options = 'scale_down') %>%
  add_header_above(headers) 

# Put caption on top:
#stadiums_summary <- stri_split_lines1(stadiums_summary)
#stadiums_summary <- str_trim(stadiums_summary, side = 'both')
#caption_label <- str_subset(stadiums_summary, 'caption|label')
#other <- str_subset(stadiums_summary, 'caption|label', negate = TRUE)
#centering_index <- which(str_detect(other, 'centering'))
#stadiums_summary <- c(other[1 : centering_index], 
#                      caption_label, 
#                      other[-1 : -centering_index])
# stadiums_summary <- str_c(stadiums_summary, collapse = '\n')

# Trim lines and modify the notes section
stadiums_summary <- stri_split_lines1(stadiums_summary)
stadiums_summary[3] <- paste0(stadiums_summary[3], '%')
stadiums_summary <- str_trim(stadiums_summary, side = 'both')
stadiums_summary <- str_c(stadiums_summary, collapse = '\n')
# stadiums_summary <- str_replace(stadiums_summary, 
#                                '\\\\textit\\{Note: \\}\\n\\\\item ', 
#                                '\\\\textit\\{Note:\\} ')
#stadiums_summary <- str_replace(stadiums_summary, 
#                                '\\\\begin\\{tablenotes\\}\\[para\\]\\n', 
#                                '\\\\begin\\{tablenotes\\}\\\n\\\\setlength\\\\labelsep{0pt}\\\n\\\\footnotesize\\\n')
stadiums_summary <- stri_split_lines1(stadiums_summary)
stadiums_summary <- stadiums_summary[3:17]
stadiums_summary <- str_c(stadiums_summary, collapse = '\n')
cat(stadiums_summary, file = file.path(output_folder,
                                       'stadiums_summary.tex'))

###############################################################################
