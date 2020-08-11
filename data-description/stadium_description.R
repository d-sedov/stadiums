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

input_folder <- file.path('/home/quser/project_dir', 
                          'stadiums/data/processed/descriptive')

output_folder <- file.path('/home/quser/project_dir',
                           'stadiums/output/tables') 

###############################################################################


############################## Import data ####################################

baseball_stadiums_summary <- read_csv(file.path(input_folder,
                                                paste0('baseball_stadiums_summary_', 
                                                       year, 
                                                       '.csv')
                                                )
                                      )
hockey_stadiums_summary <- read_csv(file.path(input_folder,
                                              paste0('hockey_stadiums_summary_',
                                                     year,
                                                     '.csv')
                                              )
                                    )
basketball_stadiums_summary <- read_csv(file.path(input_folder,
                                                  paste0('basketball_stadiums_summary_',
                                                         year,
                                                         '.csv')
                                                  )
                                        )
football_stadiums_summary <- read_csv(file.path(input_folder,
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

shared_stadium_count <- stadiums_summary %>% 
  filter(sport %in% c('basketball', 'hockey')) %>%
  group_by(sport, stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  count(stadium_id) %>%
  filter(n > 1) %>% 
  nrow()

# Aggregare duplicate stadiums
stadiums_summary <- stadiums_summary %>% 
  select(-name) %>%
  group_by(sport, stadium_id) %>%
  summarize(games = sum(games), 
            area_m2 = first(area_m2), 
            attendance = sum(attendance), 
            visits = sum(visits), 
            establishment_count = first(establishment_count))

# Create the summary table:
stadiums_summary <- stadiums_summary %>% 
  group_by(sport) %>% 
  summarize(n_stadiums = n(), 
            n_games = mean(games, na.rm = T),
            mean_area = mean(area_m2), 
            mean_establishment_count = mean(establishment_count, na.rm = T),
            mean_attendance = sum(attendance) / sum(games), 
            mean_visits = sum(stadium_visits) / sum(games), 
            ) 

stadiums_summary$sport <- stri_trans_totitle(stadiums_summary$sport)

# Rename columns:
colnames(stadiums_summary) <- c('Sport',
                                'Stadiums',
                                'Games',
                                'Area',
                                'Est. nearby',
                                'Official',
                                'Sname')


headers <- c(2, 3, 2)
names(headers) <- c(' ', 
                   'Means',
                   paste0('Mean attendance in ', as.character(year))  
                   )
stadiums_summary <- xtable(stadiums_summary,
                           caption = "Stadiums summary statistics",
                           label = "stadiums_summary_table") %>%
  xtable2kable(include.rownames = FALSE,
               booktabs = TRUE,
               format.args = list(big.mark = ',')) %>%
  add_header_above(headers) %>%
  footnote(general = paste0(basketball_shared_stadium_count,
                            ' stadium is shared by multiple basketball teams. ',
                            shared_stadium_count,
                            ' stadiums are shared by a basketball and a hockey team.'),
           general_title = 'Note: ',
           title_format = c('italic'),
           threeparttable = T, 
           footnote_as_chunk = T)

# Put caption on top:
stadiums_summary <- stri_split_lines1(stadiums_summary)
stadiums_summary <- str_trim(stadiums_summary, side = 'both')
caption_label <- str_subset(stadiums_summary, 'caption|label')
other <- str_subset(stadiums_summary, 'caption|label', negate = TRUE)
centering_index <- which(str_detect(other, 'centering'))
stadiums_summary <- c(other[1 : centering_index], 
                      caption_label, 
                      other[-1 : -centering_index])
stadiums_summary <- str_c(stadiums_summary, collapse = '\n')
stadiums_summary <- str_replace(stadiums_summary, 
                                '\\\\textit\\{Note: \\}\\n\\\\item ', 
                                '\\\\textit\\{Note:\\} ')
stadiums_summary <- str_replace(stadiums_summary, 
                                '\\\\begin\\{tablenotes\\}\\[para\\]\\n', 
                                '\\\\begin\\{tablenotes\\}\\[para\\]\\\n\\\\footnotesize\\\n')
cat(stadiums_summary, file = file.path(output_folder,
                                       'stadiums_summary.tex'))

###############################################################################
