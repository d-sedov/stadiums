###############################################################################
#
# FILE: establishment_description.R
#
# BY: Dmitry Sedov 
#
# DATE: Fri Aug 7 2020
#
# DESC: This code contains the code to produce descripritive statistics for
#       businesses close to stadiums.
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

sports <- c('baseball', 'basketball', 'football', 'hockey')

industry_bins <- list(naics_2digit = c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 
                                       51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92),
                      naics_2digit_bin = c(11, 21, 22, 23, 31, 31, 31, 42, 44, 44, 48, 48,
                                           51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92)
)

#industry_bins <- list(naics_2digit = c(44, 45, 52, 61, 62, 71, 72, 81),
#                      naics_2digit_bin = c(44, 44, 52, 61, 62, 71, 72, 81)
#                      )
industry_bins <- as.data.frame(industry_bins)

#industries <- list(naics_2digit_bin = c(44, 45, 52, 61, 62, 71, 72, 81),
#                   industry_name = c('Retail',
#                                     'Retail',
#                                     'Finance', 
#                                     'Education', 
#                                     'Health',
#                                     'Recreation',
#                                     'FoodAccommodation',
#                                     'Other Services')
#                   )

industries <- list(naics_2digit_bin = c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49,
                                        51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92),
                   industry_name = c('Agriculture', 
                                     'Mining', 
                                     'Utilities', 
                                     'Construction', 
                                     'Manufacturing', 
                                     'Manufacturing', 
                                     'Manufacturing', 
                                     'Wholesale Trade', 
                                     'Retail Trade',
                                     'Retail Trade',
                                     'Transportation',
                                     'Transportation',
                                     'Information', 
                                     'Finance', 
                                     'Real Estate', 
                                     'Professional Services', 
                                     'Management', 
                                     'Admin. Services', 
                                     'Education', 
                                     'Health',
                                     'Recreation',
                                     'Food & Accommodation',
                                     'Other Services',
                                     'Public Administration')
)
industries <- as.data.frame(industries)

#ranges <- 5
#
#distances <- paste(seq(0, ranges - 1), seq(1, ranges), sep = '-')
#distances <- paste0(distances, ' km')

descriptive_folder <- file.path('/home/quser/project_dir', 
                                'stadiums/data/processed/descriptive')
input_folder <- file.path('/home/quser/project_dir', 
                          'stadiums/data/processed/descriptive')
output_folder <- file.path('/home/quser/project_dir',
                           'stadiums/output/tables') 

###############################################################################


############################## Import data ####################################

# Summary data on stadiums
baseball_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                paste0('baseball_stadiums_summary_', 
                                                       year, 
                                                       '.csv')
                                                )
                                      ) %>%
  group_by(stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  select(stadium_id)

hockey_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                              paste0('hockey_stadiums_summary_',
                                                     year,
                                                     '.csv')
                                              )
                                    ) %>%
  group_by(stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  select(stadium_id)

basketball_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                  paste0('basketball_stadiums_summary_',
                                                         year,
                                                         '.csv')
                                                  )
                                        ) %>%
  group_by(stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  select(stadium_id)

football_stadiums_summary <- read_csv(file.path(descriptive_folder,
                                                paste0('football_stadiums_summary_', 
                                                       year, 
                                                       '.csv')
                                                )
                                      ) %>%
  group_by(stadium_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  select(stadium_id)

# Import establishment counts and visits
basketball_establishments_2018 <- read_csv(file.path(input_folder,
                                                paste0('basketball_establishments_', 
                                                       year, 
                                                       '.csv'))
                                           ) %>% 
  group_by(stadium_id, naics_2digit, distance_bin) %>%
  filter(row_number() == 1) %>%
  inner_join(basketball_stadiums_summary)

basketball_establishments_2018 <- basketball_establishments_2018 %>% 
  filter(distance_bin <= 3) %>%
  group_by(stadium_id, naics_2digit) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T), 
            total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>%
  inner_join(industry_bins) %>% 
  inner_join(industries)

baseball_establishments_2018 <- read_csv(file.path(input_folder,
                                                   paste0('baseball_establishments_', 
                                                          year, 
                                                          '.csv'))
                                         ) %>% 
  group_by(stadium_id, naics_2digit, distance_bin) %>%
  filter(row_number() == 1) %>%
  inner_join(baseball_stadiums_summary)

baseball_establishments_2018 <- baseball_establishments_2018 %>% 
  filter(distance_bin <= 3) %>%
  group_by(stadium_id, naics_2digit) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T), 
            total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>%
  inner_join(industry_bins) %>% 
  inner_join(industries)

football_establishments_2018 <- read_csv(file.path(input_folder,
                                                   paste0('football_establishments_', 
                                                          year, 
                                                          '.csv'))
                                         ) %>% 
  group_by(stadium_id, naics_2digit, distance_bin) %>%
  filter(row_number() == 1) %>%
  inner_join(football_stadiums_summary)
football_establishments_2018 <- football_establishments_2018 %>% 
  filter(distance_bin <= 3) %>%
  group_by(stadium_id, naics_2digit) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T), 
            total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>%
  inner_join(industry_bins) %>% 
  inner_join(industries)

hockey_establishments_2018 <- read_csv(file.path(input_folder,
                                                 paste0('hockey_establishments_', 
                                                        year, 
                                                        '.csv'))
                                       ) %>% 
  group_by(stadium_id, naics_2digit, distance_bin) %>%
  filter(row_number() == 1) %>%
  inner_join(hockey_stadiums_summary)
hockey_establishments_2018 <- hockey_establishments_2018 %>% 
  filter(distance_bin <= 3) %>%
  group_by(stadium_id, naics_2digit) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T), 
            total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>%
  inner_join(industry_bins) %>% 
  inner_join(industries)

###############################################################################


############################# Produce descriptives ############################

# Counts
basketball_establishments_count_2018 <- basketball_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Basketball')

hockey_establishments_count_2018 <- hockey_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Hockey')

football_establishments_count_2018 <- football_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Football')


baseball_establishments_count_2018 <- baseball_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(establishment_count = sum(establishment_count, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Baseball')

establishments_count_2018 <- baseball_establishments_count_2018 %>%
  left_join(basketball_establishments_count_2018) %>%
  left_join(football_establishments_count_2018) %>%
  left_join(hockey_establishments_count_2018)

colnames(establishments_count_2018)[2:5] <- paste(colnames(establishments_count_2018)[2:5], '_count')

# baseball_establishments_count_2018 <- baseball_establishments_2018 %>% 
#   group_by(naics_2digit_bin, distance_bin) %>%
#   summarize(establishment_count = mean(establishment_count, na.rm = T)) %>% 
#   arrange(distance_bin)
# baseball_establishments_count_2018 <- baseball_establishments_count_2018 %>% 
#   pivot_wider(id_cols = naics_2digit_bin, 
#               names_from = distance_bin, 
#               values_from = establishment_count, 
#               values_fill = list(establishment_count = 0)) %>% 
#   arrange(naics_2digit_bin) %>%
#   inner_join(industries) %>% 
#   ungroup() %>%
#   select(-naics_2digit_bin) %>%
#   select(industry_name, everything())
# establishments_count_2018 <- bind_rows(baseball_establishments_count_2018, 
#                                        basketball_establishments_count_2018,
#                                        football_establishments_count_2018,
#                                        hockey_establishments_count_2018)  
# establishments_count_2018 <- establishments_count_2018 %>% rename(' ' = industry_name,
#                                                                   '0-1 km' = `1`,
#                                                                   '1-2 km' = `2`,
#                                                                   '2-3 km' = `3`,
#                                                                   '3-4 km' = `4`,
#                                                                   '4-5 km' = `5`)
# establishments_count_2018 <- kable(establishments_count_2018, 
#                                    'latex',
#                                    caption = '\\label{establishments_near_stadiums} Mean establishment counts near stadiums', 
#                                    booktabs = TRUE,
#                                    digits = 2) %>% 
#   kable_styling() %>% 
#   pack_rows('Baseball', 1, 7) %>% 
#   pack_rows('Basketball', 8, 14) %>%
#   pack_rows('Football', 15, 21) %>%
#   pack_rows('Hockey', 22, 28)
  
#establishments_count_2018 <- str_replace(establishments_count_2018,
#                                         fixed('\\label{tab:}'), 
#                                         '')
#cat(establishments_count_2018, file = file.path(output_folder,
#                                                'establishment_near_stadiums.tex'))

# Visit counts
basketball_establishments_visits_2018 <- basketball_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = total_establishment_visits, 
              values_fill = list(total_establishment_visits = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Basketball')


hockey_establishments_visits_2018 <- hockey_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = total_establishment_visits, 
              values_fill = list(total_establishment_visits = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Hockey')

football_establishments_visits_2018 <- football_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = total_establishment_visits, 
              values_fill = list(total_establishment_visits = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Football')

baseball_establishments_visits_2018 <- baseball_establishments_2018 %>% 
  group_by(stadium_id, industry_name) %>%
  summarize(total_establishment_visits = sum(total_establishment_visits, na.rm = T)) %>% 
  pivot_wider(id_cols = stadium_id, 
              names_from = industry_name, 
              values_from = total_establishment_visits, 
              values_fill = list(total_establishment_visits = 0)) %>%
  ungroup() %>%
  summarize_all(mean) %>%
  select(-stadium_id) %>%
  pivot_longer(cols = everything(), 
               names_to = 'Industry',
               values_to = 'Baseball')

# baseball_establishments_visits_2018 <- baseball_establishments_2018 %>% 
#   group_by(naics_2digit_bin, distance_bin) %>%
#   summarize(mean_monthly_visits = mean(total_establishment_visits / (12 * establishment_count), na.rm = T)) %>% 
#   arrange(distance_bin)
# 
# baseball_establishments_visits_2018 <- baseball_establishments_visits_2018 %>% 
#   pivot_wider(id_cols = naics_2digit_bin, 
#               names_from = distance_bin, 
#               values_from = mean_monthly_visits, 
#               values_fill = list(mean_monthly_visits = 0)) %>% 
#   arrange(naics_2digit_bin) %>%
#   inner_join(industries) %>% 
#   ungroup() %>%
#   select(-naics_2digit_bin) %>%
#   select(industry_name, everything())
# 
# establishments_visits_2018 <- bind_rows(baseball_establishments_visits_2018, 
#                                        basketball_establishments_visits_2018,
#                                        football_establishments_visits_2018,
#                                        hockey_establishments_visits_2018)
# establishments_visits_2018 <- establishments_visits_2018 %>% 
#   rename(' ' = industry_name,
#          '0-1 km' = `1`,
#          '1-2 km' = `2`,
#          '2-3 km' = `3`,
#          '3-4 km' = `4`,
#          '4-5 km' = `5`)
# 
# establishments_visits_2018 <- kable(establishments_visits_2018, 
#                                     'latex',
#                                     caption = '\\label{establishments_visits_near_stadiums} Mean monthly visits to establishments near stadiums', 
#                                     booktabs = TRUE,
#                                     digits = 2) %>% 
#   kable_styling() %>% 
#   pack_rows('Baseball', 1, 7) %>% 
#   pack_rows('Basketball', 8, 14) %>%
#   pack_rows('Football', 15, 21) %>%
#   pack_rows('Hockey', 22, 28)


establishments_visits_2018 <- baseball_establishments_visits_2018 %>%
  left_join(basketball_establishments_visits_2018) %>%
  left_join(football_establishments_visits_2018) %>%
  left_join(hockey_establishments_visits_2018)

establishments_visits_2018 <- establishments_visits_2018 %>% mutate_if(is.numeric, ~ . / 1000)
colnames(establishments_visits_2018)[2:5] <- paste(colnames(establishments_visits_2018)[2:5], '_visits')

nearby_establishments <- establishments_count_2018 %>%
  left_join(establishments_visits_2018)

colnames(nearby_establishments)[2:5] <- str_to_title(sports)
colnames(nearby_establishments)[6:9] <- str_to_title(sports)
 
headers <- c(1, 4, 4)
names(headers) <- c(' ', 
                    'Mean business count within 3km of stadiums',
                    'Mean yearly local business visits (thsd.)'
)

latex_table <- kable(nearby_establishments,
      'latex',
      caption = 'Summary statisitcs on businesses within 3km of stadiums.', 
      label = 'businesseses_near_stadiums',
      booktabs = TRUE,
      digits = 1,
      linesep = '') %>% 
      kable_styling(latex_options = 'scale_down') %>%
  add_header_above(headers) 

latex_table <- stri_split_lines1(latex_table)
latex_table[5] <- paste0(latex_table[5], '%')
#latex_table <- latex_table[3:20]
#latex_table <- c(latex_table[2:18], latex_table[1])
latex_table <- latex_table[3:30]
latex_table <- c(latex_table[2:28], latex_table[1])
latex_table <- str_c(latex_table, collapse = '\n')

cat(latex_table, file = file.path(output_folder,
                                  'establishments_near_stadiums.tex'))

###############################################################################