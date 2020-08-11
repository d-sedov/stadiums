###############################################################################
#
# FILE: establishment_description.R
#
# BY: Dmitry Sedov 
#
# DATE: Fri Aug 7 2020
#
# DESC: This code contains the code to produce descripritive statistics for
#       establishments close to stadiums.
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

industry_bins <- list(naics_2digit = c(44, 45, 52, 61, 62, 71, 72, 81),
                      naics_2digit_bin = c(44, 44, 52, 61, 62, 71, 72, 81)
                      )
industry_bins <- as.data.frame(industry_bins)

industries <- list(naics_2digit_bin = c(44, 45, 52, 61, 62, 71, 72, 81),
                   industry_name = c('Retail',
                                     'Retail',
                                     'Finance', 
                                     'Education', 
                                     'Health',
                                     'Recreation',
                                     'FoodAccommodation',
                                     'Other Services')
                   )
industries <- as.data.frame(industries)

ranges <- 5

distances <- paste(seq(0, ranges - 1), seq(1, ranges), sep = '-')
distances <- paste0(distances, ' km')

input_folder <- file.path('/home/quser/project_dir', 
                          'stadiums/data/processed/descriptive')

output_folder <- file.path('/home/quser/project_dir',
                           'stadiums/output/tables') 

###############################################################################


############################## Import data ####################################

basketball_establishments_2018 <- read_csv(file.path(input_folder,
                                                paste0('basketball_establishments_', 
                                                       year, 
                                                       '.csv'))
                                           )
basketball_establishments_2018 <- basketball_establishments_2018 %>% 
  inner_join(industry_bins)

baseball_establishments_2018 <- read_csv(file.path(input_folder,
                                                   paste0('baseball_establishments_', 
                                                          year, 
                                                          '.csv'))
                                         )
baseball_establishments_2018 <- baseball_establishments_2018 %>%
  inner_join(industry_bins)

football_establishments_2018 <- read_csv(file.path(input_folder,
                                                   paste0('football_establishments_', 
                                                          year, 
                                                          '.csv'))
                                         )
football_establishments_2018 <- football_establishments_2018 %>% 
  inner_join(industry_bins)

hockey_establishments_2018 <- read_csv(file.path(input_folder,
                                                 paste0('hockey_establishments_', 
                                                        year, 
                                                        '.csv'))
                                       )
hockey_establishments_2018 <- hockey_establishments_2018 %>% 
  inner_join(industry_bins)

###############################################################################


############################# Produce descriptives ############################

# Counts
basketball_establishments_count_2018 <- basketball_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(establishment_count = mean(establishment_count, na.rm = T)) %>% 
  arrange(distance_bin)

basketball_establishments_count_2018 <- basketball_establishments_count_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())

hockey_establishments_count_2018 <- hockey_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(establishment_count = mean(establishment_count, na.rm = T)) %>% 
  arrange(distance_bin)

hockey_establishments_count_2018 <- hockey_establishments_count_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())


football_establishments_count_2018 <- football_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(establishment_count = mean(establishment_count, na.rm = T)) %>% 
  arrange(distance_bin)

football_establishments_count_2018 <- football_establishments_count_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())


baseball_establishments_count_2018 <- baseball_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(establishment_count = mean(establishment_count, na.rm = T)) %>% 
  arrange(distance_bin)

baseball_establishments_count_2018 <- baseball_establishments_count_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = establishment_count, 
              values_fill = list(establishment_count = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())

establishments_count_2018 <- bind_rows(baseball_establishments_count_2018, 
                                       basketball_establishments_count_2018,
                                       football_establishments_count_2018,
                                       hockey_establishments_count_2018)
establishments_count_2018 <- establishments_count_2018 %>% rename(' ' = industry_name,
                                                                  '0-1 km' = `1`,
                                                                  '1-2 km' = `2`,
                                                                  '2-3 km' = `3`,
                                                                  '3-4 km' = `4`,
                                                                  '4-5 km' = `5`)

establishments_count_2018 <- kable(establishments_count_2018, 
                                   'latex',
                                   caption = '\\label{establishments_near_stadiums} Mean establishment counts near stadiums', 
                                   booktabs = TRUE,
                                   digits = 2) %>% 
  kable_styling() %>% 
  pack_rows('Baseball', 1, 7) %>% 
  pack_rows('Basketball', 8, 14) %>%
  pack_rows('Football', 15, 21) %>%
  pack_rows('Hockey', 22, 28)
  
establishments_count_2018 <- str_replace(establishments_count_2018,
                                         fixed('\\label{tab:}'), 
                                         '')
cat(establishments_count_2018, file = file.path(output_folder,
                                                'establishment_near_stadiums.tex'))

# Visit counts
basketball_establishments_visits_2018 <- basketball_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(mean_monthly_visits = mean(total_establishment_visits / (12 * establishment_count), na.rm = T)) %>% 
  arrange(distance_bin)

basketball_establishments_visits_2018 <- basketball_establishments_visits_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = mean_monthly_visits, 
              values_fill = list(mean_monthly_visits = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())


hockey_establishments_visits_2018 <- hockey_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(mean_monthly_visits = mean(total_establishment_visits / (12 * establishment_count), na.rm = T)) %>% 
  arrange(distance_bin)

hockey_establishments_visits_2018 <- hockey_establishments_visits_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = mean_monthly_visits, 
              values_fill = list(mean_monthly_visits = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())


football_establishments_visits_2018 <- football_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(mean_monthly_visits = mean(total_establishment_visits / (12 * establishment_count), na.rm = T)) %>% 
  arrange(distance_bin)

football_establishments_visits_2018 <- football_establishments_visits_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = mean_monthly_visits, 
              values_fill = list(mean_monthly_visits = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())

baseball_establishments_visits_2018 <- baseball_establishments_2018 %>% 
  group_by(naics_2digit_bin, distance_bin) %>%
  summarize(mean_monthly_visits = mean(total_establishment_visits / (12 * establishment_count), na.rm = T)) %>% 
  arrange(distance_bin)

baseball_establishments_visits_2018 <- baseball_establishments_visits_2018 %>% 
  pivot_wider(id_cols = naics_2digit_bin, 
              names_from = distance_bin, 
              values_from = mean_monthly_visits, 
              values_fill = list(mean_monthly_visits = 0)) %>% 
  arrange(naics_2digit_bin) %>%
  inner_join(industries) %>% 
  ungroup() %>%
  select(-naics_2digit_bin) %>%
  select(industry_name, everything())

establishments_visits_2018 <- bind_rows(baseball_establishments_visits_2018, 
                                       basketball_establishments_visits_2018,
                                       football_establishments_visits_2018,
                                       hockey_establishments_visits_2018)
establishments_visits_2018 <- establishments_visits_2018 %>% 
  rename(' ' = industry_name,
         '0-1 km' = `1`,
         '1-2 km' = `2`,
         '2-3 km' = `3`,
         '3-4 km' = `4`,
         '4-5 km' = `5`)

establishments_visits_2018 <- kable(establishments_visits_2018, 
                                    'latex',
                                    caption = '\\label{establishments_visits_near_stadiums} Mean monthly visits to establishments near stadiums', 
                                    booktabs = TRUE,
                                    digits = 2) %>% 
  kable_styling() %>% 
  pack_rows('Baseball', 1, 7) %>% 
  pack_rows('Basketball', 8, 14) %>%
  pack_rows('Football', 15, 21) %>%
  pack_rows('Hockey', 22, 28)

establishments_visits_2018 <- str_replace(establishments_visits_2018,
                                          fixed('\\label{tab:}'),
                                          '')
cat(establishments_visits_2018, file = file.path(output_folder,
                                                 'establishment_visits_near_stadiums.tex'))

###############################################################################