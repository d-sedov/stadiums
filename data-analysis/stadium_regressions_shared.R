###############################################################################
#
# FILE: stadium_regressions_shared.R
#
# BY: Dmitry Sedov 
#
# DATE: Mon Aug 3 2020
#
# DESC: This code contains the code to estimate the visits-visits regressions
#       for shared basketball + hockey stadiums.
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

###############################################################################


################################ Constants ####################################

industries <- c('FoodAccommodation', 
                'Retail', 
                'Finance', 
                'Professional', 
                'Education', 
                'Health')

industries_main <- c('FoodAccommodation', 
                     'Retail')

outfolder <- file.path('/home/quser/project_dir/',
                       'stadiums/output/tables') 

# lag days for estimation
l_d <- 1

###############################################################################

######################### Data preparation function ###########################

prepareDataShared <- function(industry, lag_days, distance_bins, distance_bins_labels) {
  
  # Get basketball data
  basketball_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                    'basketball',
                                    paste0('stadiums_', industry, '.dta')
  )
  basketball <- read_dta(basketball_file_path)
  
  # Fix distance, compute distance bins
  basketball$distance <- basketball$distance / 1000
  basketball <- basketball %>% 
    mutate(distance_bin = cut(distance, 
                              distance_bins,
                              right = FALSE,
                              distance_bins_labels))
  
  # Compute total visits in distance ranges of stadiums
  basketball <- basketball %>% 
    group_by(stadium_id, Date, distance_bin) %>% 
    summarise(visits = sum(visits), 
              stadium_visits = first(stadium_visits), 
              game = first(game), 
              #Attendance = first(Attendance), 
              month = first(month), 
              dow = first(dow))
  
  # Get hockey data
  hockey_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis', 
                                'hockey',
                                paste0('stadiums_', industry, '.dta')
                                )
  hockey <- read_dta(hockey_file_path)
  
  # Fix distance, compute distance bins
  hockey$distance <- hockey$distance / 1000
  hockey <- hockey %>% 
    mutate(distance_bin = cut(distance, 
                              distance_bins,
                              right = FALSE,
                              distance_bins_labels))
  # Compute total visits in distance ranges of stadiums
  hockey <- hockey %>% 
    group_by(stadium_id, Date, distance_bin) %>% 
    summarise(visits = sum(visits), 
              stadium_visits = first(stadium_visits), 
              game = first(game), 
              #Attendance = first(Attendance), 
              month = first(month), 
              dow = first(dow))
  
  # Concatenate data, select game-days from both sports
  # (apply 'any' aggregation function to 'game' column)
  data <- bind_rows(basketball, hockey) %>% 
    group_by(stadium_id, Date, distance_bin) %>% 
    arrange(desc(game)) %>%
    filter(row_number() == 1)
  
  # Generate lags
  data <- data %>% 
    group_by(stadium_id, distance_bin) %>% 
    mutate(stadium_visits_lagged = lag(stadium_visits, n = lag_days, order_by = Date), 
           game_lagged = lag(game, n = lag_days, order_by = Date))
  
  # Prepare fixed effects
  data$month <- factor(data$month)
  data$dow <- factor(data$dow)
  data$stadium_id <- factor(data$stadium_id)
  data$fe <- interaction(data$month, data$dow, data$stadium_id)
  data$sm <- interaction(data$month, data$stadium_id)
  data$date <- factor(data$Date)

  return(data)
}

###############################################################################


########################### Present the results ###############################

s <- 'basketball_hockey'

# Distance ranges
ranges <- 1 : 5
distances <- paste(c(0, head(ranges, -1)), ranges, sep = '-')
distances <- paste0(distances, ' km')

n_ampresand <- length(ranges)

# Preparing the FE tables
headers_fe <- c()
main_fe <- c()
footers_fe <- c()

headers_lagged_fe <- c()
main_lagged_fe <- c()
footers_lagged_fe <- c()

# Preparing the IV tables
headers_iv <- c()
main_iv <- c()
footers_iv <- c()

headers_lagged_iv <- c()
main_lagged_iv <- c()
footers_lagged_iv <- c()

for (i in industries) {
  
  data <- prepareDataShared(industry = i, 
                            lag_days = l_d,
                            distance_bins = c(0, ranges), 
                            distance_bins_labels = ranges)
  
  # Estimate the FE models
  fits_fe <- runEstimationDateFE(s, i)
  latex_table_fe <- capture.output(stargazer(lapply(fits_fe$models, function(x) {x[[2]]}), 
                                             title = paste0(stri_trans_totitle(s), 
                                                            ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                             label = paste0(s,'_date_fe'),
                                             dep.var.caption = 'Dependent varible: establishment visits',
                                             dep.var.labels = 'Distance ranges',
                                             column.labels = distances,
                                             model.numbers = FALSE,
                                             digits = 4, digits.extra = 0,
                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                             type = 'latex', column.sep.width = '-20pt'))
  headers_fe <- latex_table_fe[1:15]
  main_fe <- c(main_fe, 
               ' \\addlinespace',
               paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
               latex_table_fe[c(16, 17)],
               ' \\addlinespace',
               latex_table_fe[20],
               ' \\addlinespace')
  footers_fe <- latex_table_fe[24:length(latex_table_fe)]
  
  # Estimate the lagged FE models
  fits_lagged_fe <- runEstimationLaggedDateFE(s, i)
  latex_table_lagged_fe <- capture.output(stargazer(lapply(fits_lagged_fe$models, function(x) {x[[2]]}), 
                                                    title = paste0(stri_trans_totitle(s), 
                                                                   ' stadiums: lagged OLS FE estimates. All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                                    label = paste0(s,'_date_lagged_fe'),
                                                    dep.var.caption = 'Dependent varible: establishment visits',
                                                    dep.var.labels = 'Distance ranges',
                                                    column.labels = distances,
                                                    model.numbers = FALSE,
                                                    digits = 4, digits.extra = 0,
                                                    align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                                    type = 'latex', column.sep.width = '-20pt'))
  headers_lagged_fe <- latex_table_lagged_fe[1:15]
  main_lagged_fe <- c(main_lagged_fe, 
                      ' \\addlinespace',
                      paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
                      latex_table_lagged_fe[c(16, 17)],
                      ' \\addlinespace',
                      latex_table_lagged_fe[20],
                      ' \\addlinespace')
  footers_lagged_fe <- latex_table_lagged_fe[24:length(latex_table_lagged_fe)]
  
  # Estimate the IV model
  fits_iv <- runEstimationDateIV(s, i)
  latex_table_iv <- capture.output(stargazer(lapply(fits_iv$models, function(x) {x[[2]]}), 
                                             title = paste0(stri_trans_totitle(s), 
                                                            ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                             label = paste0(s, '_date_iv'),
                                             dep.var.caption = 'Dependent varible: establishment visits',
                                             dep.var.labels = 'Distance ranges',
                                             column.labels = distances,
                                             model.numbers = FALSE,
                                             digits = 4, digits.extra = 0,
                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                             type = 'latex', column.sep.width = '-20pt'))
  headers_iv <- latex_table_iv[1:15]
  main_iv <- c(main_iv, 
               ' \\addlinespace',
               paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
               latex_table_iv[c(16, 17)],
               ' \\addlinespace',
               latex_table_iv[20],
               ' \\addlinespace')
  footers_iv <- latex_table_iv[24:length(latex_table_iv)]
  
  # Estimate the lagged IV model
  fits_lagged_iv <- runEstimationLaggedDateIV(s, i)
  latex_table_lagged_iv <- capture.output(stargazer(lapply(fits_lagged_iv$models, function(x) {x[[2]]}), 
                                                    title = paste0(stri_trans_totitle(s), 
                                                                   ' stadiums: lagged IV estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                                    label = paste0(s, '_date_lagged_iv'),
                                                    dep.var.caption = 'Dependent varible: establishment visits',
                                                    dep.var.labels = 'Distance ranges',
                                                    column.labels = distances,
                                                    model.numbers = FALSE,
                                                    digits = 4, digits.extra = 0,
                                                    align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                                    type = 'latex', column.sep.width = '-20pt'))
  headers_lagged_iv <- latex_table_lagged_iv[1:15]
  main_lagged_iv <- c(main_lagged_iv, 
                      ' \\addlinespace',
                      paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
                      latex_table_lagged_iv[c(16, 17)],
                      ' \\addlinespace',
                      latex_table_lagged_iv[20],
                      ' \\addlinespace')
  footers_lagged_iv <- latex_table_lagged_iv[24:length(latex_table_lagged_iv)]
}

# Construct the FE table
latex_table_fe <- c(headers_fe, main_fe, '\\addlinespace', footers_fe)
latex_table_fe <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table_fe)
latex_table_fe <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table_fe)
latex_table_fe <- gsub('Observations', '\\\\quad Observations', latex_table_fe)
latex_table_fe <- gsub('Observations', 'Obs.', latex_table_fe)

# Output the FE table
outfile_path <- file.path(outfolder, paste0(s, '_date_fe.tex'))
cat(paste0(latex_table_fe, collapse = '\n'), file = outfile_path)


# Construct the lagged FE table
latex_table_lagged_fe <- c(headers_lagged_fe, main_lagged_fe, '\\addlinespace', footers_lagged_fe)
latex_table_lagged_fe <- gsub(' stadium\\\\_visits\\\\_lagged', 
                              '\\\\quad stadium\\\\_visits\\\\_lagged', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('stadium\\\\_visits\\\\_lagged', 'Lagged stadium visits', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('Observations', '\\\\quad Observations', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('Observations', 'Obs.', latex_table_lagged_fe)

# Output the lagged FE table
outfile_path <- file.path(outfolder, paste0(s, '_date_lagged_fe.tex'))
cat(paste0(latex_table_lagged_fe, collapse = '\n'), file = outfile_path)


# Construct the IV table
latex_table_iv <- c(headers_iv, 
                    main_iv, 
                    '\\addlinespace', 
                    footers_iv)
latex_table_iv <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', 
                       '\\\\quad `stadium\\\\_visits(fit)`',
                       latex_table_iv)
latex_table_iv <- gsub('`stadium\\\\_visits\\(fit\\)`',
                       'Stadium visits',
                       latex_table_iv)
latex_table_iv <- gsub('Observations', 
                       '\\\\quad Observations', 
                       latex_table_iv)
latex_table_iv <- gsub('Observations', 
                       'Obs.', 
                       latex_table_iv)

# Output the IV table:
outfile_path <- file.path(outfolder, paste0(s, '_date_iv.tex'))
cat(paste0(latex_table_iv, collapse = '\n'), file = outfile_path)


# Construct the lagged IV table
latex_table_lagged_iv <- c(headers_lagged_iv, 
                           main_lagged_iv, 
                           '\\addlinespace', 
                           footers_lagged_iv)
latex_table_lagged_iv <- gsub(' \`stadium\\\\_visits\\\\_lagged\\(fit\\)\`', 
                              '\\\\quad `stadium\\\\_visits\\\\_lagged(fit)`',
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('`stadium\\\\_visits\\\\_lagged\\(fit\\)`',
                              'Lagged stadium visits',
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('Observations', 
                              '\\\\quad Observations', 
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('Observations', 
                              'Obs.', 
                              latex_table_lagged_iv)

# Output the lagged IV table
outfile_path <- file.path(outfolder, paste0(s, '_date_lagged_iv.tex'))
cat(paste0(latex_table_lagged_iv, collapse = '\n'), file = outfile_path)

# Output first stage:
#  iv first stage
fs <- fits_iv$models[[1]][[2]]$stage1
# lagged iv first stage
fs_lagged <- fits_lagged_iv$models[[1]][[2]]$stage1

# Output first stage table:
outfile_path <- file.path(outfolder, 'shared_date_first_stages.tex')
stargazer(fs,
          title = 'IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = s,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

outfile_path <- file.path(outfolder, 'shared_date_first_stages_lagged.tex')
stargazer(fs_lagged,
          title = 'Lagged IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_lagged'),
          dep.var.caption = 'Dependent varible: lagged stadium visits',
          dep.var.labels = 'Sports',
          column.labels = s,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

###############################################################################


##################### Alternative distance bins ###############################

ranges <- c(0.25, 1, 5)
distances <- paste(c(0, head(ranges, -1)), ranges, sep = '-')
distances <- paste0(distances, ' km')

n_ampresand <- length(ranges)

# Preparing the FE tables
headers_fe <- c()
main_fe <- c()
footers_fe <- c()

headers_lagged_fe <- c()
main_lagged_fe <- c()
footers_lagged_fe <- c()

# Preparing the IV tables
headers_iv <- c()
main_iv <- c()
footers_iv <- c()

headers_lagged_iv <- c()
main_lagged_iv <- c()
footers_lagged_iv <- c()

for (i in industries_main) {
  
  data <- prepareDataShared(industry = i, 
                            lag_days = l_d,
                            distance_bins = c(0, ranges), 
                            distance_bins_labels = ranges)
  
  # Estimate the FE models
  fits_fe <- runEstimationDateFE(s, i)
  latex_table_fe <- capture.output(stargazer(lapply(fits_fe$models, function(x) {x[[2]]}), 
                                             title = paste0(stri_trans_totitle(s), 
                                                            ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                             label = paste0(s,'_date_fe_alt_dist'),
                                             dep.var.caption = 'Dependent varible: establishment visits',
                                             dep.var.labels = 'Distance ranges',
                                             column.labels = distances,
                                             model.numbers = FALSE,
                                             digits = 4, digits.extra = 0,
                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                             type = 'latex', column.sep.width = '-20pt'))
  headers_fe <- latex_table_fe[1:15]
  main_fe <- c(main_fe, 
               ' \\addlinespace',
               paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
               latex_table_fe[c(16, 17)],
               ' \\addlinespace',
               latex_table_fe[20],
               ' \\addlinespace')
  footers_fe <- latex_table_fe[24:length(latex_table_fe)]
  
  # Estimate the lagged FE models
  fits_lagged_fe <- runEstimationLaggedDateFE(s, i)
  latex_table_lagged_fe <- capture.output(stargazer(lapply(fits_lagged_fe$models, function(x) {x[[2]]}), 
                                                    title = paste0(stri_trans_totitle(s), 
                                                                   ' stadiums: lagged OLS FE estimates. All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                                    label = paste0(s,'_date_lagged_fe_alt_dist'),
                                                    dep.var.caption = 'Dependent varible: establishment visits',
                                                    dep.var.labels = 'Distance ranges',
                                                    column.labels = distances,
                                                    model.numbers = FALSE,
                                                    digits = 4, digits.extra = 0,
                                                    align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                                    type = 'latex', column.sep.width = '-20pt'))
  headers_lagged_fe <- latex_table_lagged_fe[1:15]
  main_lagged_fe <- c(main_lagged_fe, 
                      ' \\addlinespace',
                      paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
                      latex_table_lagged_fe[c(16, 17)],
                      ' \\addlinespace',
                      latex_table_lagged_fe[20],
                      ' \\addlinespace')
  footers_lagged_fe <- latex_table_lagged_fe[24:length(latex_table_lagged_fe)]
  
  # Estimate the IV model
  fits_iv <- runEstimationDateIV(s, i)
  latex_table_iv <- capture.output(stargazer(lapply(fits_iv$models, function(x) {x[[2]]}), 
                                             title = paste0(stri_trans_totitle(s), 
                                                            ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                             label = paste0(s, '_date_iv_alt_dist'),
                                             dep.var.caption = 'Dependent varible: establishment visits',
                                             dep.var.labels = 'Distance ranges',
                                             column.labels = distances,
                                             model.numbers = FALSE,
                                             digits = 4, digits.extra = 0,
                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                             type = 'latex', column.sep.width = '-20pt'))
  headers_iv <- latex_table_iv[1:15]
  main_iv <- c(main_iv, 
               ' \\addlinespace',
               paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
               latex_table_iv[c(16, 17)],
               ' \\addlinespace',
               latex_table_iv[20],
               ' \\addlinespace')
  footers_iv <- latex_table_iv[24:length(latex_table_iv)]
  
  # Estimate the lagged IV model
  fits_lagged_iv <- runEstimationLaggedDateIV(s, i)
  latex_table_lagged_iv <- capture.output(stargazer(lapply(fits_lagged_iv$models, function(x) {x[[2]]}), 
                                                    title = paste0(stri_trans_totitle(s), 
                                                                   ' stadiums: lagged IV estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                                                    label = paste0(s, '_date_lagged_iv_alt_dist'),
                                                    dep.var.caption = 'Dependent varible: establishment visits',
                                                    dep.var.labels = 'Distance ranges',
                                                    column.labels = distances,
                                                    model.numbers = FALSE,
                                                    digits = 4, digits.extra = 0,
                                                    align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
                                                    type = 'latex', column.sep.width = '-20pt'))
  headers_lagged_iv <- latex_table_lagged_iv[1:15]
  main_lagged_iv <- c(main_lagged_iv, 
                      ' \\addlinespace',
                      paste0('\\textit{', i, '} ', paste(rep('&', n_ampresand), collapse = ' '), ' \\\\'), 
                      latex_table_lagged_iv[c(16, 17)],
                      ' \\addlinespace',
                      latex_table_lagged_iv[20],
                      ' \\addlinespace')
  footers_lagged_iv <- latex_table_lagged_iv[24:length(latex_table_lagged_iv)]
}

# Construct the FE table
latex_table_fe <- c(headers_fe, main_fe, '\\addlinespace', footers_fe)
latex_table_fe <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table_fe)
latex_table_fe <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table_fe)
latex_table_fe <- gsub('Observations', '\\\\quad Observations', latex_table_fe)
latex_table_fe <- gsub('Observations', 'Obs.', latex_table_fe)

# Output the FE table
outfile_path <- file.path(outfolder, paste0(s, '_date_fe_alt_dist.tex'))
cat(paste0(latex_table_fe, collapse = '\n'), file = outfile_path)


# Construct the lagged FE table
latex_table_lagged_fe <- c(headers_lagged_fe, main_lagged_fe, '\\addlinespace', footers_lagged_fe)
latex_table_lagged_fe <- gsub(' stadium\\\\_visits\\\\_lagged', 
                              '\\\\quad stadium\\\\_visits\\\\_lagged', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('stadium\\\\_visits\\\\_lagged', 'Lagged stadium visits', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('Observations', '\\\\quad Observations', latex_table_lagged_fe)
latex_table_lagged_fe <- gsub('Observations', 'Obs.', latex_table_lagged_fe)

# Output the lagged FE table
outfile_path <- file.path(outfolder, paste0(s, '_date_lagged_fe_alt_dist.tex'))
cat(paste0(latex_table_lagged_fe, collapse = '\n'), file = outfile_path)


# Construct the IV table
latex_table_iv <- c(headers_iv, 
                    main_iv, 
                    '\\addlinespace', 
                    footers_iv)
latex_table_iv <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', 
                       '\\\\quad `stadium\\\\_visits(fit)`',
                       latex_table_iv)
latex_table_iv <- gsub('`stadium\\\\_visits\\(fit\\)`',
                       'Stadium visits',
                       latex_table_iv)
latex_table_iv <- gsub('Observations', 
                       '\\\\quad Observations', 
                       latex_table_iv)
latex_table_iv <- gsub('Observations', 
                       'Obs.', 
                       latex_table_iv)

# Output the IV table:
outfile_path <- file.path(outfolder, paste0(s, '_date_iv_alt_dist.tex'))
cat(paste0(latex_table_iv, collapse = '\n'), file = outfile_path)


# Construct the lagged IV table
latex_table_lagged_iv <- c(headers_lagged_iv, 
                           main_lagged_iv, 
                           '\\addlinespace', 
                           footers_lagged_iv)
latex_table_lagged_iv <- gsub(' \`stadium\\\\_visits\\\\_lagged\\(fit\\)\`', 
                              '\\\\quad `stadium\\\\_visits\\\\_lagged(fit)`',
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('`stadium\\\\_visits\\\\_lagged\\(fit\\)`',
                              'Lagged stadium visits',
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('Observations', 
                              '\\\\quad Observations', 
                              latex_table_lagged_iv)
latex_table_lagged_iv <- gsub('Observations', 
                              'Obs.', 
                              latex_table_lagged_iv)

# Output the lagged IV table
outfile_path <- file.path(outfolder, paste0(s, '_date_lagged_iv_alt_dist.tex'))
cat(paste0(latex_table_lagged_iv, collapse = '\n'), file = outfile_path)

# Output first stage:
#  iv first stage
fs <- fits_iv$models[[1]][[2]]$stage1
# lagged iv first stage
fs_lagged <- fits_lagged_iv$models[[1]][[2]]$stage1

# Output first stage table:
outfile_path <- file.path(outfolder, 'shared_date_first_stages_alt_dist.tex')
stargazer(fs,
          title = 'IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_alt_dist'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = s,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

outfile_path <- file.path(outfolder, 'shared_date_first_stages_lagged_alt_dist.tex')
stargazer(fs_lagged,
          title = 'Lagged IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_lagged_alt_dist'),
          dep.var.caption = 'Dependent varible: lagged stadium visits',
          dep.var.labels = 'Sports',
          column.labels = s,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

###############################################################################


###################### ARCHIVE: stadium-month-dow fe only #####################
# 
# # Dow FE
# headers <- c()
# main <- c()
# footers <- c()
# for (i in industries) {
#   
#   data <- prepareDataShared(i)
#   fits <- runEstimationDowFE(s, i)
#   latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
#                                           title = paste0(stri_trans_totitle(s), 
#                                                          ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
#                                           label = paste0(s,'_dow_fe'),
#                                           dep.var.caption = 'Dependent varible: establishment visits',
#                                           dep.var.labels = 'Distance ranges',
#                                           column.labels = distances,
#                                           model.numbers = FALSE,
#                                           digits = 4, digits.extra = 0,
#                                           align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
#                                           type = 'latex', column.sep.width = '-20pt'))
#   headers <- latex_table[1:15]
#   main <- c(main, 
#             ' \\addlinespace',
#             paste0('\\textit{', i, '} & & & & & \\\\'), 
#             latex_table[c(16, 17)],
#             ' \\addlinespace',
#             latex_table[20],
#             ' \\addlinespace')
#   footers <- latex_table[24:length(latex_table)]
# }
# 
# latex_table <- c(headers, main, '\\addlinespace', footers)
# latex_table <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table)
# latex_table <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table)
# latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
# latex_table <- gsub('Observations', 'Obs.', latex_table)
# 
# # Output table:
# outfile_path <- file.path(outfolder, paste0(s, '_dow_fe.tex'))
# cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
# 
# # Produce the IV tables
# headers <- c()
# main <- c()
# footers <- c()
# 
# for (i in industries) {
#   
#   data <- prepareDataShared(i)
#   fits <- runEstimationDowIV(s, i)
#   latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
#                                           title = paste0(stri_trans_totitle(s), 
#                                                          ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
#                                           label = paste0(s, '_dow_iv'),
#                                           dep.var.caption = 'Dependent varible: establishment visits',
#                                           dep.var.labels = 'Distance ranges',
#                                           column.labels = distances,
#                                           model.numbers = FALSE,
#                                           digits = 4, digits.extra = 0,
#                                           align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
#                                           type = 'latex', column.sep.width = '-20pt'))
#   headers <- latex_table[1:15]
#   main <- c(main, 
#             ' \\addlinespace',
#             paste0('\\textit{', i, '} & & & & & \\\\'), 
#             latex_table[c(16, 17)],
#             ' \\addlinespace',
#             latex_table[20],
#             ' \\addlinespace')
#   footers <- latex_table[24:length(latex_table)]
# }
# 
# latex_table <- c(headers, main, '\\addlinespace', footers)
# latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad `stadium\\\\_visits(fit)`', latex_table)
# latex_table <- gsub('`stadium\\\\_visits\\(fit\\)`', 'Stadium visits', latex_table)
# latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
# latex_table <- gsub('Observations', 'Obs.', latex_table)
# 
# # Output table:
# outfile_path <- file.path(outfolder, paste0(s, '_dow_iv.tex'))
# cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
# 
# # Output first stage:
# fs <- fits$models[[1]][[2]]$stage1
# 
# # Output first stage table:
# outfile_path <- file.path(outfolder, 'shared_dow_first_stages.tex')
# stargazer(fs,
#           title = 'IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
#           label = paste0(s, '_dow_fs'),
#           dep.var.caption = 'Dependent varible: stadium visits',
#           dep.var.labels = 'Sports',
#           column.labels = sports,
#           model.numbers = FALSE,
#           digits = 2, digits.extra = 0,
#           align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
#           type = 'latex', 
#           column.sep.width = '-20pt',
#           out = outfile_path)
#
###############################################################################