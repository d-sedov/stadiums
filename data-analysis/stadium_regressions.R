###############################################################################
#
# FILE: stadium_regressions.R
#
# BY: Dmitry Sedov 
#
# DATE: Mon Aug 3 2020
#
# DESC: This code contains the code to estimate the visits-visits regressions.
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
library(dplyr)

###############################################################################


################################ Constants ####################################

#sports <- c('basketball', 'baseball', 'hockey', 'football')
sports <- c('football', 'baseball')

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


############# Function to prepare the data, estimate models ###################

prepareData <- function(sport, industry, lag_days, distance_bins, distance_bins_labels) {
  # Function that imports and prepares the data,
  # lag_days is the lag to generate
  # distance_bins is the vector of distance ranges to sort businesses to
  
  # Set path and import data
  data_file_path <- file.path('/home/quser/project_dir/stadiums/data/processed/analysis',
                              sport,
                              paste0('stadiums_', industry, '.dta')
                              )
  data <- read_dta(data_file_path)
  
  # Convert distance (meters to kilometers), prepare distance bins
  data$distance <- data$distance / 1000
  data <- data %>% 
    mutate(distance_bin = cut(distance, 
                              distance_bins,
                              right = FALSE,
                              distance_bins_labels))
  
  # Compute total visits in distance ranges of stadiums
  data <- data %>% 
    group_by(stadium_id, Date, distance_bin) %>% 
    summarise(visits = sum(visits), 
              stadium_visits = first(stadium_visits), 
              game = first(game), 
              #Attendance = first(Attendance), 
              month = first(month), 
              dow = first(dow))
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

runEstimationDowFE <- function(sport, industry) {
  # Function that estimates the dow fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ stadium_visits | fe | 0 | stadium_id,
                data = subset(data, distance_bin == r), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'dow_fe', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDowIV <- function(sport, industry) {
  # Function that estimates the iv dow fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ 0 | fe | (stadium_visits ~ game) | stadium_id,
              data = subset(data, distance_bin == r), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'dow_iv', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDateFE <- function(sport, industry) {
  # Function that estimates the date fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ stadium_visits | fe + date | 0 | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'date_fe', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationLaggedDateFE <- function(sport, industry) {
  # Function that estimates the lagged date fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ stadium_visits_lagged | fe + date | 0 | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'date_fe_lagged', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDateIV <- function(sport, industry) {
  # Function that estimates the iv date fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ 0 | fe + date | (stadium_visits ~ game) | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'date_iv', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationLaggedDateIV <- function(sport, industry) {
  # Function that estimates the lagged iv date fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
    list(distances[r],
         felm(formula = visits ~ 0 | fe + date | (stadium_visits_lagged ~ game_lagged) | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    )})
  value <- list(sport, industry, 'date_iv_lagged', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

###############################################################################


############### Baseline estimation and result presentation ###################

ranges <- 1:5
distances <- paste(c(0, head(ranges, -1)), ranges, sep = '-')
distances <- paste0(distances, ' km')

n_ampresand <- length(ranges)

# Date fixed effects

# Lists to store the first stages
first_stages_date <- list()
first_stages_date_lagged <- list()
for (s in sports) {
  
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
    
    # Import data
    data <- prepareData(sport = s, 
                        industry = i, 
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
  
  
  # Save the iv first stage
  next_fs <- fits_iv$models[[1]][[2]]$stage1
  first_stages_date <- append(first_stages_date, list(next_fs))
  
  # Save the lagged iv first stage
  next_fs_lagged <- fits_lagged_iv$models[[1]][[2]]$stage1
  first_stages_date_lagged <- append(first_stages_date_lagged, list(next_fs_lagged))
}

# Output first stage IV table:
outfile_path <- file.path(outfolder, 'first_stages_date.tex')
stargazer(first_stages_date,
          title = 'IV first stage estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

# Output first stage lagged IV table:
outfile_path <- file.path(outfolder, 'first_stages_date_lagged.tex')
stargazer(first_stages_date_lagged,
          title = 'Lagged IV first stage estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_lagged'),
          dep.var.caption = 'Dependent varible: lagged stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
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
# Date fixed effects

# Lists to store the first stages
first_stages_date <- list()
first_stages_date_lagged <- list()
for (s in sports) {
  
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
    
    # Import data
    data <- prepareData(sport = s, 
                        industry = i, 
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
  
  
  # Save the iv first stage
  next_fs <- fits_iv$models[[1]][[2]]$stage1
  first_stages_date <- append(first_stages_date, list(next_fs))
  
  # Save the lagged iv first stage
  next_fs_lagged <- fits_lagged_iv$models[[1]][[2]]$stage1
  first_stages_date_lagged <- append(first_stages_date_lagged, list(next_fs_lagged))
}

# Output first stage IV table:
outfile_path <- file.path(outfolder, 'first_stages_date_alt_dist.tex')
stargazer(first_stages_date,
          title = 'IV first stage estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_alt_dist'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)

# Output first stage lagged IV table:
outfile_path <- file.path(outfolder, 'first_stages_date_lagged_alt_dist.tex')
stargazer(first_stages_date_lagged,
          title = 'Lagged IV first stage estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs_lagged_alt_dist'),
          dep.var.caption = 'Dependent varible: lagged stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
          model.numbers = FALSE,
          digits = 2, digits.extra = 0,
          align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)


###############################################################################


################## ARCHIVE: stadium-month-dow fe only #########################
# 
# # Dow fixed effects
# first_stages_dow <- list()
# for (s in sports) {
#   
#   # Produce the FE tables
#   headers <- c()
#   main <- c()
#   footers <- c()
#   
#   for (i in industries) {
#     
#     data <- prepareData(sport = s, 
#                         industry = i, 
#                         lag_days = l_d,
#                         distance_bins = seq(0, ranges), 
#                         distance_bins_labels = 1 : ranges)
#     fits <- runEstimationDowFE(s, i)
#     latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
#                                             title = paste0(stri_trans_totitle(s), 
#                                                            ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
#                                             label = paste0(s,'_dow_fe'),
#                                             dep.var.caption = 'Dependent varible: establishment visits',
#                                             dep.var.labels = 'Distance ranges',
#                                             column.labels = distances,
#                                             model.numbers = FALSE,
#                                             digits = 4, digits.extra = 0,
#                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
#                                             type = 'latex', column.sep.width = '-20pt'))
#     headers <- latex_table[1:15]
#     main <- c(main, 
#               ' \\addlinespace',
#               paste0('\\textit{', i, '} & & & & & \\\\'), 
#               latex_table[c(16, 17)],
#               ' \\addlinespace',
#               latex_table[20],
#               ' \\addlinespace')
#     footers <- latex_table[24:length(latex_table)]
#   }
#   
#   latex_table <- c(headers, main, '\\addlinespace', footers)
#   latex_table <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table)
#   latex_table <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table)
#   latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
#   latex_table <- gsub('Observations', 'Obs.', latex_table)
#   
#   # Output table:
#   outfile_path <- file.path(outfolder, paste0(s, '_dow_fe.tex'))
#   cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
#   
#   # Produce the IV tables
#   headers <- c()
#   main <- c()
#   footers <- c()
#   
#   for (i in industries) {
#     
#     data <- prepareData(sport = s, 
#                         industry = i, 
#                         lag_days = l_d,
#                         distance_bins = seq(0, ranges), 
#                         distance_bins_labels = 1 : ranges)
#     fits <- runEstimationDowIV(s, i)
#     latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
#                                             title = paste0(stri_trans_totitle(s), 
#                                                            ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
#                                             label = paste0(s, '_dow_iv'),
#                                             dep.var.caption = 'Dependent varible: establishment visits',
#                                             dep.var.labels = 'Distance ranges',
#                                             column.labels = distances,
#                                             model.numbers = FALSE,
#                                             digits = 4, digits.extra = 0,
#                                             align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
#                                             type = 'latex', column.sep.width = '-20pt'))
#     headers <- latex_table[1:15]
#     main <- c(main, 
#               ' \\addlinespace',
#               paste0('\\textit{', i, '} & & & & & \\\\'), 
#               latex_table[c(16, 17)],
#               ' \\addlinespace',
#               latex_table[20],
#               ' \\addlinespace')
#     footers <- latex_table[24:length(latex_table)]
#   }
#   
#   latex_table <- c(headers, main, '\\addlinespace', footers)
#   latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad `stadium\\\\_visits(fit)`', latex_table)
#   latex_table <- gsub('`stadium\\\\_visits\\(fit\\)`', 'Stadium visits', latex_table)
#   latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
#   latex_table <- gsub('Observations', 'Obs.', latex_table)
#   
#   # Output table:
#   outfile_path <- file.path(outfolder, paste0(s, '_dow_iv.tex'))
#   cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
#   
#   next_fs <- fits$models[[1]][[2]]$stage1
#   first_stages_dow <- append(first_stages_dow, list(next_fs))
# }
# 
# # Output first stage table:
# outfile_path <- file.path(outfolder, 'first_stages_dow.tex')
# stargazer(first_stages_dow,
#           title = 'IV first stage estimates.  All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
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