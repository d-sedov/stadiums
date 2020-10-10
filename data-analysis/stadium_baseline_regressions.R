###############################################################################
#
# FILE: stadium_regressions_aggregated.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Aug 27 2020
#
# DESC: This code contains the code to estimate the visits-visits regressions
#       without distance breakdown.
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
library(stringr)

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
my_theme_large <- theme(legend.text = element_text(size = 14),
                        legend.title = element_text(face = 'bold', size = 16),
                        plot.title = element_text(hjust = 0, size = 18, face = 'bold'),
                        axis.text = element_text(size = 14),
                        axis.title = element_text(size = 16)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200', 'orange')

###############################################################################


################################ Constants ####################################

#sports <- c('football', 'baseball', 'basketball_hockey')
#sports_names <- c('Football', 'Baseball', 'Basketball \\& Hockey')

# industries <- c('FoodAccommodation',
#                 'Retail',
#                 'Recreation',
#                 'Finance',
#                 'Other',
#                 'Health',
#                 'Education'
#                 )

sports <- c('baseball', 'basketball_hockey', 'football')
industries <- c('FoodAccommodation',
                'Retail',
                'Recreation',
                'Other',
                'Health',
                'Finance',
                'Education'
                )

industries_main <- c('FoodAccommodation', 
                     'Retail')

tables_folder_path <- '/home/quser/project_dir/stadiums/output/tables'

# lag days for estimation
lag_days <- seq(3)

###############################################################################


############# Functions to prepare the data, estimate models ##################

readData <- function(sport, industry) {
  # Simple data read
  
  data_file_path <- file.path('/home/quser/project_dir/stadiums/data',
                              'processed/analysis',
                              sport,
                              paste0('stadiums_', industry, '.dta'))
  data <- read_dta(data_file_path)
  return(data)
}

extractHeader <- function(latex_table, additional_header) {
  latex_table <- c(latex_table[1:7],
                   '\\resizebox{\\textwidth}{!}{',
                   latex_table[8],
                   '\\toprule',
                   latex_table[11],
                   gsub('cline', 'cmidrule', latex_table[12], fixed = TRUE),
                   additional_header,
                   '\\cmidrule{2-3} \\cmidrule{4-5} \\cmidrule{6-7}',
                   latex_table[14],
                   '\\midrule')
  return(latex_table)
}

extractSimpleHeader <- function(latex_table) {
  latex_table <- c(latex_table[1:7],
                   latex_table[8],
                   '\\toprule',
                   latex_table[11],
                   gsub('cline', 'cmidrule', latex_table[12], fixed = TRUE),
                   latex_table[14],
                   '\\midrule')
  return(latex_table)
}

extractFooter <- function(latex_table) {
  latex_table <- c('\\midrule', 
                   latex_table[20:24],
                   '\\bottomrule',
                   latex_table[27:28],
                   '}',
                   latex_table[length(latex_table)])
  return(latex_table)
}

extractSimpleFooter <- function(latex_table) {
  latex_table <- c('\\midrule', 
                   latex_table[20:21],
                   '\\bottomrule',
                   latex_table[24:26])
  return(latex_table)
}


extractCoefObs <- function(latex_table, industry, n_ampersand) {
  # How many ampesands add to f-stats:
  n_ampersand_f <- n_ampersand - length(f_stats) + 1
  
  latex_table <- c(' \\addlinespace',
                   paste0('\\textit{', industry, '} ', 
                          paste(rep('&', n_ampersand), collapse = ' '), 
                          ' \\\\'), 
                   latex_table[16],
                   paste(latex_table[17], '[0.2ex]'))
  latex_table <- gsub(' stadium\\\\_visits', '\\\\quad Stadium visits', latex_table)
  latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Obs.', latex_table)
  latex_table <- c(latex_table, ' \\addlinespace')
  return(latex_table)
}

extractSimpleCoefObs <- function(latex_table, industry, n_ampersand) {
  
  latex_table <- c(' \\addlinespace',
                   paste0('\\textit{', industry, '} ', 
                          paste(rep('&', n_ampersand), collapse = ' '), 
                          ' \\\\'), 
                   latex_table[16],
                   paste(latex_table[17], '[0.2ex]'))
  latex_table <- gsub(' stadium\\\\_visits', '\\\\quad Stadium visits', latex_table)
  latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Obs.', latex_table)
  latex_table <- c(latex_table, ' \\addlinespace')
  return(latex_table)
}

readData <- function(sport, industry) {
  # Simple data read
  
  data_file_path <- file.path('/home/quser/project_dir/stadiums/data',
                              'processed/analysis',
                              sport,
                              paste0('stadiums_', industry, '.dta'))
  data <- read_dta(data_file_path)
  return(data)
}


prepareData <- function(sport, industry, distance_range, lag_days) {
  # Function that imports the data for sport-industry, filters businesss 
  # for distance range.
  
  distance_min <- min(distance_range)
  distance_max <- max(distance_range)
  
  if (sport == 'basketball_hockey') {
    # Get basketball data
    basketball <- readData('basketball', industry)
    # Filter for distance range
    basketball <- basketball %>% 
      filter(between(distance, distance_min, distance_max))
    basketball <- basketball %>%
      group_by(stadium_id, Date, place_id) %>%
      arrange(desc(game)) %>%
      filter(row_number() == 1) %>%
      ungroup()
    basketball <- basketball %>% 
      group_by(stadium_id, Date) %>% 
      summarise(visits = sum(visits), 
                stadium_visits = first(stadium_visits), 
                game = first(game),
                month = first(month), 
                dow = first(dow))
    
    # Get hockey data
    hockey <- readData('hockey', industry)
    hockey <- hockey %>%
      group_by(stadium_id, Date, place_id) %>%
      arrange(desc(game)) %>%
      filter(row_number() == 1) %>%
      ungroup()
    # Filter for distance range
    hockey <- hockey %>% 
      filter(between(distance, distance_min, distance_max))
    # Compute total visits in distance ranges of stadiums
    hockey <- hockey %>% 
      group_by(stadium_id, Date) %>% 
      summarise(visits = sum(visits), 
                stadium_visits = first(stadium_visits), 
                game = first(game), 
                month = first(month), 
                dow = first(dow))
    data <- bind_rows(basketball, hockey) %>% 
      group_by(stadium_id, Date) %>% 
      arrange(desc(game)) %>%
      filter(row_number() == 1)
    
  } else {
    data <- readData(sport, industry)
    data <- data %>%
      group_by(stadium_id, Date, place_id) %>%
      arrange(desc(game)) %>%
      filter(row_number() == 1) %>%
      ungroup()
    # Filter for distance range
    data <- data %>%
      filter(between(distance, distance_min, distance_max))
    data <- data %>% 
      group_by(stadium_id, Date) %>% 
      summarise(visits = sum(visits), 
                stadium_visits = first(stadium_visits), 
                game = first(game), 
                month = first(month), 
                dow = first(dow))
  }
  
  # Save stadium info if Food and Accommodation is the industry
  if (i == 'FoodAccommodation') {
    stadiums_games_visits[[s]] <<- data %>% select(stadium_id,
                                                   Date, 
                                                   stadium_visits,
                                                   game, 
                                                   month, 
                                                   dow)
  } else {
    data <- data %>% 
      select(-game, -stadium_visits, -month, -dow) %>% 
      right_join(stadiums_games_visits[[s]], 
                 by = c('stadium_id', 'Date')) %>%
      replace_na(list(visits = 0))
  }
  
  # Generate lags
  # Prepare lag functions
  lag_names <- paste("game_lag", formatC(lag_days, width = nchar(max(lag_days)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., n = ", lag_days, ", order_by = Date)"), lag_names)
  data <- data %>% 
    group_by(stadium_id) %>% 
    mutate_at(vars(game), funs_(lag_functions))
  # Prepare lag functions
  lag_names <- paste("stadium_visits_lag", formatC(lag_days, width = nchar(max(lag_days)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., n = ", lag_days, ", order_by = Date)"), lag_names)
  data <- data %>% 
    group_by(stadium_id) %>% 
    mutate_at(vars(stadium_visits), funs_(lag_functions))
  
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


############################### Main code #####################################

# Create a list with data frames that contain stadium info: games and visits
stadiums_games_visits <- list()

#######################################
# Alternatie distance range to consider
#######################################
distance_range <- c(0, 3000)

models <- list()
# Simple pooled, FE and IV specifications
for (s in sports) {
  
  for (i in industries) {
    print(paste(s, i))
    data <- prepareData(sport = s, 
                        industry = i, 
                        distance_range = distance_range,
                        lag_days = lag_days)
    
    # Filter out Madison Square Garden:
    # data <- data %>% 
    #  filter(stadium_id != 'sg:1cc8dbe5600542ceba0de4a2f15eb7a6') 
    
    # Run SIMPLE pooled regression
    fit_simple <- felm(formula = visits ~ stadium_visits | stadium_id | 0 | stadium_id + date, 
                          data = data, 
                          cmethod = 'reghdfe')
    models[['simple']][[i]][[s]] <- fit_simple 
    
    # Run FE regression
    fit_fe <- felm(formula = visits ~ stadium_visits | fe + date | 0 | stadium_id + date, 
                   data = data, 
                   cmethod = 'reghdfe')
    models[['fe']][[i]][[s]] <- fit_fe 
    
    # Run IV regression
    fit_iv <- felm(formula = visits ~ 0 | fe + date | (stadium_visits ~ game) | stadium_id + date,
                   data = data, 
                   cmethod = 'reghdfe')
    models[['iv']][[i]][[s]] <- fit_iv
    
    # Save the first stage
    models[['fs']][[s]] <- fit_iv$stage1
    
    lagged_stadium_visits <- paste("stadium_visits_lag", 
                                   formatC(lag_days, width = nchar(max(lag_days)), flag = "0"), 
                                   sep = "_", collapse = ' + ')
    lagged_games <- paste("game_lag", 
                          formatC(lag_days, width = nchar(max(lag_days)), flag = "0"), 
                          sep = "_", collapse = ' + ')
    # Run lagged IV regression
    fit_lagged_iv <- felm(formula = as.formula(paste0('visits ~', 
                                                      lagged_stadium_visits,
                                                      '| fe + date | (stadium_visits ~ game) | stadium_id + date')),
                   data = data, 
                   cmethod = 'reghdfe')
    models[['lagged_iv']][[i]][[s]] <- fit_lagged_iv
  }
}

simple_industries <- list()
fe_iv_industries <- list()
for (i in industries) {
  sports_names <- str_to_title(gsub('_', ' \\\\& ', names(models$fe[[i]])))
  for (s in sports) {
    rownames(models$iv[[i]][[s]]$coefficients)[rownames(models$iv[[i]][[s]]$coefficients) ==
                                                 "`stadium_visits(fit)`"] <- 'stadium_visits'
    rownames(models$iv[[i]][[s]]$beta)[rownames(models$iv[[i]][[s]]$beta) ==
                                         "`stadium_visits(fit)`"] <- 'stadium_visits'
  }
  
  # Simple FE model parts
  simple_models <- lapply(sports, function(s) { models$simple[[i]][[s]]})
  n_ampersand <- length(simple_models)
  latex_table <- stargazer(simple_models, 
                           type = 'latex', 
                           label = 'simple_table',
                           omit.stat = c('rsq', 'adj.rsq', 'ser'),
                           title = paste('Esimates from specifications with stadium fixed effects only.',
                                         'Each coefficient in the table represents',
                                         'an estimate from a regression specification',
                                         'on a subset of data by stadium sport (columns)',
                                         'and business industry (panels).',
                                         'Standard errors robust to heteroskedasticity and',
                                         'stadium and date clustering are reported in parentheses.'),
                           dep.var.caption = 'Dependent varible: business visits within 3km',
                           dep.var.labels = c('Stadium sports'), 
                           column.labels = sports_names,
                           model.numbers = FALSE, 
                           add.lines = list(`Stadium FE` = c('Stadium FE', 
                                                             rep('\\multicolumn{1}{c}{\\checkmark}',
                                                                 times = n_ampersand))
                           ),
                           digits = 4, digits.extra = 0, align = TRUE,
                           star.cutoffs = c(0.05, 0.01, 0.001))
  simple_header <- extractSimpleHeader(latex_table)
  simple_footer <- extractSimpleFooter(latex_table)
  simple_industries[[i]] <- extractSimpleCoefObs(latex_table, i, n_ampersand)
  
  # FE and IV table parts
  f_stats <- sapply(sports, function(s) {condfstat(models$iv[[i]][[s]])})
  f_stats <- sapply(f_stats, function(f) {paste0('\\multicolumn{1}{c}{', format(round(f, 1), nsmall = 1), '}')})
  
  fs_coefs <- sapply(sports, function(s) {models$fs[[s]]$coefficients[1,1]})
  fs_coefs <- sapply(fs_coefs, function(f) {paste0('\\multicolumn{1}{c}{', format(round(f, 1), nsmall = 1), '}')})
  
  n_ampersand <- length(models$fe[[i]]) + length(models$iv[[i]]) 
  
  # Alternate FE and IV models
  alternating_models <- unlist(lapply(sports,
                                      function(s) {list(models$fe[[i]][[s]],
                                                        models$iv[[i]][[s]])}), 
                               recursive = FALSE)
  
  # Additional header
  additional_header <- paste(sapply(sports_names, function(s){paste0('\\multicolumn{2}{c}{', s, '}')}),
                             collapse = ' & ')
  additional_header <- paste0('\\\\[-1.8ex] &', additional_header, '\\\\')
  latex_table <- stargazer(alternating_models, 
                           type = 'latex', 
                           label = 'fe_iv_table',
                           omit.stat = c('rsq', 'adj.rsq', 'ser'),
                           title = paste('OLS FE and IV FE estimates.',
                                         'Each coefficient in the table represents',
                                         'an estimate from a regression specification',
                                         'on a subset of data by stadium sport (columns)',
                                         'and business industry (panels).',
                                         'Standard errors robust to heteroskedasticity and',
                                         'stadium and date clustering are reported in parentheses.'),
                           dep.var.caption = 'Dependent varible: business visits within 3km',
                           dep.var.labels = c('Stadium sports'), 
                           column.labels = rep(c('FE', 'IV'), times = 3),
                           model.numbers = FALSE, 
                           add.lines = list(`Stadium-Month-DoW FE` = c('Stadium$\\times$Month$\\times$DoW FE', 
                                                                       rep('\\multicolumn{1}{c}{\\checkmark}',
                                                                           times = n_ampersand)), 
                                            `Date FE` = c('Date FE', 
                                                          rep('\\multicolumn{1}{c}{\\checkmark}',
                                                              times = n_ampersand)),
                                            `F` = c('F-stat', c(rbind(rep('\\multicolumn{1}{c}{-}', 3), f_stats))),
                                            `FS` = c('1st stage coef.', c(rbind(rep('\\multicolumn{1}{c}{-}', 3), fs_coefs)))
                           ),
                           digits = 4, digits.extra = 0, align = TRUE,
                           star.cutoffs = c(0.05, 0.01, 0.001))
  fe_iv_header <- extractHeader(latex_table, additional_header)
  fe_iv_footer <- extractFooter(latex_table)
  fe_iv_industries[[i]] <- extractCoefObs(latex_table, i, n_ampersand)
}

# Simple pooled (stadium FE only) regression table
latex_table_simple <- c(simple_header, 
                           unlist(simple_industries, use.names = FALSE), 
                           '\\addlinespace', simple_footer)
latex_table_simple <- gsub('FoodAccommodation', 'Food \\& Accommodation', 
                           latex_table_simple, fixed = TRUE)
latex_table_simple <- gsub('Retail', 'Retail Trade', 
                           latex_table_simple, fixed = TRUE)
latex_table_simple <- gsub('Other', 'Other Services', 
                           latex_table_simple, fixed = TRUE)
# Output the simple pooled regression table
outfile_path <- file.path(tables_folder_path, 'regressions_baseline3_simple.tex')
cat(paste0(latex_table_simple, collapse = '\n'), file = outfile_path)

# FE and IV table
latex_table_fe_iv <- c(fe_iv_header, 
                    unlist(fe_iv_industries, use.names = FALSE), 
                    '\\addlinespace', fe_iv_footer)

# Change column alignment
latex_table_fe_iv[9] <- gsub('D{.}{.}{-4}', 'D{.}{.}{3.4}', latex_table_fe_iv[9], fixed = TRUE)
latex_table_fe_iv <- gsub('FoodAccommodation', 'Food \\& Accommodation', 
                          latex_table_fe_iv, fixed = TRUE)
latex_table_fe_iv <- gsub('Retail', 'Retail Trade', 
                          latex_table_fe_iv, fixed = TRUE)
latex_table_fe_iv <- gsub('Other', 'Other Services', 
                          latex_table_fe_iv, fixed = TRUE)
# Output the FE and IV table
outfile_path <- file.path(tables_folder_path, 'regressions_baseline3_fe_iv.tex')
cat(paste0(latex_table_fe_iv, collapse = '\n'), file = outfile_path)
# Bare table without float environment
latex_table_fe_iv_bare <- c(latex_table_fe_iv[8:62], latex_table_fe_iv[6:7])
outfile_path <- file.path(tables_folder_path, 'regressions_baseline3_fe_iv_bare.tex')
cat(paste0(latex_table_fe_iv_bare, collapse = '\n'), file = outfile_path)


# First stage
n_ampersand_fs <- length(models$fs)
sports_names <- str_to_title(gsub('_', ' \\\\& ', names(models$fs)))
fs_table <- stargazer(models[['fs']], 
                      type = 'latex', 
                      label = 'fs_table',
                      omit.stat = c('ser'),
                      title = paste('IV first stage estimates.',
                                    'Standard errors robust to heteroskedasticity and',
                                    'stadium and date clustering are reported in parentheses.'),
                      dep.var.caption = 'Dependent varible: stadium visits',
                      column.labels = sports_names,
                      model.numbers = FALSE, 
                      add.lines = list(`Stadium-Month-DoW FE` = c('Stadium$\\times$Month$\\times$DoW FE', 
                                                                  rep('\\multicolumn{1}{c}{\\checkmark}',
                                                                      times = n_ampersand_fs)), 
                                       `Date FE` = c('Date FE', 
                                                     rep('\\multicolumn{1}{c}{\\checkmark}',
                                                         times = n_ampersand_fs))),
                      digits = 2, digits.extra = 0, align = TRUE,
                      star.cutoffs = c(0.05, 0.01, 0.001))
fs_table <- c(fs_table[1:7],
              '\\resizebox{0.85\\textwidth}{!}{',
              fs_table[8],
              '\\toprule',
              fs_table[11],                  
              gsub('cline', 'cmidrule', fs_table[12], fixed = TRUE),
              fs_table[14], 
              '\\midrule',
              paste0(gsub('game', 'Game indicator', fs_table[16], fixed = TRUE), '[0.2ex]'),
              fs_table[17],
              '\\midrule',
              fs_table[20:24], 
              '\\bottomrule',
              fs_table[27:28],
              '}',
              fs_table[29]
              )
# Output the first stage table
outfile_path <- file.path(tables_folder_path, 'regressions_baseline3_fs.tex')
cat(paste0(fs_table, collapse = '\n'), file = outfile_path)
# Bare table without float environment
fs_table_bare <- c(fs_table[8:26], fs_table[6:7])
outfile_path <- file.path(tables_folder_path, 'regressions_baseline3_fs_bare.tex')
cat(paste0(fs_table_bare, collapse = '\n'), file = outfile_path)

# LAGS:
i <- 'FoodAccommodation'
lag_models <- lapply(sports, 
                     function(s) {
                       tidy(models$lagged_iv[[i]][[s]], 
                            conf.int = TRUE, 
                            robust = TRUE) %>% mutate(model = s)
                       }
                     )
lag_models <- bind_rows(lag_models)
lag_models <- lag_models %>% 
  mutate(term = str_sub(term, -1, -1)) %>%
  mutate(term = ifelse(term == '`', '0', term)) %>%
  mutate(term = as.numeric(term)) %>%
  arrange(model, -term)

lag_models <- lag_models %>% 
  mutate(model = ifelse(model == 'basketball_hockey', 'basketball & hockey', model)) %>%
  mutate(model = str_to_title(model)) 
#%>% mutate(industry = factor(industry, levels = industries))

pic1 <- dwplot(lag_models, 
               vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  xlim(-0.125, 0.6) +
  coord_flip() +
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) +
  ylab('Stadium visits lags') +
  xlab('Regression coefficients') +
  ggtitle('Food & Accommodation')

i <- 'Retail'
lag_models <- lapply(sports, 
                     function(s) {
                       tidy(models$lagged_iv[[i]][[s]], 
                            conf.int = TRUE, 
                            robust = TRUE) %>% mutate(model = s)
                     }
                     )
lag_models <- bind_rows(lag_models)
lag_models <- lag_models %>% 
  mutate(term = str_sub(term, -1, -1)) %>%
  mutate(term = ifelse(term == '`', '0', term)) %>%
  mutate(term = as.numeric(term)) %>%
  arrange(model, -term)

lag_models <- lag_models %>% 
  mutate(model = ifelse(model == 'basketball_hockey', 'basketball & hockey', model)) %>%
  mutate(model = str_to_title(model)) 
#%>% mutate(industry = factor(industry, levels = industries))

pic2 <- dwplot(lag_models, 
               vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  xlim(-0.125, 0.6) +
  coord_flip() +
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) +
  ylab('Stadium visits lags') +
  xlab('Regression coefficients') +
  ggtitle('Retail Trade') 

pic_lags <- ggarrange(pic1,
                      pic2 + theme(axis.title.y = element_blank()),
                      ncol = 2,
                      nrow = 1, 
                      common.legend = TRUE, legend = 'right',
                      align = 'hv')

ggsave(filename = file.path(plots_folder_path, 
                            'regression_lags3.pdf'), 
       device = cairo_pdf, plot = pic_lags, width = 10, height = 4)
embed_fonts(file = file.path(plots_folder_path,
                             'regression_lags3.pdf'))

sports_names <- str_to_title(gsub('_', '\\\\&', names(models$lagged_iv[['FoodAccommodation']])))
sports_names <- gsub('Basketball', 'Bas.', sports_names)
sports_names <- gsub('Hockey', 'Hoc.', sports_names)
n_ampersand <- length(models$lagged_iv[['FoodAccommodation']]) + length(models$lagged_iv[['Retail']])
f_stats <- c(sapply(sports, function(s) {condfstat(models$iv[['FoodAccommodation']][[s]])}),
             sapply(sports, function(s) {condfstat(models$iv[['Retail']][[s]])}))
f_stats <- sapply(f_stats, function(f) {paste0('\\multicolumn{1}{c}{', format(round(f, 1), nsmall = 1), '}')})
latex_table <-
  stargazer(models$lagged_iv[['FoodAccommodation']], models$lagged_iv[['Retail']],
          order = c(4, seq(1, 3)),
          covariate.labels = as.character(seq(0, 3)),
          label = 'fe_iv_table',
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          title = paste('IV FE estimates with lagged stadium visits as controls.',
                        'Standard errors robust to heteroskedasticity and',
                        'stadium and date clustering are reported in parentheses.'),
          dep.var.caption = 'Dependent varible: business visits within 3km',
          dep.var.labels = c('Stadium sports'), 
          column.labels = rep(sports_names, times = 2),
          model.numbers = FALSE, 
          add.lines = list(`Stadium-Month-DoW FE` = c('Stadium$\\times$Month$\\times$DoW FE', 
                                                      rep('\\multicolumn{1}{c}{\\checkmark}',
                                                          times = n_ampersand)), 
                           `Date FE` = c('Date FE', 
                                         rep('\\multicolumn{1}{c}{\\checkmark}',
                                             times = n_ampersand)),
                           `F` = c('F-stat', f_stats)),
          digits = 4, digits.extra = 0, align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

latex_table <- c(latex_table[1:7],
                 '\\resizebox{\\textwidth}{!}{',
                 latex_table[8],
                 '\\toprule',
                 latex_table[11],
                 gsub('cline', 'cmidrule', latex_table[12], fixed = TRUE),
                 '& \\multicolumn{3}{c}{Food \\& Accommodation} & \\multicolumn{3}{c}{Retail}\\\\',
                 '\\cmidrule{2-4} \\cmidrule{5-7}',
                 latex_table[14],
                 '\\midrule',
                 latex_table[15:length(latex_table)])

# Change column alignment
latex_table[9] <- gsub('D{.}{.}{-4}', 'D{.}{.}{3.4}', latex_table[9], fixed = TRUE)

# Shift lags
latex_table[17] <- paste0(c("\\emph{Stadium visits lags}", rep(' & ', n_ampersand), '\\\\'), collapse = '')
latex_table[18] <- paste0('\\qquad ', latex_table[18]) 
latex_table[21] <- paste0('\\qquad ', latex_table[21]) 
latex_table[24] <- paste0('\\qquad ', latex_table[24]) 
latex_table[27] <- paste0('\\qquad ', latex_table[27]) 

latex_table_lags <- c(latex_table[1:29],
                 '\\midrule',
                 latex_table[31:34],
                 '\\bottomrule', 
                 latex_table[37:38], 
                 '}', 
                 latex_table[39])

latex_table_lags <- gsub('FoodAccommodation', 'Food \\& Accommodation', 
                         latex_table_lags, fixed = TRUE)
latex_table_lags <- gsub('Retail', 'Retail Trade', 
                         latex_table_lags, fixed = TRUE)
latex_table_lags <- gsub('Other', 'Other Services', 
                         latex_table_lags, fixed = TRUE)

outfile_path <- file.path(tables_folder_path, 'regressions_lags3.tex')
cat(paste0(latex_table_lags, collapse = '\n'), file = outfile_path)

###############################################################################

############################### ARCHIVE #######################################
# 
# fe_industries <- list()
# iv_industries <- list()
# latex_table <- stargazer(models$fe[[i]], 
#                          type = 'latex', 
#                          title = paste('OLS FE estimates.',
#                                        'All specifications include',
#                                        'stadium-month-dayofweek fixed effects.',
#                                        'Standard errors robust to heteroskedasticity and',
#                                        'stadium clustering are reported in parentheses.'),
#                          dep.var.caption = 'Dependent varible: business visits within 5km',
#                          dep.var.labels = 'Stadium sports', 
#                          column.labels = sports_names, 
#                          model.numbers = FALSE, 
#                          digits = 4, digits.extra = 0, align = TRUE,
#                          star.cutoffs = c(0.05, 0.01, 0.001))
# fe_header <- extractHeader(latex_table)
# fe_footer <- extractFooter(latex_table)
# fe_industries[[i]] <- extractCoefObs(latex_table, i)
# 
# latex_table <- stargazer(models$iv[[i]], 
#                          type = 'latex', 
#                          title = paste('IV FE estimates.',
#                                        'All specifications include',
#                                        'stadium-month-dayofweek fixed effects.',
#                                        'Standard errors robust to heteroskedasticity and',
#                                        'stadium clustering are reported in parentheses.'),
#                          dep.var.caption = 'Dependent varible: business visits within 5km',
#                          dep.var.labels = 'Stadium sports', 
#                          column.labels = sports_names, 
#                          model.numbers = FALSE, 
#                          digits = 4, digits.extra = 0, align = TRUE,
#                          star.cutoffs = c(0.05, 0.01, 0.001))
# iv_header <- extractHeader(latex_table)
# iv_footer <- extractFooter(latex_table)
# iv_industries[[i]] <- extractCoefObs(latex_table, i)
# 
# latex_table_fe <- c(fe_header, 
#                     unlist(fe_industries, use.names = FALSE), 
#                     '\\addlinespace', 
#                     fe_footer)
# latex_table_iv <- c(iv_header, 
#                     unlist(iv_industries, use.names = FALSE), 
#                     '\\addlinespace', iv_footer)
# # Output the FE table
# outfile_path <- file.path(tables_folder_path, 'regressions_baseline5_fe.tex')
# cat(paste0(latex_table_fe, collapse = '\n'), file = outfile_path)
# # Output the IV table:
# outfile_path <- file.path(tables_folder_path, 'regressions_baseline5_iv.tex')
# cat(paste0(latex_table_iv, collapse = '\n'), file = outfile_path)
#
###############################################################################