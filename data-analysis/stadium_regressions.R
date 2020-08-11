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

###############################################################################


################################ Constants ####################################

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

outfolder <- file.path('/home/quser/project_dir/',
                       'stadiums/output/tables') 

###############################################################################


############# Function to prepare the data, estimate models ###################

prepareData <- function(sport, industry) {
  # Function that imports and prepares the data
  
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
                              seq(0, ranges), 
                              right = FALSE,
                              labels = 1 : ranges))
  
  # Compute total visits in distance ranges of stadiums
  data <- data %>% 
    group_by(stadium_id, Date, distance_bin) %>% 
    summarise(visits = sum(visits), 
              stadium_visits = first(stadium_visits), 
              game = first(game), 
              #Attendance = first(Attendance), 
              month = first(month), 
              dow = first(dow))
  
  # Prepare fixed effects
  data$month <- factor(data$month)
  data$dow <- factor(data$dow)
  data$stadium_id <- factor(data$stadium_id)
  data$fe <- interaction(data$month, data$dow, data$stadium_id)
  data$date <- factor(data$Date)
  
  return(data)
}

runEstimationDowFE <- function(sport, industry) {
  # Function that estimates the dow fixed effects model
  
  value <- lapply(1 : ranges, function(r) {
    list(distances[r],
         felm(formula = visits ~ stadium_visits | fe | 0 | stadium_id,
                data = subset(data, distance_bin == r))
    )})
  value <- list(sport, industry, 'dow_fe', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDowIV <- function(sport, industry) {
  # Function that estimates the iv dow fixed effects model
  
  value <- lapply(1 : ranges, function(r) {
    list(distances[r],
         felm(formula = visits ~ 0 | fe | (stadium_visits ~ game) | stadium_id,
              data = subset(data, distance_bin == r))
    )})
  value <- list(sport, industry, 'dow_iv', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDateFE <- function(sport, industry) {
  # Function that estimates the date fixed effects model
  
  value <- lapply(1 : ranges, function(r) {
    list(distances[r],
         felm(formula = visits ~ stadium_visits | fe + date | 0 | stadium_id + date,
              data = subset(data, distance_bin == r))
    )})
  value <- list(sport, industry, 'date_fe', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

runEstimationDateIV <- function(sport, industry) {
  # Function that estimates the iv date fixed effects model
  
  value <- lapply(1 : ranges, function(r) {
    list(distances[r],
         felm(formula = visits ~ 0 | fe + date | (stadium_visits ~ game) | stadium_id + date,
              data = subset(data, distance_bin == r))
    )})
  value <- list(sport, industry, 'date_iv', value)
  names(value) <- c('sport', 'industry', 'type', 'models')
  return(value)
}

###############################################################################


########################### Present the results ###############################

# Dow fixed effects
first_stages_dow <- list()
for (s in sports) {
  
  # Produce the FE tables for baseball
  headers <- c()
  main <- c()
  footers <- c()
  
  for (i in industries) {
    
    data <- prepareData(s, i)
    fits <- runEstimationDowFE(s, i)
    latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
                             title = paste0(stri_trans_totitle(s), 
                                            ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                             label = paste0(s,'_dow_fe'),
                             dep.var.caption = 'Dependent varible: establishment visits',
                             dep.var.labels = 'Distance ranges',
                             column.labels = distances,
                             model.numbers = FALSE,
                             digits = 4,
                             align = TRUE,
                             type = 'latex', column.sep.width = '-20pt'))
    headers <- latex_table[1:15]
    main <- c(main, 
              ' \\addlinespace',
              paste0('\\textit{', i, '} & & & & & \\\\'), 
              latex_table[c(16, 17)],
              ' \\addlinespace',
              latex_table[20],
              ' \\addlinespace')
    footers <- latex_table[24:length(latex_table)]
  }
  
  latex_table <- c(headers, main, '\\addlinespace', footers)
  latex_table <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table)
  latex_table <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
  latex_table <- gsub('Observations', 'Obs.', latex_table)
  
  # Output table:
  outfile_path <- file.path(outfolder, paste0(s, '_dow_fe.tex'))
  cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
  
  # Produce the IV tables
  headers <- c()
  main <- c()
  footers <- c()
  
  for (i in industries) {
    
    data <- prepareData(s, i)
    fits <- runEstimationDowIV(s, i)
    latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
                             title = paste0(stri_trans_totitle(s), 
                                            ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.'), 
                             label = paste0(s, '_dow_iv'),
                             dep.var.caption = 'Dependent varible: establishment visits',
                             dep.var.labels = 'Distance ranges',
                             column.labels = distances,
                             model.numbers = FALSE,
                             digits = 4,
                             align = TRUE,
                             type = 'latex', column.sep.width = '-20pt'))
    headers <- latex_table[1:15]
    main <- c(main, 
              ' \\addlinespace',
              paste0('\\textit{', i, '} & & & & & \\\\'), 
              latex_table[c(16, 17)],
              ' \\addlinespace',
              latex_table[20],
              ' \\addlinespace')
    footers <- latex_table[24:length(latex_table)]
  }
  
  latex_table <- c(headers, main, '\\addlinespace', footers)
  latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad `stadium\\\\_visits(fit)`', latex_table)
  latex_table <- gsub('`stadium\\\\_visits\\(fit\\)`', 'Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
  latex_table <- gsub('Observations', 'Obs.', latex_table)

  # Output table:
  outfile_path <- file.path(outfolder, paste0(s, '_dow_iv.tex'))
  cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
  
  next_fs <- fits$models[[1]][[2]]$stage1
  first_stages_dow <- append(first_stages_dow, list(next_fs))
}

# Output first stage table:
outfile_path <- file.path(outfolder, 'first_stages_dow.tex')
stargazer(first_stages_dow,
          title = 'IV first stage estimates.  All specifications include stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
          label = paste0(s, '_dow_fs'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
          model.numbers = FALSE,
          digits = 2,
          align = TRUE,
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)


# Date fixed effects
first_stages_date <- list()
for (s in sports) {
  
  # Produce the FE tables for baseball
  headers <- c()
  main <- c()
  footers <- c()
  
  for (i in industries) {
    
    data <- prepareData(s, i)
    fits <- runEstimationDateFE(s, i)
    latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
                                            title = paste0(stri_trans_totitle(s), 
                                                           ' stadiums: OLS FE estimates. All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium and date clustering are reported in parentheses.'), 
                                            label = paste0(s,'_date_fe'),
                                            dep.var.caption = 'Dependent varible: establishment visits',
                                            dep.var.labels = 'Distance ranges',
                                            column.labels = distances,
                                            model.numbers = FALSE,
                                            digits = 4,
                                            align = TRUE,
                                            type = 'latex', column.sep.width = '-20pt'))
    headers <- latex_table[1:15]
    main <- c(main, 
              ' \\addlinespace',
              paste0('\\textit{', i, '} & & & & & \\\\'), 
              latex_table[c(16, 17)],
              ' \\addlinespace',
              latex_table[20],
              ' \\addlinespace')
    footers <- latex_table[24:length(latex_table)]
  }
  
  latex_table <- c(headers, main, '\\addlinespace', footers)
  latex_table <- gsub(' stadium\\\\_visits', '\\\\quad stadium\\\\_visits', latex_table)
  latex_table <- gsub('stadium\\\\_visits', 'Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
  latex_table <- gsub('Observations', 'Obs.', latex_table)
  
  # Output table:
  outfile_path <- file.path(outfolder, paste0(s, '_date_fe.tex'))
  cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
  
  # Produce the IV tables
  headers <- c()
  main <- c()
  footers <- c()
  
  for (i in industries) {
    
    data <- prepareData(s, i)
    fits <- runEstimationDateIV(s, i)
    latex_table <- capture.output(stargazer(lapply(fits$models, function(x) {x[[2]]}), 
                                            title = paste0(stri_trans_totitle(s), 
                                                           ' stadiums: IV estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium and date clustering are reported in parentheses.'), 
                                            label = paste0(s, '_date_iv'),
                                            dep.var.caption = 'Dependent varible: establishment visits',
                                            dep.var.labels = 'Distance ranges',
                                            column.labels = distances,
                                            model.numbers = FALSE,
                                            digits = 4,
                                            align = TRUE,
                                            type = 'latex', column.sep.width = '-20pt'))
    headers <- latex_table[1:15]
    main <- c(main, 
              ' \\addlinespace',
              paste0('\\textit{', i, '} & & & & & \\\\'), 
              latex_table[c(16, 17)],
              ' \\addlinespace',
              latex_table[20],
              ' \\addlinespace')
    footers <- latex_table[24:length(latex_table)]
  }
  
  latex_table <- c(headers, main, '\\addlinespace', footers)
  latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', '\\\\quad `stadium\\\\_visits(fit)`', latex_table)
  latex_table <- gsub('`stadium\\\\_visits\\(fit\\)`', 'Stadium visits', latex_table)
  latex_table <- gsub('Observations', '\\\\quad Observations', latex_table)
  latex_table <- gsub('Observations', 'Obs.', latex_table)
  
  # Output table:
  outfile_path <- file.path(outfolder, paste0(s, '_date_iv.tex'))
  cat(paste0(latex_table, collapse = '\n'), file = outfile_path)
  
  next_fs <- fits$models[[1]][[2]]$stage1
  first_stages_date <- append(first_stages_date, list(next_fs))
}

# Output first stage table:
outfile_path <- file.path(outfolder, 'first_stages_date.tex')
stargazer(first_stages_dow,
          title = 'IV first stage estimates.  All specifications include stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium and date clustering are reported in parentheses.', 
          label = paste0(s, '_date_fs'),
          dep.var.caption = 'Dependent varible: stadium visits',
          dep.var.labels = 'Sports',
          column.labels = sports,
          model.numbers = FALSE,
          digits = 2,
          align = TRUE,
          type = 'latex', 
          column.sep.width = '-20pt',
          out = outfile_path)


###############################################################################