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

ranges <- 5

distances <- paste(seq(0, ranges - 1), seq(1, ranges), sep = '-')
distances <- paste0(distances, ' km')

outfolder <- file.path('/home/quser/project_dir/',
                       'stadiums/output/tables') 

###############################################################################

######################### Data preparation function ###########################

prepareDataShared <- function(industry) {
  
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
                              seq(0, ranges), 
                              right = FALSE,
                              labels = 1 : ranges))
  
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
                              seq(0, ranges), 
                              right = FALSE,
                              labels = 1 : ranges))
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
  
  # Prepare fixed effects
  data$month <- factor(data$month)
  data$dow <- factor(data$dow)
  data$stadium_id <- factor(data$stadium_id)
  data$fe <- interaction(data$month, data$dow, data$stadium_id)
  data$date <- factor(data$Date)

  return(data)
}

###############################################################################


########################### Present the results ###############################

s <- 'basketball_hockey'

# Dow FE
headers <- c()
main <- c()
footers <- c()
for (i in industries) {
  
  data <- prepareDataShared(i)
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
  
  data <- prepareDataShared(i)
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

# Output first stage:
fs <- fits$models[[1]][[2]]$stage1

# Output first stage table:
outfile_path <- file.path(outfolder, 'shared_dow_first_stages.tex')
stargazer(fs,
          title = 'IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek fixed effects. Standard errors robust to heteroskedasticity and stadium clustering are reported in parentheses.', 
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

# Date FE
headers <- c()
main <- c()
footers <- c()
for (i in industries) {
  
  data <- prepareDataShared(i)
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
  
  data <- prepareDataShared(i)
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

# Output first stage:
fs <- fits$models[[1]][[2]]$stage1

# Output first stage table:
outfile_path <- file.path(outfolder, 'shared_date_first_stages.tex')
stargazer(fs,
          title = 'IV first stage estimate for shared stadiums. Specification includes stadium-month-dayofweek and date fixed effects. Standard errors robust to heteroskedasticity and stadium and date clustering are reported in parentheses.', 
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