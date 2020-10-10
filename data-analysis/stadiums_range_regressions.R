###############################################################################
#
# FILE: stadium_regressions.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Aug 27 2020
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
library(stringr)
library(dplyr)
library(ggpubr)

###############################################################################


################################ Constants ####################################

sports <- c('football', 'baseball', 'basketball_hockey')

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

runEstimationDowFE <- function(sport, industry) {
  # Function that estimates the dow fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
         felm(formula = visits ~ stadium_visits | fe | 0 | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    })
  names(value) <- distances
  return(value)
}

runEstimationDowIV <- function(sport, industry) {
  # Function that estimates the iv dow fixed effects model
  
  value <- lapply(seq_along(ranges), function(r) {
         felm(formula = visits ~ 0 | fe | (stadium_visits ~ game) | stadium_id,
              data = subset(data, distance_bin == ranges[r]), cmethod = 'reghdfe')
    })
  names(value) <- distances
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


extractCoefs <- function(latex_table, industry) {
  
  latex_table <- c(latex_table[16],
                   paste(latex_table[17], '[0.2ex]')
                   )
  latex_table <- gsub(' \`stadium\\\\_visits\\(fit\\)\`', paste0('\\\\quad ', industry), latex_table)
  return(latex_table)
}


extractObsF <- function(latex_table) {
  
  latex_table <- c(latex_table[22:23])
  latex_table <- gsub('Observations', '\\\\quad Obs.', latex_table)
  latex_table <- gsub('F-stat', '\\\\quad F-stat', latex_table)
  return(latex_table)
}

extractHeader <- function(latex_table) {
  
  latex_table <- c(latex_table[1:7], 
                   '\\resizebox{\\textwidth}{!}{',
                   gsub('D{.}{.}{-4}', 'D{.}{.}{3.4}', latex_table[8], fixed = TRUE),
                   '\\toprule',
                   latex_table[11],
                   gsub('cline', 'cmidrule', latex_table[12], fixed = TRUE),
                   paste0("& ", str_split(latex_table[13], '&')[[1]][2]),
                   latex_table[14],
                   '\\midrule')
  return(latex_table)
}

extractFooter <- function(latex_table) {
  
  latex_table <- c('\\bottomrule',
                   latex_table[26:27],
                   '}',
                   latex_table[28])
  return(latex_table)
}

panelTitle <- function(sport, n_ampersand) {
  sport <- gsub('_', ' \\\\& ', sport)
  sport <- str_to_title(sport)
  return(c(' \\addlinespace',
           paste0('\\textit{', sport, '} ', 
                  paste(rep('&', n_ampersand), collapse = ' '), 
                  ' \\\\')
           )
         )
}

prepareRangesData <- function(sport, industry, distance_bins, distance_bins_labels) {
  # Function that imports the data for sport-industry, generates distance bins
  
  if (sport == 'basketball_hockey') {
    # Get basketball data
    basketball <- readData('basketball', industry)
    
    # Fix distance, compute distance bins
    basketball$distance <- basketball$distance / 1000
    basketball <- basketball %>% 
      mutate(distance_bin = cut(distance, 
                                distance_bins,
                                right = FALSE,
                                distance_bins_labels))
    

    basketball <- basketball %>% 
      group_by(stadium_id, Date, distance_bin) %>% 
      summarise(visits = sum(visits), 
                stadium_visits = first(stadium_visits), 
                game = first(game),
                month = first(month), 
                dow = first(dow))
    
    # Get hockey data
    hockey <- readData('hockey', industry)
    
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
                month = first(month), 
                dow = first(dow))
    data <- bind_rows(basketball, hockey) %>% 
      group_by(stadium_id, Date, distance_bin) %>% 
      arrange(desc(game)) %>%
      filter(row_number() == 1)
    
  } else {
    data <- readData(sport, industry)
    
    # Convert distance (meters to kilometers), prepare distance bins
    data$distance <- data$distance / 1000
    data <- data %>% 
      mutate(distance_bin = cut(distance, 
                                distance_bins,
                                right = FALSE,
                                distance_bins_labels))
    
    data <- data %>% 
      group_by(stadium_id, Date, distance_bin) %>% 
      summarise(visits = sum(visits), 
                stadium_visits = first(stadium_visits), 
                game = first(game), 
                month = first(month), 
                dow = first(dow))
  }
  
  # Save stadium info if Food and Accommodation is the industry,
  # making sure all distance bins have the same number of observations
  if (i == 'FoodAccommodation') {
    temp <- data %>% 
      group_by(stadium_id, Date) %>%
      summarize(stadium_visits = first(stadium_visits),
                game = first(game),
                month = first(month),
                dow = first(dow))
    temp <- lapply(distance_bins_labels, function(x){ temp %>% mutate(distance_bin = x)})
    temp <- bind_rows(temp) %>% mutate(distance_bin = as.factor(distance_bin))
    stadiums_games_visits[[s]] <<- bind_rows(temp)
  }
  data <- data %>% 
    select(-game, -stadium_visits, -month, -dow) %>% 
    right_join(stadiums_games_visits[[s]], 
               by = c('stadium_id', 'Date', 'distance_bin')) %>%
    replace_na(list(visits = 0))

  
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

ranges <- seq(0.5, 3, 0.5)
distances <- paste(c(0, head(ranges, -1)), ranges, sep = '-')
distances <- paste0(distances, ' km')


# Create a list with data frames that contain stadium info: games and visits
stadiums_games_visits <- list()

models <- list()

for (s in sports) {
  
  for (i in c('FoodAccommodation', 'Retail')) {
    print(paste(s, i))
    # Import data
    data <- prepareRangesData(sport = s, 
                              industry = i, 
                              distance_bins = c(0, ranges), 
                              distance_bins_labels = ranges)
    # Run IV regression
    fits_iv <- runEstimationDateIV(s, i)
    models[['iv']][[i]][[s]] <- fits_iv
    
  }
}


# Collect data into a tidy data frame
iv_data <- list()
for (s in sports) {
  for (i in c('FoodAccommodation', 'Retail')) {
    for (r in seq_along(ranges)) { 
      coefs_iv <- tidy(models[['iv']][[i]][[s]]$models[[r]][[2]], conf.int = T, robust = TRUE)
      coefs_iv$sport <- s
      coefs_iv$industry <- i
      coefs_iv$range <- ranges[r]
      iv_data <- append(iv_data, list(coefs_iv))
    }
  }
}
iv_data <- bind_rows(iv_data)

iv_data <- iv_data %>% 
  mutate(sport = ifelse(sport == 'basketball_hockey', 'basketball & hockey', sport)) %>%
  mutate(sport = str_to_title(sport)) %>% 
  mutate(term = range, model = sport) %>% 
  arrange(-term) 

iv_data <- iv_data %>% 
  arrange(model)

pic1 <- dwplot(iv_data %>% filter(industry == 'FoodAccommodation'), 
       vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  coord_flip() +
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) +
  xlab('IV FE coefficient estimates') +
  ylab('Distance ranges') +
  ggtitle('Food & Accommodation') +
  scale_y_discrete(breaks = ranges, labels = distances)

pic2 <- dwplot(iv_data %>% filter(industry == 'Retail'), 
               vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  coord_flip() +
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) +
  xlab('IV FE coefficient estimates') +
  ylab('Distance ranges') +
  ggtitle('Retail') +
  scale_y_discrete(breaks = ranges, labels = distances)

pic_ranges <- ggarrange(pic1 + theme(axis.title.x = element_blank()),
                        pic2,
                        ncol = 1,
                        nrow = 2, 
                        common.legend = TRUE, legend = 'right',
                        align = 'hv')

ggsave(filename = file.path(plots_folder_path, 
                            'regression_ranges3.pdf'), 
       device = cairo_pdf, plot = pic_ranges, width = 10, height = 8)
embed_fonts(file = file.path(plots_folder_path,
                             'regression_ranges3.pdf'))

coefs_table <- list()
for (s in sports) {
  for (i in c('FoodAccommodation', 'Retail')) {
    n_ampersand <- length(models[['iv']][[i]][[s]]$models)
    f_stats <- sapply(lapply(models[['iv']][[i]][[s]]$models, function(x) {x[[2]]}), function(r) {condfstat(r)})
    f_stats <- sapply(f_stats, function(f) {paste0('\\multicolumn{1}{c}{', format(round(f, 1), nsmall = 1), '}')})
    latex_table <- stargazer(lapply(models[['iv']][[i]][[s]]$models, function(x) {x[[2]]}), type = 'latex', 
                             label = 'ranges_iv_table',
                             omit.stat = c('rsq', 'adj.rsq', 'ser'),
                             title = paste('IV FE estimates.',
                                           'Each coefficient in the table represents',
                                           'an estimate from a regression specification',
                                           'on a subset of data by distance range (columns), stadium sport (panels)',
                                           'and business industry (rows).',
                                           'All specifications include',
                                           'stadium-month-dayofweek and date fixed effects.',
                                           'Standard errors robust to heteroskedasticity and',
                                           'stadium clustering are reported in parentheses.'),
                             dep.var.caption = 'Dependent varible: establishment visits',
                             dep.var.labels = c('Distance range'), 
                             column.labels = distances,
                             model.numbers = FALSE, 
                             add.lines = list(`Stadium-Month-DoW FE` = c('Stadium-Month-DoW FE', 
                                                                         rep('\\multicolumn{1}{c}{\\checkmark}',
                                                                             times = n_ampersand)), 
                                              `Date FE` = c('Date FE', 
                                                            rep('\\multicolumn{1}{c}{\\checkmark}',
                                                                times = n_ampersand)),
                                              `F` = c('F-stat', f_stats)),
                             digits = 4, digits.extra = 0, align = TRUE,
                             star.cutoffs = c(0.05, 0.01, 0.001))
    if (i == 'FoodAccommodation') {
      coefs_table[[s]] <- extractCoefs(latex_table, i)
    } else {
      coefs_table[[s]] <- c(coefs_table[[s]], extractCoefs(latex_table, i), '\\addlinespace')
      coefs_table[[s]] <- c(coefs_table[[s]], extractObsF(latex_table))
    }
  }
}

full_table <- c()
for (s in seq_along(coefs_table)) {
  full_table <- c(full_table, c(panelTitle(names(coefs_table)[s], n_ampersand), 
                                coefs_table[[s]], 
                                '\\midrule')
  )
}

full_table <- c(extractHeader(latex_table),
                full_table[1:29],
                extractFooter(latex_table))

# Output the ranges
outfile_path <- file.path(tables_folder_path, 'regressions_ranges3.tex')
cat(paste0(full_table, collapse = '\n'), file = outfile_path)

###############################################################################

################################### Archive ###################################
# 
# iv_data_test <- iv_data %>% select(-term) %>% rename(term = industry, model = sport)
# pic1 <- dwplot(iv_data_test %>% filter(range == 1), 
#                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + 
#   theme_bw(base_family = 'Times') + 
#   my_theme_large + 
#   scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) + 
#   theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)) +
#   theme(plot.margin = margin(1, 0, 0, 0, "cm"))
# 
# pic2 <- dwplot(iv_data_test %>% filter(range == 2), 
#                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + 
#   theme_bw(base_family = 'Times') + 
#   my_theme_large + 
#   scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) + 
#   theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)) +
#   theme(plot.margin = margin(1, 0, 0, 0, "cm"))
# 
# pic3 <- dwplot(iv_data_test %>% filter(range == 3), 
#                vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + 
#   theme_bw(base_family = 'Times') + 
#   my_theme_large + 
#   scale_color_manual(name = 'Sport', values = mycolorscheme3[1:3]) + 
#   theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)) + 
#   theme(plot.margin = margin(1, 0, 0, 0, "cm"))
# 
# ggarrange(pic1,
#           pic2 + theme(axis.text.y = element_blank()),
#           pic3 +  theme(axis.text.y = element_blank()), 
#           ncol = 3,
#           nrow = 1, 
#           common.legend = TRUE, legend = 'right',
#           align = 'hv', 
#           labels = c('0-1km', '1-2km', '2-3km'), 
#           font.label = list(size = 14, color = "black", face = "bold", family = 'Times'))
# 
###############################################################################