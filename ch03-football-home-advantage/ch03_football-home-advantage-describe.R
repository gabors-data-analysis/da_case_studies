################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 03
# CH03C Measuring Home Team Advantage in Football
# football dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(haven)
library(cowplot)
library(grid)
library(scales)
library(Hmisc)


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

data_in <- paste(data_dir,"football","clean/", sep = "/")

use_case_dir <- "ch03-football-home-advantage/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------

# Import dataset
df <- read.csv(paste0(data_in,"epl_games.csv"),
                stringsAsFactors = F)

# look at 2016/17 season only
df <- subset(df, season==2016)
glimpse(df)

df <-  df %>%
  mutate(home_goaladv = goals_home- goals_away)


# Summary statistics
summary(df$home_goaladv)
describe(df$home_goaladv)


# Histogram
p1<-ggplot(data = df, aes (x = home_goaladv, y = (..count..)/sum(..count..))) +
  geom_histogram(color = color.outline, fill = theme_colors[1],
                 size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE,
                binwidth = 1) +
  geom_text(stat='count', aes(label=round((..count..)/sum(..count..)*100, 1)), hjust=0.5, vjust = -0.5, size = 2) +
  labs(x = "Goal difference", y = "Share of games (percent)") +
  scale_x_continuous(expand = c(0.05,0.05),limits = c(-6, 6), breaks = seq(-6, 6, by = 1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.25), breaks = seq(0,0.25, by = 0.05), labels = scales::percent_format(accuracy = 5L)) +
  theme_bg() 
p1
save_fig("ch03-figure-9-hist-homeadv", output, "small")



# look at goal advantage by team 
# table *not* used in book, but interesting
df %>%
  filter(team_home %in% c("Chelsea", "Arsenal", "Leicester", "Stoke", "West Ham") ) %>%
  group_by(team_home) %>%
  dplyr::summarize(Count = n(),
                   Mean = mean(home_goaladv, na.rm=TRUE),
                   Median = median(home_goaladv, na.rm=TRUE),
                   Std = sd(home_goaladv, na.rm=TRUE),
                   Min = min(home_goaladv, na.rm=TRUE))
df %>%
  filter(team_home %in% c("Chelsea", "Arsenal", "Leicester", "Stoke", "West Ham") ) %>%
  dplyr::summarize(Count = n(),
                   Mean = mean(home_goaladv, na.rm=TRUE),
                   Median = median(home_goaladv, na.rm=TRUE),
                   Std = sd(home_goaladv, na.rm=TRUE),
                   Min = min(home_goaladv, na.rm=TRUE))






