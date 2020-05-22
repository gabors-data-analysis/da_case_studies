
#####################################################################
#
# DATA ANALYSIS TEXTBOOK
# CH03
# Home field advantage
# football dataset
#
# WHAT THIS CODES DOES:
# creates desrciptive stats
#

# v1.3 2020-04-22 names ok

#####################################################################
  
#####################################################################
# IMPORT LIBRARIES
#####################################################################

# Clear memory
rm(list=ls())

# Libraries
require(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(grid)
library(scales)
library(RColorBrewer)
library(tidyverse)
library(Hmisc)

############################################################  
# SET YOUR DIRECTORY HERE
############################################################  

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


# Location folders
data_in <- paste0(dir,"da_data_repo/football/clean/")
data_out <- paste0(dir,"da_case_studies/ch03-football-home-advantage/")
output <- paste0(dir,"da_case_studies/ch03-football-home-advantage/output/")

func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

# Custom function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))

############################################################  

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
  geom_histogram_da(type="frequency", binwidth = 1) +
  geom_text(stat='count', aes(label=round((..count..)/sum(..count..)*100, 1)), hjust=0.5, vjust = -0.5, size = 2) +
  labs(x = "Goal difference", y = "Share of games (percent)") +
  scale_x_continuous(expand = c(0.05,0.05),limits = c(-6, 6), breaks = seq(-6, 6, by = 1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.25), breaks = seq(0,0.25, by = 0.05), labels = scales::percent_format(accuracy = 5L)) +
  theme_bg() 
p1
#ggsave(paste0(output, "homeadvantage_hist_R.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
save_fig("ch03-figure-9-hist-homeadv", output, "small")



# look at goal advantage by team 
# table used in book, but interesting
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






