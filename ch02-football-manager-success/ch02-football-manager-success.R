################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 02
# CH02B Identifying successful football managers

# football dataset
# version 0.9 2020-08-28

###########

# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(haven)
library(lspline)
library(grid)
library(cowplot)
#----------------------------------------------------------------------------------------------------


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"football","clean/", sep = "/")

use_case_dir <- "ch02-football-manager-success/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# look at basic data
epl_games <-  read_csv(paste0(data_in,"epl_games.csv"))

#---------------------------------------------------------------------------------------------------------

epl_games <-  epl_games %>%
  arrange(.,team_home)
View(epl_games)

epl_games <-  epl_games %>%
  arrange(.,season, team_home)
View(epl_games)

epl_games <-  epl_games %>%
  filter(season == 2016)
View(epl_games)
#---------------------------------------------------------------------------------------------------------
# look at data for team-game level
# TODO: change to csv when we have it
epl_teams_games <-  read_csv(paste0(data_in, "epl-teams-games.csv"))

epl_teams_games <-  epl_teams_games %>%
  arrange(.,team)
View(epl_games)

epl_teams_games <-  epl_teams_games %>%
  arrange(.,season, team)

epl_teams_games <-  epl_teams_games %>%
  filter(season == 2016) %>%
  arrange(., date)
View(epl_games)

football_managers <- read_csv(paste0(data_in, "football_managers.csv"))
View(football_managers)
Hmisc:: describe(football_managers_merged$manager_id)


#--------------------------------------------------------------------------------------------------------

# finally the merged file

# read.csv accent problem, using read_csv
football_managers_merged <-  read_csv(paste0(data_in,"football_managers_workfile.csv"))

football_managers_merged <-  football_managers_merged %>%
  arrange(.,season, team)

games <- football_managers_merged %>%
  group_by(team, manager_id, manager_name) %>%  
  summarise(manager_games=n())

points <- football_managers_merged %>%
  group_by(team, manager_id, manager_name) %>%
  summarise(manager_points=sum(points))

avg_points <-  merge(games, points, by = c('manager_id', 'team', 'manager_name')) %>%
  group_by(team, manager_id, manager_name) %>%
  mutate(manager_avg_points = (manager_points/manager_games)) %>%
  arrange(manager_avg_points)

avg_points <- avg_points %>%
  arrange(-manager_avg_points)
avg_points

top_managers <-  avg_points %>%
  filter(manager_avg_points >= 2)
top_managers

# denote caretakers
top_managers <- top_managers %>%
  mutate(manager_avg_points0 = ifelse(manager_games < 18, manager_avg_points, NA),
         manager_avg_points1 = ifelse(manager_games > 18, manager_avg_points, NA))

# --------------------------------------------------------------------------------------------
# visualize

# denote caretakers
top_managers <-  top_managers %>% 
  mutate(fill= case_when (manager_games < 18 ~ "1", 
                         manager_games > 18 ~ "0" )) 

top_managers_graph <- top_managers %>%
  ggplot(., aes( x= reorder(manager_name, manager_avg_points), y = manager_avg_points, fill = fill, alpha = fill)) +
  geom_col(show.legend=F) +
  ylab("Average points per game") +
  xlab("Manager name") +
  scale_fill_manual(values = c(color[1], color[4])) +
  scale_alpha_manual(values =c(0.8,0.3)) +
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 3), breaks=seq(0, 3, 0.3)) +
    coord_flip() +
  theme_bg() +
  cowplot::background_grid(major="x", minor="none")
top_managers_graph
#save_fig("03_top_managers_R", output, "small")
save_fig("ch02-figures1-top-managers", output, "small")


