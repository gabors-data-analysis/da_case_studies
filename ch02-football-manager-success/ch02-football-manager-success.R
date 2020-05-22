###############################################
# Chapter 02
# Data Analysis Textbook

# Teams and Managers infootball 
# (ENGLISH PREMIER LEAGUE SEASONS)
# v1.0 2019-09-16
# v1.1 2020-01-28 minor edits, all files csv_read
# v1.2 2020-03-09 graph x axis renamed + axes remastered
# v1.3 2020-04-09 FIXME (graph)
# v1.4 2020-04-22 name ok
# v1.4 2020-04-26 graph edited


###############################################

# WHAT THIS CODES DOES:
  ## opens data tables, and creates a single graph

###############################################

# CLEAR MEMORY
rm(list=ls())

library(dplyr)
library(tidyverse)
library(haven)
library(lspline)
library(lsr) # cohensD
library(grid)
library(RColorBrewer)
library(readr)
library(rstudioapi)
#----------------------------------------------------------------------------------------------------


# Set your directory here
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

#location folders
data_in <-   paste0(dir,"da_data_repo/football/clean/")
data_out <-  paste0(dir,"da_case_studies/ch02-football-manager-success/")
output <-    paste0(dir,"da_case_studies/ch02-football-manager-success/output/")
func <-      paste0(dir,"da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))


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

win_ratio <-  merge(games, points, by = c('manager_id', 'team', 'manager_name')) %>%
  group_by(team, manager_id, manager_name) %>%
  mutate(manager_win_ratio = (manager_points/manager_games)) %>%
  arrange(manager_win_ratio)

win_ratio <- win_ratio %>%
  arrange(-manager_win_ratio)
win_ratio

top_managers <-  win_ratio %>%
  filter(manager_win_ratio >= 2)
top_managers

# denote caretakers
top_managers <- top_managers %>%
  mutate(manager_win_ratio0 = ifelse(manager_games < 18, manager_win_ratio, NA),
         manager_win_ratio1 = ifelse(manager_games > 18, manager_win_ratio, NA))

# --------------------------------------------------------------------------------------------
# visualize

# denote caretakers
top_managers <-  top_managers %>% 
  mutate(fill= case_when (manager_games < 18 ~ "1", 
                         manager_games > 18 ~ "0" )) 

top_managers_graph <- top_managers %>%
  ggplot(., aes( x= reorder(manager_name, manager_win_ratio), y = manager_win_ratio, fill = fill, alpha = fill)) +
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


