#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 24
# CH24B Estimating the impact of replacing football team managers
# using the football dataset
# version 0.9 2020-09-09
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)
library(purrr)
library(kableExtra)
library(plm)
library(cowplot)
library(gridExtra)


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

use_case_dir <- "ch24-football-manager-replace/"
data_in <- paste(data_dir,"football","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# Loading and preparing data ----------------------------------------------

data <- read_csv(paste0(data_in,"football_managers_workfile.csv")) %>%
  mutate(date = as.Date(date, format = "%d%m%Y"))

# describe data
data %>%
  select(season, team, gameno, points) %>%
  summary()

# create manager change variable
data <- data %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(managchange = as.numeric(manager_id != dplyr::lag(manager_id))) %>%
  mutate(countmanagchange = sum(managchange, na.rm = TRUE)) %>%
  ungroup()

table(data$managchange, useNA = "always")
# some teams have multiple management changes in the season
data %>%
  group_by(countmanagchange) %>%
  summarise(n_distinct(team, season))

# *********************************************************************-
#   BALANCED PANEL
# *********************************************************************-

# define intervention as management change
# at least 12 games before (since season started or previous management changed)
# at least 12 games after (till season ends or next management change)
data_aux <- data %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(max_gameno = max(gameno)) %>%
  filter(managchange == 1) %>%
  mutate(gamesbefore = ifelse(is.na(dplyr::lag(gameno)), gameno - 1, gameno - dplyr::lag(gameno)),
         gamesafter = ifelse(is.na(dplyr::lead(gameno)), max_gameno - gameno, dplyr::lead(gameno) - gameno)) %>%
  mutate(intervention = ifelse(gamesbefore<12 | gamesafter<12, 0, managchange)) %>%
  select(team, season, date, intervention) %>%
  ungroup()

data_balanced <- left_join(data, data_aux) %>%
  mutate(intervention = replace_na(intervention, 0)) %>%
  group_by(team, season) %>%
  mutate(countinterv = sum(intervention, na.rm = TRUE)) %>%
  mutate(intervention_time = min(ifelse(intervention == 1, gameno, NA), na.rm = TRUE)) %>%
  mutate(t_event = ifelse(is.infinite(intervention_time), NA, gameno - intervention_time)) %>%
  mutate(t_event = ifelse(t_event>=0 & t_event<=38, t_event+1, t_event)) %>% # Intervention event study time (to -1 before, from +1 after)
  ungroup() %>%
  filter((countinterv==1 & t_event>=-12 & t_event<=12) | countmanagchange==0)

data_balanced %>%
  group_by(countinterv) %>%
  summarise(n_distinct(team, season))

# figure: average number interventions by game number
p3<- ggplot(data = data_balanced, aes(x = gameno, y = intervention)) +
  geom_col(fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Number of manager changes", x = "Game number") +
  scale_y_continuous(expand=c(0.0,0.0), breaks = seq(1, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(expand=c(0.01,0.01), breaks = seq(0, 38, 4), limits = c(0, 38)) +
  background_grid(major = "none", minor = "none")+
  theme_bg()
p3
save_fig("ch24-figure-3-football-managchanges", output, "large")

# figure: average points by event time
p4<-getPointsGraph(data_balanced, colors = color)
p4
save_fig("ch24-figure-4-football-manager-points1", output, "large")

# *********************************************************************-
#   * CREATE CONTORL GROUP WITH PSEUDO-INTERVENTIONS
# *********************************************************************-
# for each game, define avg diff of points 12-7 before
# dip: avg diff of points 6-1 before minus 12-7 before
data_balanced <- data_balanced %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  mutate(points_b_7_12 = dplyr::lag(points, 12) + dplyr::lag(points, 11) + dplyr::lag(points, 10) +
           dplyr::lag(points, 9) + dplyr::lag(points, 8) + dplyr::lag(points, 7),
         points_b_1_6 = dplyr::lag(points, 6) + dplyr::lag(points, 5) + dplyr::lag(points, 4) +
           dplyr::lag(points, 3) + dplyr::lag(points, 2) + dplyr::lag(points, 1)) %>%
  mutate(dip = points_b_1_6/6 - points_b_7_12/6,
         points_b_1= dplyr::lag(points)) %>%
  ungroup()

# summary stats of dip when intervention
data_balanced %>%
  filter(intervention == 1) %>%
  select(points_b_1_6, points_b_7_12, dip, points_b_1) %>%
  summary()

# set ranges to define control group
points_b_7_12min <-  5
points_b_7_12max <-  8
dipmin <- -1.33
dipmax <- -0.166
points_b_1min <-  0
points_b_1max <-  0

# define pseudo-intervention
data_balanced <- data_balanced %>%
  mutate(pseudo = as.numeric(countmanagchange==0 & dip>=dipmin & dip<=dipmax
                   & points_b_7_12>=points_b_7_12min & points_b_7_12<=points_b_7_12max
                   & points_b_1>=points_b_1min & points_b_1<=points_b_1max
                   & gameno < (38-12) # games with 12 left in the season
                   ))

table(data_balanced$pseudo, useNA = "always")
data_balanced %>%
  filter(pseudo == 1) %>%
  select(points_b_7_12, dip, points_b_1) %>%
  summary()

# if more such games in a teamXseason, choose one randomly
data_balanced <- chooseRandomPseudo(data_balanced)

table(data_balanced$pseudo, useNA = "always")
data_balanced %>%
  filter(pseudo == 1) %>%
  select(points_b_7_12, dip, points_b_1) %>%
  summary()

data_balanced <- data_balanced %>%
  group_by(team, season) %>%
  mutate(countpseudo = sum(pseudo, na.rm = TRUE)) %>%
  mutate(pseudo_time = min(ifelse(pseudo == 1, gameno, NA), na.rm = TRUE)) %>%
  mutate(t_pseudo = ifelse(is.infinite(pseudo_time), NA, gameno - pseudo_time)) %>%
  mutate(t_pseudo = ifelse(t_pseudo>=0 & t_pseudo<=38, t_pseudo+1, t_pseudo)) %>%
  mutate(t_event = ifelse(is.na(t_event), t_pseudo, t_event)) %>%
  ungroup() %>%
  filter(t_event>=-12 & t_event<=12)

table(data_balanced$intervention, data_balanced$pseudo)
data_balanced %>%
  group_by(countinterv, countpseudo) %>%
  summarise(n_distinct(team, season))

# FIGURE with intervention and pseudo-intervention averages
p5<-getPointsGraphWithPseudo(data_balanced, colors = color)
p5
save_fig("ch24-figure-5-football-manager-points2", output, "large")

# ******************************************************************
# REGRESSION with 6-game averages
data_balanced_agg <- data_balanced %>%
  mutate(teamseason = factor(paste0(team, season))) %>%
  group_by(teamseason, t6_event = droplevels(cut(t_event, c(-13,-6,0, 1,7,13), right = FALSE))) %>%
  summarise(treated = mean(countinterv), points6avg = round(mean(points), 6)) %>%
  arrange(teamseason, t6_event) %>%
  group_by(teamseason) %>%
  mutate(Dp6avg = points6avg - dplyr::lag(points6avg)) %>%
  ungroup()

data_balanced_agg <- cbind(data_balanced_agg,
                           sapply(levels(data_balanced_agg$t6_event),
                                  function(x) as.integer(x == data_balanced_agg$t6_event)))

colnames(data_balanced_agg)[6:9] <- c("before_7_12", "before_1_6", "after_1_6", "after_7_12")

data_balanced_agg %>%
  select(Dp6avg, after_1_6, after_7_12) %>%
  summary()

# FD REGRESSIONS
fd_treatment <- lm(Dp6avg ~ after_1_6 + after_7_12, 
                   data = filter(data_balanced_agg, treated == 1))
fd_control <- lm(Dp6avg ~ after_1_6 + after_7_12, 
                 data = filter(data_balanced_agg, treated == 0))
fd <- lm(Dp6avg ~ after_1_6 + after_7_12 + treated + I(treated*after_1_6) + I(treated*after_7_12),
                 data = data_balanced_agg)

summary(fd_treatment)
summary(fd_control)
summary(fd)

stargazer_r(list(fd_treatment, fd_control, fd), se = 'robust', 
            float=T, digits=3, out=paste(output,"football-manager-reg1.tex",sep=""))



data_panel <- pdata.frame(data_balanced_agg, index=c("teamseason","t6_event"), drop.index=TRUE, row.names=TRUE)
head(data_panel)


###################
# EXTENSIONS: NOT USED
###################


fd_panel <- plm(points6avg ~ after_1_6 + after_7_12 + treated + treated*after_1_6 + treated*after_7_12,
          data = data_panel, model = "fd")
summary(fd_panel)

stargazer_r(list(fd_panel)) %>%
  cat(.,file= paste0(output, "football-manager-reg1_panel.tex"))


# FE REGRESSIONS
data_panel_treatmemnt <- pdata.frame(filter(data_balanced_agg, treated == 1),
                                     index=c("teamseason","t6_event"), drop.index=TRUE, row.names=TRUE)
data_panel_control <- pdata.frame(filter(data_balanced_agg, treated == 0),
                                  index=c("teamseason","t6_event"), drop.index=TRUE, row.names=TRUE)


fe_panel_treatment <- plm(points6avg ~ before_7_12 + after_1_6 + after_7_12,
                data = data_panel_treatmemnt, model = "within")
fe_panel_control <- plm(points6avg ~ before_7_12 + after_1_6 + after_7_12,
                data = data_panel_control, model = "within")
fe_panel <- plm(points6avg ~ before_7_12 + after_1_6 + after_7_12 + treated*before_7_12 + treated*after_1_6 + treated*after_7_12,
                data = data_panel, model = "within")

summary(fe_panel_treatment)
summary(fe_panel_control)
summary(fe_panel)

stargazer_r(list(fe_panel_treatment, fe_panel_control, fe_panel)) %>%
  cat(.,file= paste0(output, "football-manager-reg2.tex"))

#ch24-table-1-football-manager-reg1






