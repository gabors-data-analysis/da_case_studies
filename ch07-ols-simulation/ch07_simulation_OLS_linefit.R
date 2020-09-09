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

# Chapter 07
# CH07 OLS fit simulation
# version 0.9 2020-09-07
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Import libraries
library(tidyverse)


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

use_case_dir <- "ch07-ols-simulation/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------

# set the seed
set.seed(1458)

# sample size
n <- 100

# uniformly distributed x, [0,4]
xvar <-  runif(n,0,4)

# y  = a + bx + u (u normally distributed)
a <- 2
b <- 0.5
sigmau <- 0.7
yvar <- a+b*xvar+rnorm(n,0,sigmau)


reg <- lm(yvar~xvar)
summary(reg)

# save coefficients
coeffs = coefficients(reg)

# scatterplot and OLS regression line
# average y and average x shown
ols <- data.frame(xvar,yvar)


F07_sim <- ggplot(data = ols, aes(x = xvar, y = yvar)) +
  geom_point_da() + 
  geom_smooth_da(method = "lm") +
  #geom_abline(intercept=coeffs[1], slope=coeffs[2], size=1.2, color=color[3]) + # alternative
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 4), breaks=seq(0, 4, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  labs(x = "Simulated x variable",y = "Simulated y variable")+
  theme_bg() +
  geom_vline(xintercept = mean(xvar), color=color[3], linetype="dashed", size=0.4) +
  geom_hline(yintercept = mean(yvar), color=color[3], linetype="dashed", size=0.4) +
  geom_segment(aes(x = 0.5, y = 3.5, xend = 0.5, yend = 2.9), arrow = arrow(length = unit(0.01, "cm")))+
  annotate("text", x = 0.3, y = 3.6, label = "Average y", size=2) +
  geom_segment(aes(x = 1.2, y = 4, xend = 1.9, yend = 4), arrow = arrow(length = unit(0.01, "cm")))+
  annotate("text", x = 0.9, y = 4, label = "Average x", size=2)
F07_sim
save_fig("ch07-figure-4-olsfit", output, "small")

