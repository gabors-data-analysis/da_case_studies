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

# Chapter 12 
# CH12 Time series simulation
# version 0.9 2020-09-09
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

use_case_dir <- "ch12-time-series-simulations/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------
# PART 1
# Random walk simulation 
# Generate k random walks across time {0, 1, ... , T}


# set parameters
set.seed (10)
T <- 100  # number of obs
k <- 5    # nr of random walks generated
initial.value <- 0

# create a function
GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(rnorm(T)))
}

# Matrix of random walks
values <- replicate(k, GetRandomWalk())

# visualize
rws <- as.data.frame(values)
rws <- rws %>%
  mutate(time=as.numeric(rownames(.)))

rws <- rws %>% 
  gather(var, value, V1:V5) 

rws_plot <- ggplot(rws,aes(time, value, color=var)) + 
  geom_line (show.legend = FALSE, size =0.8) +
  theme_bg() +
  scale_color_manual(values = c(color[1], color[2], color[3], color[4], color[5])) +
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,100), breaks=seq(0,100,10)) 
rws_plot
save_fig("ch12-figure-1-randomwalks", output, "small")


#-----------------------------------------------------------------------------------------
# PART 2
# Serially correlated vs serially uncorrelated series
# simulation exercies



# rnorm(n, mean = 0, sd = 1)

# serially uncorrelated series/white noise
set.seed(2016)
uncorr <- as.data.frame(ts(rnorm(100, mean=0, sd=1)) )

uncorr <- uncorr %>%
  mutate(t=as.numeric(rownames(.)))
uncorr

whitenoise <- ggplot(uncorr,aes(t, x)) + 
  geom_line (show.legend = FALSE, size =0.6, color=color[1]) +
  geom_hline(yintercept=0, 
             color = color[2], size=1)+
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
  theme_bg() +
  scale_y_continuous(expand = c(0.01,0.01)) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks=seq(0,100,10)) 
whitenoise
save_fig("ch12-figure-9a-serialcorr-whitenoise", output, "small")


# serially correlated series, pho=0.8
set.seed(2016)
rho=0.8
E <- rnorm(100, 0, 1)
x <- numeric()
x[1] <- E[1]
for(i in 2:100) x[i] <- rho*x[i-1] + E[i]

E <- as.data.frame(E)

corr08 <- E %>%
  mutate(t=as.numeric(rownames(.)))

corr08_graph <- ggplot(corr08,aes(t, x)) + 
  geom_line (show.legend = FALSE, size =0.6, color=color[1]) +
  geom_hline(yintercept=0, 
             color = color[2], size=1)+
  labs(x = "Time period", 
       y = "Value of the simulated variable") +
    theme_bg() +
  scale_y_continuous(expand = c(0.01,0.01)) + 
  scale_x_continuous(expand = c(0.01,0.01),breaks=seq(0,100,10)) 
corr08_graph
save_fig("ch12-figure-9b-serialcorr-corr08", output, "small")






