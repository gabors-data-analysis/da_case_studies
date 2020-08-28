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

use_case_dir <- "ch03-simulations/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#------------------------------------
# set the seed
set.seed(16460)

# sample size
N <- 100000
obs <- N

# Bernoulli
bernoulli <- as.data.frame(rbinom(obs, 1, 0.7))
colnames(bernoulli) <- "bernoulli"

g_bernoulli<- ggplot(data = bernoulli, aes (x = bernoulli, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.1, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bg() 
g_bernoulli
save_fig("ch03-figure-rb7-1a-bernoulli", output, size = "small")


# Binomial
# with smaller sample
Nbinom <- 20
binomial <- as.data.frame(rbinom(obs,Nbinom,0.4))
colnames(binomial) <- "binomial"

g_binom<-ggplot(data = binomial, aes (x = binomial, y = (..count..)/sum(..count..))) +
  geom_histogram_da(binwidth = 0.5, type="percent") +
  labs(y = "Percent") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0.01,0.01), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 16), breaks=seq(0, 16, by=2)) + 
  theme_bg() 
g_binom
save_fig("ch03-figure-rb7-1b-binomial", output, size = "small")


# uniform [0,1]

uniform <- as.data.frame(runif(obs, 0, 1))
colnames(uniform) <- "uniform"

g_uniform<-ggplot(data = uniform, aes (x = uniform, y = (..count..)/sum(..count..))) +
  geom_histogram(bins =50, center=1,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 1), breaks=seq(0, 1, by=0.1)) + 
  scale_y_continuous(expand = c(0.001,0.001), labels = scales::percent_format(accuracy = .1)) +
  coord_cartesian(clip = "off") +
    theme_bg() 
g_uniform
save_fig("ch03-figure-rb7-1c-uniform", output, size = "small")

# normal
normal <- as.data.frame(rnorm(obs, 0,1))
colnames(normal) <- "normal"

g_normal<-ggplot(data = normal, aes (x = normal, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.2, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 0.1), breaks=seq(0, 0.1, by=0.02)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(-5, 5), breaks=seq(-4, 4, by=1)) + 
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(clip = "off") +
  theme_bg() 
g_normal
save_fig("ch03-figure-rb7-2a-normal", output, size = "small")

# lognoromal
# take the exponential of the randomly generated normal above
lognormal <- as.data.frame(exp(normal))
colnames(lognormal) <- "lognormal"

g_lognormal<-ggplot(data = subset(lognormal, lognormal <10), aes (x = lognormal, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.1, boundary=0.0,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 0.08), breaks=seq(0, 0.08, by=0.02)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 8), breaks=seq(0, 8, by=1)) + 
  expand_limits(x = 0, y = 0) +
  coord_cartesian(clip = "off") +
  theme_bg() 
g_lognormal
save_fig("ch03-figure-rb7-2b-lognormal", output, size = "small")


# power-law
alpha <- 3
xmin <- 1
x <- seq(1, obs, 1)
powerlaw <-  xmin * (x ^ (-alpha))
histrange <- quantile(powerlaw, .75)
powerlaw <- powerlaw / sum(powerlaw)

powerlaw <- as.data.frame(powerlaw)


g_power<-ggplot(data = subset(powerlaw, powerlaw < histrange), aes (x = powerlaw, y = (..count..)/sum(..count..))) +
  geom_histogram(bins=50, boundary=0.5,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(y = "Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_x_continuous(labels = fancy_scientific) +
  theme_bg() 
g_power
save_fig("ch03-figure-rb7-2c-powerlaw", output, size = "small")


