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
# CH24A Estimating the effect of the 2010 Haiti earthquake on GDP
# using the haiti-earthquake dataset
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
library(cowplot)
library(gridExtra)
library(purrr)
library(kableExtra)
library(haven)
library(Synth)



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

use_case_dir <- "ch24-haiti-earthquake-gdp/"
data_in <- paste(data_dir,"haiti-earthquake","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# FIXME.
# somehow matching is not good.


# Loading and preparing data ----------------------------------------------

data <- read_dta(paste0(data_in,"haiti-earthquake-mod.dta"))

# donor pool based on threshold calculations below:
# it is those countries with incomethreshold=1, and a balanced panel for all variables
dp_countries <- c("Benin","Burkina Faso","Burundi","Bangladesh","Cambodia","Cameroon",
                  "Kenya","Kyrgyz Republic","Liberia","Madagascar","Mali","Moldova","Mozambique",
                  "Nicaragua","Nepal","Rwanda","Senegal","Sierra Leone","Sudan","Tanzania","Togo","Uganda",
                  "Haiti")
dp_countries <- dp_countries %>%
  .[order(dp_countries)] %>%
  .[.!="Haiti"] %>%
  c("Haiti", .)

data <- data %>%
  mutate(dp = as.numeric(country %in% dp_countries)) %>%
  filter(dp==1) %>%
  mutate(country = factor(country, levels = dp_countries, ordered = TRUE)) %>%
  arrange(country) %>%
  mutate(country = as.character(country)) %>%
  mutate(ccode = as.numeric(factor(countrycode, levels = unique(countrycode), ordered = TRUE)))

# time series in Haiti
tsdata<-data %>%
  filter(ccode==1) 



g1<- ggplot(data=tsdata, aes(x = year, y = gdptb_us)) +
  geom_line(color = color[1], size = 0.7) +
  geom_vline(xintercept = 2010, color = color[3], size = 0.7, linetype="dashed") +
  labs(y = "Total GDP (bn US dollars)", x = "Date (year)") +
  scale_y_continuous(breaks = seq(6, 9.5, 0.5), limits = c(6, 9.5)) +  scale_x_continuous(breaks = seq(2004, 2014, 2), limits = c(2004, 2015)) +
  geom_segment(aes(x = 2009, y = 7.7, xend = 2010, yend = 7.7), arrow = arrow(length = unit(0.05, "cm")))+
  annotate("text", x = 2008, y = 7.7, label = "Earthquake", size=2)+
  theme_bg()
g1
save_fig("ch24-figure-1-haiti-gdp", output, "small")


###############
# use from stata


###############

data_helper <- read_dta(paste0(data_out, "temp/gdp-1-temp.dta")) 



# figure with total GDP in Haiti and synthetid control
# line _Y_treated _Y_synth _time, lw(vthick vthick) lc(blue mint) ///

g2 <- ggplot(data=data_helper, aes(x = year, y = Ytreated)) +
  geom_line(color = color[1], size = 1) +
  geom_line(aes(y = Ysynthetic), color = color[2], size = 0.7) +
  geom_vline(xintercept = 2010, color = color[3], size = 0.7, linetype="dashed") +
  labs(y = "Total GDP (bn US dollars)", x = "Date (year)") +
  geom_segment(aes(x = 2009, y = 7.7, xend = 2010, yend = 7.7), arrow = arrow(length = unit(0.05, "cm")))+
  annotate("text", x = 2008, y = 7.7, label = "Earthquake", size=2)+
  scale_y_continuous(breaks = seq(6, 9.5, 0.5), limits = c(6, 9.5)) +
  scale_x_continuous(breaks = seq(2004, 2014, 2), limits = c(2004, 2015)) +
  theme_bg()
g2
save_fig("ch24-figure-2a-haiti-gdp-synth", output, "small")





# figure with differenence in log total GDP
g3 <- ggplot(data=data_helper, aes(x = year, y = log(Ytreated) - log(Ysynthetic))) +
  geom_line(color = color[1], size = 1) +
  geom_vline(xintercept = 2010, color = color[3], size = 0.7, linetype="dashed") +
  geom_hline(yintercept = 0, color = color[5], size = 0.7) +
  labs(y = "Effect estimate, ln(bn US dollars)", x = "Date (year)") +
  geom_segment(aes(x = 2009, y = -0.17, xend = 2010, yend = -0.17), arrow = arrow(length = unit(0.05, "cm")))+
  annotate("text", x = 2008, y = -0.17, label = "Earthquake", size=2)+
  scale_y_continuous(breaks = seq(-0.2, 0.05, 0.05), limits = c(-0.2, 0.05)) +
  scale_x_continuous(breaks = seq(2004, 2014, 2), limits = c(2004, 2015)) +
  theme_bg()
g3
save_fig("ch24-figure-2b-haiti-lndiffgdp-synth", output, "small")



#############################
#STOP










# Haiti and synthetic control
depvar <- "gdptb_us"
unitvar <- "ccode"
unit_values <- unique(pull(data, get(unitvar)))
timevar <- "year"
predictorvars <- c("cons", "exp", "imp", "gcf", "land", "pop", "inf", "gdppc_w")
special_predictorvars <- list(list("gdptb_us",seq(2005,2009,2),c("mean")))
trunit <- 1
trperiod <- c(2004:2009)
xperiod <- c(2004:2009)
timeplot <-  c(2004:2015)
unitnames <- "country"

dataprep.out <-
  dataprep(
    foo = as.data.frame(data)
    ,predictors= predictorvars
    ,predictors.op = c("mean")
    ,dependent     = depvar
    ,unit.variable = unitvar
    ,time.variable = timevar
    ,special.predictors = special_predictorvars
    ,treatment.identifier  = trunit
    ,controls.identifier   = unit_values[unit_values != trunit]
    ,time.predictors.prior = xperiod
    ,time.optimize.ssr     = trperiod
    ,unit.names.variable   = unitnames
    ,time.plot            = timeplot
  )

# https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/synth
# synth.out <- synth(dataprep.out, optimxmethod = c("Nelder-Mead", "BFGS"))
# nested in Stata
synth.out <- synth(dataprep.out, optimxmethod = "L-BFGS-B")

syn_haiti<-dataprep.out$Y0plot%*%synth.out$solution.w

# figure with total GDP in Haiti and synthetid control
tsdata<-data %>%
  filter(ccode==1) 

g2 <- ggplot(data=tsdata, aes(x = year, y = gdptb_us)) +
  geom_line(color = color[1], size = 1) +
  geom_line(aes(y = syn_haiti), color = color[5], size = 1) +
  geom_vline(xintercept = 2010, color = color[2], size = 0.7) +
  labs(y = "Total GDP (billion US dollar)", x = "Date (year)") +
  scale_x_continuous(breaks = seq(2004, 2014, 2), limits = c(2004, 2015)) +
  theme_bg()
g2
save_fig("haiti-gdp-synth", "small")


# figure with differenence in log total GDP
tsdata<-data %>%
  filter(ccode==1) 
g3 <- ggplot(data=tsdata, aes(x = year, y = log(gdptb_us) - log(syn_haiti))) +
  geom_line(color = color[1], size = 2) +
  geom_vline(xintercept = 2010, color = color[2], size = 1.5) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(y = "Effect estimate, ln(total GDP)", x = "Date (year)") +
  scale_x_continuous(breaks = seq(2004, 2014, 2), limits = c(2004, 2015)) +
  theme_bg()
g3
save_fig("haiti-lndiffgdp-synth", "small")

