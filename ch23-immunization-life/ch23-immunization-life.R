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

# Chapter 23
# CH23B Immunization against measles and saving children using the worldbank-immunization dataset
# version 0.95 2021-03-29
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)
library(fixest)


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")

# set data dir, data used
source("ch00-tech-prep/set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/xy/Dropbox/gabors_data_analysis/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch23-immunization-life/"
data_in <- paste(data_dir,"worldbank-immunization","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-------------------------------------------------------
# Import data

data <- read_csv(paste(data_in, "worldbank-immunization-continents.csv", sep = ""))
# Load from OSF
# data <- read_csv("https://osf.io/58zrj/download")

# **************************************************
# * info graph on measles vaccination continent aggregates

p1 <- ggplot(data, aes(x = year, y = imm_SAS)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = imm_SSF), color = color[2], size = 0.7) +
  geom_text(data = data[12,], aes(label = "South Asia"), hjust = 1.2, vjust = 1, size=2) +
  geom_text(data = data[16,], aes(y = imm_SSF, label = "Sub-Saharan Africa"), hjust = 0.4, vjust = 1.5, size=2) +
  labs(y = "Immunization rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(50, 100, by = 10), limits = c(50, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()

for (name in names(data)[2:6]) {
  p1 <- p1 + geom_line(aes_string(x = "year", y = name), color = "grey", size=0.5)
}
p1
save_fig("ch23-figure-2a-tsimmun", output, size = "small")


data[,9:15] <- data[,9:15] / 10

p2 <- ggplot(data, aes(x = year, y = surv_SAS)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = surv_SSF), color = color[2], size = 0.7) +
  geom_text(data = data[11,], aes(label = "South Asia"), hjust = 0, vjust = 1.5, size=2) +
  geom_text(data = data[15,], aes(y = surv_SSF, label = "Sub-Saharan Africa"), hjust = 0.2, vjust = 1.5, size=2) +
  labs(y = "Child survival rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(80, 100, by = 5), limits = c(80, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()

for (name in names(data)[9:13]) {
  p2 <- p2 + geom_line(aes_string(x = "year", y = name), color = "grey", size=0.5)
}
p2
save_fig("ch23-figure-2b-tssurvival", output, size = "small")

# **************************************************
# * regressions on countries

data_panel <- read_csv(paste(data_in, "worldbank-immunization-panel.csv", sep = "/"))
# From OSF
#data_panel <- read_csv("https://osf.io/gk5cn/download")

data_panel <- data_panel %>%
  filter(!(is.na(imm) | is.na(gdppc))) %>%
  mutate(c = factor(c)) %>%
  group_by(c) %>%
  mutate(balanced = min(year) == 1998 & max(year) == 2017 & length(unique(year)) == 20) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

data_balanced <- data_balanced %>%
  arrange(c, year) %>%
  group_by(c) %>%
  mutate(
    lnpop=log(pop),
    d_surv = surv- lag(surv),
    d_imm = imm - lag(imm),
    d2_imm = d_imm - lag(d_imm), 
    d_lngdppc= lngdppc- lag(lngdppc),
    d_lnpop = lnpop - lag(lnpop),
    avgpop = mean(pop), #for weights in xtreg fe
    year = factor(year)
  ) %>%
  ungroup()



# *****************************************************
# * FE REGRESSSIONS
# Set the panel.id for all estimation
setFixest_estimation(panel.id = ~c + year)

fe_lm <- feols( surv ~ imm + year | c , 
                data = data_balanced,
                weights = data_balanced$avgpop,
                cluster = "c" )

fe_lm2 <- feols(surv ~ imm + lngdppc + lnpop + year | c ,
                data = data_balanced, 
                weights = data_balanced$avgpop, 
                cluster = "c" )

etable(fe_lm, fe_lm2  , drop = 'year')

####################### not in book
# no weights
fe_lm2_nowts <- feols(surv ~ imm + lngdppc + lnpop + year| c ,
                      data = data_balanced, 
                      cluster = "c" )

etable( fe_lm2, fe_lm2_nowts , drop = 'year')

###########################################################


# *************************
# ** CLUSTER SE VS BIASED SE 

fe_lm3 <- feols(surv ~ imm + lngdppc + lnpop + year | c ,
                data = data_balanced, 
                weights = data_balanced$avgpop,
                vcov = 'iid')

etable( fe_lm2 , fe_lm3 , drop = 'year' )


# *************************
# * FD REGRESSIONS 

# * basic FD 
fd_lm <- feols(d_surv ~ d_imm ,
               data = data_balanced, 
               weights = data_balanced$pop,
               cluster = "c")

# * FD, 5 lags
fd_lm_5 <- feols(d_surv ~ l(d_imm,0:5),
                 data = data_balanced, 
                 weights = data_balanced$pop,
                 cluster = "c"
)

# Showing only the d_imm renamed
dictName = c("l(d_imm,1)"="d_imm lag1",
             "l(d_imm,2)"="d_imm lag2",
             "l(d_imm,3)"="d_imm lag3",
             "l(d_imm,4)"="d_imm lag4",
             "l(d_imm,5)"="d_imm lag5",
             "(Intercept)"="Constant")
etable( fd_lm, fd_lm_5,dict = dictName,
        keep = 'd_imm|Constant', digits = 3)


# * FD, 5 lags, cumul
fd_lm_5_cumul <- feols( d_surv ~ l( d_imm , 5 )+ l( d2_imm , 0:4) ,
                        data = data_balanced, 
                        weights = data_balanced$pop,
                        cluster = "c" )

# * FD, 5 lags, cumul, lead (!different than in book!)
fd_lm_5_cumul_lead <- feols( d_surv ~ l( d_imm , 5 ) + l( d2_imm , -3:4 ) ,
                             data = data_balanced, 
                             weights = data_balanced$pop,
                             cluster = "c" )


# Showing only the d_imm renamed
dictName2 = c("l(d_imm,5)"="d_imm cumulative",
              "f(d2_imm,1)"="d_imm lead 1",
              "f(d2_imm,2)"="d_imm lead 2",
              "f(d2_imm,3)"="d_imm lead 3",
              "(Intercept)"="Constant")
etable( fd_lm_5_cumul, fd_lm_5_cumul_lead, dict=dictName2,
         digits = 3,drop="d2_imm")



# *************************
# * AGGREG TREND, CONFOUNDERS, CTRY TRENDS
# * FD, 5 lags, cumul, aggreg trend

fd_lm_5_cumul_trend <- feols(d_surv ~ l( d_imm , 5 ) + l(d2_imm , 0 : 4) | year,
                             data = data_balanced, 
                             weights = data_balanced$pop,
                             cluster = "c" ) 

# * FD, 5 lags, cumul, aggreg trend, confounders 
fd_lm_5_cumul_trend_c <- feols( d_surv ~ l( d_imm , 5 ) + l(d2_imm , 0 : 4)
                                + l(d_lngdppc , 0:5) + l(d_lnpop,0:5) | year,
                                data = data_balanced, 
                                weights = data_balanced$pop,
                                cluster = "c") 

# * check: it's not the number of observations
data_balanced_filtered <- data_balanced %>%
  filter(!is.na(d_lngdppc))
fd_lm_5_cumul_trend2 <- feols(d_surv ~ l( d_imm , 5 ) + l(d2_imm , 0 : 4) | year,
                              data = data_balanced_filtered, 
                              weights = data_balanced_filtered$pop,
                              cluster = "c")

# * FD, 5 lags, cumul, aggreg trend, cofounders, country linear trend
fd_lm_5_cumul_trend_c_country <- feols(d_surv ~ l( d_imm , 5 ) + l(d2_imm , 0 : 4)
                                       + l(d_lngdppc , 0:5) + l(d_lnpop,0:5) | year + c,
                                       data = data_balanced, 
                                       weights = data_balanced$pop,
                                       cluster = "c"
) 

# etable format for output
etable(fd_lm_5_cumul_trend, fd_lm_5_cumul_trend_c, fd_lm_5_cumul_trend_c_country,
       keep = "d_imm",digits=3,
       extralines = list("Confounders" = c(F,T,T)))

###
# To carry out hypothesis test for cumulative coeffs on the confounders:

library(estimatr)
library(car)
library(lmtest)
# * FD, 5 lags, cumul, aggreg trend, confounders 
lags_helper  <- paste(paste0("lag(d2_imm, ", c(0:4), ")"), collapse = " + ")
lags_helper2 <- paste(paste0("lag(d_lngdppc, ", c(0:5), ")"), collapse = " + ")
lags_helper3 <- paste(paste0("lag(d_lnpop, ", c(0:5), ")"), collapse = " + ")

fd_lm_5_cumul_trend_c_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
                                                   lags_helper, "+",
                                                   lags_helper2, "+",
                                                   lags_helper3, "+",
                                                   "year"))
fd_lm_5_cumul_trend_c <- lm_robust(fd_lm_5_cumul_trend_c_formula,
                                   data = data_balanced, 
                                   weights = pop,
                                   se_type = "stata", 
                                   clusters = c
) 
# run tests
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper2," =0"))
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper3," =0"))

