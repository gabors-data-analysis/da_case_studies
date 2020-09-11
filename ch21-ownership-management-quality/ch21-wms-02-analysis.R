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

# CHAPTER 21
# CH20A Founder/family ownership and quality of management
# using the wms-management dataset
# version 0.9 2020-09-11
#########################################################################################

# Clear memory
rm(list=ls())

library(tidyverse)
library(purrr)
library(haven)
library(stargazer)
library(MatchIt)
library(Matching) # masks dplyr select!!! #
library(gmodels)

getwd()
# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

use_case_dir <- file.path("ch21-ownership-management-quality/")

data_in <- use_case_dir
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# This is the second part
# you must run ch21-wms-01-dataprep.R first.



# Read in data ------------------------------------------------------------

data <- read_rds(paste0(data_out, "wms_da_textbook-work.rds"))

data %>% 
  group_by(foundfam_owned) %>% 
  summarise (mean(management))

# Set variables to use -------------------------------------------------------

y_var <- "management"
x_var <- "foundfam_owned"

control_vars <- c("degree_nm", "degree_nm_sq", "compet_moder", "compet_strong", 
                  "lnemp", "age_young", "age_old", "age_unknown")
control_vars_to_interact <- c("industry", "countrycode")


data %>%
	dplyr::select(c(control_vars, control_vars_to_interact)) %>%
	summary()

# *************************************************************
# * REGRESSIONS
# *************************************************************

# OLS with no control vars. -------------------------------------------------------
formula1 <- as.formula(paste0(y_var, " ~ ",x_var))
ols1 <- lm(formula1, data=data)

# OLS with all control vars -------------------------------------------------------

formula2 <- as.formula(paste0(y_var, " ~ ",x_var," + ", 
                  paste(c(control_vars, control_vars_to_interact), collapse = " + ")))

ols2 <- lm(formula2, data=data)


# OLS with all controls + interactions -------------------------------------------------------

formula3 <- as.formula(paste(y_var, " ~ ",x_var," + ", 
	paste(control_vars_to_interact, collapse = ":"), 
	" + (", paste(control_vars, collapse = "+"),")*(",
	paste(control_vars_to_interact, collapse = "+"),")",sep=""))

ols3 <- lm(formula3, data=data)


stargazer_r(
	list_of_models = list(ols1, ols2, ols3), 
	keep.stat=c("n", "rsq"), keep = c(x_var, "Constant"), dep.var.labels.include = FALSE, dep.var.caption = "", 
	column.labels = c("'no confounders'", "'with confounders'", "'with confounders interacted'")) %>%
cat(.,file= paste0(output, "ch21-foundfam-reg1.tex"))


# *************************************************************
# * EXACT MATCHING
# ***************************************************************** 
Hmisc::describe(data$management)
data <- data %>%
	mutate(
		empbin5 = cut(emp_firm, quantile(emp_firm, seq(0,1,1/5)), include.lowest = TRUE, right = FALSE),
	    agecat = (age_young == TRUE) + 2*(age_mid == TRUE) + 3*(age_old == TRUE) + 4*(age_unknown == TRUE))

data_agg <- data %>%
	group_by(degree_nm_bins, agecat, competition, empbin5, industry, countrycode) %>%
  dplyr::summarise(
		n = n(), n0 = sum(1-foundfam_owned), n1 = sum(foundfam_owned),
		y0 = sum(management*(foundfam_owned == 0))/sum(1-foundfam_owned), 
		y1 = sum(management*(foundfam_owned == 1))/sum(foundfam_owned)
	) %>%
	ungroup()

# firms with/without exact match
data_agg %>%
	group_by(n0 == 0, n1 == 0) %>%
	summarise(n())

# random order just for the examples
set.seed(12345)
data_sample <- data_agg %>%
	sample_n(size = 340) %>%
  dplyr::select(industry, countrycode, degree_nm_bins, competition, agecat, empbin5, n1, n0, n)

# examples with founder/family only
data_sample %>%
	.[1:19,] %>%
	filter(n1==1 & n0==0)

# examples with other only: 
data_sample %>%
	.[1:19,] %>%
	filter(n1==0 & n0==1)

# examples of similar firms unmatched
data_sample %>%
	.[1:339,] %>%
	filter(countrycode == "us" & industry == "food" & n == 1) %>%
	arrange(countrycode, industry, degree_nm_bins, competition, agecat, empbin5, n)

# ATE/ATET
data_agg %>%
	filter(n0>0 & n1>0) %>%
	summarise(ATE = weighted.mean(y1-y0, n), ATET = weighted.mean(y1-y0, n1))

# ***************************************************************** 
# * Matching on the propensity score 
# ***************************************************************** 

# SOLUTION With replacement
# Function only works with non-missing values
data_pscore <- data %>% 
  dplyr::select(c(y_var, x_var, control_vars, control_vars_to_interact)) %>%
  na.omit()

# with all control vars -------------------------------------------------------

# Step 1 - Matching
formula_pscore1 <- as.formula(paste0(x_var, " ~ ", 
                  paste(c(control_vars, control_vars_to_interact), collapse = " + ")))

# same with factors?
mod_match <- matchit(formula_pscore1, 
                     data = data_pscore, 
                     method = 'nearest', distance = 'logit', replace=TRUE)

summary(mod_match)

# Step 2 - restrict data to matched 
data_match <- match.data(mod_match)
dim(data_match)

# Step 3 - Estimate treatment effects
reg_match <- lm(management ~ foundfam_owned, 
                data = data_match, weights = data_match$weights)

out1 <- summary(reg_match)

ATET_PSME1 <- out1$coefficients[2]
ATET_PSME1_SE <- out1$coefficients[2,2]

# with all controls + interactions -------------------------------------------------------

# Step 1 - Matching
formula_pscore2 <- as.formula(paste(x_var, " ~ " , 
	paste(control_vars_to_interact, collapse = ":"), 
	" + (", paste(control_vars, collapse = "+"),")*(",
	paste(control_vars_to_interact, collapse = "+"),")",sep=""))

# same with factors?
mod_match2 <- matchit(formula_pscore2, 
                     data = data_pscore, 
                     method = 'nearest', distance = 'logit', replace=TRUE)

summary(mod_match2)

# Step 2 - restrict data to matched 
data_match2 <- match.data(mod_match2)
dim(data_match2)

# Step 3 - Estimate treatment effects
reg_match2 <- lm(management ~ foundfam_owned, 
                data = data_match2, weights = data_match2$weights)

out2 <- summary(reg_match2)

ATET_PSME2 <- out2$coefficients[2]
ATET_PSME2_SE <- out2$coefficients[2,2]

out1
out2


# fixme
# add ate
# https://r.iq.harvard.edu/docs/matchit/2.4-15/Conducting_Analyses_af2.html

# ***************************************************************** 
# * CHECK common support
# ***************************************************************** 

# Country, cometition, industry
c1 <- CrossTable(data$foundfam_owned, data$compet_moder, na.rm=T )
c2 <- CrossTable(data$foundfam_owned, data$compet_strong, na.rm=T)

i <- CrossTable(data$foundfam_owned, data$industry, na.rm=T)
c <- CrossTable(data$foundfam_owned, data$countrycode, na.rm=T)


cbind(c1$prop.row, c2$prop.row, i$prop.row, c$prop.row)

# College Degree
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(degree_nm , na.rm=T), 
            max = max(degree_nm , na.rm=T),
            p1 = quantile(degree_nm , probs = 0.01, na.rm=T),
            p5 = quantile(degree_nm , probs = 0.05, na.rm=T),
            p95 = quantile(degree_nm , probs = 0.95, na.rm=T),
            q99 = quantile(degree_nm, probs = 0.99, na.rm=T),
            n = n())

# Employment
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(emp_firm , na.rm=T), 
            max = max(emp_firm , na.rm=T),
            p1 = quantile(emp_firm , probs = 0.01, na.rm=T),
            p5 = quantile(emp_firm, probs = 0.05, na.rm=T),
            p95 = quantile(emp_firm, probs = 0.95, na.rm=T),
            q99 = quantile(emp_firm, probs = 0.99, na.rm=T),
            n = n())

# * common support check passed
 