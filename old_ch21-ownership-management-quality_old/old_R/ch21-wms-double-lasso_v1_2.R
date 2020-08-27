######################################################################
#
# Data Analysis Textbook
# Case Study for Chapter 21 - Methods for Uncovering effects in observational data
# Data : wms-management
# Using World Management Survey



# v1.0. 2018
# v1.1. 2019-12-23 doing it by hand
# v1.2. 2019-12-24 adding diagnostics, rlassoEffects
#
######################################################################

# ***************************************************** 
#   ** Baseline regressions and Double Lasso
# ***************************************************** 

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stargazer)
library(hdm)

# Change working directory -------------------------------------------
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
#dir <- "/Users/zholler/Documents/Private/da_case_studies/ch21/wms-management-survey/"

#location folders
data_in <- paste0(dir,"da_case_studies/ch21-ownership-management-quality/")
data_out <- paste0(dir,"da_case_studies/ch21-ownership-management-quality/")
output <- paste0(dir,"da_case_studies/ch21-ownership-management-quality/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

# load ggplot theme function
source(paste0(func, "theme_bg.R"))


# Set location folders -----------------------------------------------
func <- paste0(dir, "ch00_tech_prep/")



# Load in data -------------------------------------------------------
data <- read_rds(paste0(data_out, "Ch21_wms_workfile_xsec.rds"))

data %>% 
  group_by(foundfam_owned) %>% 
  summarise (mean(management))

# Set variables to use -------------------------------------------------------

y_var <- "management"
x_var <- "foundfam_owned"

control_vars <- c("degree_nm", "degree_nm_sq", "compet_moder", "compet_strong", 
                  "lnemp", "age_young", "age_old", "age_unknown")
control_vars_dl <- c("degree_nm", "degree_nm_sq", "compet_moder", "compet_strong")
control_vars_to_interact <- c("industry", "countrycode")

# OLS with all control vars -------------------------------------------------------

formula1 <- as.formula(paste0(y_var, " ~ ",x_var," + ", 
                  paste(c(control_vars, control_vars_to_interact), collapse = " + ")))

ols1 <- lm(formula1, data=data)
ols1_ncovariates = length(ols1$coefficients)-2
summary(ols1)


# OLS with all controls + interactions -------------------------------------------------------

formula2 <- as.formula(paste(y_var, " ~ ",x_var," + ", paste(control_vars_to_interact, collapse = ":"), 
                 " + (", paste(control_vars, collapse = "+"),")*(",
                 paste(control_vars_to_interact, collapse = "+"),")",sep=""))

ols2 <- lm(formula2, data=data)
ols2_ncovariates = length(ols2$coefficients)-2
summary(ols2)


# Double Lasso -------------------------------------------------------

set.seed(61435)

ds_forula <- as.formula(paste(" ~ ",x_var," + ", paste(control_vars_to_interact, collapse = ":"), 
                             " + (", paste(control_vars_dl, collapse = "+"),")*(",
                             paste(control_vars_to_interact, collapse = "+"),")",sep=""))
X_matrix <- model.matrix(ds_forula,data=data)[,-1]
y <- data[, y_var]

# First step
lasso1 = rlasso(management~., data = as.data.frame(cbind(y,X_matrix)), 
                post = T, intercept = T) 
selected1 = which(coef(lasso1)[-c(1)] !=0) 
selected1


# Second step
lasso2 = rlasso(foundfam_owned~., data = as.data.frame(X_matrix), 
                post = T, intercept = T) 
selected2 = which(coef(lasso2)[-c(1)] !=0) 
selected2


# create mix of vars
selected <- unique(c(names(selected1),names(selected2)))
selected

formula = paste(c("management ~ foundfam_owned", selected), collapse = "+")
DS = lm(formula, data = as.data.frame(cbind(y,X_matrix)))
summary(DS)



# Output results -------------------------------------------------------

stargazer(ols1, ols2, DS, out=paste0(output, "DS_tab.tex"), digits=2, float = F, no.space = T, 
          keep=c("foundfam_owned"), column.labels   = c("OLS", "DS"),column.separate = c(2, 1),
          add.lines = list(
            c("N. of covariates",ols1_ncovariates,ols2_ncovariates,length(selected) )))



##############################################x
# Diagnostics
# for checking do OLS and also save lasso

selected_lasso1 <- unique(c(names(selected1)))
formula = paste(c("management ~ ", selected_lasso1), collapse = "+")
ols_lasso1 = lm(formula, data = as.data.frame(cbind(y,X_matrix)))

selected_lasso2 <- unique(c(names(selected2)))
formula = paste(c("management ~ ", selected_lasso2), collapse = "+")
ols_lasso2 = lm(formula, data = as.data.frame(cbind(y,X_matrix)))


# repeat ols for x
formula1a <- as.formula(paste0(x_var, " ~ "," + ", 
                               paste(c(control_vars, control_vars_to_interact), collapse = " + ")))
olsx1 <- lm(formula1a, data=data)
olsx1_ncovariates = length(olsx1$coefficients)-2
summary(olsx1)


formula2a <- as.formula(paste(x_var, " ~ "," + ", paste(control_vars_to_interact, collapse = ":"), 
                              " + (", paste(control_vars, collapse = "+"),")*(",
                              paste(control_vars_to_interact, collapse = "+"),")",sep=""))
olsx2 <- lm(formula2a, data=data)
olsx2_ncovariates = length(olsx2$coefficients)-2
summary(olsx2)

stargazer(ols1, ols2, ols_lasso1, olsx1, olsx2 , ols_lasso2, DS, out=paste0(output, "DS_tab_all3.html"), digits=2, float = F, no.space = T, 
          column.labels   = c("OLS y","Lasso y", "OLS x", "Lasso x", "DS"),column.separate = c(2, 1,2, 1,1),
          add.lines = list(
            c("N. of covariates",ols1_ncovariates,ols2_ncovariates,length(ols_lasso1), olsx1_ncovariates,olsx2_ncovariates, length(ols_lasso2), length(selected) )))


####################################
# double lasso described here itt https://cran.r-project.org/web/packages/hdm/vignettes/hdm.pdf --> 4.2 examples
# rlassoeffects(https://github.com/cran/hdm/blob/master/R/rlassoEffects.R)--> also runs rlasso, slightly differently


# two v similar versions fro Chernuzhukov et al, they also say it's similar, and indeed. 

Eff = rlassoEffect(X_matrix[, -1], y, X_matrix[, 1], method = "double selection")
summary(Eff)$coef[, 1:2]
Eff2 = rlassoEffect(X_matrix[, -1], y, X_matrix[, 1], method = "partialling out")
summary(Eff2)$coef[, 1:2]

# very similar if not exactly the same results
summary(DS)$coefficients[2, 1:2]
