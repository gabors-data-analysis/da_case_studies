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
# CH23A Import demand and industrial production
# using the asia-industry dataset
# version 0.9 2020-12-09
#########################################################################################


# clear environment
rm(list=ls())

# Libraries
library(tidyverse)
library(haven)
library(estimatr)
library(huxtable)
library(Hmisc)
library(modelsummary)

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3)

data_in <- paste(data_dir,"asia-industry","clean/", sep = "/")
use_case_dir <- "ch23-import-demand-and-production/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



# ------------------------------------------------------------------------------------------------------



# import data
# raw <- read.dta13(paste0(data_in,'asia-industry_tidy.dta'))
raw <- read_dta(paste(data_in, 'asia-industry_tidy.dta', sep = ""))

data <- raw %>%
  filter(year >= 1998) %>%
  filter(!(year==1998 & month==1)) %>%
  filter(!(year==2018 & month > 4))

data %>%
  group_by(countrycode) %>%
  dplyr::mutate(mean=mean(ind_prod_const), 
                std.dev=sd(ind_prod_const),
                freq = n()) %>%
  drop_na() %>%
  mutate(mean=formatC(mean, format = 'e', digits=3),
         std.dev=formatC(std.dev, format = 'e', digits=3)) %>% # scientific format
  arrange(countrycode) %>%
  distinct(countrycode, mean, std.dev, freq) %>%
  dplyr::select(countrycode, mean, std.dev, freq)


# feature engineering
data <- data %>%
  group_by(time) %>%
  mutate(temp1 = ifelse(countrycode=='USA',ind_prod_const_sa, 0), 
         usa_ip_sa = max(temp1), 
         temp2 = ifelse(countrycode == 'CHN', ind_prod_const_sa, 0), 
         chn_ip_sa = max(temp2)) %>%
  dplyr::select(-temp1, -temp2)

data <- data %>%
  mutate(ln_ip = log(ind_prod_const_sa),
         ln_usa_ip=log( usa_ip_sa),
         ln_chn_ip= log( chn_ip_sa),
         ln_usa_imports= log (usa_imp_sa),
         ln_er_usd= log(exchnage_rate_vs_usd)) %>%
  filter(!is.na(ln_ip))

# keep countries of choice
data <-  data %>%
  filter(countrycode == 'MYS' | countrycode == 'PHL' | countrycode == 'SGP' | countrycode == 'THA')

data %>%
  group_by(countrycode) %>%
  count()

# panel setup
data <- data %>%
  mutate(
    cc= factor(countrycode, levels = c("THA", "MYS", "PHL", "SGP"), labels = c("Thailand" ,"Malaysia", "Philippines", "Singapore")),
    date = lubridate::make_date(year, month, 1)
  )

# lagged variables
work <- data %>%
  arrange(cc, date)  %>%
  group_by(cc) %>%
  mutate(
    dln_ip = ln_ip - lag(ln_ip),
    dln_usa_ip = ln_usa_ip - lag(ln_usa_ip),
    dln_chn_ip = ln_chn_ip - lag(ln_chn_ip),
    dln_usa_imports = ln_usa_imports - lag(ln_usa_imports),
    ddln_usa_imports = dln_usa_imports - lag(dln_usa_imports),
    dln_er_usd = ln_er_usd - lag(ln_er_usd)
  ) %>%
  ungroup()

work %>%
  group_by(countrycode) %>%
  dplyr::summarise(mean=mean(dln_ip, na.rm = TRUE))


work %>%
  filter(countrycode == 'THA') %>%
  group_by(year) %>%
  count()  %>%
  ungroup()


work %>%
  ungroup() %>%
  filter(countrycode == 'THA') %>%
  dplyr::summarise(mean = mean(dln_ip, na.rm =TRUE))


# create TS graphs
lnip_THA <- ggplot(data=work, aes(x=as.Date(date), y=ln_ip))+
  geom_line(data=subset(work[work$country=='Thailand', ]), color=color[1], size=0.4) +
  theme_bg() +
  xlab("Date (month)") +
  ylab("ln(Thai industrial production, bn US dollars)") +
  coord_cartesian(ylim=c(22.4, 23.4), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2008-09 crisis', y=22.7, x=as.Date('2008-07-01 UTC'), angle=90) +
  geom_vline(xintercept = as.Date('2011-10-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2011-12 flood', y=22.68, x=as.Date('2011-06-01 UTC'), angle=90)
lnip_THA
#save_fig("ch23_lnip_THA", output, "small")
save_fig("ch23-figure-1a-thai-lnip", output, "small")

dlnip_THA <- ggplot(data=work, aes(x=as.Date(date), y=dln_ip))+
  geom_line(data=subset(work[work$country=='Thailand', ]), color=color[1], size=0.4) +
  theme_bg() +
  xlab("Date (month)") +
  ylab('Change ln(Thai industrial prod., bn US dollars)') +
  coord_cartesian(ylim=c(-0.3, 0.2), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3],  label='2008-09 crisis', y=-0.2, x=as.Date('2008-12-01 UTC'), angle=90) +
  geom_vline(xintercept = as.Date('2011-10-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3],  label='2011-12 flood', y=-0.2, x=as.Date('2011-05-01 UTC'), angle=90)
dlnip_THA
save_fig("ch23_dlnip_THA", output, "small")


lnusaimp <- ggplot(data=work, aes(x=as.Date(date), y=ln_usa_imports))+
  geom_line_da() +
  theme_bg() +
  xlab("Date (month)") +
  ylab("ln(USA imports, bn US dollars)") +
  scale_y_continuous(breaks=c(seq(11.2, 12.2, 0.2))) +
  coord_cartesian(ylim=c(11.2, 12.25), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text',size=2, color=color[3], label='2008-09 crisis', y=11.5, x=as.Date('2008-07-01 UTC'), angle=90)
lnusaimp
#save_fig("ch23_lnusaimp", output, "small")
save_fig("ch23-figure-1b-usa-lnimp", output, "small")



dlnusaimp <- ggplot(data=work, aes(x=as.Date(date), y=dln_usa_imports))+
  geom_line_da() +
  theme_bg() +
  xlab("Date (month)") +
  ylab('Change in ln(USA imports, bn US dollars)') +
  coord_cartesian(ylim=c(-0.15, 0.1), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2008-09 crisis', y=-0.10, x=as.Date('2008-06-01 UTC'), angle=90)
dlnusaimp
#save_fig("ch23_dlnusaimp", output, "small")

# REGRESSIONS

# Serial correlation matters because it may lead to biased standard error estimates.
# We recommended two ways to address this problem: estimate Newey–West standard errors
# or include the lag of the dependent variable in the regression. (Used here.)


# Thailand
lags_helper <- paste(paste0("lag(dln_usa_imports,", c(0:4), ")"), collapse = " + ")
lags_helper2 <- paste(paste0("lag(dln_ip,", c(1:2), ")"), collapse = " + ")

thai_formula <- as.formula(paste0("dln_ip ~ ", lags_helper, " + ",lags_helper2))

thai_reg <- lm(thai_formula, data = filter(work, countrycode=="THA"))

# long-term coeff, lagged dy, countries separately
lags_helper <- paste(paste0("lag(ddln_usa_imports,", c(0:3), ")"), collapse = " + ")
lt_formula <- as.formula(paste0("dln_ip ~ lag(dln_usa_imports, 4) + ", lags_helper, " + lag(dln_ip, 1)"))

tha_reg_lt <- lm(lt_formula, data = filter(work, countrycode=="THA"))
mys_reg_lt <- lm(lt_formula, data = filter(work, countrycode=="MYS"))
phl_reg_lt <- lm(lt_formula, data = filter(work, countrycode=="PHL"))
sgp_reg_lt <- lm(lt_formula, data = filter(work, countrycode=="SGP"))

# long-term coeff, lagged dy, countries pooled
lt_formula_pooled <- as.formula(paste0("dln_ip ~ lag(dln_usa_imports, 4) + ", lags_helper, " + lag(dln_ip, 1) + cc"))
pooled_reg_lt <- lm(lt_formula_pooled, data = work)


#ch23-table-1-asia-reg
huxreg(
  list(
    "Thailand" = tha_reg_lt, 
    "Malaysia" = mys_reg_lt, 
    "Philippines" = phl_reg_lt,
    "Singapore" = sgp_reg_lt,
    "Pooled" = pooled_reg_lt
  ), 
  statistics = c(Observations = "nobs", R2 = "r.squared"), 
  coefs = c(
    "USA imports log change, cumulative coeff." = "lag(dln_usa_imports, 4)", 
    "Industrial production log change, lag" = "lag(dln_ip, 1)", 
    "Malaysia" = "ccMalaysia", 
    "Philippines" = "ccPhilippines", 
    "Singapore" = "ccSingapore", 
    "Constant" = "(Intercept)"  ))



