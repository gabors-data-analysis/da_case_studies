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

# CHAPTER 18
# CH18B Forecasting a home price index
# case-schiller-la dataset
# version 0.91 2020-01-08
#########################################################################################


###########

#
# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(fpp3)
library(cowplot)

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

data_in <- paste(data_dir,"case-shiller-la","clean/", sep = "/")
use_case_dir <- "ch18-case-shiller-la/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#############################
# RMSE functions
#############################

get_RMSE_from_model <- function(m, resid_col_name = ".resid", groupby = c(".id", ".model")){
  m %>%
    residuals() %>%
    as_tibble() %>%
    group_by_at(groupby) %>%
    summarise(RMSE = mean(get(resid_col_name)**2, na.rm = TRUE)**(1/2))
}

get_MSE_from_forecast <- function(forecast, groupby = c(".id", ".model")){
  forecast %>%
    as_tibble() %>%
    group_by_at(groupby) %>%
    summarise(MSE = mean(e^2)) %>%
    ungroup()
}


#############################
# DATA PREP
#############################
#load raw data

data <- read_csv(paste0(data_in,"homeprices-data-2000-2018.csv"))
data <- read_csv("https://osf.io/download/n3jty/")

# 18 years data
# 1 year holdout
# 4 years of test
# 13 years of train (rolling window)
#data <- data %>% mutate(date = yearmonth(date))
# pick if seasonal or non seasonal version used, will be cut later
# here we pick pn, not seasonally adjusted

data <- data %>% mutate(date = yearmonth(date))

data <- data %>%
  mutate(
    p=pn,
    u=us,
    emp=emps
  )

data <- data %>%
  mutate(
    dp   = difference(p, lag=1, order_by = date),
    p_lag = lag(p),
    lnp = log(p),
    dlnp   = difference(lnp, lag=1, order_by = date),
    lnp_lag = lag(lnp),
    dlnp_lag   = lag(dlnp),
    du     = difference(u, lag=1, order_by = date),
    lnemp = log(emp),
    dlnemp = difference(lnemp, lag=1, order_by = date)
  ) %>%
  mutate(
    trend = 1:nrow(data),
    month = as.factor(month(date))
  )

data <- data %>% as_tsibble(index=date)

# now save the workfile with data from 2000 through 2018
data %>% write_rds(paste(data_in,"case-shiller-workfile-2000-2018.rds",sep=""))


# and now create and save the workfile with data from 2000 through 2017
data <- data %>% filter(year <= 2017)

data %>% write_rds(paste(data_in,"case-shiller-workfile-2000-2017.rds",sep=""))


#############################
# EXPLORE
#############################

data <- read_rds(paste(data_in,"case-shiller-workfile-2000-2017.rds",sep=""))

# Last year of data
data_holdout <- data %>%
  slice((n()-11):n())

# Rest of data for work set
data_work <- data %>%
  slice(1:(n()-12))

# Prepare for cross-validation, define size of train
train_length=13
data_tr <- data_work %>%
  slice(1:(n()-12)) %>% # last year of training data not used in any fold as training
  slide_tsibble(.size = train_length*12, .step = 12)

data_cv_test <- data_work %>%
  slice(train_length*12+1:n()) %>% 
  slide_tsibble(.size = 12, .step = 12) %>%
  select(trend, month)


#############################
# GRAPH 18.8
# Plot price index
price_index_plot <- ggplot(data = data, aes(x = as.Date(date), y = p))+
  geom_line_da() +
  ylab("Case-shiller Price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(50,300), breaks = seq(50,300,50)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
price_index_plot
#save_fig("cs_tseries_p_R", output, "small")
save_fig("ch18-figure-8-cs-tseries-p", output, "small")


# additional graphs, not in textbook
# Plot log difference of price index
dp_plot <- ggplot(data = data, aes(x = as.Date(date), y = dp))+
  geom_line_da() +
  ylab("First difference of price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(-10,8), breaks = seq(-10,8,2)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
dp_plot

# Plot log difference of price index
dlnp_plot <- ggplot(data = data, aes(x = as.Date(date), y = dlnp))+
  geom_line_da() +
  ylab("Log first difference of price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(-0.04,0.04), breaks = seq(-0.04,0.04,0.01)) +
  scale_x_date(date_breaks="2 years", labels = date_format("%b%Y")) +
  theme_bg()
dlnp_plot

#############################
# GRAPH 18.10
#############################

# Plot employment
emp_plot<-ggplot(data = data, aes(x = as.Date(date), y = emp))+
  geom_line_da() +
  ylab("Employment (in thousands)") +
  xlab("Date (month)") +
  #  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
emp_plot
save_fig("ch18-figure-10c-cs-tseries-emp", output, "small")

# Plot log diff employment
ldemp_plot<- ggplot(data = data, aes(x = as.Date(date), y = dlnemp))+
  geom_line_da() +
  ylab("Log change in employment") +
  xlab("Date (month)") +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
ldemp_plot
save_fig("ch18-figure-10d-cs-tseries-dlnemp", output, "small")

# Plot unemployment rate
u_plot<-ggplot(data = data, aes(x = as.Date(date), y = u))+
  geom_line_da() +
  ylab("Unemployment rate (percent)") +
  xlab("Date (month)") +
  #  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
u_plot
save_fig("ch18-figure-10a-cs-tseries-u", output, "small")

# Plot diff unemployment
du_plot<- ggplot(data = data, aes(x = as.Date(date), y = du))+
  geom_line_da() +
  ylab("Change in unemployment rate") +
  xlab("Date (month)") +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
du_plot
save_fig("ch18-figure-10b-cs-tseries-du", output, "small")



##########################################################
# Create work set and houldout set
##########################################################


# Last year of data
data_holdout <- data %>%
  slice((n()-11):n())

# Rest of data for work set
data_work <- data %>%
  slice(1:(n()-12))

# Prepare for cross-validation, define size of train
train_legth=13
data_tr <- data_work %>%
  slice(1:(n()-12)) %>% # last year of training data not used in any fold as training
  slide_tsibble(.size = train_legth*12, .step = 12)

data_cv_test <- data_work %>%
  slice(train_legth*12+1:n()) %>% 
  slide_tsibble(.size = 12, .step = 12) %>%
  select(trend, month)


#############################################
# Use tseries of price index only
# Fit  models with months, trend, ARIMA
#############################################

# To cross-validate auto.arima,
# step 1: run it and find ARIMA specification on the whole train data, p,q chosen by BIC
#   note, need to add PDQ(0,0,0) to models 
#         in order to shut down the fancy seasonality-fitting part of auto ARIMA
# step 2: use the selected model as a candidate


# M1 p ~ month + trend, without any ARIMA
m1_formula <- "p ~ month + trend"
m1 <- TSLM(as.formula(m1_formula))

# M2 p ~ auto ARIMA
m2_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~  PDQ(0,0,0)))
p2_auto <- m2_pre$auto_arima[[1]]$fit$spec$p
q2_auto <- m2_pre$auto_arima[[1]]$fit$spec$q
d2_auto <- m2_pre$auto_arima[[1]]$fit$spec$d
m2_formula <- paste0("p ~  pdq(",paste(p2_auto,d2_auto,q2_auto, sep=","),") + PDQ(0,0,0)")
m2 <-  ARIMA(as.formula(m2_formula))

# M3 p ~ auto ARIMA + month
m3_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~ month+  PDQ(0,0,0)))
p3_auto <- m3_pre$auto_arima[[1]]$fit$spec$p
q3_auto <- m3_pre$auto_arima[[1]]$fit$spec$q
d3_auto <- m3_pre$auto_arima[[1]]$fit$spec$d
m3_formula <- paste0("p ~  pdq(",paste(p3_auto,d3_auto,q3_auto, sep=","),") + PDQ(0,0,0) + month")
m3 <-  ARIMA(as.formula(m3_formula))


# M4 p ~ auto ARIMA + month + trend
m4_pre <- data_work %>%
  model(auto_arima = ARIMA(p ~ month + trend + PDQ(0,0,0)))
p4_auto <- m4_pre$auto_arima[[1]]$fit$spec$p
q4_auto <- m4_pre$auto_arima[[1]]$fit$spec$q
d4_auto <- m4_pre$auto_arima[[1]]$fit$spec$d
m4_formula <- paste0("p ~  pdq(",paste(p4_auto,d4_auto,q4_auto, sep=","),") + PDQ(0,0,0) + month + trend")
m4 <-  ARIMA(as.formula(m4_formula))


# M5 dp ~ month + trend, without any ARIMA
m5_formula <- "dp ~ month + trend"
m5 <- TSLM(as.formula(m5_formula))

# M6 lnp ~ auto ARIMA + month
m6_pre <- data_work %>%
  model(auto_arima = ARIMA(lnp ~  month + PDQ(0,0,0)))
p6_auto <- m6_pre$auto_arima[[1]]$fit$spec$p
q6_auto <- m6_pre$auto_arima[[1]]$fit$spec$q
d6_auto <- m6_pre$auto_arima[[1]]$fit$spec$d
m6_formula <- paste0("lnp ~ month + pdq(",paste(p6_auto,d6_auto,q6_auto, sep=","),") + PDQ(0,0,0)")
m6 <-  ARIMA(as.formula(m6_formula))

###########################################
# create forecasts and cross-validate

# cross-validating M1-M4 with p on left-hand-side
models_1_4 <- data_tr %>%
  model(m1 = m1,
        m2 = m2,
        m3 = m3,
        m4 = m4
  )
rmse_train_1_4 <- models_1_4 %>%
  get_RMSE_from_model()
forecast_1_4 <- models_1_4 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(p_pred = .mean) %>%
  select(.id, .model, date, p_pred) %>%
  left_join(data[,c("date","p")]) %>%
  group_by(.id, .model) %>%
  mutate(e = p - p_pred) %>%
  ungroup()
# Compute MSE for folds
summary_1_4 <- forecast_1_4 %>%
  get_MSE_from_forecast()

# cross-validating M5 with dp on left-hand-side
model_5 <- data_tr %>%
  model(m5 = m5)
rmse_train_dp <- model_5 %>%
  get_RMSE_from_model()
forecast_5 <- model_5 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(dp_pred = .mean) %>%
  select(.id, .model, date, dp_pred) %>%
  left_join(data[,c("date","p","p_lag")]) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()
# Compute MSE for folds
summary_5 <- forecast_5 %>%
  get_MSE_from_forecast()


# cross-validating M6 with lnp on left-hand-side
model_6 <- data_tr %>%
  model(m6 = m6)
rmse_train_6 <- model_6 %>%
  get_RMSE_from_model()
forecast_6 <- model_6 %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(lnp_pred = .mean) %>%
  select(.id, .model, date, lnp_pred) %>%
  left_join(data[,c("date","p")]) %>%
  left_join(rmse_train_6) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = exp(lnp_pred)*exp((RMSE**2)/2) ) %>%
  mutate(e = p - p_pred) %>%
  ungroup()
# Compute MSE for folds
summary_6 <- forecast_6 %>%
  get_MSE_from_forecast()
summary_6


######################################
# Table 18.2
# average cv RMSE for models 1-6
######################################

summary_folds <- bind_rows(list(summary_1_4, summary_5, summary_6)) %>%
  spread(.id, MSE) %>%
  as.data.frame()
colnames(summary_folds) <- c("Model", paste0("Fold ", colnames(summary_folds)[-1]))

summary_final <- bind_rows(list(summary_1_4, summary_5, summary_6)) %>%
  group_by(.model) %>%
  dplyr::summarise(CV_RMSE = sum(MSE/4)**0.5) %>%
  as.data.frame()

model_formulas <- summary_final %>%
  dplyr::pull(.model) %>%
  paste0("_formula") %>%
  sapply(FUN=get)

colnames(summary_final) <- c("Model", "CV RMSE")
summary_table_18_2 <- summary_final %>%
  add_column("Model def" = model_formulas, .before = "CV RMSE")
summary_table_18_2


############################################
# VAR

# Comment: In the textbook, Table 18.3 has VAR RMSE values for the model without seasonality. 
# It’s noted at \url{https://gabors-data-analysis.com/errata/#part-iii} 
# Without seasonality, we have: RMSE (average) =8.0. With seasonality, we have: RMSE (average) =4.5. 
# In R we could do not figure out how to add seasonality. Let us know if you solved it...    


var_formula <- "vars(dp, du, dlnemp) ~ AR(1) "
var <- VAR(as.formula(var_formula))

# create forecast and cross-validate
var_data <- data_tr %>%
  filter(!is.na(dp)) %>% # need to exclude first row
  model(var = var)
rmse_train_var <- var_data %>%
  get_RMSE_from_model(resid_col_name = "dp")
forecast_var <- var_data %>%
  forecast(h=12) %>%
  as_tsibble() %>%
  dplyr::rename(dp_pred = .mean_dp) %>%
  select(.id, .model, date, dp_pred) %>%
  left_join(data[,c("date","p","p_lag")]) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()
# Compute MSE for folds
summary_var <- forecast_var %>%
  get_MSE_from_forecast()


##########################################
# TABLE 18.3 
# rmse by folds + cv rmse, for all 7 models
##########################################

summary_folds <- bind_rows(list(summary_1_4, summary_5, summary_6, summary_var)) %>%
  spread(.id, MSE) %>%
  as.data.frame()
colnames(summary_folds) <- c("Model", paste0("Fold ", colnames(summary_folds)[-1]))

# Table 18.3 RMSE by folds
summary_rmse_folds <- summary_folds %>%
  mutate_at(vars(-Model), sqrt)
summary_rmse_folds

# Table 18.3 last column: cv average RMSE
# create average MSE across folds and take square root
summary_cvavg <- bind_rows(list(summary_1_4, summary_5, summary_6, summary_var)) %>%
  group_by(.model) %>%
  dplyr::summarise(CV_RMSE = sum(MSE/4)**0.5) %>%
  as.data.frame()
model_formulas <- summary_cvavg %>%
  dplyr::pull(.model) %>%
  paste0("_formula") %>%
  sapply(FUN=get)
colnames(summary_cvavg) <- c("Model", "CV RMSE")
summary_table_18_3_lastcol <- summary_cvavg %>%
  add_column("Model def" = model_formulas, .before = "CV RMSE")
summary_table_18_3_lastcol



###########################x
# predict for holdout
###########################x
conf_level <-  80
conf_level_chr <- paste0(as.character(conf_level),"%")

# best model is M4
bestm <- "m4"

# re-estimate best models on full work set
model_best <- data_work %>%
  model(best = get(bestm))

rmse_train_best <- model_best %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best <- model_best %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred, conf_level_chr) %>%
  unpack_hilo(conf_level_chr) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best <- forecast_holdout_best %>%
  get_MSE_from_forecast(groupby = c(".model"))
summary_holdout_best


#############################
# GRAPHS 
#############################
# graph actual vs prediction from best arima
data_plot <- data %>%
  left_join(forecast_holdout_best) %>%
  filter(year(date)>=2015)


pred_p_plot <- ggplot(data = data_plot , aes(x = as.Date(date), y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = as.Date(date), y = p_pred, color = "Prediction "),  size = 1) +
  #annotate("text", x = yearmonth("2017-08"), y = 257, label = "Prediction ", size=2.5, vjust = 2, color = color[2])+  
  #annotate("text", x = yearmonth("2017-03"), y = 258, label = "Actual", size=2.5, hjust = 1.5, color = color[1])+  
  ylab("Case-Shiller Home Price Index") +
  xlab("Date (month)") +
  scale_color_manual(name="",values=c(color[1], color[2])) +
  scale_x_date(date_breaks="1 years", labels = date_format("%b%Y")) +
  theme_bg()+
  theme(legend.position=c(0.7,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
pred_p_plot
save_fig("ch18-figure-9a-pred-p-mp", output, "small")  


# with uncertainty fan
conf_level_lower <- paste0(conf_level_chr, "_lower")
conf_level_upper <- paste0(conf_level_chr, "_upper")
pred_p_mp_fan_R <- ggplot(data = data_plot , aes(x = as.Date(date), y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = as.Date(date), y = p_pred, color = "Prediction "),  size = 1) +
  geom_ribbon(aes(ymin =  get(conf_level_lower), ymax = get(conf_level_upper)), alpha=0.2,   bg=color[2]) +
  ylab("Case-Shiller Price index") +
  xlab("Date (month)") +
  scale_color_manual(name="",values=c(color[1], color[2])) +
  scale_x_date(date_breaks="1 years", labels = date_format("%b%Y")) +
  theme_bg()+
  theme(legend.position=c(0.7,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
pred_p_mp_fan_R
save_fig("ch18-figure-9b-pred-p-mp-fan", output, "small")

###########################
# EXTERNAL VALIDITY
# do the prediction for an extra year
###########################

data <- read_rds(paste(use_case_dir,"case-shiller-workfile-2000-2018.rds",sep=""))

# Last year of data
data_holdout<- data %>%
  slice((n()-11):n())

# Rest of data for work set
data_work <- data %>%
  slice(1:(n()-12))


###########################x
# predict for holdout
###########################x

# best model is M4
bestm <- "m4"

# re-estimate best model on full work set
model_best <- data_work %>%
  model(best = get(bestm))

rmse_train_best <- model_best %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_bes <- model_best %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best <- forecast_holdout_best %>%
  get_MSE_from_forecast(groupby = c(".model"))
summary_holdout_best


#############################
# GRAPH 18.11
# 2015-18, actual vs prediction from best arima

bestm <- "m4"

# re-estimate best models on full train set
model_best <- data_work %>%
  model(best = get(bestm))

rmse_train_best <- model_best %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best <- model_best %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred, conf_level_chr) %>%
  unpack_hilo(conf_level_chr) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best <- forecast_holdout_best %>%
  get_MSE_from_forecast(groupby = c(".model"))

# graph actual vs prediction from best arima
data_plot <- data %>%
  left_join(forecast_holdout_best) %>%
  filter(year(date)>=2015)

# with uncertainty fan
pred_p_mp_fan2018_R <- ggplot(data = data_plot , aes(x = as.Date(date), y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = as.Date(date), y = p_pred, color = "Prediction "),  size = 1) +
  geom_ribbon(aes(ymin =  get(conf_level_lower), ymax = get(conf_level_upper)), alpha=0.2,   bg=color[2]) +
  ylab("Case-Shiller Price index") +
  xlab("Date (month)") +
  scale_color_manual(name="", values=c(color[1], color[2])) +
  scale_x_date(date_breaks="1 years", labels = date_format("%b%Y")) +
  theme_bg()+
  theme(legend.position=c(0.7,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
pred_p_mp_fan2018_R
save_fig("ch18-figure-11-pred-p-mp-fan2018", output, "small")

