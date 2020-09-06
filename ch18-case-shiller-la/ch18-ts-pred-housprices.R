################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 18
# CH18B Forecasting a house price index

# case-schiller-la dataset
# version 0.9 2020-09-06

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
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

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
#load data

data <- read_rds(paste0(data_in,"houseprices-data-1990-2018.rds")) %>%
  as.data.frame() %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

data <- data %>%
  filter(date>="2000-01-01" & date<"2018-01-01")
 # 18 years data
 # 1 year holdout
 # 4 years of test
 # 13 years of train (rolling window)

# pick if seasonal or non seasonal version used, will be cut later
# here we pick pn, not seasonally adjusted
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


#############################
# GRAPHS part 1
#############################



# Plot price index
price_index_plot <- ggplot(data = data, aes(x = date, y = p))+
  geom_line_da() +
  ylab("Case-shiller Price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(50,300), breaks = seq(50,300,50)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
price_index_plot
#save_fig("cs_tseries_p_R", output, "small")
save_fig("ch18-figure-8-cs-tseries-p", output, "small")





# Plot log difference of price index
dp_plot <- ggplot(data = data, aes(x = date, y = dp))+
  geom_line_da() +
  ylab("First difference of price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(-10,8), breaks = seq(-10,8,2)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
dp_plot
#save_fig("cs_tseries_dp_R", output, "small")


# Plot log difference of price index
dlnp_plot <- ggplot(data = data, aes(x = date, y = dlnp))+
  geom_line_da() +
  ylab("Log first difference of price index") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(-0.04,0.04), breaks = seq(-0.04,0.04,0.01)) +
  scale_x_date(date_breaks="2 years", labels = date_format("%b%Y")) +
  theme_bg()
dlnp_plot
#save_fig("cs_tseries_dlnp_R", output, "small")



#############################
# GRAPHS part 2
#############################

# Plot employment
emp_plot<-ggplot(data = data, aes(x = date, y = emp))+
  geom_line_da() +
  ylab("Employment (in thousands)") +
  xlab("Date (month)") +
#  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
emp_plot
#save_fig("cs_tseries_emp_R", output, "small")
save_fig("ch18-figure-10c-cs-tseries-emp", output, "small")


# Plot log diff employment
ldemp_plot<- ggplot(data = data, aes(x = date, y = dlnemp))+
  geom_line_da() +
  ylab("Log change in employment") +
  xlab("Date (month)") +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
ldemp_plot
#save_fig("cs_tseries_dlnemp_R", output, "small")
save_fig("ch18-figure-10d-cs-tseries-dlnemp", output, "small")


# Plot unemplyiment rate
u_plot<-ggplot(data = data, aes(x = date, y = u))+
  geom_line_da() +
  ylab("Unemployment rate (percent)") +
  xlab("Date (month)") +
#  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
u_plot
#save_fig("cs_tseries_u_R", output, "small")
save_fig("ch18-figure-10a-cs-tseries-u", output, "small")


# Plot diff unemployment
du_plot<- ggplot(data = data, aes(x = date, y = du))+
  geom_line_da() +
  ylab("Change in unemployment rate") +
  xlab("Date (month)") +
  scale_x_date(expand = c(0.01, 0.01),   breaks = as.Date(c("2000-01-01", "2003-01-01", "2006-01-01",  "2009-01-01", "2012-01-01", "2015-01-01", "2018-01-01")),  labels = date_format("%b%Y")) +
  theme_bg()
du_plot
#save_fig("cs_tseries_du_R", output, "small")
save_fig("ch18-figure-10b-cs-tseries-du", output, "small")



#############################
# Create train/houldout data
#############################

# Last year of data
data_holdout <- data %>%
  slice((n()-11):n())

# Rest of data for training
data_train <- data %>%
  slice(1:(n()-12))

# Prepare for cross-validation, define size of train
train_legth=13
data_tr <- data_train %>%
  slice(1:(n()-12)) %>% # last year of training data not used in any fold as training
  slide_tsibble(.size = train_legth*12, .step = 12)

data_cv_test <- data_train %>%
  slice(train_legth*12+1:n()) %>% 
  slide_tsibble(.size = 12, .step = 12) %>%
  select(trend, month)

#####################################
# Look at some TS regressions, tests
#####################################

# test unit root
data %>%
  features(p, unitroot_kpss)

data %>%
  features(lnp, unitroot_kpss)

# Look at model coefficients on train data to see if seasonality matters
fit <-data_train %>%
  model(
    m5 =  ARIMA(p ~ 1 + month +  PDQ(0,0,0))
  )
report(fit)

fit2 <-data_train %>%
  model(
    m5 =  ARIMA(dp ~  1 + month + PDQ(0,0,0))
  )
report(fit2)

# AR(1) is like simple regression
r<- lm(data=data, dlnp~ dlnp_lag)
summary(r)


#############################
# Fit ARIMA type models
#############################
# To cross-validate auto.arima,
# step 1: run it and find ARIMA specification on the whole train data, p,q chosen by BIC
# step 2: use the selected model as a candidate


#################
# TARGET: p
#################

m2_pre <- data_train %>%
  model(auto_arima = ARIMA(p ~  PDQ(0,0,0)))
p2_auto <- m2_pre$auto_arima[[1]]$fit$spec$p
q2_auto <- m2_pre$auto_arima[[1]]$fit$spec$q
d2_auto <- m2_pre$auto_arima[[1]]$fit$spec$d

m3_pre <- data_train %>%
  model(auto_arima = ARIMA(p ~ month+  PDQ(0,0,0)))
p3_auto <- m3_pre$auto_arima[[1]]$fit$spec$p
q3_auto <- m3_pre$auto_arima[[1]]$fit$spec$q
d3_auto <- m3_pre$auto_arima[[1]]$fit$spec$d

m4_pre <- data_train %>%
  model(auto_arima = ARIMA(p ~ month + trend + PDQ(0,0,0)))
p4_auto <- m4_pre$auto_arima[[1]]$fit$spec$p
q4_auto <- m4_pre$auto_arima[[1]]$fit$spec$q
d4_auto <- m4_pre$auto_arima[[1]]$fit$spec$d

m1_p_formula <- "p ~ month + trend"
m2_p_formula <- paste0("p ~  pdq(",paste(p2_auto,d2_auto,q2_auto, sep=","),") + PDQ(0,0,0)")
m3_p_formula <- paste0("p ~  pdq(",paste(p3_auto,d3_auto,q3_auto, sep=","),") + PDQ(0,0,0) + month")
m4_p_formula <- paste0("p ~  pdq(",paste(p4_auto,d4_auto,q4_auto, sep=","),") + PDQ(0,0,0) + month + trend")

m1_p <- TSLM(as.formula(m1_p_formula))
m2_p <-  ARIMA(as.formula(m2_p_formula))
m3_p <-  ARIMA(as.formula(m3_p_formula))
m4_p <-  ARIMA(as.formula(m4_p_formula))

models_p <- data_tr %>%
  model(m1_p = m1_p,
        m2_p = m2_p,
        m3_p = m3_p,
        m4_p = m4_p
        )

rmse_train_p <- models_p %>%
  get_RMSE_from_model()

forecast_p <- models_p %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(p_pred = .mean) %>%
  select(.id, .model, date, p_pred) %>%
  left_join(data[,c("date","p")]) %>%
  group_by(.id, .model) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

# Compute MSE for folds
summary_p <- forecast_p %>%
  get_MSE_from_forecast()

summary_p

#################
# TARGET: dp
#################


m12_pre <- data_train %>%
  model(auto_arima = ARIMA(dp ~ month +  PDQ(0,0,0)))
p12_auto <- m12_pre$auto_arima[[1]]$fit$spec$p
q12_auto <- m12_pre$auto_arima[[1]]$fit$spec$q
d12_auto <- m12_pre$auto_arima[[1]]$fit$spec$d

# Cross-validate models predicting first differences
m11_dp_formula <- "dp ~ month + trend"
m12_dp_formula <- paste0("dp ~ month + pdq(",paste(p12_auto,d12_auto,q12_auto, sep=","),") + PDQ(0,0,0)")

m11_dp <-  TSLM(as.formula(m11_dp_formula))
m12_dp <-  ARIMA(as.formula(m12_dp_formula))

models_dp <- data_tr %>%
  model(m11_dp = m11_dp,
        m12_dp = m12_dp)

rmse_train_dp <- models_dp %>%
  get_RMSE_from_model()

forecast_dp <- models_dp %>%
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
summary_dp <- forecast_dp %>%
  get_MSE_from_forecast()
summary_dp

#################
# TARGET: lnp
#################

m21_pre <- data_train %>%
  model(auto_arima = ARIMA(lnp ~  month + PDQ(0,0,0)))
p21_auto <- m21_pre$auto_arima[[1]]$fit$spec$p
q21_auto <- m21_pre$auto_arima[[1]]$fit$spec$q
d21_auto <- m21_pre$auto_arima[[1]]$fit$spec$d

m21_lnp_formula <- paste0("lnp ~ month + pdq(",paste(p21_auto,d21_auto,q21_auto, sep=","),") + PDQ(0,0,0)")
m21_lnp <-  ARIMA(as.formula(m21_lnp_formula))

models_lnp <- data_tr %>%
  model(m21_lnp = m21_lnp)

rmse_train_lnp <- models_lnp %>%
  get_RMSE_from_model()

forecast_lnp <- models_lnp %>%
  forecast(new_data = data_cv_test) %>%
  as_tsibble() %>%
  dplyr::rename(lnp_pred = .mean) %>%
  select(.id, .model, date, lnp_pred) %>%
  left_join(data[,c("date","p")]) %>%
  left_join(rmse_train_lnp) %>%
  group_by(.id, .model) %>%
  mutate(p_pred = exp(lnp_pred)*exp((RMSE**2)/2) ) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

# Compute MSE for folds
summary_lnp <- forecast_lnp %>%
  get_MSE_from_forecast()
summary_lnp


# VAR
var2_formula <- "vars(dp, du, dlnemp) ~ AR(1)"
var2 <- VAR(as.formula(var2_formula))

var21 <- data_tr %>%
  filter(!is.na(dp)) %>% # need to exclude first row
  model(var2 = var2)

rmse_train_var2 <- var21 %>%
  get_RMSE_from_model(resid_col_name = "dp")


forecast_var2 <- var21 %>%
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
summary_var2 <- forecast_var2 %>%
  get_MSE_from_forecast()
summary_var2


#############################
# COMPARISON
#############################

summary_folds <- bind_rows(list(summary_p, summary_dp, summary_lnp, summary_var2)) %>%
  spread(.id, MSE) %>%
  as.data.frame()
colnames(summary_folds) <- c("Model", paste0("Fold ", colnames(summary_folds)[-1]))

summary_final <- bind_rows(list(summary_p, summary_dp, summary_lnp, summary_var2)) %>%
  group_by(.model) %>%
  dplyr::summarise(CV_RMSE = sum(MSE/4)**0.5) %>%
  as.data.frame()

model_formulas <- summary_final %>%
  dplyr::pull(.model) %>%
  paste0("_formula") %>%
  sapply(FUN=get)

colnames(summary_final) <- c("Model", "CV RMSE")
summary_final <- summary_final %>%
  add_column("Model def" = model_formulas, .before = "CV RMSE")

summary_final

###########################x
# predict for holdout
###########################x
conf_level <-  80
conf_level_chr <- paste0(as.character(conf_level),"%")

# best model when p is target
# (need to adjust code if lnp is best)

best_arima_model_p <- "m4_p"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred, conf_level_chr) %>%
  unpack_hilo(conf_level_chr) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best_arima_p <- forecast_holdout_best_arima_p %>%
  get_MSE_from_forecast(groupby = c(".model"))

# if var is better
#  var

# var21 <- data_train %>%
#   filter(!is.na(dp)) %>% # need to exclude first row
#   model(var2 = var2)

# rmse_train_var2 <- var21 %>%
#   get_RMSE_from_model(resid_col_name = "dp", groupby = c(".model"))


# forecast_var2 <- var21 %>%
#   forecast(new_data = select(data_holdout, trend, month)) %>%
#   as_tsibble() %>%
#   rename(dp_pred = .mean_dp)  %>%
#   select(.model, date, dp_pred) %>%
#   left_join(data_holdout[,c("date","p","p_lag")]) %>%
#   mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
#   mutate(e = p - p_pred) %>%
#   ungroup()

# summary_holdout_var <- forecast_var2 %>%
#   get_MSE_from_forecast(groupby = c(".model"))
# summary_holdout_var


#############################
# GRAPHS part 3
#############################
# graph actual vs prediction from best arima
data_plot <- data %>%
  left_join(forecast_holdout_best_arima_p) %>%
  filter(year(date)>=2015)


pred_p_plot <- ggplot(data = data_plot , aes(x = as.Date(date), y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = as.Date(date), y = p_pred, color = "Prediction "),  size = 1) +
  #annotate("text", x = yearmonth("2017-08"), y = 257, label = "Prediction ", size=2.5, vjust = 2, color = color[2])+  
  #annotate("text", x = yearmonth("2017-03"), y = 258, label = "Actual", size=2.5, hjust = 1.5, color = color[1])+  
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
pred_p_plot
#save_fig("pred_p_mp_R", output, "small")
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
#save_fig("pred_p_mp_fan_R", output, "small")
save_fig("ch18-figure-9b-pred-p-mp-fan", output, "small")

###########################
# EXTERNAL VALIDITY
# do the prediction for an extra year
###########################


data <- read_rds(paste0(data_in,"houseprices-data-1990-2018.rds")) %>%
  as.data.frame() %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

data <- data %>%
  filter(date>="2000-01-01" & date<"2019-01-01")
# pick if seasonal or non seasonal version used, will be cut later
data <- data %>%
  mutate(
    p=pn,
    u=us,
    emp=emps,
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
  )%>%
  mutate(
    trend = 1:nrow(data),
    month = as.factor(month(date))
  )


# Last year of data
data_holdout<- data %>%
  slice((n()-11):n())

# Rest of data for training
data_train <- data %>%
  slice(1:(n()-12))


###########################x
# predict for holdout
###########################x

# best model when p is target
# (need to adjust code if lnp is best)

best_arima_model_p <- "m4_p"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best_arima_p <- forecast_holdout_best_arima_p %>%
  get_MSE_from_forecast(groupby = c(".model"))
summary_holdout_best_arima_p


#############################
# GRAPHS part 4
#############################
# graph from 2015-18, actual vs prediction from best arima


# best model when p is target
# (need to adjust code if lnp is best)

best_arima_model_p <- "m4_p"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(new_data = select(data_holdout, trend, month)) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = .mean)  %>%
  select(.model, date, p_pred, conf_level_chr) %>%
  unpack_hilo(conf_level_chr) %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best_arima_p <- forecast_holdout_best_arima_p %>%
  get_MSE_from_forecast(groupby = c(".model"))

# graph actual vs prediction from best arima
data_plot <- data %>%
  left_join(forecast_holdout_best_arima_p) %>%
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
#save_fig("pred_p_mp_fan2018_R", output, "small")
save_fig("ch18-figure-11-pred-p-mp-fan2018", output, "small")

#ch18-table-1-swim-rmse
#ch18-table-2-cs-models-rmse
#ch18-table-3-arima-folds