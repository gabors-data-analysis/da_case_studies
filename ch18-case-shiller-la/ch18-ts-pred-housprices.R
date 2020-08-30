######################################################################
# Data Analysis Textbook
# Chapter 18
# Case study  Forecasting a house price index in LA
# Data : Case Shiller

# v1.0 2019.01.01
# v1.1 2019-01-09
# v1.2 2019-12-29 folders edited

# v2.0 - 2020.01.11 overhaul to tstibble + fable
# v2.1 - 2020.01.21 add many models + extra holdout
# v2.2 - 2020.01.23 price index nsa
# v2.3 - 2020.01.24 models finalized
# v2.4 - 2020.01.26 pred graph runs from 2013
# v2.5 - 2020.02.02 fan graph  + add RMSE fn
# v2.6 - 2020.02.04 corrected emo graph
# v1.1 2020-04-22 names ok

######################################################################

# LXXRSNA - Los Angeles Home Price Index NSA
# CAUR - California Unemployment Rate SA
# CANA - All Employees: Total Nonfarm in California SA

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(tidyverse)
library(fpp3)

# Check working directory --------------------------------------------
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


# Set location folders -----------------------------------------------
data_in <- paste0(dir,"da_data_repo/case-shiller-la/clean/")
data_out <- paste0(dir,"da_case_studies/ch18_case-shiller-la/")
output <- paste0(dir,"da_case_studies/ch18_case-shiller-la/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))

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

data <- read_rds(paste0(data_in,"houseprices-data-1990-2018.rds"))
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
    m5 =  ARIMA(p ~ 1 +season() +  PDQ(0,0,0))
  )
report(fit)

fit2 <-data_train %>%
  model(
    m5 =  ARIMA(dp ~  1 + season()+ PDQ(0,0,0))
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
  model(auto_arima = ARIMA(p ~ season()+  PDQ(0,0,0)))
p3_auto <- m3_pre$auto_arima[[1]]$fit$spec$p
q3_auto <- m3_pre$auto_arima[[1]]$fit$spec$q
d3_auto <- m3_pre$auto_arima[[1]]$fit$spec$d

m4_pre <- data_train %>%
  model(auto_arima = ARIMA(p ~ season() + trend()+ PDQ(0,0,0)))
p4_auto <- m4_pre$auto_arima[[1]]$fit$spec$p
q4_auto <- m4_pre$auto_arima[[1]]$fit$spec$q
d4_auto <- m4_pre$auto_arima[[1]]$fit$spec$d


m1_p_formula <- "p ~ season() + trend()"
m2_p_formula <- paste0("p ~  pdq(",paste(p2_auto,d2_auto,q2_auto, sep=","),") + PDQ(0,0,0)")
m3_p_formula <- paste0("p ~  pdq(",paste(p3_auto,d3_auto,q3_auto, sep=","),") + PDQ(0,0,0) + season()")
m4_p_formula <- paste0("p ~  pdq(",paste(p4_auto,d4_auto,q4_auto, sep=","),") + PDQ(0,0,0) + season() + trend()")

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
  forecast(h=12) %>%
  as_tsibble() %>%
  dplyr::rename(p_pred = p) %>%
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
  model(auto_arima = ARIMA(dp ~ season()+  PDQ(0,0,0)))
p12_auto <- m12_pre$auto_arima[[1]]$fit$spec$p
q12_auto <- m12_pre$auto_arima[[1]]$fit$spec$q
d12_auto <- m12_pre$auto_arima[[1]]$fit$spec$d

# Cross-validate models predicting first differences
m11_dp_formula <- "dp ~ season() + trend()"
m12_dp_formula <- paste0("dp ~ season()+ pdq(",paste(p12_auto,d12_auto,q12_auto, sep=","),") + PDQ(0,0,0)")

m11_dp <-  TSLM(as.formula(m11_dp_formula))
m12_dp <-  ARIMA(as.formula(m12_dp_formula))

models_dp <- data_tr %>%
  model(m11_dp = m11_dp,
        m12_dp = m12_dp)

rmse_train_dp <- models_dp %>%
  get_RMSE_from_model()

forecast_dp <- models_dp %>%
  forecast(h=12) %>%
  as_tsibble() %>%
  dplyr::rename(dp_pred = dp) %>%
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
  model(auto_arima = ARIMA(lnp ~  season()+ PDQ(0,0,0)))
p21_auto <- m21_pre$auto_arima[[1]]$fit$spec$p
q21_auto <- m21_pre$auto_arima[[1]]$fit$spec$q
d21_auto <- m21_pre$auto_arima[[1]]$fit$spec$d

m21_lnp_formula <- paste0("lnp ~ season()+ pdq(",paste(p21_auto,d21_auto,q21_auto, sep=","),") + PDQ(0,0,0)")
m21_lnp <-  ARIMA(as.formula(m21_lnp_formula))

models_lnp <- data_tr %>%
  model(m21_lnp = m21_lnp)

rmse_train_lnp <- models_lnp %>%
  get_RMSE_from_model()

forecast_lnp <- models_lnp %>%
  forecast(h=12) %>%
  as_tsibble() %>%
  dplyr::rename(lnp_pred = lnp) %>%
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
  dplyr::rename(dp_pred = dp)  %>%
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
  dplyr::summarise(CV_RMSE = sum(MSE)**0.5) %>%
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

# best model when p is target
# (need to adjust code if lnp is best)

best_arima_model_p <- "m4_p"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(h=12) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = p)  %>%
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
#
# rmse_train_var2 <- var21 %>%
#   get_RMSE_from_model(resid_col_name = "dp")
#
#
# forecast_var2 <- var21 %>%
#   forecast(h=12) %>%
#   as_tsibble() %>%
#   rename(dp_pred = dp)  %>%
#   left_join(data_holdout[,c("date","p","p_lag")]) %>%
#   group_by(.id, .model) %>%
#   mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %>%
#   mutate(e = p - p_pred) %>%
#   ungroup()
#
# summary_holdout_var <- forecast_var2 %>%
#   get_MSE_from_forecast(groupby = c(".model"))
# summary_holdout_var


#############################
# GRAPHS part 3
#############################
# graph actual vs prediction from best arima
conf_level_chr <- paste0(as.character(conf_level),"%")
data_plot <- data %>%
  left_join(unnest(forecast_holdout_best_arima_p[,c("date","p_pred",conf_level_chr)])) %>%
  filter(year(date)>=2015)


pred_p_plot <- ggplot(data = data_plot , aes(x = date, y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = date, y = p_pred, color = "Prediction "),  size = 1) +
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
pred_p_mp_fan_R <- ggplot(data = data_plot , aes(x = date, y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = date, y = p_pred, color = "Prediction "),  size = 1) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha=0.2,   bg=color[2]) +
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


data <- read_rds(paste0(data_in,"houseprices-data-1990-2018.rds"))
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

best_arima_model_p <- "m4"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(h=12) %>%
  as_tsibble() %>%
  rename(p_pred = p)  %>%
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


##
conf_level <-  80

# best model when p is target
# (need to adjust code if lnp is best)

best_arima_model_p <- "m4_p"

# re-estimate best models on full train set
models_best_arima_p <- data_train %>%
  model(best = get(best_arima_model_p))

rmse_train_best_p <- models_best_arima_p %>%
  get_RMSE_from_model(groupby = c(".model"))

forecast_holdout_best_arima_p <- models_best_arima_p %>%
  forecast(h=12) %>%
  hilo(level = c(conf_level)) %>%
  as_tsibble() %>%
  rename(p_pred = p)  %>%
  left_join(data_holdout[,c("date","p")]) %>%
  mutate(e = p - p_pred) %>%
  ungroup()

summary_holdout_best_arima_p <- forecast_holdout_best_arima_p %>%
  get_MSE_from_forecast(groupby = c(".model"))

# graph actual vs prediction from best arima
conf_level_chr <- paste0(as.character(conf_level),"%")
data_plot <- data %>%
  left_join(unnest(forecast_holdout_best_arima_p[,c("date","p_pred",conf_level_chr)])) %>%
  filter(year(date)>=2015)


# with uncertainty fan
pred_p_mp_fan2018_R <- ggplot(data = data_plot , aes(x = date, y = p))+
  geom_line(size = 0.8, aes(color = "Actual")) +
  geom_line(aes(x = date, y = p_pred, color = "Prediction "),  size = 1) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha=0.2,   bg=color[2]) +
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