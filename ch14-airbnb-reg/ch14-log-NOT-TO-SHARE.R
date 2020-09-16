# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch14-airbnb-reg/"

# data used
data_in <- use_case_dir

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

options(digits = 3)



########################################
# PART I.
########################################

# !!! make sure you have run ch14_airbnb_prepare.R before


#############
# Load data #
#############

data <- read_csv(paste0(data_out, "airbnb_hackney_work.csv"))


# how it's done log

# redo features
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
    )

datau <- subset(data, price<=400)

# lnprice
g3b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b
save_fig("ch14-figure-3b-airbnb-lnprice", output, "small")

#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")
basic_log <- c("ln_accommodates", "ln_beds", "f_property_type", "f_room_type","ln_days_since", "flag_days_since")
poly_log <- c("ln_accommodates2","ln_days_since2","ln_days_since3")
# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type")
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_familykidfriendly")

# Additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(amenities, collapse=" + "),")"))

# Create models in logs, models: 1-8
modellog1 <- " ~ ln_accommodates"
modellog2 <- paste0(" ~ ",paste(basic_log,collapse = " + "))
modellog3 <- paste0(" ~ ",paste(c(basic_log, basic_add),collapse = " + "))
modellog4 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log),collapse = " + "))
modellog5 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1),collapse = " + "))
modellog6 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2),collapse = " + "))
modellog7 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities),collapse = " + "))
modellog8 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)

##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()

for (i in (1:8)){
  model_name <-  paste0("modellog",i)
  model_pretty_name <- paste0("(",i,")")

  yvar <- "ln_price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))

  # Initialize values
  rmse_train <- c()
  rmse_test <- c()

  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared

  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)

    # Criteria evaluation
    rmselog <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_train[k] <- mse_log(prediction_train, data_train[,yvar] %>% pull,rmselog)**(1/2)
    rmse_test[k] <- mse_log(prediction_test, data_test[,yvar] %>% pull,rmselog)**(1/2)
    

  }

  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model7_log <- model_results_cv[["modellog7"]][["model_work_data"]]

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                 "Test RMSE")

t14_2_log <- t1 %>%
  filter(grepl("log",model_name)) %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2_log) <- column_names
print(xtable(t14_2_log, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_log.tex"),include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

t1_logs <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

# FIXME: a problem
# Logged target variable
# model log 7 on log price
Ylog <- data_holdout[["ln_price"]]
#predictionlog_test <- predict(model7_log, newdata = data_holdout)
#predictionlog_test2 <- exp(predictionlog_test) * exp((rmselog)^2/2)
#d <- data.frame(ylev=Ylev, ylog=Ylog, predlev=predictionlev_holdout[,"fit"] , predlog=predictionlog_test2)
# repeat for log (not in book)
model_result_plot_logs <- ggplot(data = t1_logs, aes(x = factor(nvars), y = value, color=var, group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(34, 44), breaks = seq(34,44, 2)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()