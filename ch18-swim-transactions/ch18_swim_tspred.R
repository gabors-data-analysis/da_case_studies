
############################################################
#
# DATA ANALYSIS TEXTBOOK
# Chapter 18 time series - swim ticket sales

# version 1.2 2009-09-04
# version 1.3 2019-12-28 edits
# version 1.4 2020-01-23 substantial change
# version 1.5 2020-01-25 CV added
# version 1.6 2020-01-26 graphics changes
# version 1.7 2020-02-02 new graph with names, minor changes in graphs
# version 1.8 2020-02-12 starts from daily aggregates
# version 1.9 2020-04-21 save fig, graph edits, adds TODO
# v2.0 2020-04-22 names ok
# v2.1 2020-04-26 grapg edits
# v2.2 2020-04-28 date labels edited
# v2.3 2020-04-30 date labels edited
# v2.4 2020-08-24 library eidts
# v2.5 2020-08-25 data soure B 


# WHAT THIS CODES DOES:
#
# Sets up models

#
###########################################################

# It is advised to start a new session for every case study
# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(tidyverse)
library(stargazer)
library(Hmisc)
library(timeDate)
library(lubridate)
library(caret)
library(prophet)



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

data_in <- paste(data_dir,"swim-transactions","clean/", sep = "/")

use_case_dir <- "ch18-swim-transactions/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)




#####################################
# Creating time features  ----------
#####################################


#import data
daily_agg<-read.csv(file = paste(data_in,"swim_work.csv",sep="")) %>% 
  mutate(date = as.Date(date))

# dow: 1=Monday, weekend: Sat and Sun.
daily_agg <- daily_agg %>%
  mutate(year = year(date),
         quarter = quarter(date),
         month = factor(month(date)),
         day = day(date)) %>%
  mutate(dow = factor(lubridate::wday(date, week_start = getOption("lubridate.week.start", 1)))) %>%
  mutate(weekend = factor(as.integer(dow %in% c(6,7))))


daily_agg <- daily_agg %>% 
  mutate(school_off = ((day>15 & month==5 & day <=30) | (month==6 |  month==7) |
                         (day<15 & month==8) | (day>20 & month==12) ))

daily_agg <- daily_agg %>% 
  mutate(trend = c(1:dim(daily_agg)[1]))

summary(daily_agg$QUANTITY)



# Get holiday calendar ----------------------------------

holidays <-  as.Date(holidayNYSE(2010:2017))
  
daily_agg <- daily_agg %>% 
  mutate(isHoliday = ifelse(date %in% holidays,1,0))

Hmisc::describe(daily_agg)

# Define vars for analysis ----------------------------------

daily_agg <- 
  daily_agg %>% 
  group_by(month) %>% 
  mutate(q_month = mean(QUANTITY)) %>% 
  ungroup()

daily_agg <- daily_agg %>% 
  mutate(QUANTITY2 = ifelse(QUANTITY<1, 1, QUANTITY)) %>% 
  mutate(q_ln = log(QUANTITY2))

daily_agg <- 
  daily_agg %>% 
  group_by(month, dow) %>% 
  mutate(tickets = mean(QUANTITY),
         tickets_ln = mean(q_ln)) %>% 
  ungroup()

# named date vars for graphs
mydays <- c("Mon","Tue","Wed",
            "Thu","Fri","Sat",
            "Sun")
daily_agg$dow_abb   <-factor(   mydays[daily_agg$dow],  levels=mydays)
daily_agg$month_abb <-factor(month.abb[daily_agg$month],levels=month.abb)

################################
# Descriptive graphs ----------
#################################


g1 <-ggplot(data=daily_agg[daily_agg$year==2015,], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.4, color=color[1]) +
  theme_bg() +
  scale_x_date(breaks = as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" ) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")
g1
#save_fig("Ch18_swimmingpool_2015", output, "small")
save_fig("ch18-figure-3a-swimmingpool-2015", output, "small")

g2<-ggplot(data=daily_agg[(daily_agg$year>=2010) & (daily_agg$year<=2014),], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.2, color=color[1]) +
  theme_bg() +
  scale_x_date(breaks = as.Date(c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01")),
               labels = date_format("%d%b%Y"),
               minor_breaks = "3 months") +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")
g2
save_fig("ch18-figure-3b-swimmingpool-2010-2014", output, "small")


g3<-ggplot(data=daily_agg, aes(x=month_abb, y=QUANTITY)) +
  theme_bg() +
  labs( x = "Date (month)", y="Daily ticket sales" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.4)
g3
#save_fig("Ch18_swimmingpool_monthly", output, "small")
save_fig("ch18-figure-4a-swimmingpool-monthly", output, "small")

g4<-ggplot(data=daily_agg, aes(x=dow_abb, y=QUANTITY)) +
  theme_bg() +
  labs( x = "Day of the week", y="Daily ticket sales" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.4)
  #geom_boxplot(color=color[1], outlier.shape = NA)

g4
#save_fig("Ch18_swimmingpool_dow", output, "small")
save_fig("ch18-figure-4b-swimmingpool-dow", output, "small")

# to check for interactions, look at the heatmap
swim_heatmap <- 
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = tickets)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of the week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_bg() +
  theme(legend.position = "right",
    legend.text = element_text(size=6),
    legend.title =element_text(size=6)
    )
swim_heatmap

#save_fig("ch18_swim_heatmap", output, "large")
save_fig("ch18-figure-5-swim-heatmap", output, "large")

swim_heatmap_log <-
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = tickets_ln)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_bg()  
swim_heatmap_log
#not saved

#####################################
# PREDICTION  ----------
#####################################


#############################
# Create train/houldout data
#############################

# Last year of data
data_holdout<- daily_agg %>%
  filter(year==2016)

# Rest of data for training
data_train <- daily_agg %>%
  filter(year<2016)

# Prepare for cross-validation
data_train <- data_train %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.integer(rowname))

test_index_list <- data_train %>% 
  split(f = factor(data_train$year)) %>% 
  lapply(FUN = function(x){x$rowname})
  
train_index_list <- test_index_list %>% 
  lapply(FUN = function(x){setdiff(data_train$rowname, x)})
  
train_control <- trainControl(
  method = "cv",
  index = train_index_list, #index of train data for each fold
  # indexOut = index of test data for each fold, complement of index by default
  # indexFinal = index of data to use to train final model, whole train data by default
  savePredictions = TRUE
)

# Fit models ---------------------------------------------------------

#Model 1 linear trend + monthly seasonality
model1 <- as.formula(QUANTITY ~ 1 + trend + month)
reg1 <- train(
  model1,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 2 linear trend + monthly seasonality + days of week seasonality 
model2 <- as.formula(QUANTITY ~ 1 + trend + month + dow)
reg2 <- train(
  model2,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 3 linear trend + monthly seasonality + days of week  seasonality + holidays 
model3 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday)
reg3 <- train(
  model3,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 4 linear trend + monthly seasonality + days of week  seasonality + holidays + sch*dow
model4 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg4 <- train(
  model4,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 5 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions
model5 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow + weekend*month)
reg5 <- train(
  model5,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 6 =  multiplicative trend and seasonality (ie take logs, predict log values and transform back with correction term)
model6 <- as.formula(q_ln ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg6 <- train(
  model6,
  method = "lm",
  data = data_train,
  trControl = train_control
)


stargazer(reg2$finalModel, reg3$finalModel, reg4$finalModel, reg5$finalModel, 
          out=paste(output,"Ch18_swim_tsregs.txt",sep=""), type = "text", digits=2)
stargazer(reg6$finalModel, 
          out=paste(output,"Ch18_swim_tsregs2.txt",sep=""), type = "text", digits=2)

# Get CV RMSE ----------------------------------------------

model_names <- c("reg1","reg2","reg3","reg4","reg5")
rmse_CV <- c()

for (i in model_names) {
  rmse_CV[i]  <- get(i)$results$RMSE
}
rmse_CV

#had to cheat and use train error on full train set because could not obtain CV fold train errors
corrb <- mean((reg6$finalModel$residuals)^2)
rmse_CV["reg6"] <- reg6$pred %>% 
  mutate(pred = exp(pred  + corrb/2)) %>% 
  group_by(Resample) %>% 
  summarise(rmse = RMSE(pred, exp(obs))) %>% 
  as.data.frame() %>% 
  summarise(mean(rmse)) %>% 
  as.numeric()
rmse_CV["reg6"] 

# Use prophet prediction -------------------------------------------
# add CV into prophet
# can be done with prophet: https://facebook.github.io/prophet/docs/diagnostics.html
# done but this is a different cross-validation as for the other models as it must be time-series like

# prophet -  multiplicative option -- tried but produced much worse results (~34. RMSE)


model_prophet <- prophet( fit=F, 
                          seasonality.mode = "additive", 
                          yearly.seasonality = "auto",
                          weekly.seasonality = "auto",
                          growth = "linear",
                          daily.seasonality=TRUE)

model_prophet <-  add_country_holidays(model_prophet, "US")
model_prophet <- fit.prophet(model_prophet, df= data.frame(ds = data_train$date,
                                                           y = data_train$QUANTITY ))

cv_pred <- cross_validation(model_prophet, initial = 365, period = 365, horizon = 365, units = 'days')
rmse_prophet_cv <- performance_metrics(cv_pred, rolling_window = 1)$rmse
rmse_prophet_cv

###########################x
# Evaluate best model on holdout set --------------------------------------------
###########################x

data_holdout <- data_holdout %>% 
  mutate(y_hat_5 = predict(reg5, newdata = .))

rmse_holdout_best <- RMSE(data_holdout$QUANTITY, data_holdout$y_hat_5)
rmse_holdout_best

###########################x
# Plot best predictions --------------------------------------------
###########################x

#graph relative RMSE (on holdout) per month 
rmse_monthly <- data_holdout %>% 
  mutate(month = factor(format(date,"%b"), 
                        levels= unique(format(sort(.$date),"%b")), 
                        ordered=TRUE)) %>% 
  group_by(month) %>% 
  summarise(
    RMSE = RMSE(QUANTITY, y_hat_5),
    RMSE_norm= RMSE(QUANTITY, y_hat_5)/mean(QUANTITY)
            ) 

g_predictions_rmse<- ggplot(rmse_monthly, aes(x = month, y = RMSE_norm)) +
  geom_col(bg=color[1], color=color[1]) +
  labs( x = "Date (month)", y="RMSE (normalized by monthly sales)" ) +
    theme_bg() 
g_predictions_rmse
#save_fig("ch18_swim_predictions_rmse", output, "small")
save_fig("ch18-figure-7b-swim-predictions-rmse", output, "small", plot=g_predictions_rmse)

g_predictions<-
  ggplot(data=data_holdout, aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand=c(0,0), breaks = as.Date(c("2016-01-01","2016-03-01","2016-05-01","2016-07-01","2016-09-01","2016-11-01", "2017-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" )+
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-07-15"), y = 50, label = "Predicted", color=color[2], size=3)+
  #annotate("text", x = as.Date("2016-09-01"), y = 125, label = "Actual", color=color[1], size=3)
  theme(legend.position=c(0.7,0.8),
      legend.direction = "horizontal",
      legend.text = element_text(size = 6),
      legend.key.width = unit(.8, "cm"),
      legend.key.height = unit(.3, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.8))
         )
g_predictions
#save_fig("ch18_swim_predictions", output, "large")
save_fig("ch18-figure-6-swim-predictions", output, "large", plot=g_predictions)


g_predictions_m <- ggplot(data=data_holdout %>% filter(month==8), aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat_5), fill=color[4], alpha=0.3) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
  scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-08-01","2016-08-08","2016-08-15","2016-08-22","2016-08-29")),
               limits = as.Date(c("2016-08-01","2016-08-31")),
               labels = date_format("%d%b")) +
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
  #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6))
  )
g_predictions_m
#save_fig("ch18_swim_predictions_m", output, "small")
save_fig("ch18-figure-7a-swim-predictions-m", output, "small", plot=g_predictions_m)

#ch18-table-1-swim-rmse
#ch18-table-2-cs-models-rmse
#ch18-table-3-arima-folds