################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 12
# CH12B Electricity consumption and temperature
# case-shiller-la dataset
# version 0.9 2020-08-31


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraies
library(tidyverse)
library(lubridate)
library(cowplot)
library(scales)
library(DataCombine)
library(stargazer)
library(sandwich)
library(dyn) 
library(lmtest)
library(estimatr)
library(huxtable)

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

# set directory for code and data
data_in <- paste(data_dir,"arizona-electricity","raw/", sep = "/")
use_case_dir          <- "ch12-electrictiy-temperature/"


data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



################################################################################

### Import data
## source 1: electricity consumption, by month
electricity <- read_csv(paste0(data_in,"electricity_resid_AZ.csv"))

electricity <- electricity %>% mutate(date = parse_date_time(as.character(MY), orders = "my"))



# Create time variables
electricity <- electricity %>% mutate(year = year(as.Date(electricity$date,"%Y/%m/%d", tz="cet")),
                                      month = month(as.Date(electricity$date,"%Y/%m/%d", tz="cet")),
                                      ym = format(electricity$date,'%Ym%m'))


electricity <- electricity %>% dplyr::select(-c("MY","year","month"))

# Take logs of q
electricity <- electricity %>% mutate(lnQ = log(Q))


# Save working file # 1
write_csv(electricity, paste0(data_out, "electricity_resid_AZ.csv"))

## source 2: climate (cooling degree days etc, by month)
climate <- read_csv(paste0(data_in,"climate_Phoenix_AZ.csv"))
climate <- climate %>% mutate(tempdate = parse_date_time(DATE, orders = "ym"))
climate <- climate %>% mutate(year = year(as.Date(tempdate,"%Y/%m/%d", tz="cet")),
                              month = month(as.Date(tempdate,"%Y/%m/%d", tz="cet")),
                              ym = format(tempdate, '%Ym%m'))

# Generate averages from sums
# when dividing by N, must take into account N of days
climate <-  climate %>% mutate(ndays = ifelse(month %in% c(1, 3, 5, 7, 8, 10, 12),31,
                                       ifelse(month==2,28,30)))
climate <- climate %>% mutate_at(c("CLDD", "HTDD", "DX70", "DX90"),list(avg=~./ndays))

climate <- climate %>% dplyr::select(-c("DATE", "tempdate", "STATION", "NAME"))


climate %>% dplyr::select(CLDD_avg,HTDD_avg,DX70_avg,DX90_avg) %>% summary()


# Save working file # 2
write_csv(climate, paste0(data_out, "climate_Phoenix.csv"))

################################################################################

# Create workfile
data <- inner_join(climate,electricity,by = "ym")
rm(electricity, climate)

data <- data %>% filter(year>=2001 & year <= 2017)
write_csv(data, paste0(data_out, "electricity_AZ_workfile.csv"))


################################################################################
# DATA EXPLORATION

#TODO
# need to have data in Year-month format now -- ym variable should be the date, or stg like that

data <- read_csv(paste0(data_out, "electricity_AZ_workfile.csv"))

data %>% filter(!is.na(Q)) %>% dplyr::select(year,month) %>% summary()
data %>% filter(!is.na(CLDD)) %>% dplyr::select(year,month) %>% summary()

data <- data %>% mutate(date=as.Date(date))
data <- data %>% mutate(lnQ=log(Q))

data %>% dplyr::select(Q,lnQ,CLDD_avg,HTDD_avg) %>% summary()


# PLOT THE TIME SERIES
electricity_ts_Q<- ggplot(data = data, aes(x = date, y = Q))+
  geom_line(color = color[1], size = 0.7) +
  ylab("Residential electricity consumption (GWh)") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(1000,5000), breaks = seq(1000,5000,1000)) +  
  scale_x_date(breaks = as.Date(c("2001-01-01","2004-01-01",  "2007-01-01", "2010-01-01","2013-01-01","2016-01-01")),
             limits = as.Date(c("2001-01-01","2017-12-31")), labels = date_format("%b%Y")) +
    theme_bg()
electricity_ts_Q
#save_fig("electricity_ts_Q_R", "small", plot=electricity_ts_Q)
save_fig("ch12-figure-7a-electricity-ts-q", output, "small")



electricity_ts_lnQ<- ggplot(data = data, aes(x = date, y = lnQ))+
  geom_line_da(size = 0.5) +
  ylab("ln(residential electricity consumption, GWh)") +
  xlab("Date (month)") +
  scale_y_continuous(limits = c(7,8.5), breaks = seq(7,8.5,0.25)) +  
  scale_x_date(breaks = as.Date(c("2001-01-01","2004-01-01",  "2007-01-01", "2010-01-01","2013-01-01","2016-01-01")),
               limits = as.Date(c("2001-01-01","2017-12-31")), labels = date_format("%b%Y")) +
  theme_bg()
electricity_ts_lnQ
#save_fig("electricity_ts_lnQ_R", "small")
save_fig("ch12-figure-7b-electricity-ts-lnq", output, "small")


electricity_ts_CL<-ggplot(data = data, aes(x = date, y = CLDD_avg))+
  geom_line_da(size = 0.5) +
  ylab("Cooling degrees (Farenheit)") +
  xlab("Date (month)") +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,35), breaks = seq(0,35,5)) +  
  scale_x_date(breaks = as.Date(c("2001-01-01","2004-01-01",  "2007-01-01", "2010-01-01","2013-01-01","2016-01-01")),
               limits = as.Date(c("2001-01-01","2017-12-31")), labels = date_format("%b%Y")) +
  theme_bg()
electricity_ts_CL
#save_fig("electricity_ts_CL_R", "small")
save_fig("ch12-figure-8a-electricity-ts-ct", output, "small")


electricity_ts_HT<- ggplot(data = data, aes(x = date, y = HTDD_avg))+
  geom_line_da( size = 0.5) +
  ylab("Heating degrees (Farenheit)") +
  xlab("Date (month)") +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,14), breaks = seq(0,14,2)) +  
  scale_x_date(breaks = as.Date(c("2001-01-01","2004-01-01",  "2007-01-01", "2010-01-01","2013-01-01","2016-01-01")),
               limits = as.Date(c("2001-01-01","2017-12-31")), labels = date_format("%b%Y")) +
  theme_bg()
electricity_ts_HT
#save_fig("electricity_ts_HT_R", "small")
save_fig("ch12-figure-8b-electricity-ts-ht", output, "small")

################################################################################

# SERIAL CORRELATION
# Create differences
data <- data %>% mutate(DlnQ=lnQ-lag(lnQ),
                        DCLDD_avg=CLDD_avg-lag(CLDD_avg),
                        DHTDD_avg=HTDD_avg-lag(HTDD_avg),
                        DDX90_avg=DX90_avg-lag(DX90_avg)
                        )


# functional form investigations (not in textbook)
ggplot(data = data, aes(x=DCLDD_avg, y=DlnQ)) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "Cooling degrees (Farenheit), first difference",y = "ln(monthly electricity consumption), first difference") +
  theme_bg() +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20,20, 10))

ggplot(data = data, aes(x=DHTDD_avg, y=DlnQ)) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "Heating degrees (Farenheit), first difference",y = "ln(monthly electricity consumption), first difference") +
  theme_bg() +
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10, 10))

################################################################################
# Linear regressions

# !!! FIXME some error in huxreg


# same t RHS variables only, Newey-West SE
reg1 <- lm(DlnQ ~ DCLDD_avg + DHTDD_avg, data=data)
reg2 <- lm(DlnQ ~ DCLDD_avg + DHTDD_avg + as.factor(month), data=data)

reg1a <- coeftest(reg1, vcov.=NeweyWest(reg1, prewhite=FALSE, lag=18, verbose=TRUE)) 
reg2a <- coeftest(reg2, vcov.=NeweyWest(reg2, prewhite=FALSE, lag=18, verbose=TRUE)) 
huxreg(DlnQ=reg1a,
       DlnQ=reg2a,
       statistics = c(N = "nobs"))


# same t RHS variables + seasonality
reg4 <- lm(DlnQ ~ DCLDD_avg + DHTDD_avg + as.factor(month), data=data)
reg5 <- lm(DlnQ ~ DCLDD_avg + DHTDD_avg + as.factor(month), data=data)
reg5 <- coeftest(reg5, vcov.=NeweyWest(reg5, prewhite=FALSE, lag=18, verbose=TRUE)) 
reg6 <- dyn$lm(DlnQ ~ DCLDD_avg + DHTDD_avg + as.factor(month) + lag(DlnQ), data=data)
huxreg(DlnQ_simple_SE=reg4,
       DlnQ_Newey_West=reg5,
       DlnQ_lagged=reg6,
       statistics = c(N = "nobs"))


reg7 <- dyn$lm(DlnQ ~ DCLDD_avg + lag(DCLDD_avg) + lag(lag(DCLDD_avg))
               + DHTDD_avg + lag(DHTDD_avg) + lag(lag(DHTDD_avg)) + as.factor(month), data=data)
reg7 <- coeftest(reg7, vcov.=NeweyWest(reg7, prewhite=FALSE, lag=18, verbose=TRUE)) 


data<-data %>% mutate(DDCLDD_avg=(DCLDD_avg-lag(DCLDD_avg)),
                      DDHTDD_avg=(DHTDD_avg-lag(DHTDD_avg)))


reg8 <- dyn$lm(DlnQ ~ lag(lag(DCLDD_avg)) + lag(lag(DHTDD_avg)) + DDCLDD_avg + DDHTDD_avg + lag(DDCLDD_avg) + lag(DDHTDD_avg) + as.factor(month), data=data)
huxreg(reg7,reg8,statistics = c(N = "nobs"))

#ch12-table-1-stocks-stats
#ch12-table-2-stocks-reg-1
#ch12-table-3-stocks-reg-2
#ch12-table-4-electricity-az-reg1
#ch12-table-5-electricity-az-reg2
#ch12-table-6-electricity-az-reg3
