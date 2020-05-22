#*********************************************************************
# DATA ANALYSIS TEXTBOOK
# CH 23 IMPORT DEMAND AND PRODUCTION
# 

#  v1.0 2020-02-03 draw only the graphs (data: work.dta)
#  v1.1 2020-03-01 added feature engineering part & panel setup (data: asia-industry_tidy.dta)
#  v1.2 2020-04-07 errors corrected
#  v1.3 2020-04-20 labels edited
#  v1.4 2020-04-22 names ok
#  v1.5 2020-04-28 label edits
#  v1.6 2020-04-30 label edits

#*********************************************************************
# WHAT THIS CODES DOES:
##  


# clear environment
rm(list=ls())

# Libraries
library(readstata13)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(zoo)
library(lubridate)
library(fpp3)


# Set directories ---------------------------------------------------------
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

data_in <- paste0(dir,"da_data_repo/asia-industry/clean/")
data_out <-  paste0(dir,"da_case_studies/ch23-import-demand-and-production/")
output <-  paste0(dir,"da_case_studies/ch23-import-demand-and-production/output/")
func <-  paste0(dir,"da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R"))


# import data
raw <- read.dta13(paste0(data_in,'asia-industry_tidy.dta'))

data <- raw %>%
  filter(year >= 1998)

data <- data[!(data$year==1998 & data$month==1),]
data <- data[!(data$year==2018 & data$month > 4),]


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
# encode country , gen(cc)
# sort cc time
# tsset cc time
data <- data %>%
  mutate(cc=as.factor(countrycode))


# lagged variables
work <- data %>%
  arrange(cc, time) 

work <- work %>%
  group_by(cc) %>%
  mutate(
    dln_ip =            difference(ln_ip,          lag=1, order_by = time),
    dln_usa_ip =        difference(ln_usa_ip,      lag=1, order_by = time),
    dln_chn_ip =        difference(ln_chn_ip,      lag=1, order_by = time),
    dln_usa_imports =   difference(ln_usa_imports, lag=1, order_by = time),
    dln_er_usd =        difference(ln_er_usd,      lag=1, order_by = time)
    ) %>%
  ungroup()


work %>%
  group_by(countrycode) %>%
  dplyr::summarise(mean=mean(dln_ip, na.rm = TRUE))


# create time variables
work$Date <- with(work, sprintf("%d-%02d", year, month))
work <- work %>%
  mutate(date=parse_date_time(Date, orders='ym'),
         ym=format(date, '%b%Y')) 


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

#ch23-table-1-asia-reg

  




