################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 06
# CH06A 
# billion-prices dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(xtable)


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

data_in <- paste(data_dir,"billion-prices","clean/", sep = "/")

use_case_dir <- "ch06-online-offline-price-test/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------

# load data
pd <- read.csv(paste0(data_in,"online_offline_ALL_clean.csv"))


# FILTER DATA
pd <- pd %>% filter(COUNTRY=="USA") %>% 
             filter(PRICETYPE == "Regular Price") %>%
             filter(is.na(sale_online)) %>%
             filter(!is.na(price)) %>%
             filter(!is.na(price_online))


# Drop obvious errors
pd <- pd %>% filter(price<1000)

# Compare variables
pd<-pd %>% mutate(diff = price_online-price)

descr <- pd %>% summarise(mean = mean(diff,na.rm=T), sd = sd(diff,na.rm=T), min=min(diff,na.rm=T),
                          median=median(diff,na.rm=T), max=max(diff,na.rm=T))
descr

hist1<- ggplot(data=pd, aes(diff))+
  geom_histogram(binwidth = 5, boundary=0, closed="left",
                 fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Online - offline price difference (US dollars)", y = "Frequency") +
  theme_bg()+
  scale_x_continuous(limits = c(-420, 420), breaks = seq(-400, 420, by = 100)) +
  scale_y_continuous(limits=c(0,6000), breaks = seq(0, 6000, by = 1000), expand = c(0.01,0.01))+
  geom_segment(aes(x = 300, y = 500, xend = 415, yend = 20), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 300, y = 700, label = "max value= 415", size=2.5) +
  geom_segment(aes(x = -280, y = 500, xend = -380, yend = 20), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = -300, y = 700, label = "min value= -380", size=2.5)
hist1
#save_fig("R_F06_1", output, "small")
save_fig("ch06-figure-1a-pricediff", output, "small")

# 4.99999 not 5 -- needed because of data imported from stata may be stored wierdly. 
pd1<-subset(pd,abs(pd$diff)<4.999999)
Hmisc::describe(pd1$diff)

hist2<- ggplot(data=pd, aes(diff))+
  geom_histogram(binwidth = 0.5, boundary=-0, closed="left",
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Online - offline price difference (US dollars)", y = "Frequency") +
  theme_bg()+
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) +
  scale_y_continuous(expand = c(0.00,0.00), limits=c(0,5000), breaks = seq(0, 5000, by = 1000))
hist2
#save_fig("R_F06_2", output, "small")
save_fig("ch06-figure-1b-pricediff2", output, "small")

# HYPOTHESIS
t.test(pd$diff,mu=0)	

# MULTIPLE HYPOTHESES
spd <- split(pd, pd$retailer,drop=FALSE) 
out <- vector("list", length = length(spd))
out <- lapply(1:length(spd),function (x) out[[x]]<- t.test(spd[[x]]$diff,mu=0))
out

# create table
table_out <- pd %>% group_by(retailer) %>% group_modify(~ tidy(t.test(.x$diff))) 
table_out<-table_out %>% select(retailer,estimate,p.value)

xt<-xtable(table_out,align='llcc', digits = c(0,0,2,2))
names(xt) <- c('Retailer ID','Diff','p-value')
#print(xt, type = "latex",include.rownames = FALSE,       file = paste0(output,"ch06.tex"))
print(xt, type = "latex",include.rownames = FALSE,
      file = paste0(output,"ch06-table-2-test.tex"))





