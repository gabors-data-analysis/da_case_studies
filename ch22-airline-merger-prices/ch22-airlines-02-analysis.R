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

# CHAPTER 22
# CH22A How does a merger between airlines affect prices?
# using the airline-tickets-usa dataset
# version 0.9 2020-09-11
#########################################################################################



# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(haven)
library(zoo)
library(stargazer)
library(estimatr)
library(modelsummary)
library(cowplot)

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3)

data_in <- paste(data_dir,"airline-tickets-usa","clean/", sep = "/")
use_case_dir <- "ch22-airline-merger-prices/"


data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



# This is the analysis file, you must run ch22-airlines-01-dataprep.R first


# I.  Examining pre-treatment trends in avg ln price
#------------------------------

# workfile to identify treated and untreated markets
data_work <- read_rds(paste0(data_out,"ch22-airline-workfile.rds")) %>%
  filter(balanced==1 & year == 2011) %>%
  arrange(market, year) %>%
  select(origin, finaldest, return, treated, smallmkt)

# * use year-quarter panel data 
# *  and merge to it treated-untreated 
# *	(keep matched ones; no unmatched from "using")
data <- read_dta(paste(data_in, "originfinal-panel.dta", sep="/"))
data <- merge(data, data_work, by = c("origin", "finaldest", "return"))


# * aggreagete data to create average price by treated-untreated and year-quarter
# * and draw time series graphs of log avg price
# * all markets
data_agg <- data %>%
  group_by(treated, year, quarter) %>%
  summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(
    date = as.yearqtr(paste(year, quarter, sep="-")),
    lnavgprice = log(avgprice)
  )


ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1), size = 1.3) +
  geom_line(data = filter(data_agg, treated==0), size = 1.3) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.14, label = "Treated markets", size=3, color = color[2]) + 
  annotate("text", x = as.yearqtr("2013-1"), y = 5.46, label = "Unreated markets", size=3, color = color[1]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.9, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.9, linetype="longdash") +
  annotate("text", x = as.yearqtr("2011-1"), y = 5.57, label = "Announcement", size=2.5, color = color[3]) + 
  annotate("text", x = as.yearqtr("2014-3"), y = 5.58, label = "Merger happens", size=2.5, color = color[3]) +
  scale_y_continuous(limits = c(5, 5.6), breaks = seq(5, 5.6, 0.1)) +
  scale_color_manual(values=color[1:2], name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(axis.text.x=element_text(size=9)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) +
  theme(legend.position="none")
save_fig("ch22-figure-2-pretrends-all", output, size = "large")

# small markets
data_agg <- data %>%
  filter(smallmkt==1) %>%
  group_by(treated, year, quarter) %>%
  summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(
    date = as.yearqtr(paste(year, quarter, sep="-")),
    lnavgprice = log(avgprice)
  )

ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1),  size = 0.7) +
  geom_line(data = filter(data_agg, treated==0), size = 0.7) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.59, label = "Unreated markets", size=2, color = color[1]) +
  annotate("text", x = as.yearqtr("2013-1"), y = 5.49, label = "Treated markets", size=2, color = color[2]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.6, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.6, linetype="longdash") +
  scale_color_manual(values=color[1:2], labels = c("Untreated markets", "Treated markets"), name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(legend.position="none")
save_fig("ch22-figure-3a-pretrends-small", output, size = "small")


# large markets
data_agg <- data %>%
  filter(smallmkt==0) %>%
  group_by(treated, year, quarter) %>%
  summarise(avgprice = weighted.mean(avgprice, passengers))  %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(
    date = as.yearqtr(paste(year, quarter, sep="-")),
    lnavgprice = log(avgprice)
  )

ggplot(data_agg, aes(x = date, y = lnavgprice, color = factor(treated))) +
  geom_line(data = filter(data_agg, treated==1),  size = 0.7) +
  geom_line(data = filter(data_agg, treated==0), size = 0.7) +
  annotate("text", x = as.yearqtr("2013-1"), y = 4.3, label = "Unreated markets", size=2, color = color[1]) +
  annotate("text", x = as.yearqtr("2013-1"), y = 4.9, label = "Treated markets", size=2, color = color[2]) +
  geom_vline(xintercept = as.yearqtr("2012-1"), color = color[3], size = 0.6, linetype="longdash")+
  geom_vline(xintercept = as.yearqtr("2015-3"), color = color[3], size = 0.6, linetype="longdash") +
  scale_color_manual(values=color[1:2], labels = c("Untreated markets", "Treated markets"), name="") +
  labs(y = "ln(average price)", x="") +
  scale_x_yearqtr(format = "%YQ%q") +
  theme_bg() +
  theme(legend.position="none")
save_fig("ch22-figure-3b-pretrends-large", output, size = "small")




# **************************************************************************
# * II. ANALYSIS
# * Basic diff-in-diffs regtrssion
# *  weighted by # passengers on market, in before period
# **************************************************************************

# reload main file
data_agg <- read_rds(paste0(data_out,"ch22-airline-workfile.rds"))

# keep balanced
data_balanced <- data_agg %>%
  filter(balanced == 1)  

fd <- lm(d_lnavgp ~ treated, weights = data_balanced$pass_bef, data = data_balanced)
fd_small <- lm(d_lnavgp ~ treated, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd_large <- lm(d_lnavgp ~ treated, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))


summary(fd)
summary(fd_small)
summary(fd_large)

stargazer_r(list(fd, fd_small, fd_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=paste0(output,"airlines-reg1.tex")
) 

# Corresponding diff-in-diffs table
data_balanced %>%
  group_by(after, treated) %>%
  summarise(weighted.mean(lnavgp, pass_bef, na.rm = TRUE), n())


# **************************************************************************
# * Diff-in-diffs regerssion with confounder variables
# *  weighted by # passengers on market, in before period
# **************************************************************************

# potential confouders: # passengers before, share of largest carrier before
data_balanced <- data_balanced %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    lnpass_bef = mean(ifelse(before == 1, log(passengers), NA), na.rm = TRUE), 
    share_bef = mean(ifelse(before == 1, shareAA + shareUS, NA), na.rm = TRUE),
    sharelarge_bef = mean(ifelse(before == 1, sharelargest, NA), na.rm = TRUE)
  ) %>%
  ungroup()

formula2 <- as.formula(d_lnavgp ~ treated + lnpass_bef + return + stops + sharelarge_bef)
fd2 <- lm(formula2, weights = data_balanced$pass_bef, data = data_balanced)
fd2_small <- lm(formula2, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd2_large <- lm(formula2, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))

summary(fd2)
summary(fd2_small)
summary(fd2_large)

stargazer_r(list(fd2, fd2_small, fd2_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=paste0(output,"airlines-reg2.tex")
)

# **************************************************************************
# * ANALYSIS
# * Diff-in-diffs regerssion with quantitative treatment
# *  weighted by # passengers on market, in before period
# **************************************************************************

data_balanced %>%
  filter(before == 1) %>%
  group_by(share_bef==0, share_bef==1) %>%
  summarise(sum(passengers), mean(passengers), n())


ggplot(data_balanced, aes(x=share_bef,  y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.05, boundary=0, aes(weight = pass_bef),
                 color = color.outline, fill = color[1], alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Market share of AA and US combined, at baseline", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0,0.5), breaks = seq(0, 0.50, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bg()
save_fig("ch22-figure-4-airlines-sharehist", output, size = "small")

formula3 <- as.formula(d_lnavgp ~ share_bef + lnpass_bef + return + stops + sharelarge_bef)
fd3 <- lm(formula3, weights = data_balanced$pass_bef, data = data_balanced)
fd3_small <- lm(formula3, weights = filter(data_balanced, smallmkt==1)$pass_bef, data = filter(data_balanced, smallmkt==1))
fd3_large <- lm(formula3, weights = filter(data_balanced, smallmkt==0)$pass_bef, data = filter(data_balanced, smallmkt==0))

summary(fd3)
summary(fd3_small)
summary(fd3_large)

stargazer_r(list(fd3, fd3_small, fd3_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=paste0(output,"airlines-reg3.tex")
)

# **************************************************************************
# * Diff-in-diffs on pooled cross-sections regeression 
# * use entire unbalanced panel 
# *   - errr... after only is dropped here see later
# *  weighted by # passengers on market, in before period
# **************************************************************************

data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    lnpass_bef = mean(ifelse(before == 1, log(passengers), NA), na.rm = TRUE), 
    sharelarge_bef = mean(ifelse(before == 1, sharelargest, NA), na.rm = TRUE)
  ) %>%
  ungroup()

data_agg %>%
  group_by(balanced, before) %>%
  summarise(sum(passengers), n())


# treatment group defined if observed before only or both before and after
data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    treatment = mean(ifelse(before == 1, AA_and_US, NA), na.rm = TRUE)
  ) %>%
  ungroup()


data_agg %>%
  group_by(is.na(treatment), balanced) %>%
  summarise(n(), sum(passengers))


# conditioning on observed confounders
formula4 <- as.formula(lnavgp ~ (treatment + lnpass_bef + return + stops + sharelarge_bef)*after )
fd4 <- lm(formula4, weights = data_agg$pass_bef, data = data_agg)
fd4_small <- lm(formula4, weights = filter(data_agg, smallmkt==1)$pass_bef, data = filter(data_agg, smallmkt==1))
fd4_large <- lm(formula4, weights = filter(data_agg, smallmkt==0)$pass_bef, data = filter(data_agg, smallmkt==0))

summary(fd4)
summary(fd4_small)
summary(fd4_large)

stargazer_r(list(fd4, fd4_small, fd4_large), se = 'robust', 
  column.labels = c("All markets", "Small markets", "Large markets"), 
  float=TRUE, digits=3, out=paste0(output,"airlines-reg4.tex")
)
