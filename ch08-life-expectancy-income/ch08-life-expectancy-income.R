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

# Chapter 08
# CH08B How is life expectancy related to the average income of a country?
# using the worldbank-lifeexpectancy dataset
# version 0.9 2020-09-07
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(arm)
library(segmented)
library(lmtest)
library(lspline)
library(gridExtra)
library(cowplot)


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

data_in <- paste(data_dir,"worldbank-lifeexpectancy","clean/", sep = "/")
use_case_dir <- "ch08-life-expectancy-income/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------

xc <- read_delim(paste0(data_in, "worldbank-lifeexpectancy.csv"), 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)


xc <- xc %>% dplyr::rename(year = "Time",
                    countryname="Country Name",
                    countrycode = "Country Code",
                    gdp_per_capita="GDP per capita, PPP (constant 2011 international $) [NY.GDP.PCAP.PP.KD]",
                    population="Population, total [SP.POP.TOTL]",
                    lifeexp="Life expectancy at birth, total (years) [SP.DYN.LE00.IN]"
                    )
xc<-xc %>% dplyr::select(-"Time Code")



# SELECT OBSERVATIONS
xc <- xc %>% filter(countrycode!="")

# CLEAN VARIABLES
xc<-xc %>% mutate(population=as.numeric(population)/1000000) %>%
            mutate(gdppc=as.numeric(gdp_per_capita)/1000) %>%
            mutate(lifeexp=as.numeric(lifeexp))


xc %>% tally()
xc %>% filter(!is.na(gdppc)) %>% tally()
xc %>% filter(!is.na(lifeexp)) %>% tally()
xc %>% filter(!is.na(lifeexp)) %>% filter(!is.na(gdppc)) %>% tally()



# DESCRIBE OBSERVATIONS WITH MISSING VALUE
xc %>% filter(!is.na(lifeexp)) %>% filter(!is.na(gdppc)) %>% dplyr::select(population) %>% summary()
xc %>% filter(is.na(lifeexp) | is.na(gdppc)) %>% dplyr::select(population) %>% summary()

xc <- xc %>% filter(year==2017) %>%
   filter(!is.na(lifeexp)) %>%
   filter(!is.na(gdppc)) 
  
# GDP total, log
xc <- xc %>% mutate(gdptot=gdppc*population) %>%
             mutate(lngdppc=log(gdppc)) %>%
             mutate(lngdptot=log(gdptot))

xc %>%dplyr:: select(lifeexp,gdppc,gdptot,lngdppc,lngdptot) %>% summary()

###########################################
# Graphs 
###########################################


####################
## PER CAPITA GDP
####################

# HISTOGRAMS

ch08_lifeexp_hist1<- ggplot(data = xc, 
                       aes (x = gdppc, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 3, boundary=0, closed='left', 
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "GDP per capita (thousand US dollars)", y = "Percent") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 120), breaks = seq(0, 120, by = 15)) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0,0.2), breaks = seq(0, 0.20, by = 0.04), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
ch08_lifeexp_hist1
save_fig("ch08-figure-3a-gdppercap-hist", output, "small")


ch08_lifeexp_hist2<- ggplot(data = xc, 
                        aes (x = lngdppc, y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.2, boundary=0, closed='left', 
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "ln(GDP per capita, thousand US dollars)", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(expand = c(0.0,0.0),limits = c(0,0.1), breaks = seq(0, 0.10, by = 0.02), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
ch08_lifeexp_hist2
save_fig("ch08-figure-3b-gdppercap-hist-log", output, "small")


# LEVEL-LEVEL REGRESSION
reg3 <- lm(lifeexp ~ gdppc, data=xc)
summary(reg3)

ch08_lifeexp_linreg1 <- ggplot(data = xc, 
                      aes(x = gdppc, y = lifeexp)) +
  geom_point_da() +
  geom_smooth_da(method="lm")+
  coord_cartesian(xlim = c(0, 120), ylim = c(50, 100)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,100, by=5)) +
  labs(x = "GDP per capita (thousand US dollars)",y = "Life expectancy  (years)")+
  theme_bg()
ch08_lifeexp_linreg1
save_fig("ch08-figure-4-lifeexp", output, "small")


# --------------------------------------------
# LOG GDP PER CAPITA (shown on two scales)

reg4 <- lm(lifeexp ~ lngdppc, data=xc)
summary(reg4)



ch08_lifeexp_linreg2<- ggplot(data = xc, 
                              aes(x = lngdppc, y = lifeexp)) +
  geom_point_da() +
  geom_smooth_da(method="lm")+
  coord_cartesian(xlim = c(-0.5, 4.8), ylim = c(50, 85)) +
  scale_x_continuous(breaks = seq(-0.5,4.5, by=0.5)) +
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "ln(GDP per capita, thousand US dollars) ",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_linreg2
save_fig("ch08-figure-5a-lifeexp-log", output, "small")

ch08_lifeexp_linreg2a<- ggplot(data = xc, 
                      aes(x = gdppc, y = lifeexp)) +
  geom_point_da() +
  geom_smooth_da(method="lm")+
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(0.1, 0.5, 1,2,5,10,20,50,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "GDP per capita, thousand US dollars (ln scale) ",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_linreg2a
save_fig("ch08-figure-5b-lifeexp-log-scale", output, "small")

ch08_lifeexp_linreg2a
save_fig("ch08-figure-9a-lifeexp-unweighted", output, "small")


################################
## TOTAL GDP
################################


# LEVEL-LEVEL REGRESSION
reg1 <- lm(lifeexp ~ gdppc, data=xc)
summary(reg1)

ch08_lifeexp_linreg3<- ggplot(data = xc, 
                       aes(x = gdptot, y = lifeexp)) +
  geom_point_da() +
  geom_smooth_da(method="lm")+
  coord_cartesian(xlim = c(0, 24000), ylim = c(50, 85)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 24000), breaks = seq(0, 24000, by = 4000)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(50,85), breaks = seq(50, 85, by = 5)) +
  labs(x = "Total GDP  (billion US dollars)",y = "Life expectancy  (years)")+
  theme_bg()
ch08_lifeexp_linreg3
save_fig("ch08-figure-6a-lifeexp-totgdp", output, "small")



# LEVEL-LOG REGRESSION
reg2 <- lm(lifeexp ~ lngdptot, data=xc)
summary(reg2)

ch08_lifeexp_linreg4<- ggplot(data = xc, 
                       aes(x = gdptot, y = lifeexp)) +
  geom_point_da() +
  geom_smooth_da(method="lm")+
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = c(50,55,60,65,70,75,80,85)) +
  labs(x = "Total GDP (in ln scale))",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_linreg4
save_fig("ch08-figure-6b-lifeexp-totgdp-ln", output, "small")







##########################################
# GPD PER CAPITA PIECEWISE LINEAR SPLINE 
##########################################
cutoff<- 50
cutoff_ln<- log(cutoff)


reg5 <- lm(lifeexp ~ lspline(lngdppc,cutoff_ln), data=xc)
summary(reg4)

coeftest(reg5)
xc$e3 <- resid(reg5)
xc$sppred<-predict(lm(lifeexp ~ lspline(lngdppc,cutoff_ln), data=xc))

ch08_lifeexp_spline<- ggplot(data = xc, 
       aes(x = gdppc, y = lifeexp)) +
  geom_point_da() +
  geom_line(data = xc, aes(x = gdppc, y = sppred), color = color[2], size = 0.7)+ 
  geom_vline(xintercept = cutoff, color=color[3], size=0.5, linetype="dotted") +
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(0.1, 0.5, 1,2,5,10,20,50,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "GDP per capita, thousand US dollars (ln scale) ",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_spline
save_fig("ch08-figure-7a-lifeexp-spline", output, "small")


# QUADRATIC IN LEVEL-LOG REGRESSION
xc$lngdppc_sq <- xc$lngdppc^2
reg6 <- lm(lifeexp ~ lngdppc + lngdppc_sq, data=xc)
summary(reg6)
xc$e6 <- resid(reg6)

#ln units

# not in book
ch08_lifeexp_quad<- ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point_da() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[2], se = F, size=0.7)+
  coord_cartesian(xlim = c(-1, 5), ylim = c(50, 90)) +
  scale_x_continuous(breaks = seq(-1,5, by=0.5)) +
  scale_y_continuous(breaks = seq(50,90, by=5)) +
  labs(x = "ln(GDP per capita, thousand US dollars)",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_quad

ch08_lifeexp_quad_level<- ggplot(data = xc, aes(x = gdppc, y = lifeexp)) +
  geom_point_da() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[2], se = F, size=0.7)+
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(0.1, 0.5, 1,2,5,10,20,50,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "GDP per capita, thousand US dollars (ln scale) ",y = "Life expectancy  (years)")+
  theme_bg() 
ch08_lifeexp_quad_level
save_fig("ch08-figure-7b-lifeexp-quad", output, "small")


########################################xxx
# WEIGHTED AND UNWEIGHTED REGRESSION
########################################xxx


reg7 <- lm(lifeexp ~ lngdppc, data=xc)
summary(reg7)

reg8 <- lm(lifeexp ~ lngdppc, data=xc, weights=population)
summary(reg8)

ch08_lifeexp_weighted<- ggplot(data = xc, aes(x = gdppc, y = lifeexp)) +
  geom_point(data = xc, aes(size=population),  color = color[1], shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color=color[2], se = F, size=0.7)+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(0.1, 0.5, 1,2,5,10,20,50,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "GDP per capita, thousand US dollars (ln scale) ",y = "Life expectancy  (years)")+
  theme_bg() +
    #geom_segment(aes(x = 2, y = 25, xend = 1.15, yend = 50), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 70, y = 80, label = "USA", size=2)+
  annotate("text", x = 10, y = 82, label = "China", size=2)+
  annotate("text", x = 7,  y = 63, label = "India", size=2)
ch08_lifeexp_weighted
save_fig("ch08-figure-9b-lifeexp-weighted", output, "small")

# ADDITIONAL bits

# not in book
F08_lifeexp_w_compare<- ggplot(data = xc, aes(x = gdppc, y = lifeexp)) +
  geom_smooth(method = "lm", color=color[2], se = F, size=1)+
  geom_smooth(aes(weight = population), method = "lm", color=color[3], se = F, size=1)+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(50, 85)) +
  scale_x_continuous(trans = log_trans(),  breaks = c(0.1, 0.5, 1,2,5,10,20,50,100)) + 
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(50,85, by=5)) +
  labs(x = "GDP per capita, thousand US dollars (ln scale) ",y = "Life expectancy  (years)")+
  theme_bg() 
F08_lifeexp_w_compare


# not in book
ch08_lifeexp_weighted<- ggplot(data = xc, aes(x = lngdppc, y = lifeexp)) +
  geom_point(data = xc, aes(size=population),  color = color[1], shape = 16, alpha = 0.4,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color=color[2], se = F, size=0.7)+
  scale_size(range = c(1, 15)) +
  coord_cartesian(xlim = c(-1, 5), ylim = c(50, 90)) +
  scale_x_continuous(breaks = seq(-1,5, by=0.5)) +
  scale_y_continuous(breaks = seq(50,90, by=5)) +
  labs(x = "ln(GDP per capita, thousand US dollars)",y = "Life expectancy  (years)", size = "Population")+
  theme_bg() 
ch08_lifeexp_weighted

#ch08-table-1-hotel-reg1
#ch08-table-2-log-approx
