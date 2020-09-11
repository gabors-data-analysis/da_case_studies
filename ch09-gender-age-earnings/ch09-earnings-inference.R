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

# Chapter 09
# CH09A Estimating gender and age differences in earnings
# using the cps-earnings dataset
# version 0.9 2020-09-07
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)


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
options(digits = 3) 

data_in <- paste(data_dir,"cps-earnings","clean/", sep = "/")
use_case_dir <- "ch09-gender-age-earnings/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------


#import data
data_all <- read_csv(paste0(data_in,"morg-2014-emp.csv"))

#SELECT OCCUPATION
# keep only two occupation types: Market research analysts and marketing specialists 
#and Computer and Mathematical Occupations
data_all <- data_all %>% mutate(sample=ifelse(occ2012==0735,1,
                                              ifelse(data_all$occ2012>=1005 & data_all$occ2012<=1240,2,0)))

data_all<- data_all %>% filter(sample==1 | sample==2)
tabulate(data_all$sample)

#gen female variable
#gen wage variables
data_all <- data_all %>% mutate(female=as.numeric(sex==2)) %>%
                         mutate(w=earnwke/uhours) %>%
                         mutate(lnw=log(w)) %>%
                         mutate(agesq=age^2)


#SET SAMPLE - Choose one of the occupations!
#Market research analysts and marketing specialists -1
#Computer and Mathematical Occupations-2
i=1
data <- data_all %>% filter(sample==i)

write_csv(data,paste(data_out,"earnings_inference.csv",sep=""))

#####################
#DISTRIBUTION OF EARNINGS
#######################
data %>% dplyr::select(earnwke,uhours,w) %>% summary()

data %>% filter(w>=1) %>% dplyr::select(earnwke,uhours,w) %>% summary()

tabulate(data$female)
table(data$occ2012,data$female)


##############################
#linear regressions
##############################

# First, look at them one by one

# female binary

# plain SE
reg1<-lm(lnw~female,data) 
summary(reg1)
# with robust SE (Stata)
reg2 <- lm_robust(lnw ~ female, data = data, se_type = "HC1")
summary(reg2)

msummary(list(reg1, reg2),
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01)
         )

# age
reg3 <- lm_robust(lnw ~ age, data = data, se_type = "HC1")
reg4 <- lm_robust(lnw ~ age+agesq, data = data, se_type = "HC1")
reg5 <- lm_robust(lnw ~ lspline(age, c(30,40)), data = data, se_type = "HC1")

gm <-  c('R2' = 'R-squared (%)',
              'se_type' = 'SE type')

cm <- c('R2' = 'R-squared (%)',
        'lspline(age, c(30, 40))2' = 'spline(age, c(20,30))',
        '(Intercept)' = 'Constant')
msummary(list(reg1, reg2, reg3, reg4, reg5),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         #output = paste(output,"ch09_reg2-R.tex",sep="")
         )
#lowess
reg6 <- loess(lnw ~ age, data, control = loess.control(surface = "direct"))
summary(reg6)



##############################
# graphs
##############################

F09_3a <- ggplot(data = data, aes(x = age, y = lnw)) +
  geom_point_da() + 
  geom_smooth_da(method="loess") +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(20, 65),   breaks=seq(20, 65,   by=5)) + 
  scale_y_continuous(expand=c(0.01, 0.01),limits = c(1.5, 4.5), breaks=seq(1.5, 4.5, by=0.50)) +
  labs(x = "Age (years)",y = "ln(earnings per hour)")+
  theme_bg() 
  F09_3a
  save_fig("ch09-figure-3a-wage-lowess",output, "small")


# Confidence intervals

# for polynomial  
z <- predict(reg4, data, se.fit=TRUE)
data<- data %>% mutate(lnwpred_ageq=z[[1]],
                       lnwpred_ageqSE=z[[2]],
                       lnwpred_ageqCIUP=lnwpred_ageq + 2*lnwpred_ageqSE,
                       lnwpred_ageqCILO=lnwpred_ageq - 2*lnwpred_ageqSE
                       ) 




# for spline
z<-predict(reg5,data,se.fit=TRUE)
data <- data %>% mutate(lnwpred_agesp=predict(reg5,data),
                        lnwpred_agespSE=z[[2]],
                        lnwpred_agespCIUP=lnwpred_agesp + 2*lnwpred_agespSE,
                        lnwpred_agespCILO=lnwpred_agesp - 2*lnwpred_agespSE
                       )

# pred values for lowess
data <- data %>% mutate(lnwpred_agel=predict(reg6, data))


# FIXME: 3b, 4 legend, lines solid, dashed, dotted

#graph with the fitted values from the three reg
F09_3b<- ggplot(data=data,aes(x=age)) +
  geom_line(aes(y = lnwpred_agel,  color = "Lowess", linetype = "Lowess"), size = 0.8)+
  geom_line(aes(y = lnwpred_ageq,  color = "Quadratic", linetype = "Quadratic"), size = 0.8)+
  geom_line(aes(y = lnwpred_agesp, color = "Piecewise linear spline", linetype = "Piecewise linear spline"), size = 0.8)+
  scale_color_manual(name = "", values=c(color[1], color[2], color[3])) +
  scale_linetype_manual(name = "", values=c("solid", "dashed", "dotted")) +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(20, 65),   breaks=seq(20, 65,   by=5)) +  
  scale_y_continuous(expand=c(0.01, 0.01), limits = c(2.4, 3.6), breaks=seq(2.4, 3.6, by=0.20)) +
  labs(x = "Age (years)",y = "ln(earnings per hour)")+
  theme_bg() +
  theme(legend.position=c(0.55,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 3),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
  F09_3b
  save_fig("ch09-figure-3b-wage-various",output, "small")
  
  
  Hmisc::describe(data$age)
  
  # same but in log scale
  F09_2b<- ggplot(data=data,aes(x=age)) +
    geom_line(aes(y = lnwpred_agel,  color = "Lowess", linetype = "Lowess"), size = 0.8)+
    geom_line(aes(y = lnwpred_ageq,  color = "Quadratic", linetype = "Quadratic"), size = 0.8)+
    geom_line(aes(y = lnwpred_agesp, color = "Piecewise linear spline", linetype = "Piecewise linear spline"), size = 0.8)+
    scale_color_manual(name = "", values=c(color[1], color[2], color[3])) +
    scale_linetype_manual(name = "", values=c("solid", "dashed", "dotted")) +
    scale_x_continuous(limits = c(20, 65),   breaks=seq(20, 65,   by=5)) +  
    scale_y_continuous(limits = c(2.302585093, 3.555348061), 
                       breaks=c(2.302585093, 2.708050201, 2.995732274,3.218875825, 3.401197382, 3.555348061),
                       labels=c(10, 15, 20, 25, 30, 35))+
    labs(x = "Age (years)",y = "Earnings per hour (ln scale)")+
    theme_bg()+
    theme(legend.position=c(0.65,0.1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 4),
          legend.key.width = unit(.8, "cm"),
          legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
  F09_2b
  
    
#graph with the fitted values from the three reg with CI
  F09_2_CI <- ggplot(data=data,aes(x=age))+
  geom_line(aes(y=lnwpred_agel, color="Lowess", linetype="Lowess"),size=1.2)+
  geom_line(aes(y=lnwpred_ageq, color="Quadratic", linetype="Quadratic"), size=1.2)+
  geom_line(aes(y=lnwpred_ageqCIUP, color="Quadratic", linetype="Quadratic"), size=0.6)+
  geom_line(aes(y=lnwpred_ageqCILO, color="Quadratic", linetype="Quadratic"), size=0.6)+
  geom_ribbon(aes(ymin = lnwpred_ageqCILO, ymax = lnwpred_ageqCIUP), alpha=0.2, bg = color[3]) +
  geom_line(aes(y=lnwpred_agesp, color="Piecewise linear spline", linetype="Piecewise linear spline") ,size=1.2)+
  geom_line(aes(y=lnwpred_agespCIUP,color="Piecewise linear spline", linetype="Piecewise linear spline"),size=0.6)+
  geom_line(aes(y=lnwpred_agespCILO,color="Piecewise linear spline", linetype="Piecewise linear spline"),size=0.6)+
  geom_ribbon(aes(ymin = lnwpred_agespCILO, ymax = lnwpred_agespCIUP), alpha=0.2,   bg=color[2]) +
  coord_cartesian(xlim = c(20, 65), ylim = c(2.6, 3.6)) +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(20, 65),   breaks=seq(20, 65,   by=5)) +  
  scale_y_continuous(expand=c(0.01, 0.01), limits = c(2.4, 3.6), breaks=seq(2.4, 3.6, by=0.20)) +
  labs(x = "Age (years)",y = "ln(earnings per hour)")+
  scale_color_manual(name = "", values=c(color[1], color[2], color[3])) +
  scale_linetype_manual(name = "", values=c("solid", "dashed", "dotted")) +
  theme_bg() +
  theme(legend.position=c(0.65,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6)))
  F09_2_CI
  save_fig("ch09-figure-4-wage-age-reg-ci",output, "large")


##########################################
# CI and PI for the linear model
##########################################
reg7<-lm(lnw ~ age, data=data[data$sample==1, ])
predict(reg7,data,se.fit=TRUE)

pred_confidence <- bind_cols(data,as_tibble(predict(reg7, data, interval="confidence",level = 0.95)))

F09_2a_CI<- ggplot(data = pred_confidence %>% filter(data$lnw<4.4 & data$lnw>2), aes(x = age, y = lnw)) +
  geom_point(color = color[1], size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="lm", colour=color[2], se=F, size = 0.8, linetype = 1)+
  geom_line(data = pred_confidence, aes(x = age, y = lwr), size = 0.5, linetype = 2, colour=color[2]) +
  geom_line(data = pred_confidence, aes(x = age, y = upr), size = 0.5, linetype = 2, colour=color[2]) +
  coord_cartesian(xlim = c(20, 65), ylim = c(1.5, 4.5)) +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(20, 65),   breaks=seq(20, 65,   by=5)) +  
  scale_y_continuous(expand=c(0.01, 0.01), limits = c(1.5, 4.5), breaks=seq(1.5, 4.5, by=0.50)) +
  labs(x = "Age (years)",y = "ln(earnings per hour)")+
  scale_linetype_manual(name = "", values=c(1, 1, 2), 
                       labels = c("Lowess", "Confidence interval (95%)", "Confidence interval (95%)")) +
  theme_bg() 
F09_2a_CI
save_fig("ch09-figure-2a-wage-age-ci",output, "small")


# add PI for a linear
pred_interval <- bind_cols(data,as_tibble(predict(reg7, data, interval="prediction",level = 0.95)))

F09_2b_PI<- ggplot(data=pred_interval %>% filter(lnw<4.4 & lnw>2),aes(x=age,y=lnw)) +  
  geom_point(color = color[1], size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) +
  geom_smooth(method="lm", colour=color[2], se=F, size = 0.8, linetype = 1)+
  geom_line(data = pred_interval, aes(y = lwr), size = 0.5, linetype = 2, colour=color[2]) +
  geom_line(data = pred_interval, aes(y = upr), size = 0.5, linetype = 2, colour=color[2]) +
  coord_cartesian(xlim = c(20, 65), ylim = c(1.5, 4.5)) +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(20, 65),   breaks=seq(20, 65,   by=5)) +  
  scale_y_continuous(expand=c(0.01, 0.01), limits = c(1.5, 4.5), breaks=seq(1.5, 4.5, by=0.50)) +
  labs(x = "Age (years)",y = "ln(earnings per hour)") +
  theme_bg() 
F09_2b_PI
save_fig("ch09-figure-2b-wage-age-pi",output, "small")


#####################################
# bootstrap
#####################################
data <- read_csv(paste0(data_out,"earnings_inference.csv"))

set.seed(201711)


# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}

# bootstrapping with 1000 replications
results <- boot(data=data, statistic=bs,
                R=1000, formula=lnw~female)

b_earnings_female <- as.data.frame(results$t)
colnames(b_earnings_female) <- c('_b_intercept','_b_female')


F09_5<- ggplot(data=b_earnings_female, aes(`_b_female`)) +
geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.025,  center=0.0125, closed="left", 
               color = color.outline, fill = color[1],
               size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  geom_segment(aes(x = -0.11, y = 0, xend = -0.11, yend = 0.2), colour = color[2], size = 1)+
  annotate("text", x = -0.07, y = 0.18, label = "mean", size=2.5) +
coord_cartesian(xlim = c(-0.3, 0.15), ylim = c(0, 0.2)) +
  labs(x = "Slope coefficients from bootstrap samples",y = "Percent")+
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0,0.2), 
                     labels = scales::percent_format(accuracy = 1)) +
theme_bg() 
F09_5
save_fig("ch09-figure-1-bootstrap-dist-wdiff",output, "small")

#ch09-table-3-hotels-extval-time1
#ch09-table-4-hotels-extval-time2
#ch09-table-5-hotels-extval-city1
#ch09-table-6-hotels-extval-type

