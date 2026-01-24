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

# CHAPTER 20
# CH20A Working from home and employee performance
# using the wfh dataset
# version 0.91 2021-02-27
#########################################################################################


###########

#
# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(haven)
library(fixest)
library(reshape)
library(cowplot)

# set data dir, data used
source("ch00-tech-prep/set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")
if (basename(getwd()) != "da_case_studies") {
  setwd("da_case_studies")
}

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3)

data_in <- paste(data_dir,"working-from-home","clean/", sep = "/")
use_case_dir <- "ch20-working-from-home/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# Load in data -------------------------------------------------------
#data <- read_csv('https://osf.io/download/5c3rf/')
data <- read_csv(paste0(data_in, "wfh_tidy_person.csv"))


data <- data %>% 
  dplyr::select(personid:perform11, age, male, second_technical, high_school, tertiary_technical, university,
                        prior_experience, tenure, married, children, ageyoungestchild, rental,
                        costofcommute, internet, bedroom, basewage, bonus, grosswage, phonecalls1 )


# Balance ------------------------------------------------------------

# Modify variable
data$ageyoungestchild <- ifelse(data$children == 0, NA, data$ageyoungestchild)


# Table of averages in control and treatment

data_temp <- data %>% 
  dplyr::select (perform10, age:grosswage, ordertaker)


vars <- colnames(data_temp)
rm(data_temp)

mean_t <- c()
mean_c <- c()
sd <- c()
p_value <- c()
model <- c()



for(i in vars){
  # Regression model
  model <- lm(paste(i, "~treatment"), data=data) 
  
  # Mean control
  mean_c[i] <- mean(data[data$treatment==0, ][[paste(i)]], na.rm=T)
  # mean_c[i] <- model$coefficients[1] # or get it directly from regression
  
  # Mean treated
  mean_t[i] <- mean(data[data$treatment==1, ][[paste(i)]], na.rm=T)
  # mean_t[i] <- model$coefficients[1] + model$coefficients[2] # or get it directly from regression
  
  # p-value from regression
  p_value[i] <- anova(model)$'Pr(>F)'[1]
  
  # Standard deviation
  sd[i] <- sd(data[[paste(i)]], na.rm=T)
}

# Put together 
table <- data.frame(round(mean_t, 2), round(mean_c, 2), round(sd, 2), round(p_value, 2))
col.names <- c("Treatment mean", "Control mean", "Std.dev.", "p-value of test of equal means")  
names(table) <- col.names
print(table)

############################
# outcomes:
# quit firm during 8 months of experiment
# phone calls worked, for order takers
#

quitjob <- data %>%
  group_by(treatment) %>%
  dplyr::summarise(mean=mean(quitjob),
            sd = sd(quitjob),
            N=n())
total_quitjob <-  data %>%
  dplyr::summarise(mean_total=mean(quitjob),
            sd_total=sd(quitjob),
            N_total=n()) 

quitjob
total_quitjob

phonecalls1 <- data %>%
  group_by(treatment) %>%
  filter(ordertaker==1) %>%
  dplyr::summarise(mean=mean(phonecalls1),
            sd = sd(phonecalls1),
            N=n())
total_phonecalls <-  data%>%
    filter(ordertaker==1) %>%
  dplyr::summarise(mean_total=mean(phonecalls1),
            sd_total=sd(phonecalls1),
            N_total=n()) 
phonecalls1
total_phonecalls

# Bar chart for quit rates

data <-  data %>%
  mutate(quit_pct=quitjob*100,
         stayed_pct = (1-quitjob)*100)

barchart_data <- data %>%
  select(treatment, quit_pct, stayed_pct) %>%
  group_by(treatment) %>%
  summarise(quit_pct=mean(quit_pct),
         stayed_pct = mean(stayed_pct)) %>%
  gather(employees, pct, quit_pct:stayed_pct)

quitrates_barchart <-  ggplot(barchart_data,aes(fill = employees, y= pct, x=factor(treatment))) +
  geom_bar(stat = "identity") +
  theme_bg() +
  labs(y = "Share of employees (percent)",
       x = "") +
  scale_x_discrete(labels = c ("Non-treatment group", "Treatment group")) +
  scale_fill_manual(labels=c("Quit", "Stayed"), name = "", values= c(color[2], color[1])) +
  theme(legend.position="right", 
    legend.background = element_rect(linewidth =0.1, linetype="solid", colour = color.background),
    plot.margin=unit(x=c(0.1,0.1,0.1,0.1),units="mm")  )+
    background_grid(major="y", minor="none")
quitrates_barchart
save_fig("ch20-figure-1-wfh-quitrates-barchart", output, "small")

# --------------------------------------------------------------------
# Regression analysis 
# Outcome variables: 1) quit firm during 8 months of experiment , 2) phone calls worked, for ordertakers
# --------------------------------------------------------------------

# Outcomes by treatment

# 1) Quit firm

data %>%
  group_by(treatment) %>%
  summarise(
    N = n(),
    Mean = mean(quitjob, na.rm = TRUE),
    Sd = sd(quitjob, na.rm = TRUE)
  )

# 2) Phonecalls (ordertakers only)
data %>%
  group_by(treatment) %>%
  filter(ordertaker==1) %>%
  summarise(N=n(), 
            Mean=mean(phonecalls1, na.rm=T),
            Sd=sd(phonecalls1, na.rm=T))



# Regression 1: ATE estimates, no covariates -------------------------

reg1 <- feols(quitjob ~ treatment, data=data , vcov = "HC1")
reg2 <- feols(phonecalls1 ~ treatment, data=data[data$ordertaker==1, ], vcov = "HC1")


etable(reg1,reg2,fitstat = c('n','r2','rmse'))



# Regression 2: ATE estimates, with covariates of some unbalance -----
reg3 <- feols(quitjob ~ treatment + married + children + internet, data=data, vcov = "HC1")
reg4 <- feols(phonecalls1 ~ treatment + married + children, data=data[data$ordertaker==1, ], vcov = "HC1")

etable(reg3,reg4,fitstat = c('n','r2','rmse'))





