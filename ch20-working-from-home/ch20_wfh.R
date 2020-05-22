######################################################################
#
# Data Analysis Textbook
# Case Study for Chapter 20 - Experiments
# Data : work-from-home
# Using Bloom et al. (2015): Does Working from Home Work? Evidence from a Chineses Experiment. QJE. 165-218
#
# v2.1 2020-01-01 
# v2.2 2020-02-24 Minor edits in folders
# v2.3 2020-04-19 Edits 
# v2.4 2020-04-20 Minor edits of graph
# v2.5 2020-04-23 names ok
# v2.6 2020-04-27 label edit

######################################################################
#
# What this code does:
#

######################################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(haven)
library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)
library(ggplot2)
library(reshape)


# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


#location folders
data_in <- paste0(dir,"da_data_repo/working-from-home/clean/")
data_out <-  paste0(dir,"da_case_studies/ch20-working-from-home/")
output <-  paste0(dir,"da_case_studies/ch20-working-from-home/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))





# Load in data -------------------------------------------------------
data <- read_dta(paste0(data_out, "ch20-wfh-workfile.dta"))


data <- data %>% select(personid:perform11, age, male, second_technical, high_school, tertiary_technical, university,
                        prior_experience, tenure, married, children, ageyoungestchild, rental,
                        costofcommute, internet, bedroom, basewage, bonus, grosswage)


# Balance ------------------------------------------------------------

# Modify variable
data$ageyoungestchild <- ifelse(data$children == 0, NA, data$ageyoungestchild)


# Table of averages in control and treatment

data_temp <- data %>% 
  select (perform10, age:grosswage, ordertaker)


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
varlabel(data, var.name = c("quitjob", "phonecalls1"))

quitjob <- data %>%
  group_by(treatment) %>%
  summarise(mean=mean(quitjob),
            sd = sd(quitjob),
            N=n())
total_quitjob <-  data %>%
  summarise(mean_total=mean(quitjob),
            sd_total=sd(quitjob),
            N_total=n()) 

quitjob
total_quitjob

phonecalls1 <- data %>%
  group_by(treatment) %>%
  filter(ordertaker==1) %>%
  summarise(mean=mean(phonecalls1),
            sd = sd(phonecalls1),
            N=n())
total_phonecalls <-  data%>%
    filter(ordertaker==1) %>%
    summarise(mean_total=mean(phonecalls1),
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
    legend.background = element_rect(size=0.1, linetype="solid", colour = color.background),
    plot.margin=unit(x=c(0.1,0.1,0.1,0.1),units="mm")  )+
    background_grid(major="y", minor="none")
quitrates_barchart
#save_fig("wfh-quitrates-barchart_R", output, "small")
save_fig("ch20-figure-1-wfh-quitrates-barchart", output, "small")

# --------------------------------------------------------------------
# Regression analysis 
# Outcome variables: 1) quit firm during 8 months of experiment , 2) phone calls worked, for ordertakers
# --------------------------------------------------------------------

# Outcomes by treatment

# 1) Quit firm
data %>%
  group_by(treatment) %>%
  summarise_at(vars(quitjob),  funs(N=n(), Mean=mean(., na.rm=T), Sd=sd(., na.rm=T)))

# 2) Phonecalls (ordertakers only)
data %>%
  group_by(treatment) %>%
  filter(ordertaker==1) %>%
  summarise_at(vars(phonecalls1),  funs(N=n(), Mean=mean(., na.rm=T), Sd=sd(., na.rm=T)))



# Regression 1: ATE estimates, no covariates -------------------------

reg1 <- lm(quitjob ~ treatment, data=data)
reg1_1 <- coeftest(reg1, vcov = sandwich)

reg2 <- lm(phonecalls1 ~ treatment, data=data[data$ordertaker==1, ])
reg2_2 <- coeftest(reg2, vcov = sandwich)

#stargazer(reg1, reg2, out=paste0(output,"Ch20_wfh_reg1_R.tex",sep=""), digits=2, float = F, no.space = T)
stargazer(reg1, reg2, out=paste0(output,"#ch20-table-4-wfh-reg1.tex",sep=""), digits=2, float = F, no.space = T)

# Regression 2: ATE estimates, with covariates of some unbalance -----
reg3 <- lm(quitjob ~ treatment + married + children + internet, data=data)
reg3_3 <- coeftest(reg3, vcov = sandwich)

reg4 <- lm(phonecalls1 ~ treatment + married + children + internet, data=data[data$ordertaker==1, ])
reg4_4 <- coeftest(reg4, vcov = sandwich)


#stargazer(reg3, reg4, out=paste(output,"Ch20_wfh_reg2_R.tex",sep=""), digits=2, float = F, no.space = T)
stargazer(reg3, reg4, out=paste(output,"ch20-table-4-wfh-reg2.tex",sep=""), digits=2, float = F, no.space = T)




