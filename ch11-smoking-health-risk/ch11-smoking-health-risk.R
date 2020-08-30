################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 11
# CH11 smoking
# share-health dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(xtable)
library(haven)
library(cowplot,
        lspline,
data.table,
mfx,
margins,
stargazer,
psych,
estimatr,
huxtable)



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

data_in <- paste(data_dir,"share-health","clean/", sep = "/")

use_case_dir <- "ch11-smoking-health-risk/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------



################################################################################
# 1. PART - CREATE WORKFILE
################################################################################

# load in clean and tidy data and create workfile
share <- read.csv(paste(data_in,"share-health.csv", sep = "/"),stringsAsFactors = F)

share$healthy <- ifelse(share$sphus>0 & share$sphus<=5, 
                        ifelse(share$sphus==1 | share$sphus==2, 1, 0), NA)
table(share$healthy)

share <- share[!is.na(share$healthy), ]

# baseline: wave 4; endline: wave 6
share$baseline <- ifelse(share$wave==4, 1, 0)
share$endline <- ifelse(share$wave==6, 1, 0)

table(share$baseline)
table(share$endline)

# stays healthy at endline
share$temp <- ifelse(share$endline==1, 
                     ifelse(share$healthy==1, 1, 0), NA)

table(share$temp)

share <- share %>% group_by(mergeid) %>% mutate(stayshealthy = max(temp, na.rm=TRUE)) 
table(share$stayshealthy)
share$temp <- NULL

# keep if endline health outcome non-missing
share <- share[share$stayshealthy==1 | share$stayshealthy==0, ]

# keep baseline observations (endline outcome already defined for them)
share <- share[share$baseline==1, ]

# keep age 50-60 at baseline
share <- share[share$age>=50 & share$age<=60, ]

# keep healthy individuals at baseline
share <- share[share$healthy==1, ]

# keep those with non-missing observations for smoking at baseline
# and re-define smoking to be 0-1
share$smoking[share$smoking == 5] <- 0
share <- share[share$smoking==1 | share$smoking==0, ]

share$ever_smoked[share$ever_smoked == 5] <- 0
share <- share[share$ever_smoked==1 | share$ever_smoked==0, ]

# other variables
share$exerc <- ifelse(share$br015==1, 1, ifelse(share$br015>0 & share$br015!=1 , 0, NA))
table(share$exerc)

share$bmi <- ifelse(share$bmi<0, NA, share$bmi)
summary(share$bmi)

names(share)[names(share) == 'income_pct_w4'] <- 'income10'

share$married <- ifelse(share$mar_stat==1 | share$mar_stat==2, 1, 0 )

share$eduyears <- ifelse(share$eduyears_mod<0, NA, share$eduyears_mod)
summary(share$eduyears)
share$eduyears_mod <- NULL


share <- share[!is.na(share$bmi) & !is.na(share$eduyears) & !is.na(share$exerc), ]

# Summary stats
summary(share$stayshealthy)
summary(share$smoking)
summary(share$ever_smoked)
summary(share$female)
summary(share$age)
summary(share$income10)
summary(share$eduyears)
summary(share$bmi)
summary(share$exerc)

table(share$country,share$stayshealthy)


# save temp file
write.csv(share, paste0(data_out, "ch11_share.csv"), row.names = F)

################################################################################
# 2. PART - SIMPLE LPM MODELS
################################################################################

# Linear probability models of good health at endline and smoking

# (1) current smoker on RHS
lpm1 <- lm(stayshealthy ~ smoking, data=share)
summary(lpm1, vcov=sandwich)

# visualize this regression
share$pred1 <- predict(lpm1)

table(share$pred1, share$smoking)
table(share$stayshealthy, share$smoking)

#create weights
share<-share %>%
  group_by(smoking, stayshealthy) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))


g1<-ggplot(data = share, label=smoking) +
  geom_point(aes(x = smoking, y = pred1), size = 1, color=color[1], shape = 16) +
  geom_line(aes(x = smoking, y = pred1), colour=color[1],  size=0.7) +
  geom_point(aes(x = smoking, y = stayshealthy, size=weight_2), fill = color[2], color=color[2], shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Current smoker",y = "Staying healthy / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))+
  theme_bg() 
g1
#save_fig("health_smoking_lpm_R", output, "small")FIXME
save_fig("ch11-figure-1-health-smoking-lpm", output, "small")

# (2) current smoker and ever smoked on RHS
lpm2 <- lm(stayshealthy ~ smoking + ever_smoked, data=share)
summary(lpm2, vcov=sandwich)

stargazer(list(lpm1, lpm2), digits=3, out=paste(output,"T11_reg1_R.html",sep=""))
rm(lpm2)
rm(lpm1)



# adding other right-hand-side variables
# first check some functional forms

share<-share %>%
  group_by(eduyears, stayshealthy) %>%
  mutate(weight = n()/100)
  
g2a<-ggplot(data = share, aes(x=eduyears, y=stayshealthy)) +
#  geom_point(aes(x = eduyears, y = stayshealthy, size=weight), color=color[1], shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  geom_smooth_da(method="loess", color=color[1]) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,25), breaks = seq(0,25,4))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Years of education",y = "Probability of staying healthy ") +
  #labs(x = "Education (years of schooling)",y = "Probability of staying healthy ") +
  theme_bg() 
g2a
#save_fig("health_edu_R", output, "small") 
save_fig("ch11-figure-2a-health-edu", output, "small")

g2b<-ggplot(data = share, aes(x=income10, y=stayshealthy)) +
  geom_smooth_da(method="loess", color=color[1]) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(1,10), breaks = seq(1,10,1))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Income group within country (deciles)",y = "Probability of staying healthy ") +
  theme_bg()
g2b
#save_fig("health_income_R", output, "small") FIXME
save_fig("ch11-figure-2b-health-income", output, "small")

g2c<-ggplot(data = share, aes(x=age, y=stayshealthy)) +
  geom_smooth_da(method="loess", color=color[1]) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
  labs(x = "Age at interview (years)",y = "Probability of staying healthy") +
  theme_bg() 
g2c



g2d<-ggplot(data = share, aes(x=bmi, y=stayshealthy)) +
  geom_smooth(method="loess", se=F, color=color[1], size=1.5) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "Body mass index",y = "Stays healthy") +
  scale_x_continuous(limits = c(10,50), breaks = seq(10,50, 10))+
  theme_bg() 
g2d

################################################################################
# 3. PART - PROBABILITY MODELS (LPM, LOGIT, PROBIT) & PREDICTION
################################################################################

# creating piecewise linear spline variables from education and bmi
lpm3 <-lm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
            income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share)
summary(lpm3, vcov=sandwich)
stargazer(lpm3, digits=3, out=paste(output,"T11_reg2_R.html",sep=""))

# predicted probabilities 
share$pred_lpm <- predict(lpm3)
summary(share$pred_lpm)

share_pred_lpm <- data.table(share)
share_pred_lpm[,.(mean=mean(share$pred_lpm), sd = sd(share$pred_lpm), min = min(share$pred_lpm) , max = max(share$pred_lpm),
         q25 = quantile(share$pred_lpm, probs = c(0.25)), q50 = quantile(share$pred_lpm, probs = c(0.5)), 
         q75 = quantile(share$pred_lpm, probs = c(0.75)),.N)]

rm(share_pred_lpm)

g3<-ggplot(data=share, aes(x=pred_lpm)) +
  geom_histogram_da(type='percent', binwidth=0.02) +
  coord_cartesian(xlim = c(0, 1.2)) +
  labs(x = "Predicted probability of staying healthy (LPM)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,0.07), breaks = seq(0, 0.07, 0.01), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.001,0.01), limits = c(0,1.1), breaks = seq(0,1.1, 0.2)) +
  theme_bg() 
g3
#save_fig("pred_histogram_lpm_R", output, "small")
save_fig("ch11-figure-3-pred-histogram-lpm", output, "small")

# list top 1% and bottom 1%
share <- share[!is.na(share$pred_lpm), ]

share <- share %>% 
  mutate(q100_pred_lpm = ntile(pred_lpm, 100))

summary(share[share$q100_pred_lpm==1, ]$smoking)
summary(share[share$q100_pred_lpm==1, ]$ever_smoked) 
summary(share[share$q100_pred_lpm==1, ]$female) 
summary(share[share$q100_pred_lpm==1, ]$age) 
summary(share[share$q100_pred_lpm==1, ]$eduyears) 
summary(share[share$q100_pred_lpm==1, ]$income10) 
summary(share[share$q100_pred_lpm==1, ]$bmi)
summary(share[share$q100_pred_lpm==1, ]$exerc)

summary(share[share$q100_pred_lpm==100, ]$smoking)
summary(share[share$q100_pred_lpm==100, ]$ever_smoked) 
summary(share[share$q100_pred_lpm==100, ]$female) 
summary(share[share$q100_pred_lpm==100, ]$age) 
summary(share[share$q100_pred_lpm==100, ]$eduyears) 
summary(share[share$q100_pred_lpm==100, ]$income10) 
summary(share[share$q100_pred_lpm==100, ]$bmi)
summary(share[share$q100_pred_lpm==100, ]$exerc)

rm(lpm3)

################################################################################
# 4. PART - LOGIT VS. PROBIT MODELS
################################################################################

# lpm versus logit and probit
# with all right-hand-side variables

# lpm (repeating the previous regression)
lpm <-lm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
              income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share)
summary(lpm, vcov=sandwich)

# logit coefficients
logit <- glm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
               income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, family='binomial')
summary(logit)

#TODO
# consider change to margins package


# predicted probabilities 
share$pred_logit <- predict.glm(logit, type="response")
summary(share$pred_logit)


# logit marginal differences
logit_marg <- logitmfx(formula = stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
                          income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, atmean=FALSE)
print(logit_marg)

# Or calculate manually
share$eduyears0_8 <- ifelse(share$eduyears <=8, share$eduyears ,8)
share$eduyears8_18 <- ifelse(share$eduyears <=8, 0, ifelse(share$eduyears>8 & share$eduyears<18, share$eduyears-8, share$eduyears-(share$eduyears-18)-8))
share$eduyears18 <- ifelse(share$eduyears<=18, 0, share$eduyears-18)

share$bmi16_35 <- ifelse(share$bmi <=35, share$bmi ,35)
share$bmi35 <- ifelse(share$bmi<=35, 0, share$bmi-35)

logit2 <- glm(formula = stayshealthy ~ smoking + ever_smoked + female + age + eduyears0_8 + eduyears8_18 + eduyears18 + 
                income10 + bmi16_35 + bmi35 + exerc + as.factor(country), data=share, family='binomial')

logit_marg2 <- margins(logit2)
summary(logit_marg2)

# probit coefficients
probit <- glm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
               income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, family=binomial(link="probit"))
summary(probit)

# predicted probabilities 
share$pred_probit<- predict.glm(probit, type="response") 
summary(share$pred_probit)

# probit marginal differences
probit_marg <- probitmfx(formula = stayshealthy ~ smoking + ever_smoked + female + age + eduyears0_8 + eduyears8_18 + eduyears18 + 
                       income10 + bmi16_35 + bmi35 + exerc + as.factor(country), data=share, atmean=FALSE)
print(probit_marg)

# FIXME: looks weird
g5<-ggplot(data = share) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.4,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.4,  shape=16) +
  #geom_line(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=0.3) +
  #geom_line(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=0.3) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=0.4) +
  labs(x = "Predicted probability of staying healthy (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c(color[3], color[1],color[2])) +
  theme_bg()+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4))
g5
#save_fig("pred_scatter_3models_R", output, "small")
save_fig("ch11-figure-5-pred-scatter-3models", output, "small")

stargazer(list(lpm, logit, probit), digits=3, out=paste(output,"T11_reg3_R.html",sep=""))

# FIXME:  save marginals 
# if mfx - could be useful https://github.com/tidymodels/broom/pull/756

################################################################################
# 5. PART - GOF
################################################################################
# GOODNESS OF FIT

# re-estimate the simplest lpm
lpmbase <- lm(stayshealthy ~ smoking, data=share)
share$pred_lpmbase <- predict(lpmbase) 


# DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
# LPM simple model
g7a<-ggplot(data = share,aes(x=pred_lpmbase)) + 
  geom_histogram(data=subset(share[share$stayshealthy == 1, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(share[share$stayshealthy == 0, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = color[1]),labels=c("Did not stay healthy","Stayed healthy")) +
  scale_color_manual(name="", values=c("0" = color[2], "1" = color[1]),labels=c("Did not stay healthy","Stayed healthy")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20)) +
  theme_bg() +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))
g7a
#save_fig("pred_hist_byoutcome_lpmbase_R", output, "small")FIXME
save_fig("ch11-figure-7a-pred-hist-byoutcome-lpmbase", output, "small")

# LPM rich model
g7b<-ggplot(data = share,aes(x=pred_lpm)) + 
  geom_histogram(data=subset(share[share$stayshealthy == 1, ]), 
    aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100),
     binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(share[share$stayshealthy == 0, ]), 
    aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100), 
    binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = color[1]),labels=c("Did not stay healthy","Stayed healthy")) +
  scale_color_manual(name="", values=c("0" = color[2], "1" = color[1]),labels=c("Did not stay healthy","Stayed healthy")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,20), breaks = seq(0,20,4)) +
  theme_bg() +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))
g7b
#save_fig("pred_hist_byoutcome_lpm_R", output, "small")FIXME
save_fig("ch11-figure-7b-pred-hist-byoutcome-lpm", output, "small")

### SUMMARY STATS OF PREDICTED PROBABILITIES BY OUTCOME
dt_pred = data.table(share)
dt_pred[,list(mean_lpmbase=mean(pred_lpmbase), mean_lpm=mean(pred_lpm), mean_logit=mean(pred_logit), mean_probit=mean(pred_probit)),by=list(stayshealthy)]
dt_pred[,list(median_lpmbase=median(pred_lpmbase), median_lpm=median(pred_lpm), median_logit=median(pred_logit), median_probit=median(pred_probit)),by=list(stayshealthy)]

rm(logit, logit_marg, logit_marg2, logit2, lpm, lpmbase, probit, probit_marg, dt_pred)


# calibration curves

share %>% 
  ungroup() %>%
  create_calibration_plot( 
    file_name = "ch11-figure-8b-calib-logit", 
    prob_var = "pred_logit", 
    actual_var = "stayshealthy", 
    breaks = c(0, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 1.05)
  )  


share %>% 
  ungroup() %>%
  create_calibration_plot( 
    file_name = "ch11-figure-8a-calib-lpm", 
    prob_var = "pred_lpm", 
    actual_var = "stayshealthy", 
    # n_bins = 10
    breaks = c(0, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 1.05)
    )


################################################################################
# 7. PART - CONFUSION TABLES
################################################################################
### CLASSIFICATION, CONFUSION TABLES
# classify ofservations
df <- data.frame(share$pred_lpmbase, share$pred_lpm, share$pred_logit, share$pred_probit)

for (i in 1:nrow(df)) {
  for (j in 1:ncol(df)) {
    
    if (df[i,j]>0.5) {df[i,j]=1}
    else {df[i,j]=0}
  }
}

# confusion matrix
for (j in 1:ncol(df)){
  print(prop.table(table(df[, j], share$stayshealthy)))
  print(prop.table(table(df[, j], share$stayshealthy), margin=2))
}

rm(df, i, j)
################################################################################
# 8. PART - LOGIT & PROBIT CURVES
################################################################################

# ILLUSTRATION LOGIT AND PROBIT CURVES
share <- read.csv(paste0(data_out,"ch11_share.csv"),stringsAsFactors = F)

share <- share[!is.na(share$bmi) & !is.na(share$eduyears) & !is.na(share$exerc), ]

# estimate logit, predict bx instead of p
logit <- glm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
               income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, family='binomial')
summary(logit)
share$bx_logit <- predict.lm(logit)
share$illustr_logit <- logistic(share$bx_logit)

# estimate probit, predict bx instead of p
probit <- glm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
                income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, family=binomial(link="probit"))
summary(probit)

share$bx_probit <- predict.lm(probit)
share$illustr_probit <- pnorm(share$bx_probit)

#Figure 11.4: The logit and probit link functions
g4<-ggplot(data = share) + 
  geom_line(aes(x=bx_logit, y=illustr_logit, color="Logit"), size=0.7) +
  geom_line(aes(x=bx_logit, y=illustr_probit, color="Probit"), size=0.7) +
  ylab("Probability") +
  xlab("z values") +
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_color_manual(name="", values=c(color[1],color[2])) +
theme_bg() +
  theme(axis.line.y=element_line(color="grey70",size=.1))

g4
#save_fig("logit_probit_curves_R", output, "small")
save_fig("ch11-figure-4-logit-probit-curves", output, "small")

#TODO save tables
#ch11-table-1-smoking-reg1
#ch11-table-2-smoking-reg2
#ch11-table-3-smoking-reg3
#ch11-table-4-mean-med