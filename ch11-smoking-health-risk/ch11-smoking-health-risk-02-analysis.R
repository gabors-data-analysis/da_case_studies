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
# version 0.94 2021-11-29 - revised, two files now (cleaning separate)


#########################################
# PART 2 Analysis
#########################################

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# you may need this
#install.packages("mfx")
#install.packages("skimr")


# Import libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(lspline)
library(skimr)
library(fixest)
library(mfx)


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

data_work <- paste(data_dir,"share-health","clean/work/", sep = "/")

use_case_dir <- "ch11-smoking-health-risk/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)
create_output_if_doesnt_exist(data_work)


#-----------------------------------------------------------------------------------------

# load work data
#share <- read_csv(paste0(data_work,'share-health-filtered.csv'))

# load directly from OSF.io
share <- read_csv("https://osf.io/3ze58/download")
share%>%nrow()

# let us review the data
skimr::skim(share)

# Remove if any of important variable is missing
share <- share[!is.na(share$bmi) & !is.na(share$eduyears) & !is.na(share$exerc), ]


# Make descriptive statistics for selected variables
datasummary(stayshealthy+smoking+ever_smoked+female+age+income10+eduyears+bmi+exerc~
              mean + median + min + max + sd , data = share )

share%>%nrow()
####
# SATURATED LPM MODELS
#
# main regression is stayshealthy ~ RHS

# Linear probability models of good health at endline and smoking

# 1st model:current smoker on RHS
lpm1 <- feols( stayshealthy ~ smoking , data = share , vcov = 'hetero' )
lpm1

# Get the predicted values
share$pred1 <- predict( lpm1 )

# Compare smoking with predicted values and real outcomes
table(share$pred1, share$smoking)
table(share$stayshealthy, share$smoking)

# Create weights for prettier plot
share<-share %>%
  group_by(smoking, stayshealthy) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

# Show graph with actual and predicted probabilities
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


##
# 2nd model: current smoker and ever smoked on RHS
lpm2 <- feols( stayshealthy ~ smoking + ever_smoked , data = share , vcov = 'hetero' )
lpm2

# Table 11.1 Compare models
etable( lpm1 , lpm2,
        digits=3)


####
# 3. PART - LINEAR PROBABILITY MODELS & PREDICTION
#
# Using more RHS variables!
#   first check some functional forms

# For pretty plots create weigths for education
share<-share %>%
  group_by( eduyears, stayshealthy ) %>%
  mutate( weight = n()/100 )

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

###
# lpm3: linear probability model with many covariates:
#   smoking + ever_smoked + female + age + eduyears + income10 + bmi + exerc 
#       + as.factor(country)
#   use the P.L.S transformations:
#     eduyears: with knots at 8 (elementary only) and 18 (Diploma)
#     bmi: with knot at 35
#   and include country dummy variables as.factor(country) -> 
#     -> it automatically drops the first category: 11 (Austria), which is now the reference category

lpm3 <-feols( stayshealthy ~ smoking + ever_smoked + female +
             age + lspline(eduyears,c(8,18)) + income10 + lspline(bmi,c(35)) +
             exerc + as.factor(country),
           data = share , vcov = 'hetero')

# etable is very versatile to produce tables, can keep/drap vars, etc
# Table 11.2
etable(lpm3, 
       drop="country", digits=3
       )


# Check predicted probabilities: is there any interesting values?
# predicted probabilities
share$pred_lpm <- predict( lpm3 )
# Make a descriptive summary of the predictions!
datasummary( pred_lpm ~ min + max + mean + median + sd , 
             data = share, fmt="%.3f"  )

# Show the predicted probabilities' distribution (ggplot)
ggplot( share , aes( x = pred_lpm ) ) +
  geom_histogram( fill = 'navyblue' , color = 'grey90')


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


# We are interested in the top 1% and bottom 1% characteristics!
#   Is there any significant difference?

# Create bins which categorize the predicted values between 1-100
share <- share %>% 
  mutate( q100_pred_lpm = ntile(pred_lpm, 100) )

# Make a summary statistics, using sum_stat for the bottom (q100_pred_lpm==1) 
#   and top 1% (q100_pred_lpm==100), using stats = c('mean','median','sd')
#   and variables c('smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc')
#   use the num_obs = F input for sum_stat

# Top 1%
datasummary(smoking+ever_smoked+female+age+eduyears+income10+bmi+exerc~
              mean + median + sd , data = filter( share , q100_pred_lpm==100 ) ) 

# Bottom 1%
datasummary(smoking+ever_smoked+female+age+eduyears+income10+bmi+exerc~
              mean + median + sd , data = filter( share , q100_pred_lpm==1 ) ) 


# You may change the variable names to remove...
rm(lpm3)

####
# 4. PART - LOGIT AND PROBIT MODELS
#
# Lets compare
# lpm versus logit and probit
# with all right-hand-side variables

# If comparing different estimation methods for the same model setup:
#   good practice to make a 'formula' variable!
model_formula <- formula( stayshealthy ~ smoking + ever_smoked + female + age + 
                            lspline(eduyears, c(8,18)) + 
                            income10 + lspline(bmi, c(35)) + exerc + as.factor(country) )

# lpm (repeating the previous regression)
lpm <-feols( model_formula , data=share, vcov = 'hetero')
etable(lpm,        
       drop="country", digits=3)


# logit coefficients:
#   alternatively: familiy='binomial' automatically gives you logit, but not probit...
logit <- feglm( model_formula , data=share, family = binomial( link = "logit" ) )
etable(logit,
       drop="country", digits=3)


# predicted probabilities 
share$pred_logit <- predict( logit, type="response" )

# Calculate logit marginal differences
logit_marg <- logitmfx( model_formula, data=share, atmean=FALSE, robust = T )
print(logit_marg)

##
# Probit coefficients: replicate logit, but now use 'probit'
probit <- feglm( model_formula , data=share, family = binomial( link = "probit" ) )
probit

# predicted probabilities 
share$pred_probit<- predict( probit , type = "response" )

# probit marginal differences
probit_marg <- probitmfx(  model_formula, data=share, atmean=FALSE, robust = T)
print( probit_marg )

# Comparing predictions from the two models
datasummary(pred_logit + pred_probit~min+P25+Median+Mean+P75+Max,data=share)

###
# Creating a model summary output
etable( lpm, logit, probit,
        drop="country", digits=3 , fitstat = c('r2','pr2'))

###
# Replicate table 11.5
fitstat_register("brier", function(x){mean(x$residual^2)}, "Brier score")
fitstat_register("logloss", function(x){
  log_id <- !is.na( x$fitted.values ) & x$fitted.values != 1 & x$fitted.values != 0
  y   <- x$fitted.values[ log_id ] + x$residuals[ log_id ]
  lp  <- log( x$fitted.values[log_id] )
  lnp <- log( 1 - x$fitted.values[log_id] )
  nobs <- sum( log_id )
  return( 1 / nobs * sum( y * lp + ( 1 - y ) * lnp ) )
}, "log-loss")

etable( lpm, logit, probit , drop = "factor|lspline|income|exerc",fitstat = ~ r2 + brier + pr2 + logloss )

# If you want to include the marginals:
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpm, logit, logit_marg, probit, probit_marg)

msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          coef_omit = 'as.factor(country)*')

# could also save it by adding output...
msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          coef_omit = 'as.factor(country)*',
          output = paste0(output,"prob_models_coeff.html")
)


# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
cm <- c('(Intercept)' = 'Constant')
msummary(list(lpm, logit, probit),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*'
)

##
# Comparing predicted probabilities of logit and probit to LPM

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


####
# 5. PART - GOODNESS OF FIT
#


# re-estimate the simplest lpm
lpmbase <- feols( stayshealthy ~ smoking, data=share ,vcov = 'hetero')
share$pred_lpmbase <- predict( lpmbase ) 


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

#####
# Summary statistics on predicted probabilities:
#
# TO DO:
#   Create a CONDITIONAL sum_stat on stayhealth for:
#     "pred_lpmbase","pred_lpm","pred_logit","pred_probit" 
#   use: "mean","median","min","max","sd"
#
#   Hint: you may do two tables for staying healthy and not staying healty and use sum_stat on those
#  
datasummary(pred_lpmbase+pred_lpm+pred_logit+pred_probit~mean+median+min+max+sd,
            data = filter( share , stayshealthy == 1) )
datasummary(pred_lpmbase+pred_lpm+pred_logit+pred_probit~mean+median+min+max+sd,
            data = filter( share , stayshealthy == 0) )


###
# Bias and Calibration curve
#
# Lets use the logit model!
#
# Biased prediction? Calculate bias!
#   Hint: bias = mean(prediction) - mean(actual)
bias <- mean( share$pred_logit ) - mean(share $stayshealthy)
# 

# calibration curves using the create_calibration_plot function we wrote

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
library(psych)

# ILLUSTRATION LOGIT AND PROBIT CURVES

share <- read_csv("https://osf.io/3ze58/download")

# Remove if any of important variable is missing
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