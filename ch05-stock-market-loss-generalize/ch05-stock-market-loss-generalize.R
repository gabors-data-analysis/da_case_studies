################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 03
# CH03B Comparing hotel prices in Europe: Vienna vs. London 	
# hotels-europe dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(arm)
library(pastecs)
library(DataCombine)
library(janitor)


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

data_in <- paste(data_dir,"sp500","clean/", sep = "/")

use_case_dir <- "ch05-stock-market-loss-generalize/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------
# LOAD  DATA
sp500 <- read_csv(paste0(data_in,"SP500_2006_16_data.csv"),na = c("", "#N/A"))
sp500 <- subset(sp500, VALUE != "NA")



# CREATE PERCENT RETURN
sp500<- sp500 %>% 
  mutate(pct_return = (VALUE - lag(VALUE)) / lag(VALUE) * 100)

# CREATE DATE VARIABLE
sp500$year <- format(sp500$DATE, "%Y")
sp500$month <- format(sp500$DATE, "%m")
sp500$year <- as.numeric(sp500$year)
sp500$month <- as.numeric(sp500$month)
sp500$yearmonth <- sp500$year*100 + sp500$month

# Distribution 

# Figure 5.1
returns_histogram <-ggplot(sp500,aes(pct_return))+
  geom_histogram_da(binwidth = 0.25, type="frequency")+
  geom_vline(xintercept = -5, size = 0.7, color=color[2])+
  labs(x = "Daily return (percent)", y = "Frequency") +
  coord_cartesian(xlim = c(-10, 10), ylim = c(0, 400)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_segment(aes(x = -6, y = 220, xend = -5, yend = 220), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = -8, y = 220, label = "5% loss", size=2.5)+
    theme_bg() 
returns_histogram
#save_fig("returns_histogram_R", output, "small")
save_fig("ch05-figure-1-returns-histogram", output, "small")


# Figure 5.2 prep

# Create 10 000 samples, with 500 and 1000 observations in each sample, taken from sp500$pct_return

# in every sample: for each observation, check if it is a loss of 5% or more. Then calculate the percentage of observations out of 500 or 1000
# where the loss exceeds 5%. E.g. for the 233rd sample, 9 out of 1000 obs had larger than 5% losses. 

# remove first row as it has NA in pct_return
pct_return <- sp500 %>% filter(!is.na(pct_return)) %>% pull(pct_return)


#  function for a specified number of samples: draws a specified number of observations from a vector, calculates the percentage of obs with greater than 5% losses 
# 3 inputs: 'vector' is a vector of the source data, in this case pct_return. 'n_samples' is the number of samples we want to use. 
# 'n_obs' is the number of observations in each sample
# output is a vector
create_samples <- function(vector, n_samples, n_obs) {
  samples_pcloss <- c()
  for (i in 1:n_samples){
    
    single_sample <- sample(vector,n_obs, replace = FALSE)
    samples_pcloss[i] <- sum(single_sample < -5)/n_obs*100 
   
  }
  samples_pcloss
}

set.seed(123)

# Figure 5.2, 5.3, 5.4 input
nobs_1000 <- create_samples(pct_return, 10000, 1000)
nobs_500 <- create_samples(pct_return, 10000, 500)
#nobs_df <- as.data.frame(cbind(nobs_500, nobs_1000))
nobs_df <- tibble(nobs_500,nobs_1000)


error <- qnorm(0.975)*sd(nobs_df$nobs_1000)/sqrt(length(nobs_df$nobs_1000))
left <- mean(nobs_df$nobs_1000)-error
right <- mean(nobs_df$nobs_1000)+error


# Figure 5.2
options(digits = 2)

resample1000<-ggplot(nobs_df,aes(nobs_1000)) +
      geom_histogram(binwidth = 0.1, color = color.outline, fill = color[1], alpha = 0.8, boundary=0, closed='left') +
      labs(x = "Percent of days with losses of 5% or more", y = "Frequency") +
      geom_vline(aes(xintercept = mean(nobs_500)), color =color[2], size = 0.7) +
      coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 2500)) +
      scale_x_continuous(expand=c(0.01, 0.01), limits = c(0,1.5), breaks = seq(0, 1.5, by = 0.25)) +
      scale_y_continuous(expand=c(0.00, 0.00), limits = c(0,2500),breaks = seq(0, 2500, by = 500)) +
      geom_segment(aes(x = 0.8, y = 2000, xend = 0.53, yend = 2000), arrow = arrow(length = unit(0.1, "cm")))+
      annotate("text", x = 0.85, y = 2200, label = "Mean", size=2.5)+
      theme_bg()
resample1000
save_fig("ch05-figure-2-resample1000", output, "small")


# Figure 5.3
ggplot(nobs_df,aes(nobs_1000))+
  stat_density(geom="line", aes(color = 'n1000'), bw = 0.45,size = 1,kernel = "epanechnikov")+
  stat_density(geom="line",aes(nobs_500, color = "n500"), bw=0.45,linetype="twodash", size = 1,kernel = "epanechnikov")+
  labs(x="Percent of days with losses over 5%", y="Density")+
  geom_vline(xintercept = 0.5,colour=color[3], size = 0.7, linetype="dashed")+
  geom_segment(aes(x = 0.9, y = 0.72, xend = 0.65, yend = 0.72), size = 0.5, arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 1.1, y = 0.72, label = "Larger sample", size=2)+
  geom_segment(aes(x = 0.9, y = 0.68, xend = 0.65, yend = 0.68), size = 0.5, arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 1.1, y = 0.68, label = "Smaller sample", size=2) +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(0,1.5), breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_continuous(expand=c(0.00, 0.00), limits = c(0,0.8),breaks = seq(0, 0.8, by = 0.2)) +
  scale_color_manual(name = "", values = c(n1000= color[1], n500 = color[2]))+
  theme_bg()+
  theme(legend.position = "none")
#save_fig("ch05-figure-3-resample-densities", output, "small")



gh<-ggplot(data=nobs_df)+
  geom_histogram( aes(x=nobs_500, y = (..count..)/sum(..count..)*100, color = "n500", fill = "n500"), binwidth = 0.2, boundary=0, closed='left', alpha=0.7) +
  geom_histogram( aes(x=nobs_1000, y = (..count..)/sum(..count..)*100, color = "n1000", fill = "n1000"), binwidth = 0.2, boundary=0, closed='left', alpha=0.1, size=0.7) +
    ylab("Percent") +
  xlab("Percent of days with losses over 5%") +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(0,1.6), breaks = seq(0, 1.6, by = 0.2)) +
  scale_y_continuous(expand=c(0.00, 0.00), limits = c(0,50)) +
  scale_color_manual(name = "", values = c(color[2], color[1])) +
  scale_fill_manual(name = "", values = c(color[2], color[1])) +
  theme_bg() +
  theme(legend.position = c(0.7,0.9),
        legend.key.size = unit(x = 0.4, units = "cm"),
        legend.direction = "horizontal")
gh
save_fig("ch05-figure-3-resample-hist", output, "small")


options(digits = 1)

#table

janitor::tabyl(nobs_df$nobs_500, sort = TRUE)
#summarytools::freq(nobs_df$nobs_500, order = "freq")
janitor::tabyl(nobs_df$nobs_1000, sort = TRUE)

####################################
#BOOTSRTAP SAMPLES
set.seed(573164)
M <- 10000

Results <- matrix(rep(0,(M*10)),nrow=M,ncol=10)

for (i in 1:M){
  bsample <- sample(sp500$pct_return,size=dim(sp500)[1], replace = TRUE)
  
  for (j in 1:10){
    loss <- as.numeric(bsample<(-j))*100
    Results[i,j] <- mean(loss, na.rm=T)
  }
}

Results <- as_tibble(Results)
names(Results) <- c("loss1","loss2","loss3","loss4","loss5","loss6",
                       "loss7","loss8","loss9","loss10") 
  

# Figure 5.5
bootstrap<- ggplot(Results,aes(loss5))+
    geom_histogram_da(type="frequency", binwidth = 0.04, boundary=0, closed='left')+
    scale_y_continuous(expand=c(0,0),limits = c(0,1200), breaks = seq(0,1200,200)) +
    scale_x_continuous(expand=c(0.01,0.01),limits=c(0,1.2), breaks = seq(0,1.2,0.1)) +
  labs(x = "Percent of days with losses of 5% or more", y = "Frequency")+
  theme_bg()
bootstrap
save_fig("ch05-figure-5-bootstrap", output, "small")

