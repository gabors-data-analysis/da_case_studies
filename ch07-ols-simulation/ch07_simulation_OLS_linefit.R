############################################################
#
# DATA ANALYSIS TEXTBOOK
# CHAPTER 07
# ols fit  simulations

# v1.1 2019-12-23 line edits
# v1.2 2020-03-10 minor axes edits
# v1.3 2020-04-24 names ok
# v1.4 2020-04-27 labels

############################################################  
# WHAT THIS CODES DOES:
# ols and avg y avg x  simulation

# CLEAR MEMORY
rm(list=ls())

# Libraries
library(tidyverse)
library(ggplot2)


############################################################  
# SET YOUR OWN PATH HERE
############################################################  
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

#location folders
output <- paste0(dir,"da_case_studies/ch07-ols-simulation/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))

# set the seed
set.seed(1458)

# sample size
n <- 100

# uniformly distributed x, [0,4]
xvar <-  runif(n,0,4)

# y  = a + bx + u (u normally distributed)
a <- 2
b <- 0.5
sigmau <- 0.7
yvar <- a+b*xvar+rnorm(n,0,sigmau)


reg <- lm(yvar~xvar)
summary(reg)

# save coefficients
coeffs = coefficients(reg)

# scatterplot and OLS regression line
# average y and average x shown
ols <- data.frame(xvar,yvar)


F07_sim <- ggplot(data = ols, aes(x = xvar, y = yvar)) +
  geom_point_da() + 
  geom_smooth_da(method = "lm") +
  #geom_abline(intercept=coeffs[1], slope=coeffs[2], size=1.2, color=color[3]) + # alternative
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 4), breaks=seq(0, 4, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  labs(x = "Simulated x variable",y = "Simulated y variable")+
  theme_bg() +
  geom_vline(xintercept = mean(xvar), color=color[3], linetype="dashed", size=0.4) +
  geom_hline(yintercept = mean(yvar), color=color[3], linetype="dashed", size=0.4) +
  geom_segment(aes(x = 0.5, y = 3.5, xend = 0.5, yend = 2.9), arrow = arrow(length = unit(0.01, "cm")))+
  annotate("text", x = 0.3, y = 3.6, label = "Average y", size=2) +
  geom_segment(aes(x = 1.2, y = 4, xend = 1.9, yend = 4), arrow = arrow(length = unit(0.01, "cm")))+
  annotate("text", x = 0.9, y = 4, label = "Average x", size=2)
F07_sim
save_fig("ch07-figure-4-olsfit", output, "small")

