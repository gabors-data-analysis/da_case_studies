###############################################
# Chapter 03

# city_size_japan : pop & lnpop
###############################################

# v1.1 2020-04-22 names ok


# WHAT THIS CODES DOES:
## creates desrciptive stats

###############################################


# CLEAR MEMORY
rm(list=ls())

# Import libraries

library(ggplot2)
library(tidyverse)
library(scales)


# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


#location folders
data_in <- paste0(dir,"da_data_repo/city-size-japan/clean/")
data_out <-  paste0(dir,"da_case_studies/ch03-city-size-japan/")
output <- paste0(dir,"da_case_studies/ch03-city-size-japan/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


# load ggplot theme function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))

#-----------------------------------------------------------------------------------------
# import data
city_size <- read_csv(paste0(data_in, "city-size-japan.csv"))
Hmisc::describe(city_size)
# create variables

city_size <-  city_size %>%
  mutate(
    pop = (pop_2015/1000),
    lnpop = log(pop)) %>%
  arrange(-pop)

city_size <- city_size %>%
  mutate (rank = seq( from = 1, to = nrow(.), by = 1))

#------------------------------------------------------------
# ln(rank) vs ln(x)

city_size <-  city_size %>%
  mutate(lnrank = log(rank))


R_03_lnrank <- ggplot(data = city_size, aes(x=lnpop, y=lnrank)) +
  geom_smooth_da(method="lm")+
  geom_point_da()+
  labs(x="ln(population)",y="ln(rank)")+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(5, 9.5), breaks = seq(5, 9.5, by = 0.5)) +
    theme_bg() 
R_03_lnrank
#save_fig("ch03_citysize-japan-logrank", output, "small")
save_fig("ch03-figure-12-city-logrank", output, "small")


#------------------------------------------------------------
## ln P(X>x) vs ln(x) figure
## should be the same s with ln(rank) except for constant shift


# city_size <-  city_size %>%
#   mutate(P = (rank / max(nrow(.))),
#          lnP = log(P))
# 
# R_03_lnP <- ggplot(data = city_size, aes(x=lnpop, y=lnP)) +
#   geom_point_da()+
#   labs(x="ln(population)",y="")+
#   geom_smooth_da(method="lm")+
#   theme_bg()
# R_03_lnP

#---------------------------------------------------------------
# scale invariance

x1 <-  200
x2 <- 300
bound <-  0.2

print(paste0(x1, " ", x2))

city_size %>%
  filter(pop >= x1*(1-bound) & pop <= x1*(1+bound)) %>%
  count()
  
city_size %>%
  filter(pop >= x2*(1-bound) & pop <= x2*(1+bound)) %>%
  count()  

shift <-  3  
x3 <-  x1*shift
x4 <-  x2*shift

print(paste0(x3, " ", x4))

city_size %>%
  filter(pop >= x3*(1-bound) & pop <= x3*(1+bound)) %>%
  count() 

city_size %>%
  filter(pop >= x4*(1-bound) & pop <= x4*(1+bound)) %>%
  count() 






  

