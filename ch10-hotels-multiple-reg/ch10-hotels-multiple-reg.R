################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 10
# CH10B Finding a good deal among hotels with multiple regression
# version 0.9 2020-08-31


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries 
library(tidyverse)
library(stargazer)
library(haven)
library(scales)
library(lspline)



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

data_in <- paste(data_dir,"hotels-vienna","clean/", sep = "/")
use_case_dir <- "ch10-hotels-multiple-reg/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

       
#####################################################################
       
       
# load vienna
hotels <- read_csv(paste0(data_in,"hotels-vienna.csv"))
# ------------------------------------------------------------------------------------------------------
####SAMPLE SELECTION
# Apply filters:  3-4 stars, Vienna actual, without  extreme value
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
        filter(city_actual=="Vienna") %>%
        filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
        filter(price<=600)
       
       
       
       
       
#####################################################################
# TAKE LOG 
hotels$lnprice <- log(hotels$price)

hotels$distance2<-hotels$distance
hotels$distance2[hotels$distance2<0.05] <- 0.05

hotels$lndistance<-log(hotels$distance2)



# Stars: binary indicators
hotels$star35 = ifelse(hotels$stars==3.5, 1, 0)
hotels$star4 = ifelse(hotels$stars==4, 1, 0)


summary(hotels$price)
summary(hotels$distance)
summary(hotels$lnprice)


#####################################################################
# Regressions
#####################################################################
# Basic
reg0 <- lm(lnprice ~ rating, data=hotels)

reg1 <- lm(lnprice ~ distance, data=hotels)

reg2 <- lm(lnprice ~ distance + rating, data=hotels)

# _r robust errors in stargazer
stargazer_r(list(reg0, reg1, reg2), se = 'robust', digits=3, out=paste(output,"T10_hotels_R.html",sep=""))

# more complex models
# Predicted values 
reg3 <- lm(lnprice ~ lspline(distance, c(1,4)) + lspline(rating, 3.5) + star35 + star4, data=hotels)
summary(reg3, vcov=sandwich)
hotels$lnprice_hat <- predict(reg3)
hotels$lnprice_resid <- hotels$lnprice - hotels$lnprice_hat
hotels$bestdeals <- ifelse(hotels$lnprice_resid %in% tail(sort(hotels$lnprice_resid, decreasing=TRUE),5),TRUE,FALSE)

# Compare R-sqared with distance only
reg4 <- lm(lnprice ~ lspline(distance, c(1,4)), data=hotels)
summary(reg4)

stargazer_r(list(reg1, reg2, reg3, reg4), se = 'robust', digits=3, out=paste(output,"T10_hotels2_R.tex",sep=""))


# List of 5 best deals
hotels %>%
  select(hotel_id, price, lnprice_resid, distance, stars, rating) %>%
  arrange(lnprice_resid) %>%
  .[1:5,] %>%
  as.data.frame() %>% 
  stargazer(summary= FALSE, digits = 1, out = paste(output,"T10_hotels_best_deals.tex",sep=""))

# y - yhat graph
y_yhat_hotels<- ggplot(data = hotels, aes(x = lnprice_hat, y = lnprice)) +
  geom_point(aes(color=bestdeals,shape=bestdeals), size = 1.2, fill=color[3], alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  #geom_smooth_da(method="lm") +
  geom_segment(aes(x = 3.8, y = 3.8, xend = 6, yend =6), size=0.8, color=color[2], linetype=2) +
    labs(x = "ln(predicted price, US dollars) ",y = "ln(price, US dollars)")+
  coord_cartesian(xlim = c(3.8, 6), ylim = c(3.8, 6)) +
  scale_colour_manual(name='',values=c(color[1],'black')) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 4.8, y = 3.9, xend = 4.68, yend = 4.05), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 4.93, y = 3.9, label = "Best deal", size=2.5)+
  theme_bg() +
  theme(axis.text.x=element_text(size=9)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) 
y_yhat_hotels
save_fig("ch10-figure-3-hotels-yhat-y", output, "large")


# residual - yhat graph (not in book)
ggplot(data = hotels, aes(x = lnprice_hat, y = lnprice_resid)) +
  geom_point(color = color[1], size = 1,  shape = 16, alpha = 0.6, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="lm", colour=color[4], se=F, size=1) +
  labs(x = "ln(Predicted hotel price, US dollars)",y = "Residuals")+
  coord_cartesian(xlim = c(4, 5.5)) +
  theme_bg() +
  background_grid(major = "xy", minor="xy", size.major = 0.2)    





