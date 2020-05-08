

########################################################################
#
# DATA ANALYSIS TEXTBOOK
# ILLUSTRATION STUDY FOR CHAPTER 8
#
# Measurement error, data cleaning
# data downloaded from a hotels price comparison site on October 27, 2017
#
########################################################################

# WHAT THIS CODES DOES:
# looks at the role of measurement error

# v 2.1 2018-11-01  graph edits, + xtable
# v 2.2 2020-03-21  graph axis edits
# v 2.3 2020-04-10 minor check
# v1.1 2020-04-25 names ok


# CLEAR MEMORY
rm(list=ls())

library(haven)
library(lspline)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(plyr)
library(scales)

# CHANGE IT TO YOUR WORKING DIRECTORY
############################################################  
# SET YOUR OWN PATH HERE
############################################################  
# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


#location folders
data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
data_out <- paste0(dir,"da_case_studies/ch08-hotels-measurement-error/")
output <-  paste0(dir,"da_case_studies/ch08-hotels-measurement-error/output/")
func <-    paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R"))


# load vienna
hotels <- read_csv(paste0(data_in,"hotels-vienna.csv"))


# ------------------------------------------------------------------------------------------------------
####SAMPLE SELECTION
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(price<=600)




#######################################
# Look at measurement error by rating count
#######################################


# TAKE LOG PRICE
hotels$lnprice <- log(hotels$price)

# define cutoffs
k1=100
k2=200

# FIGURE
reg_me <- lm(lnprice ~ rating, data=subset(hotels, rating_count<k1))
summary(reg_me)
hotels$yhat<-predict(reg_me,hotels)

reg_me2 <- lm(lnprice ~ rating, data=subset(hotels, rating_count>=k1 & rating_count<k2))
summary(reg_me2)
hotels$yhat2<-predict(reg_me2,hotels)

reg_me3 <- lm(lnprice ~ rating, data=subset(hotels, rating_count>=k2))
summary(reg_me3)
hotels$yhat3<-predict(reg_me3,hotels)


F08_noise1<- ggplot(data = hotels) +
  geom_line(aes(x = rating, y = yhat, color = color[2]), size = 1)+ 
  geom_line(aes(x = rating, y = yhat3, color = color[1]), size = 1)+ 
  scale_color_manual(name = "", values=c(color[2], color[1]), labels=NULL, guide = 'none') +
  coord_cartesian(xlim = c(2, 5), ylim = c(3.5, 5)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(2,5), breaks=seq(2,5, by=0.5)) +
  labs(x = "Average rating",y = "ln(Hotel price, US dollars)")+
  theme_bg() +
  annotate("text", x = 2.6, y = 4.4, label = "More noisy: # of ratings<100", size=2, color=color[2])+
  annotate("text", x = 3.1, y = 3.6, label = "Less noisy: # of ratings>200", size=2, color=color[1])
F08_noise1
#save_fig("F08_noise1_R", output, "small")
save_fig("ch08-figure-8-hotels-measerror", output, "small")



