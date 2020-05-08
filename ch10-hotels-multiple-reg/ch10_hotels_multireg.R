#####################################################################
# DATA ANALYSIS TEXTBOOK
# MULTIVARIATE REGRESSION MODEL
# ILLUSTRATION STUDY FOR CHAPTER 10
#
# DATA HOTELS 
#####################################################################
  
# WHAT THIS CODES DOES:
# Run regression with multiple explanatory variables
# shows yhat y graph


# v 2.0 2019-11-14 edits
# v 2.1 2020-03-24 graph edits
# v 2.2 2020-03-27 graph edits
# v 2.3 2020-03-31 y-yhat graph edits
# v 2.4 2020-04-28 y-yhat graph edits, names ok



# Clear memory
rm(list=ls())

# Import libraries 
library(tidyverse)
library(lmtest)
library(sandwich) #
library(segmented)
library(stargazer)
library(haven)
library(lspline) #
library(gridExtra)
library(cowplot)
library(scales)


#TODO
# clear library list


       # CHANGE IT TO YOUR WORKING DIRECTORY
       ############################################################  
       # SET YOUR OWN PATH HERE
       ############################################################  
        # Sets the core parent directory
        current_path = rstudioapi::getActiveDocumentContext()$path 
        dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

       
       #location folders
       data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
       data_out <-  paste0(dir,"da_case_studies/ch10-hotels-multiple-reg/")
       output <- paste0(dir,"da_case_studies/ch10-hotels-multiple-reg/output/")
       func <- paste0(dir, "da_case_studies/ch00-tech-prep/")
       
       
       #call function
       source(paste0(func, "theme_bg.R"))
       
       # Created a helper function with some useful stuff
       source(paste0(func, "da_helper_functions.R")) 
       options(digits = 3) 
       
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
df <- data.frame(hotel_id = hotels$hotel_id, price= hotels$price, lnprice_resid=hotels$lnprice_resid, distanc=hotels$distance, stars=hotels$stars, rating=hotels$rating)
df[order(df$lnprice_resid)[1:5], ]
#TODO
# print out nice table

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
#save_fig("y_yhat_hotels_R", output, "large")
save_fig("ch10-figure-3-hotels-yhat-y", output, "large")


# residual - yhat graph
ggplot(data = hotels, aes(x = lnprice_hat, y = lnprice_resid)) +
  geom_point(color = color[1], size = 1,  shape = 16, alpha = 0.6, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="lm", colour=color[4], se=F, size=1) +
  labs(x = "ln(Predicted hotel price, US dollars)",y = "Residuals")+
  coord_cartesian(xlim = c(4, 5.5)) +
  theme_bg() +
  background_grid(major = "xy", minor="xy", size.major = 0.2)    





