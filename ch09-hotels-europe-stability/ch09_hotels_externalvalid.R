####################################################
#DATA ANALYSIS TEXTBOOK
#FUNDAMENTALS OF REGRESSION ANALYSIS
#ILLUSTRATION STUDY
#Hotel prices and distance to city center

# several cities and dates
#data downloaded from a hotels price comparison site on Oct 29, 2017


# v 1.1 2020-04-01 huxtable added + graph changes
# v 1.1 2020-04-23 names ok

#################

# CLEAR MEMORY
rm(list=ls())

#install.packages("rms")

# Inmort libraries
library(haven)
library(data.table)
library(rms)
library(plyr)
library(xtabs)
library(dplyr)
library(lspline)
library(tidyverse)
library(huxtable)

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

data_in <- paste0(dir,"da_data_repo/hotels-europe/clean/")
data_out<- paste0(dir,"da_case_studies/ch09-hotels-europe-stability/")
output  <- paste0(dir,"da_case_studies/ch09-hotels-europe-stability/output/")
func    <- paste0(dir,"da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))


# load in clean and tidy data and create workfile
hotels_europe_price <- read_csv(paste0(data_in,"hotels-europe_price.csv"))
hotels_europe_features <- read_csv(paste0(data_in,"hotels-europe_features.csv"))

data <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price)
rm(hotels_europe_features)

# filter a few cities
data <- data[data$city_actual=="Vienna" | data$city_actual=="Amsterdam" | data$city_actual=="Barcelona", ]
data <- data[data$accommodation_type=="Hotel" | data$accommodation_type=="Apartment",]

# drop long stay , 1000E+
data <- data[data$nnights!=4, ]
data <- data[data$price<=1000, ]

# check for duplicates
data<-data[!duplicated(data),]

# filter for days
data$date <- ifelse(data$month==11 & data$weekend==0, "2017-NOV-weekday", 
            ifelse(data$month==11 & data$weekend==1, "2017-NOV-weekend", 
            ifelse(data$month==12 & data$holiday==1, "2017-DEC-holiday", 
            ifelse(data$month==6 & data$weekend==1, "2018-JUNE-weekend", NA))))

data <- data[!is.na(data$date), ]

# Crosstabs - number of obs
table(data$city)
xtabs(~ data$accommodation_type + data$city, data)
xtabs(~ data$date + data$city, data)

# take log if price
data$lnprice <- log(data$price)

# select variables
data <- data[, c("hotel_id", "date", "city", "accommodation_type", "stars", "rating", "distance", "price", "lnprice")]
                 
write.csv(data,paste(data_out,"hotels_work.csv",sep=""))               
        
#############################################################################################################################x
# External validity by time
data <- as.data.frame(read.csv(paste0(data_out,"hotels_work.csv"),
                  stringsAsFactors = F))

data <- data[data$stars>=3 & data$stars<=4, ]
data <- data[data$accommodation_type=="Hotel",]
data <- data[data$city=="Vienna",]
table(data$date)

summary(data$distance)
summary(data$price)
summary(data$lnprice)

# TODO simplify, make it nicer as before

# summary stats by variables
ddply(data,~date,summarise,mean=mean(distance), min=min(distance), max=max(distance), median=median(distance), n=length(distance))
ddply(data,~date,summarise,mean=mean(price), min=min(price), max=max(price), median=median(price), n=length(price))
ddply(data,~date,summarise,mean=mean(lnprice), min=min(lnprice), max=max(lnprice), median=median(lnprice), n=length(lnprice))


t <-data %>% group_by(date) %>% dplyr::summarize(mean=mean(distance), 
                                                 median=median(distance),
                                                 min=min(distance),
                                                 max(distance),
                                                 n())

t %>% as_huxtable(add_colnames=TRUE) %>%
  set_all_borders(1) %>% 
  set_number_format(1:5,2:5,2) %>%  
  set_col_width(c(2,1,1,1,1,1)) %>%
  set_caption("Distance")



t <-data %>% group_by(date) %>% dplyr::summarize(mean=mean(price), 
                                                 median=median(price),
                                                 min=min(price),
                                                 max(price),
                                                 n())

t %>% as_huxtable(add_colnames=TRUE) %>%
                                    set_all_borders(1) %>% 
                                    set_number_format(1:5,2:5,2) %>%  
                                    set_col_width(c(2,1,1,1,1,1)) %>%
                                    set_caption("Price")

t <-data %>% group_by(date) %>% dplyr::summarize(mean=mean(lnprice), 
                                                 median=median(lnprice),
                                                 min=min(lnprice),
                                                 max(lnprice),
                                                 n())



# Regressions with three dates for textbook
# Original regression
reg_rob<-ols(lnprice ~ lspline(distance, 2)  ,data[data$date=="2017-NOV-weekday", ], x=TRUE)
reg_rob



# show robust SE
robcov(reg_rob)

# Other dates
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

for (d in unique(data[data$date!="2017-NOV-weekday", ]$date)) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE)
  print(ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE))
  coeff_intercept[d] <- reg$coefficients[[1]]
  coeff_dist_0_2[d] <- reg$coefficients[[2]]
  coeff_dist_2_7[d] <- reg$coefficients[[3]]
}

data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

# we compare coeffs for different dates
rm(reg, reg_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, d)

# TODO
# Table 9.3 dates, all  hotels
# simplify and use stargazer or estimatR, show regressions with robust SE as in chapter09


# same with hotels restricted to be the same

# first create variable that counts the number of times a hotel is in the data
data <- data %>%
  group_by(hotel_id) %>%
  mutate(hotelcount = n())
table(data$hotelcount)

data <- data[data$hotelcount==4, ]

# original regression
reg2_rob<-ols(lnprice ~ lspline(distance, 2)  ,data[data$date=="2017-NOV-weekday", ], x=TRUE)
robcov(reg2_rob)

# other dates 
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

for (d in unique(data[data$date!="2017-NOV-weekday", ]$date)) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE)
  #print(ols(lnprice ~ lspline(distance, 2)  ,data[data$date==d, ], x=TRUE))
  coeff_intercept[d] <- reg$coefficients[[1]]
  coeff_dist_0_2[d] <- reg$coefficients[[2]]
  coeff_dist_2_7[d] <- reg$coefficients[[3]]
}

data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

rm(reg, reg2_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, d)
# ERROR, baseline is not added.

# TODO
# Table 9.4 dates, same hotels


#########################################################################################################################xx
# External validity by city
data <- as.data.frame(read.csv(paste0(data_out,"hotels_work.csv"),
                               stringsAsFactors = F))

data <- data[data$stars>=3 & data$stars<=4, ]
data <- data[data$accommodation_type=="Hotel",]
data <- data[data$date=="2017-NOV-weekday",]

ddply(data,~city,summarise,mean=mean(distance), min=min(distance), max=max(distance), median=median(distance), n=length(distance))
ddply(data,~city,summarise,mean=mean(price), min=min(price), max=max(price), median=median(price), n=length(price))
ddply(data,~city,summarise,mean=mean(lnprice), min=min(lnprice), max=max(lnprice), median=median(lnprice), n=length(lnprice))

# Regressions for three cities
# original regression
reg3_rob<-ols(lnprice ~ lspline(distance, 2)  ,data[data$city=="Vienna", ], x=TRUE)
robcov(reg3_rob)

# other cities 
coeff_intercept <- c()
coeff_dist_0_2 <- c()
coeff_dist_2_7 <- c()
i=0

for (c in unique(data[data$city=="Amsterdam" | data$city=="Barcelona", ]$city)) {
  i=i+1
  reg <- ols(lnprice ~ lspline(distance, 2)  ,data[data$city==c, ], x=TRUE)
  #print(ols(lnprice ~ lspline(distance, 2)  ,data[data$city==c, ], x=TRUE))
  coeff_intercept[c] <- reg$coefficients[[1]]
  coeff_dist_0_2[c] <- reg$coefficients[[2]]
  coeff_dist_2_7[c] <- reg$coefficients[[3]]
}

data.frame(coeff_intercept, coeff_dist_0_2, coeff_dist_2_7)

rm(reg, reg3_rob, i, coeff_intercept, coeff_dist_0_2, coeff_dist_2_7, c)

# TODO
# Table 9.5 cities

#######################################################################################################################
# External validity by accommodation type: hotels vs apartments
data <- as.data.frame(read.csv(paste0(data_out,"hotels_work.csv"),
                               stringsAsFactors = F))

data <- data[data$stars>=3 & data$stars<=4, ]
data <- data[data$city=="Vienna",]
data <- data[data$date=="2017-NOV-weekday",]

table(data$accommodation_type, data$stars)


ddply(data,~stars,summarise,mean=mean(distance), min=min(distance), max=max(distance), median=median(distance), n=length(distance))
ddply(data,~stars,summarise,mean=mean(price), min=min(price), max=max(price), median=median(price), n=length(price))
ddply(data,~stars,summarise,mean=mean(lnprice), min=min(lnprice), max=max(lnprice), median=median(lnprice), n=length(lnprice))


# regressions
reg4_rob<-ols(lnprice ~ lspline(distance, 2)  ,data[data$accommodation=="Hotel", ], x=TRUE)
robcov(reg4_rob)

reg5_rob<-ols(lnprice ~ lspline(distance, 2)  ,data[data$accommodation=="Apartment", ], x=TRUE)
robcov(reg5_rob)

# TODO
# Table 9.6 hotels v apartments
rm(reg4_rob, reg5_rob)

