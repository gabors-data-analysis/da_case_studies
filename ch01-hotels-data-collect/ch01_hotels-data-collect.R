#############################################
# Chapter 01
#
# First look at the hotels data
# 
#############################################


library(plyr)

# set the path
dir <- "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

#location folders
data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
data_out  <- paste0(dir,"da_case_studies/ch01-hotels-data-collect/")
output <- paste0(dir,"da_case_studies/ch01-hotels-data-collect/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")



# load in clean and tidy data and create workfile
df <-  read.csv(paste0(data_in,"hotels-vienna.csv"))

############################################
# First look
############################################
df <- df%>%
  select(hotel_id, accommodation_type, country, city, city_actual, neighbourhood, center1label, distance,
          center2label, distance_alter, stars, rating, rating_count, ratingta, ratingta_count, year, month,
          weekend, holiday, nnights, price, scarce_room, offer, offer_cat)

summary(df)

# export list
df <- subset(df, select = c(hotel_id, accommodation_type, country, city, city_actual, center1label, distance, stars, rating, price))
write.csv(df[1:5,], paste0(output, "hotel_listobs.csv"), row.names = F)

