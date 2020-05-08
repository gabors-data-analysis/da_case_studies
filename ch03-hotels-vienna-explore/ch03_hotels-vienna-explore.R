###############################################
# Chapter 03
 
# DATA ANALYSIS TEXTBOOK
# CH03
# Describe hotels-vienna
# 
# v1.0.2019-10-07
# v1.1 2020-03-09 graph axes remastered
# v1.2 2020-03-13 labels edited
# v1.3 2020-03-21 labels edited
# v1.4 2020-04-06 graphs
# v1.4. 2020-04-06 minor graphs



# WHAT THIS CODES DOES:
# Focus on histograms

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
data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
data_out <-  paste0(dir,"da_case_studies/ch03-hotels-vienna-explore/")
output <- paste0(dir,"da_case_studies/ch03-hotels-vienna-explore/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))




# load vienna
vienna <- read_csv(paste0(data_in,"hotels-vienna.csv"))


####################################################################################
# Figures 1a and 1b
####################################################################################
# apply filters: Hotels
table(vienna$accommodation_type)

vienna_cut <- vienna %>% filter(accommodation_type=="Hotel")


histstars_Vienna <- ggplot(data = vienna_cut ,aes(x=stars, y = (..count..)/sum(..count..)))+
  geom_bar(color = color.outline, fill = color[1], alpha=0.8, na.rm=T) +
  geom_text(stat='count', aes(label=round((..count..)/sum(..count..)*100, 1)), vjust=-0.5, size = 2.5) +
  labs(x="Star rating (N. stars)", y="Percent") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0.5,5.5), breaks = seq(1, 5, 0.5)) +
  scale_y_continuous(expand = c(0.00,0.00), limits = c(0,0.5), breaks = seq(0, 0.5, 0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
histstars_Vienna
#save_fig("histstars_Vienna_R", output, "small")
save_fig("ch03-figure-1a-hist-stars", output, "small")


histstars_Vienna2 <- ggplot(data = vienna_cut ,aes(x=stars))+
  geom_bar(color = color.outline, fill = color[1], alpha=0.8, na.rm=T) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 2.5) +
  labs(x="Star rating (N. stars)", y="Frequency") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0.5,5.5), breaks = seq(1, 5, 0.5)) +
  scale_y_continuous(expand = c(0.00,0.00), limits = c(0,140), breaks = seq(0, 140, 20)) +
  theme_bg() 
histstars_Vienna2
#save_fig("histstars_Vienna2_R", output, "small")
save_fig("ch03-figure-1b-hist-stars", output, "small")



###############################################
### DISTRIBUTIONS
###############################################
# Apply filters:  3-4 stars, less than 8miles from center, without 1000 euro extreme value
vienna_cut <- vienna %>% filter(accommodation_type=="Hotel") %>%
                         filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
                         filter(price<=1000)


# brief look at data
table(vienna_cut$city)
table(vienna_cut$stars)

####################################################################################
# Figure 3.2 a) and b)
####################################################################################

histprice_Vienna1 <- ggplot(data =  vienna_cut, aes (x = price)) +
  #geom_histogram_da(type="frequency", binwidth = 1, size=0.5)+
  geom_histogram(binwidth = 1,  fill = color[1], size = 0.5, alpha = 0.8,  show.legend=F, na.rm =TRUE) +
  labs(x = "Price (US dollars)", y = "Frequency") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,500), breaks = seq(0, 500, by = 50)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bg() 
histprice_Vienna1
#save_fig("histprice_Vienna1_R", output, "small")
save_fig("ch03-figure-2a-hist-price", output, "small")


histprice_Vienna2 <- ggplot(data =  vienna_cut, aes (x = price)) +
  geom_histogram_da(type="frequency", binwidth = 10)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,500), breaks = seq(0, 500, by = 50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,45), breaks = seq(0, 45, by =5)) +
    theme_bg() 
histprice_Vienna2
#save_fig("histprice_Vienna2_R", output, "small")
save_fig("ch03-figure-2b-hist-price", output, "small")



####################################################################################
# Figure 3 a,b
####################################################################################

histprice_Vienna3 <- ggplot(data =  vienna_cut, aes (x = price)) +
  geom_histogram_da(type="frequency", binwidth = 40)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  scale_x_continuous(expand = c(0.0,0.0), limits = c(0,510), breaks = seq(0, 500, by = 80)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0,120), breaks = seq(0, 120, by = 20)) +
    theme_bg() 
histprice_Vienna3
#save_fig("histprice_Vienna3_R", output, "small")
save_fig("ch03-figure-3a-hist-price", output, "small")


histprice_Vienna4 <- ggplot(data =  vienna_cut, aes (x = price)) +
  geom_histogram_da(type="frequency", binwidth = 80)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  scale_x_continuous(expand = c(0.0,0.0), limits = c(0,510), breaks = seq(0, 500, by = 80)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0,160), breaks = seq(0, 150, by = 50)) +
  theme_bg() 
histprice_Vienna4
#save_fig("histprice_Vienna4_R", output, "small")
save_fig("ch03-figure-3b-hist-price", output, "small")



###############################################
  ### EXTREME VALUES
###############################################
# Apply filters: 3-4 stars, less than 8miles from center, without 1000 euro extreme value
vienna_cut <- vienna %>% filter(accommodation_type=="Hotel") %>% 
                     filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
                     filter(price<=1000)

########################x
#Figure 3.4
histdist_Vienna <- ggplot(data =  vienna_cut, aes (x = distance)) +
  geom_histogram_da(type="frequency", binwidth = 0.5)+
  labs(x = "Distance to city center (miles)", y = "Frequency") +
  scale_x_continuous(expand = c(0.0,0.0), limits = c(0,14), breaks = seq(0, 14, by = 2)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0,61), breaks = seq(0, 60, by = 10)) +
  theme_bg() 
histdist_Vienna
#save_fig("histdist_Vienna_R", output, "small")
save_fig("ch03-figure-4-hist-dist", output, "small")


# with annotation
histdist_Vienna_annot <- ggplot(data =  vienna_cut, aes (x = distance)) +
  geom_histogram_da(type="frequency", binwidth = 0.5)+
  labs(x = "Distance to city center (miles)", y = "Frequency") +
  scale_x_continuous(expand = c(0.0,0.0), limits = c(0,14), breaks = seq(0, 14, by = 2)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0,61), breaks = seq(0, 60, by = 10)) +
  geom_segment(aes(x = 8.2, y = 0, xend = 8.2, yend = 60), color = color[2], size=1) +
  #geom_segment(aes(x = 10, y = 40, xend = 8.4, yend = 40), arrow = arrow(length = unit(0.2, "cm")))+
  annotate("text", x = 11, y = 29, label = "Too far out", size=2)+
  annotate("rect",xmin=8.2, xmax=14, ymin=0, ymax=60, fill=color[4], alpha=0.1)+
  theme_bg() 
histdist_Vienna_annot
#save_fig("histdist_Vienna_annot_R", output, "small")
save_fig("ch03-figure-5-hist-dist-annot-large", output, "small")


# look at actual city
table(vienna_cut$city_actual)



####################################################################################
# Figure price with extreme
# Apply filters:  3-4 stars, less than 8miles from center, without 1000 euro extreme value
vienna_cut <- vienna %>% filter(accommodation_type=="Hotel") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars))
  

histprice_Vienna1 <- ggplot(data =  vienna_cut, aes (x = price)) +
  geom_histogram_da(type="frequency", binwidth = 20)+
  labs(x = "Price (US dollars)", y = "Frequency") +
  scale_x_continuous(expand = c(0.00,0.00), limits = c(0,1100), breaks = seq(0, 1100, by = 100)) +
  expand_limits(x = 0.00, y = 0.00) +
    theme_bg() 
histprice_Vienna1
#save_fig("histprice_Vienna_E_R", output, "small")
