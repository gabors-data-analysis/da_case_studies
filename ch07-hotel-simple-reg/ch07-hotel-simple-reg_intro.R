################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 07
# CH07A Finding a good deal among hotels with simple regression
# hotels-vienna  dataset
# version 0.9 2020-08-26


# ------------------------------------------------------------------------------------------------------
#### SET UP

# START NEW SESSION --- it is best to start a new session !
# CLEAR MEMORY
rm(list=ls())

# packages
library(tidyverse)
library(grid)
library(viridis)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"hotels-vienna","clean", sep = "/")

use_case_dir <- "ch07-hotel-simple-reg/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# ------------------------------------------------------------------------------------------------------
#### GET DATA AND SAMPLE SELECTION

hotels <- read_csv(paste(data_in,"hotels-vienna.csv", sep = "/"))

# Apply filters:  3-4 stars, Vienna actual, without  extreme value
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(price<=600)

# save a copy of the work file
write_csv(hotels, paste0(data_out,"hotels_work.csv"))


# ------------------------------------------------------------------------------------------------------

# SUMMARY STATISTICS ON PRICE AND DISTANCE

descr_price <- hotels %>% select(price) %>% 
              dplyr::summarize(mean=mean(price),
                                       sd=sd(price),
                                       min=min(price),
                                       max=max(price),
                                       p50=quantile(price,.50),
                                       p95=quantile(price,.95),
                                       n=length(price)
                                       )
print(descr_price)


descr_dist <- hotels %>% select(distance) %>% dplyr::summarize(mean=mean(distance),
                                                      sd=sd(distance),
                                                      min=min(distance),
                                                      max=max(distance),
                                                      p50=quantile(distance,.50),
                                                      p95=quantile(distance,.95),
                                                      n=length(distance)
)
print(descr_dist)

# Remove objects
rm(descr_dist, descr_price)


### REGRESSION 1: CLOSE VS FAR REGRESSION WITH BINARY DISTANCE
hotels <- hotels %>% mutate(dist2=as.numeric(distance>=2)) 
dist2 <-hotels %>% group_by(dist2) %>% dplyr:: summarize(Eprice_cat2=mean(price))
hotels<-left_join(hotels,dist2)
hotels <- hotels %>%  mutate(dist2 = recode(dist2,`0` = "Close",`1` = "Far"))

hotels %>% group_by(dist2) %>% dplyr::summarize(mean_dist=mean(distance), 
                                         sd_dist=sd(distance),
                                         min_dist=min(distance),
                                         max_dist=max(distance),
                                         mean_dist=mean(price), 
                                         sd_dist=sd(price),
                                         min_dist=min(price),
                                         max_dist=max(price),
                                         N=n()
                                         )


############
# Figure 7.1a

F07_1a<- ggplot(data = hotels) +
  geom_point(aes(x = dist2, y = Eprice_cat2), 
             size = 2.5, color = color[1], fill=color[1], shape = 21, alpha = 0.4, na.rm=T) +
  geom_text(aes(x = dist2, y = Eprice_cat2, label = round(Eprice_cat2)), hjust = -0.8, vjust = 0, color = "black", size = 3) +
  scale_y_continuous(expand=c(0.01,0.01),limits = c(0, 400), breaks = seq(0,400, by=50)) +
  expand_limits( y = 0.01) +
  scale_x_discrete() +
  labs(x = "Distance to city center (categories)", y = "Average price (US dollars)") +
  theme_bg()
F07_1a
save_fig("ch07-figure-1a-scatter-nonpar1", output, "small")


### REGRESSION 2: 4 DISTANCE CATEGORIES
hotels <-hotels %>% mutate(dist4=0.5+ 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) + 2.5*as.numeric(hotels$distance>=3))
dist4 <- hotels %>% group_by(dist4) %>% dplyr::summarize(Eprice_cat4=mean(price))
hotels<-left_join(hotels,dist4)
hotels %>% group_by(dist4) %>% dplyr::summarize(mean_dist=mean(distance), 
                                         sd_dist=sd(distance),
                                         min_dist=min(distance),
                                         max_dist=max(distance),
                                         mean_dist=mean(price), 
                                         sd_dist=sd(price),
                                         min_dist=min(price),
                                         max_dist=max(price),
                                         N=n())

#######################
# Figure 7.1b
#######################

F07_1b<- ggplot(data = hotels) +
  #geom_point(aes(x = dist4, y = price), size = 1, color = color[1], shape = 16, alpha = 0.5, na.rm=T) +
  geom_point(aes(x = dist4, y = Eprice_cat4), 
             size = 2.5, color = color[1], fill=color[1], shape = 21, alpha = 0.4, na.rm=T) +
  geom_text(aes(x = dist4, y = Eprice_cat4, label = round(Eprice_cat4)), hjust = -0.6, vjust = 0, color = "black", size = 3) +
  expand_limits(x = 0.01, y = 0.01) +
  coord_cartesian(xlim = c(0,7), ylim = c(0, 400)) +
  scale_y_continuous(expand=c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by=50)) +
  scale_x_continuous(expand=c(0.01,0.01), limits= c(0,7), breaks = c(0, 1, 2, 3,4,5, 6,7)) +
  labs(x = "Distance to city center (miles)", y = "Price (US dollars)") +
  theme_bg()
F07_1b
save_fig("ch07-figure-1b-scatter-nonpar2", output, "small")


#Look at a bar chart (NOT in BOOK)
F07_x2<- ggplot(data = hotels, aes(x = dist2, y = price)) +
  stat_boxplot(aes(group = dist2), geom = "errorbar", width = 0.25, color = viridis(2, begin=0.3, end=0.7), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = dist2),  color = viridis(2, begin=0.3, end=0.7), fill = viridis(2, begin=0.3, end=0.7), size = 0.5, width = 0.5, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  geom_jitter(aes(color = dist2), position=position_jitter(0.1), size = 0.5, show.legend=F,  na.rm=T) +
  labs(x = "Distance to city center (categories)",y = "Price (US dollars)") +
  scale_y_continuous(expand=c(0.01,0.01),limits = c(0,400), breaks = seq(0,400,50)) +
  expand_limits( y = 0.01) +
    scale_color_viridis(discrete = TRUE, option = "D", begin=0.3, end=0.7)+
  theme_bg() 
F07_x2



#######################
# FIGURE 7.2a, 7.2b 
#######################

p1 <- ggplot(data = hotels, aes(x = distance, y = price)) +
  geom_point_da() +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
  theme_bg() 
p1


# large sized graphs
p1l <- ggplot(data = hotels) +
  geom_point(aes(x = distance, y = price), color = color[1], size = 2,  shape = 16, alpha = 0.5, show.legend=F, na.rm = TRUE) + 
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
  theme_bg() 

# Scatterplot with step function (we use 1km bits for simpplicity using 4 bits for 3-7km)
hotels <-hotels %>% mutate(dist4_s = 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) +   1*as.numeric(hotels$distance>=3) +1*as.numeric(hotels$distance>=4) +1*as.numeric(hotels$distance>=5) + 1*as.numeric(hotels$distance>=6))  
hotels$xend <- c(hotels$dist4_s+1)
hotels$yend <- c(hotels$Eprice_cat4)

F07_2a <-  p1+
  geom_segment(data=hotels, aes(x = dist4_s, y=yend, xend=xend, yend=yend), color=color[2], size=0.7, na.rm=TRUE) 
F07_2a
save_fig("ch07-figure-2a-scatter-binscat2", output, "small")



# New intervals
hotels <-hotels %>% mutate(dist7_new = 0.5+ 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) +   1*as.numeric(hotels$distance>=3) +1*as.numeric(hotels$distance>=4) +1*as.numeric(hotels$distance>=5) + 1*as.numeric(hotels$distance>=6))  
dist7_new <- hotels %>% group_by(dist7_new) %>% dplyr::summarize(Eprice_cat7_new=mean(price))
hotels<-left_join(hotels,dist7_new)
hotels %>% group_by(dist7_new) %>% dplyr::summarize(mean_dist=mean(distance), 
                                         sd_dist=sd(distance),
                                         min_dist=min(distance),
                                         max_dist=max(distance),
                                         mean_dist=mean(price), 
                                         sd_dist=sd(price),
                                         min_dist=min(price),
                                         max_dist=max(price),
                                         N=n())

# Scatterplot with step function, starting point is simply at cut-off
hotels <-hotels %>% mutate(dist7_s = 1*as.numeric(hotels$distance>=1) + 1*as.numeric(hotels$distance>=2) +   1*as.numeric(hotels$distance>=3) +1*as.numeric(hotels$distance>=4) +1*as.numeric(hotels$distance>=5) + 1*as.numeric(hotels$distance>=6))  

hotels$xend <- c(hotels$dist7_s+1)
hotels$yend <- c(hotels$Eprice_cat7_new)



F07_2b <- p1 +
 #geom_point(data= dist7_new, aes(x = dist7_new, y = Eprice_cat7_new), size = 2, color = color[4], fill= color[2],  shape = 21, alpha = 0.8) 
 geom_segment(data=hotels, aes(x = dist7_s, y=yend, xend=xend, yend=yend), color=color[2], size=0.7, na.rm=TRUE) 
F07_2b
save_fig("ch07-figure-2b-scatter-binscat2", output, "small")


#######################
# Figure 7.3
# LOWESS NONPARAMETRIC REGRESSION

F07_3 <- p1  +
  geom_smooth_da(method='loess')
F07_3
save_fig("ch07-figure-3-scatter-lowess", output, "small")




### LINEAR REGRESSIONS
regression <- lm(price ~ distance, data=hotels)
summary(regression)


#######################
# Figure 7.5
# SCATTERPLOT + REGRESSION LINE
F07_5 <-  p1  +
geom_smooth_da(method = "lm")
F07_5
save_fig("ch07-figure-5-scatter-linreg", output, "small")


# THE LINEAR REGRESSION GOES THROUGH THE AVERAGES
# SCATTERPLOT + REGRESSION LINE + LINES FOR AVERAGES (NOT in BOOK)
F07_x5 <-  p1  +
  geom_smooth_da(method = "lm") +
  geom_vline(xintercept = mean(hotels$distance),color = color[3], lty="dashed", size=0.3)+
  geom_hline(yintercept = mean(hotels$price),color = color[3], lty="dashed", size=0.3)
F07_x5



# PREDICTED VALUES & RESIDUALS OF LINEAR REGRESSION 
regression <- lm(price ~ distance, data=hotels)
hotels$predprice <- predict(regression)
hotels$e <- resid(regression)

###########
# Figure 7.6, Table 7.1
xa<- 2.9
ya<- 208
ym<- 90.24 
# FIXME
# ym  --should be replaced with predicted value directly

F07_6a <-   ggplot(data = hotels, aes(x = distance, y = price)) +
  geom_point_da()+ 
  geom_smooth_da(method="lm")+
  annotation_custom(grid.text("Residual", x=0.48,  y=0.5, gp=gpar(color="black", fontsize=4, fontface="bold"))) +
  annotate("pointrange", x = xa, y = ya, ymin = ya, ymax = ya, color = color[3], size = 0.1)+
  geom_errorbar(data=subset(hotels, hotels$distance==xa), aes(x=distance, ymin=ym, ymax=ya), width=0.2, size=0.2, color=color[1]) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7),     breaks= seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks= seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
  theme_bg()
F07_6a
save_fig("ch07-figure-6a-resid-scatter", output, "small")


# histogram of residuals
F07_6b<-   ggplot(data = hotels, aes (x = e)) +
  #geom_histogram_da(binwidth = 20, type='percent')+
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 20, color = color.outline, fill = theme_colors[1],
                 size = 0.2, alpha = 0.8,  show.legend=F, na.rm=TRUE, boundary=1)+
  labs(x = "Residuals", y = "Percent") +
  scale_x_continuous(limits = c(-100, 300), breaks = seq(-100, 300, by = 100)) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), 
                     labels = scales::percent_format(accuracy = 1)) +
theme_bg() 
F07_6b
save_fig("ch07-figure-6b-resid-hist", output, "small")


# hotels with most negative residuals

reg1 <- lm(price ~ distance, data=hotels)
summary(reg1)

hotels$reg1_resid <- reg1$residuals
hotels$reg1_res <- ifelse(reg1$residuals >=0, "overpriced", "underpriced")
hotels$reg1_res <- ifelse(hotels$reg1_resid %in% tail(sort(reg1$residuals, decreasing=TRUE),5), "bottom5",
                          ifelse(hotels$reg1_resid %in% head(sort(reg1$residuals, decreasing=TRUE), 5), "top5", hotels$reg1_res))


# (stored in a new data frame; check data frame)
bestdeals <- hotels%>%
  arrange(e)%>%
  head(5)
bestdeals

# FIXME
# print ch07-table-1-resid



##############
# Figure 7.7
# adding annotation

Fig7<-   ggplot(data= hotels, aes(x = distance, y = price)) +
  geom_point(data = filter(hotels,reg1_res=="overpriced"), aes(color=factor(reg1_res)), 
             size = 1.3, shape = 16, alpha = 0.6, show.legend=F) +
  geom_point(data = filter(hotels,reg1_res=="underpriced"), aes(color=factor(reg1_res)), 
             size = 1.3, shape = 16, alpha = 0.6, show.legend=F) +
  geom_point(data = filter(hotels,reg1_res=="bottom5"), aes(color=factor(reg1_res), fill=factor(reg1_res)), 
             size = 1.5, shape = 21, alpha = 0.8, show.legend=F) +
  geom_point(data = filter(hotels,reg1_res=="top5"), aes(color=factor(reg1_res)), 
             size = 1.3, shape = 16, alpha = 0.6, show.legend=F) +
  geom_smooth_da(method="lm", size=1)+
  coord_cartesian( xlim = c(0, 7), ylim = c(0, 400)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
  scale_color_manual(name="",
                     values =c(color[1], color[1], color[1], color[1])) +
  scale_fill_manual(name="", values =c(color[4])) +
  geom_segment(aes(x = 2, y = 25, xend = 1.15, yend = 50), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 3, y = 25, label = "Most underpriced hotels", size=3)+
  theme_bg()+
  theme(axis.text.x=element_text(size=9)) +
  theme(axis.text.y=element_text(size=9)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) 
Fig7
save_fig("ch07-figure-7-underpriced-deals", output, "large")

