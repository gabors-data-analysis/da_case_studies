######################################################################
# Chapter 03
#
# wms-management-survey
# v1.1

# using WMS data 2004-2015
#
######################################################################


######################################################################

# Clear memory
rm(list=ls())

# Import libraries
require(ggplot2)
require(plyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(viridis)
library(haven)
#library(Hmisc)
library(dplyr)
library(tidyr)
library(binsreg)

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

# Location folders
data_in <- paste0(dir,"da_data_repo/wms-management-survey/clean/")
data_out <- paste0(dir,"da_case_studies/ch04-management-firm-size/")
output <- paste0(dir,"da_case_studies/ch04-management-firm-size/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

#call function
source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R"))

########################################################################

# Import data


########################################################################

# Import data
df <- read_csv(paste0(data_in,"wms_da_textbook.csv"))

# Sample selection
df <- df %>%
  filter(country=="Mexico" & wave==2013 & emp_firm>=100  & emp_firm<=5000)




# ---------------------------------------------------------------
# Figure 3.6
# Boxplot
df2 <- df %>%
  select(emp3bins, management) %>%
  filter(emp3bins=="Small")

q1 <- quantile(df2$management, 0.25)
q3 <- quantile(df2$management, 0.75)
q2 <- quantile(df2$management, 0.50)
iqr <- q3 - q1
ub <- max(df2[df2$management<q3+1.5*iqr, ]$management)
lb <- min(df2[df2$management>=(q1-iqr*1.5), ]$management)
out_lb <- min(df2$management)
min <- min(df$management)
max <- max(df$management)


ggplot(data = df2, aes(x = emp3bins, y = management)) +
  geom_boxplot(color = "blue", fill = color[1], size = 0.5, width = 0.1, alpha = 0.5, na.rm=T) +
  stat_boxplot(geom = "errorbar", width = 0.05, color = "blue", size = 0.5, na.rm=T) +
  scale_y_continuous(limits = c(min,max)) +
  annotate("text", x = 1.1, y = ub, label = "← Upper adjacent value", hjust=0) +
  annotate("text", x = 1.1, y = q3, label = "← 75th percentile (upper hinge)", hjust=0) +
  annotate("text", x = 1.1, y = q2, label = "← Median", hjust=0) +
  annotate("text", x = 1.1, y = q1, label = "← 25th percentile (upper hinge)", hjust=0) +
  annotate("text", x = 1.1, y = lb, label = "← Lower adjacent value", hjust=0) +
  annotate("text", x = 1.1, y = out_lb, label = "← Outside values", hjust=0) +
  
  annotate("text", x = 0.63, y = ub, label = "Adjacent line", hjust=0) +
  annotate("text", x = 0.63, y = q3, label = "Whiskers", hjust=0) +
  annotate("text", x = 0.63, y = q2, label = "Median", hjust=0) +
  annotate("text", x = 0.63, y = q1, label = "Whiskers", hjust=0) +
  annotate("text", x = 0.63, y = lb, label = "Adjacent line", hjust=0) +
  
  geom_segment(aes(x = 0.9, y = lb, xend = 0.9, yend = ub)) +
  geom_segment(aes(x = 0.88, y = lb, xend = 0.9, yend = lb)) +
  geom_segment(aes(x = 0.88, y = q1, xend = 0.9, yend = q1)) +
  geom_segment(aes(x = 0.88, y = q2, xend = 0.9, yend = q2)) +
  geom_segment(aes(x = 0.88, y = q3, xend = 0.9, yend = q3)) +
  geom_segment(aes(x = 0.88, y = ub, xend = 0.9, yend = ub)) +
  
  theme(      axis.title.x=element_blank(),
              axis.line.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x = element_blank(),
              axis.title.y=element_blank(),
              axis.line.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank(), panel.border = element_blank())
ggsave(paste0(output, "boxlot_R.png"), width=14, height=8, units = "cm", dpi = 1200)


#Violin
ggplot(data = df2, aes(x = emp3bins, y = management)) +
  geom_violin(size=0.2,  width = 0.3, trim = F, show.legend=F, na.rm =TRUE, color = "blue", fill = "blue", alpha = 0.3) +
  geom_boxplot(color = "blue", fill = color[1], size = 0.6, width = 0.01, alpha = 0.5, na.rm=T, outlier.shape = NA) +
  annotate("text", x = 1.05, y = ub, label = "← 95% Confidence Interval", hjust=0) +
  annotate("text", x = 1.18, y = q3, label = "← Interquartile range", hjust=0) +
  annotate("text", x = 1.18, y = q2, label = "← Median", hjust=0) +
  
  
  theme(      axis.title.x=element_blank(),
              axis.line.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x = element_blank(),
              axis.title.y=element_blank(),
              axis.line.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank(), panel.border = element_blank())
ggsave(paste0(output, "violin_R.png"), width=12, height=8, units = "cm", dpi = 1200)



# ---------------------------------------------------------------

