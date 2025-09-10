#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# CHAPTER 04 
# CH04A Management quality and firm size: describing patterns of association
# WMS  dataset
# version 0.9 2020-08-28
#########################################################################################



######################################################################


######################################################################

# START NEW SESSION --- it is best to start a new session !
# CLEAR MEMORY
rm(list=ls())

# packages
library(tidyverse)
library(haven)
library(Hmisc)
library(binsreg)
library(xtable)
library(modelsummary)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("ch00-tech-prep/set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"wms-management-survey","clean/", sep = "/")

use_case_dir <- "ch04-management-firm-size/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



########################################################################
# Import data
df <- read_csv(paste0(data_in,"wms_da_textbook.csv"))
# Can load from the web as well
# df <- read_csv( "https://osf.io/uzpce/download" )

# Sample selection
df <- df %>%
  filter(country=="Mexico" & wave==2013 & emp_firm>=100  & emp_firm<=5000)

# Summary in two steps
df %>% select(emp_firm) %>% datasummary_skim()
describe(df$emp_firm)
summary(df$emp_firm)

# Save workfile
write.csv(df,file = paste0(data_out, "ch04-wms-work.csv"), row.names = F)

 ########################################################################

# Summary
datasummary( management + emp_firm ~ mean + Median + SD + Min + Max + N , data = df )
# Somewhat more coumbersome to use dplyr:
df %>%
  dplyr::select(management, emp_firm) %>% 
  summarise_all(tibble::lst(min, max, mean, median, sd, length))

# Histogram
g1<-ggplot(data = df, aes (x = management)) +
  geom_histogram_da(binwidth = 0.25, type="percent", boundary = 0) +
  labs(x = "Management score", y = "Percent") +
  #scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(1,5))+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.25), breaks = seq(0, 0.25, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
g1
save_fig("ch04-figure-1-wms-mex-mgmt-hist",output , "small") 


g2a<-ggplot(data = df, aes (x = emp_firm )) +
  geom_histogram_da(binwidth = 200, type="percent") +
  labs(x = "Firm size (employment)", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
g2a
save_fig("ch04-figure-2a-wms-mex-emp-hist",output , "small") 

# Generate variable
df$lnemp = log(df$emp_firm)
Hmisc::describe(df$lnemp)

# Histogram
g2b<-ggplot(data = df, aes (x = lnemp)) +
  geom_histogram_da(binwidth = 0.25, type="percent", boundary=0) +
  labs(x = "Firm size (ln(employment))", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(4,9)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.2), breaks = seq(0, 0.2, by = 0.04), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
g2b
save_fig("ch04-figure-2b-wms-mex-lnemp-hist",output , "small")
########################################################################


# Stack bar charts some management items by emp bins

# Generate employment bins
df$emp3bins <- ifelse(df$emp_firm<200, 1, 
                      ifelse(df$emp_firm>=200 & df$emp_firm<1000, 2,
                             ifelse(df$emp_firm>=1000, 3,100)
                          )
                      )
describe(df$emp3bins)

# Create pivot
df$emp3bins <- as.factor(df$emp3bins)

df1 <- df %>% 
  dplyr::select(emp3bins,lean1) %>% 
  group_by (emp3bins,lean1) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(Percent= round(Count / sum(Count),digits = 5)) %>% ungroup()

# Stacked bar
g3a<-ggplot(data=df1, aes(x=emp3bins, y=Percent, fill = factor(lean1, levels = rev(unique(lean1))))) +
  geom_bar(stat = "identity", position = "fill",width = 0.6,  color = "white",  size = 0.5, alpha = 0.8) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::percent_format()) +
  scale_x_discrete(labels=c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  scale_fill_manual(values = c(color[3], color[1], color[5], color[2], color[4]), name = NULL) +
    #scale_fill_manual(values = viridis(5, begin=0, end=0.9), name = NULL) +
  labs(x = "Firm size (employment), 3 bins", y = "Percent") +
  theme_bg() +
  theme(legend.position = "right")
g3a
save_fig("ch04-figure-3a-wms-mex-lean1-emp3bins",output , "small")

# Create pivot
df1 <- df %>% 
  dplyr::select(emp3bins,perf2) %>% 
  group_by (emp3bins,perf2) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(Percent= round(Count / sum(Count),digits = 5)) %>% ungroup()
                # use %>% ungroup() when do multiple times group_by


g3b<-ggplot(data=df1, aes(x=emp3bins, y=Percent, fill = factor(perf2, levels = rev(unique(perf2))))) +
  geom_bar(stat = "identity", position = "fill",width = 0.6,  color = "white",  size = 0.5, alpha = 0.8) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::percent_format()) +
  scale_x_discrete(labels=c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  scale_fill_manual(values = c(color[3], color[1], color[5], color[2], color[4]), name = NULL) +
  #scale_fill_manual(values = viridis(5, begin=0, end=0.8), name = NULL) +
  labs(x = "Firm size (employment), 3 bins", y = "Percent") +
  theme_bg() +
  theme(legend.position = "right")
g3b
save_fig("ch04-figure-3b-wms-mex-perf2-emp3bins",output , "small")

##############################################################################

# Bin scatters avg score by employment bins

# Option 1: create 3 bins as defined by thresholds

# Summary
datasummary( emp_firm * emp3bins ~ mean + Median + SD + Min + Max + N , data = df )

# Recode employee bins
df$emp3bins <- ifelse(df$emp3bins == 1 , 150, 
                      ifelse(df$emp3bins == 2, 600,
                             ifelse(df$emp3bins == 3, 3000, NA)))
# Summary
datasummary( emp_firm * Factor( emp3bins ) ~ Mean + Median + SD + Min + Max + N , data = df )

# Generate variables by mean
df1<-df %>% group_by(emp3bins) %>%
  dplyr::summarize(management_emp3bins=mean(management))


# Bin scatters
g4a<-ggplot(data = df1, aes(x = emp3bins, y = management_emp3bins)) +
  geom_point(size = 2, color = color[3], fill= color[1], shape = 21, alpha = 0.8, na.rm=T) +
  #geom_text(aes(label = round(management_emp3bins, 1)), hjust = 0.5, vjust = -1, color = "black", size = 3) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(2.4, 3.4), breaks = seq(2.4, 3.4, by=0.2)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 3000), breaks = seq(0,3000, by=500)) +
  labs(x = "Firm size (employment), 3 bins", y = "Management score") +
  theme_bg() 
g4a
save_fig("ch04-figure-4a-wms-mex-mgmt-emp3bins",output , "small")

# Option 2: create 10 bins as defined by equal cutoffs

df$emp10bins <- df$emp_firm %>% cut_number(10)

    # another way would be to make sure we exactly 30 units/bin
    # df <- df %>%   dplyr::arrange(emp_firm) %>%   dplyr::mutate(id = row_number()) 
    # df$emp10bins <- as.factor(cut(df$id, 10))
    # levels(df$emp10bins) <- c('0','1', '2', '3', '4', '5', '6', '7', '8', '9')

# Summary
df_summary<-df %>%
  select(emp_firm, emp10bins) %>% 
  group_by(emp10bins) %>%
  summarise_all(tibble::lst(min, max, mean, median, sd, length))
df_summary

# Recode
levels(df$emp10bins) <-  df_summary %>% pull(mean) %>% round()
df$emp10bins<-as.numeric(levels(df$emp10bins))[df$emp10bins]

# Summary
df %>%
  select(emp_firm, emp10bins) %>% 
  group_by(emp10bins) %>%
  dplyr::summarise_all(tibble::lst(min, max, mean, median, sd, length))

# Generate variables by mean
df1 <- df %>% group_by(emp10bins) %>% 
              dplyr::summarize(management_emp10bins=mean(management))

# Bin scatters
g4b<-ggplot(data = df1, aes(x = emp10bins, y = management_emp10bins)) +
  geom_point(size = 2, color = color[3], fill= color[1], shape = 21, alpha = 0.8, na.rm=T) +
  #geom_text(aes(label = round(management_emp10bins, 1)), hjust = 0.5, vjust = -1, color = "black", size = 3) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, by=0.25)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 3500), breaks = seq(0,3500, by=500)) +
  labs(x = "Firm size (employment), 10 bins", y = "Management score") +
  theme_bg() 
g4b
save_fig("ch04-figure-4b-wms-mex-mgmt-emp10bins",output , "small")

# This is a simpler solution, similar looking graph (not in book):
binsreg(df$management, df$emp_firm, nbins = 10)



##############################################################################

# Scatterplot avg score by employment

g5a<-ggplot(data = df, aes(x = emp_firm, y = management)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=1000)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (employment)",y = "Management score")+
  theme_bg() 
g5a
save_fig("ch04-figure-5a-wms-mex-mgmt-emp-scatter",output , "small")

df$lnemp = log(df$emp_firm)

g5b<-ggplot(data = df, aes(x = lnemp, y = management)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(expand = c(0.01,0.01),limits=c(4, 9), breaks=seq(4, 9, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (ln(employment))",y = "Management score")+
  theme_bg() 
g5b
  save_fig("ch04-figure-5b-wms-mex-mgmt-lnemp-scatter",output , "small")

# Box plots by emp bins
df$emp3bins <- as.factor(df$emp3bins)
levels(df$emp3bins) <- c('Small','Medium', 'Large')

# Boxplot
g6a<-ggplot(data = df, aes(x = emp3bins, y = management)) +
  stat_boxplot(aes(group = emp3bins), geom = "errorbar", width = 0.5, color = c(color[2], color[1], color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = emp3bins),  color = c(color[2], color[1], color[3]), fill  = c(color[2], color[1], color[3]), size = 0.5, width = 0.5, alpha = 0.3, na.rm=T) +
  labs(x = "Firm size (employment), 3 bins",y = "Management score")+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(1,5), breaks = seq(1,5,1)) +
  #  geom_jitter(aes(color = emp3bins), position=position_jitter(0.1), size = 0.5, show.legend=F,  na.rm=TRUE) +
  #scale_color_viridis(discrete = TRUE, option = "D", begin = 0, end=0.7)+
  theme_bg() 
g6a
save_fig("ch04-figure-6a-wms-mex-boxplot-mgmt-emp3bins",output , "small")

# Violin plot
g6b<-ggplot(data = df, aes(x = emp3bins, y = management, color=emp3bins, fill=emp3bins)) +
  geom_violin(aes(group = emp3bins),   size=0.3,  alpha=0.3, trim = F, show.legend=F, na.rm =TRUE) +
  geom_boxplot(aes(group = emp3bins),  color = c(color[2], color[1], color[3]), fill  = c(color[2], color[1], color[3]), size = 0.5, width = 0.2, alpha = 0.3, na.rm=T) +
#  geom_jitter(aes(color = emp3bins), position=position_jitter(0.1), size = 0.5, show.legend=F,  na.rm=TRUE, alpha = 0.8) +  labs(x = "Number of Employees, 3 bins",y = "Average management quality score")+
  labs(x = "Firm size (employment), 3 bins",y = "Management score")+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,6), breaks = seq(0,6,1)) +
  scale_color_manual(name="", 
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="", 
                    values=c(color[2],color[1], color[3])) +
  theme_bg() 
g6b
save_fig("ch04-figure-6b-wms-mex-violin-mgmt-emp3bins", output, "small")




##############################################################################
# Correlation
cor(df$management, df$emp_firm, use = "complete.obs")

datasummary( Factor( sic ) ~ N + Percent() , data = df )

# by industry
df$industry_broad[df$sic<=21] <- 'food_drinks_tobacco'
df$industry_broad[df$sic>=22 & df$sic<=23 | df$sic==31  ] <- 'textile_apparel_leather_etc'
df$industry_broad[df$sic>=24& df$sic<=27] <- 'wood_furniture_paper'
df$industry_broad[df$sic>=28 & df$sic<=30] <- 'chemicals_etc'
df$industry_broad[df$sic>=32 & df$sic<35] <- 'materials_metals'
df$industry_broad[df$sic>=35 & df$sic<37] <- 'electronics'
df$industry_broad[df$sic==37 ] <- 'auto'
df$industry_broad[df$sic>=38]             <- 'other'

datasummary( industry_broad ~ N , data = df )

# Correlation
df %>%
  group_by(industry_broad) %>%
  dplyr::summarize(COR=cor(management, emp_firm))

# Summarize along industries for both emp_firm and management
datasummary( Median + SD + Min + Max + N ~ Factor( industry_broad ) * ( emp_firm + management ) , data = df )

cor<-df %>%
  group_by(industry_broad) %>%
  dplyr::summarize(COR=cor(management, emp_firm))


table41 <-df %>%
  select(emp_firm, industry_broad,management) %>% 
  # filter(!is.na(industry_broad)) %>% 
  group_by(industry_broad) %>%
  dplyr::summarise(Mean = mean(management),Obs=n())

table41$cor<-cor$COR


#table41<-table41 %>% replace_na(list(industry_broad = "other"))
table41$industry_broad<-table41$industry_broad %>% dplyr::recode(auto='Auto',
                                        chemicals_etc='Chemicals',
                                        electronics='Machinery, equipment, electronics',
                                        food_drinks_tobacco='Food, drinks, tobacco',
                                        materials_metals='Materials, metals',
                                        textile_apparel_leather_etc='Textile, apparel, leather',
                                        wood_furniture_paper='Wood, furniture, paper',
                                        other = 'Other'
                                        )
last_row<-table41 %>% summarise(Mean=mean(Mean),Obs=sum(Obs),cor=mean(cor))
last_row$industry_broad<-'All'

table41<-table41 %>% add_row(industry_broad=last_row$industry_broad,
                   Mean=last_row$Mean,
                   cor=last_row$cor,
                   Obs=last_row$Obs
                   )


table41<-table41 %>% select(industry_broad,cor,Mean,Obs)
table41
xt<-xtable(table41,align='llccc', digits = c(0,0,2,1,0))
names(xt) <- c('Industry','Management - employment correlation','Management score','Observations' )
print(xt, type = "latex",include.rownames = FALSE,
      file = paste0(output,"ch04-table-1-wms-industry-correlations.tex"))



