############################################################
#
# DATA ANALYSIS TEXTBOOK
# TESTING
# ILLUSTRATION STUDY
# Billion prices project
#
############################################################  

# v 2.1 2020-03-16  graph edits, + xtable
# v 2.2 2020-03-21  graph axis edits
# v 2.3 2020-04-08 save_fig
# v 2.4 2020-04-24 names ok
# v 2.5 2020-08-09 hist2 adj

# WHAT THIS CODES DOES:

# Filter the dataset
# Checking price distribution
# Hypothesis testing on online-offline price difference
# Clear memory
rm(list=ls())

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

#location folders
data_in <- paste0(dir,"/da_data_repo/billion-prices/clean/")
data_out <- paste0(dir,"/da_case_studies/ch06-online-offline-price-test/")
output <- paste0(dir,"/da_case_studies/ch06-online-offline-price-test/output/")
func <- paste0(dir, "/da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))

# PACKAGES
library(dplyr)
library(tidyverse)
library(broom)
library(xtable)

# load data
pd <- read.csv(paste0(data_in,"online_offline_ALL_clean.csv"))


# FILTER DATA
pd <- pd %>% filter(COUNTRY=="USA") %>% 
             filter(PRICETYPE == "Regular Price") %>%
             filter(is.na(sale_online)) %>%
             filter(!is.na(price)) %>%
             filter(!is.na(price_online))


# Drop obvious errors
pd <- pd %>% filter(price<1000)

# Compare variables
pd<-pd %>% mutate(diff = price_online-price)

descr <- pd %>% summarise(mean = mean(diff,na.rm=T), sd = sd(diff,na.rm=T), min=min(diff,na.rm=T),
                          median=median(diff,na.rm=T), max=max(diff,na.rm=T))
descr

hist1<- ggplot(data=pd, aes(diff))+
  geom_histogram(binwidth = 5, boundary=0,
                 fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Online - offline price difference (US dollars)", y = "Frequency") +
  theme_bg()+
  scale_x_continuous(limits = c(-420, 420), breaks = seq(-400, 420, by = 100)) +
  scale_y_continuous(limits=c(0,6000), breaks = seq(0, 6000, by = 1000), expand = c(0.01,0.01))+
  geom_segment(aes(x = 300, y = 500, xend = 415, yend = 20), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 300, y = 700, label = "max value= 415", size=2.5) +
  geom_segment(aes(x = -280, y = 500, xend = -380, yend = 20), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = -300, y = 700, label = "min value= -380", size=2.5)
hist1
#save_fig("R_F06_1", output, "small")
save_fig("ch06-figure-1a-pricediff", output, "small")

# 4.99999 not 5 -- needed because of data imported from stata may be stored wierdly. 
pd1<-subset(pd,abs(pd$diff)<4.999999)
Hmisc::describe(pd1$diff)

hist2<- ggplot(data=pd, aes(diff))+
  geom_histogram(binwidth = 0.5, boundary=-0,
                 color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  labs(x = "Online - offline price difference (US dollars)", y = "Frequency") +
  theme_bg()+
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) +
  scale_y_continuous(expand = c(0.00,0.00), limits=c(0,5000), breaks = seq(0, 5000, by = 1000))
hist2
#save_fig("R_F06_2", output, "small")
save_fig("ch06-figure-1b-pricediff2", output, "small")

# HYPOTHESIS
t.test(pd$diff,mu=0)	

# MULTIPLE HYPOTHESES
spd <- split(pd, pd$retailer,drop=FALSE) 
out <- vector("list", length = length(spd))
out <- lapply(1:length(spd),function (x) out[[x]]<- t.test(spd[[x]]$diff,mu=0))
out

# create table
table_out <- pd %>% group_by(retailer) %>% group_modify(~ tidy(t.test(.x$diff))) 
table_out<-table_out %>% select(retailer,estimate,p.value)

xt<-xtable(table_out,align='llcc', digits = c(0,0,2,2))
names(xt) <- c('Retailer ID','Diff','p-value')
#print(xt, type = "latex",include.rownames = FALSE,       file = paste0(output,"ch06.tex"))
print(xt, type = "latex",include.rownames = FALSE,
      file = paste0(output,"ch06-table-2-test.tex"))





