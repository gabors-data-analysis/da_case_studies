######################################################################
#
# Data Analysis Textbook
# Case Study for Chapter 21 - Methods for Uncovering effects in observational data
# Data : wms-management
# Using World Management Survey
#
######################################################################
#
# What this code does:
#

######################################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------

# install.packages("MatchIt")
# install.packages("gmodels")

library(haven)
library(tidyverse)
library(dplyr)
library(sandwich)
library(lmtest)
library(stargazer)
library(cowplot)
library(KernSmooth) # poly
library(MatchIt)
library(gmodels)

# Change working directory -------------------------------------------
# dir <-  "C:/Users/Administrator/Downloads/R__DA4_ch21/wms-management-survey/"
dir <-  "/Users/agostonreguly/Documents/Egyetem/CEU/Winter_III_year/DA4/Seminar_I/"

# Set location folders -----------------------------------------------
data_in <- paste0(dir,"clean/")
data_out <- paste0(dir,"")
func <- paste0(dir, "ch00_tech_prep/")
output <- paste0(dir,"output/")

# call custom function
source(paste0(func, "theme_bg.R"))



######################################################################
#
# Part 1 - Understanding nature and patterns of data
#
######################################################################


# Load in data -------------------------------------------------------
data <- read_dta(paste(data_in,"wms_2004_2010_xsec.dta",sep=""))


# Define variables ---------------------------------------------------

# 1. Define founder/family owned and drop ownership that is missing or not relevant

# Ownership
data %>%
  group_by(ownership) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# Define foundfam owned
data$foundfam_owned <- ifelse(
       data$ownership== "Family owned, external CEO" |
       data$ownership== "Family owned, family CEO" |
       data$ownership== "Founder owned, CEO unknown" |
       data$ownership== "Founder owned, external CEO" |
       data$ownership== "Founder owned, family CEO" |
       data$ownership== "Founder owned, founder CEO" , 1, 0)

# Foundfam owned
data %>%
  group_by(foundfam_owned) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# Crosstab
data %>% 
  count(ownership, foundfam_owned) %>% 
  spread(foundfam_owned, n, fill = 0)

# Describe data: observations ----------------------------------------

# Wave - Years
data %>%
  group_by(wave) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq) * 100, Cum = cumsum(Percent))

# Country
View( data %>%
        group_by(country) %>%
        summarise (Freq = n()) %>%
        mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent)) )

# 2. Proportion with college degree & make bins

# Proportion of managers with a college degree
data$degree_m <- data$degree_m/100

# Proportion of non-managers with a college degree
data$degree_nm <- data$degree_nm/100

data %>%
  select(degree_nm) %>%
  summarise(min = min(degree_nm, na.rm=T), 
            max = max(degree_nm, na.rm=T),
            q10 = quantile(degree_nm, probs = 0.1, na.rm=T),
            q25 = quantile(degree_nm, probs = 0.25, na.rm=T),
            q50 = quantile(degree_nm, probs = 0.50, na.rm=T),
            q75 = quantile(degree_nm, probs = 0.75, na.rm=T),
            q90 = quantile(degree_nm, probs = 0.90, na.rm=T))

# Workers without college degree -> to get more precise cuts drop values with 0s
sum( data$degree_nm==0 , na.rm = T );
data %>%
  select(degree_nm) %>%
  filter (degree_nm>0)  %>%
  summarise(min = min(degree_nm, na.rm=T), 
            max = max(degree_nm, na.rm=T),
            q10 = quantile(degree_nm, probs = 0.1, na.rm=T),
            q25 = quantile(degree_nm, probs = 0.25, na.rm=T),
            q50 = quantile(degree_nm, probs = 0.50, na.rm=T),
            q75 = quantile(degree_nm, probs = 0.75, na.rm=T),
            q90 = quantile(degree_nm, probs = 0.90, na.rm=T))


# Generate bins from degree_nm
data$degree_nm_bins <- cut(data$degree_nm, c(0,0.001,0.05,0.20,1.01), right= F)


# Crosstab
data %>% 
  group_by(degree_nm_bins) %>% 
  summarise(min = min(degree_nm),
            max = max(degree_nm), n = n())


#---------------------------------------------------------------------
# AS1 - practice: 
# create a nice table/graph summarizing key info
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------


# 3. Take log of employment

data$lnemp <- log(data$emp_firm)


# 4. Competition

data$compet_weak <- ifelse(data$competition == "   0 competitors" | data$competition == "  1-4 competitors", 1, 0)
data$compet_moder <- ifelse( data$competition == " 5-9 competitors", 1, 0)
data$compet_strong <- ifelse( data$competition == "10+ competitors", 1, 0)

# Summary
data %>% 
  group_by(competition) %>% 
  summarise(weak = max(compet_weak), 
            moder = max(compet_moder),
            strong = max(compet_strong))

data$competition <- 
  ifelse(data$compet_weak == 1, "0-4 competitors",
        ifelse(data$compet_moder == 1, "5-9 competitors", "10+ competitors"))


# 5. Industry in 2 digits

data$industry <- floor(data$sic/10)


# 6. Country as numeric variable (factor)

data$countrycode <- as.numeric(as.factor(data$cty))


# Sample selection ---------------------------------------------------

# Keep observations with:
#     Non-employee/Research/Gov/Other type of ownership
#     non-missing variables 
data <- data %>%
  filter(!ownership %in% c("", "Employees/COOP" , "Foundation/Research Institute", "Government", "Other" ),
         !is.na(management),
         !is.na(foundfam_owned),
         !is.na(degree_nm),
         !is.na(compet_weak),
         !is.na(compet_moder),
         !is.na(compet_strong),
         !is.na(industry), 
         !is.na(countrycode),
         !is.na(lnemp)
         )

# Summary of num. of employment
data %>%
  select(emp_firm) %>%
  summarise(min = min(emp_firm , na.rm=T), 
            max = max(emp_firm , na.rm=T),
            p1 = quantile(emp_firm , probs = 0.01, na.rm=T),
            p50 = quantile(emp_firm , probs = 0.50, na.rm=T),
            q99 = quantile(emp_firm , probs = 0.99, na.rm=T),
            n = n())


# Drop tiny and large firms
data %>%
  filter(emp_firm<50)  %>%
  summarise(n = n())

data %>%
  filter(emp_firm>5000)  %>%
  summarise(n = n())

data <- data %>%
  filter (!(emp_firm<50 | emp_firm>5000))



##################################################################
#                                                                #
#   Variation in x, distribution of other variables - Controls   #
#                                                                #
##################################################################

# foundfam_owned
data %>%
  group_by(foundfam_owned) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# foundfam_owned by competition
data %>%
  group_by(competition) %>%
  summarise (Mean = mean(foundfam_owned), Std.Dev. = sd(foundfam_owned), Freq = n()) 

# degree_nm_bins
data %>%
  group_by(degree_nm_bins) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# foundfam_owned by country
data %>%
  group_by(country) %>%
  summarise (Mean = mean(foundfam_owned), Std.Dev. = sd(foundfam_owned), Freq = n()) 


# foundfam_owned by industry
data %>%
  group_by(industry) %>%
  summarise (Mean = mean(foundfam_owned), Std.Dev. = sd(foundfam_owned), Freq = n()) 


# Histogram: employment
ggplot(data=data, aes(x=emp_firm)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 100, color="black", fill="blue", alpha = 0.8) +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(x = "No. of employees",y = "Percent")+
  theme_bg() +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  scale_y_continuous(labels = scales::percent) 

ggsave(paste0(output, "F21_h_emp_R.png"), width=12, height=7.5)


# Histogram: log employment
ggplot(data=data, aes(x=lnemp)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.3, color="black", fill="blue", alpha = 0.8) +
  coord_cartesian(xlim = c(3, 9)) +
  labs(x = "No. of employees in logs",y = "Percent")+
  theme_bg() +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  scale_y_continuous(labels = scales::percent) 

ggsave(paste0(output, "F21_h_lnemp_R.png"), width=12, height=7.5)

# Histogram: degree_nm
ggplot(data=data, aes(x=degree_nm)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.05, color="black", fill="blue", alpha = 0.8) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Proportion of non-managers with a college degree",y = "Percent")+
  theme_bg() +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  scale_y_continuous(labels = scales::percent) 

ggsave(paste0(output, "F21_h_degree_R.png"), width=12, height=7.5)


#---------------------------------------------------------------------
# AS2 - practice
# create nice data viz on key variables
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------


# Lowess: degree and management
ggplot(data = data, aes(x=degree_nm, y=management)) +
  geom_smooth(method="loess", se=T, colour="black", size=1.5, span=0.9) +
  labs(x = "Proportion of non-managers with a college degree",y = "Average of management questions") +
  theme_bg() +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, 0.1))

ggsave(paste0(output, "F21_h_degree_lowess_R.png"), width=12, height=7.5)


# Thus - Generate degree_nm_sq
data$degree_nm_sq <- data$degree_nm^2

#---------------------------------------------------------------------
# practice: 
# create a graph that combines this with frequency
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------


# Lowess: degree and management
ggplot(data = data, aes(x=lnemp, y=management)) +
  geom_smooth(method="loess", se=T, colour="black", size=1.5, span=0.9) +
  labs(x = "Proportion of non-managers with a college degree",y = "Employment in logs") +
  theme_bg() +
  scale_x_continuous(limits = c(4,9), breaks = seq(4,9,1))

ggsave(paste0(output, "F21_h_emp_lowess_R.png"), width=12, height=7.5)


# Summary

rows <- c("management",
          "foundfam_owned",
          "degree_nm", 
          "degree_nm_sq", 
          "compet_moder", 
          "compet_strong",
          "industry",
          "countrycode")

summary <- data %>% 
  select(rows) %>% 
  summarise_each(funs(Mean = mean(., na.rm=T),
                      Max = max(., na.rm=T),
                      Min = min(., na.rm=T),
                      Std.Dev. = sd(., na.rm=T),
                      Freq = n()
  )) 

tab <- data.frame(row.names= rows,
                  Mean = t(select(summary, ends_with("Mean"))),
                  Std.Dev. = t(select(summary, ends_with("Std.Dev."))),
                  Min = t(select(summary, ends_with("Min"))),
                  Max = t(select(summary, ends_with("Max"))),
                  Freq = t(select(summary, ends_with("Freq"))))

print(tab)


# Remove objects
rm(rows, summary, tab)


# Save workfile ------------------------------------------------------
write.csv(data, paste0(data_out, "Ch21_wms_workfile_xsec.csv"), row.names = F)



######################################################################
#
# Matching and regressions for ATE
#
######################################################################


######################################################################
# Exact matching prep and describe

# Import workfile 
data <- read.csv(paste0(data_out,"Ch21_wms_workfile_xsec.csv"))


# New data <- All cells grouped by same covariate values
data2 <- data %>%
  group_by(degree_nm_bins, competition, industry, countrycode) %>%
  summarise(n = n(), 
            foundfam_sum = sum(foundfam_owned),
            foundfam_mean = mean(foundfam_owned, na.rm=T),
            management_mean = mean(management, na.rm=T))

print(data2)

# Theoretical combinations (5040)
4*3*20*21
#length(unique(data$degree_nm_bins)) * length(unique(data$competition)) * length(unique(data$industry)) * length(unique(data$countrycode)) 

# Combinations in the data (2435)
dim(data2)[1]


# Theoretical combinations not in the data 
tab <- data2 %>% 
  select(countrycode, industry) %>%
  group_by(countrycode, industry) %>%
  count(countrycode, industry) %>% 
  spread(industry, n, fill = 0)

print(tab)

dim(tab)[1]*dim(tab)[2]


# An example: Japan, furniture

tab2 <- data2 %>% 
  select(countrycode, industry) %>%
  filter(countrycode==14 & industry==25) %>%
  group_by(countrycode, industry) %>%
  count(countrycode, industry) %>% 
  spread(industry, n, fill = 0)

print(tab2)


# Few firms in a cell
data2 %>%
  select(n) %>%
  group_by(n) %>%
  summarise(Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

  
# Number of cells with no exact matches
data2$other_sum <- data2$n - data2$foundfam_sum


data2 %>%
  select(other_sum, foundfam_sum) %>% 
  group_by(other_sum, foundfam_sum)  %>% 
  count(other_sum, foundfam_sum) %>% 
  spread(other_sum, n, fill = 0)


# Number of treated firms with no exact match
tab3 <- data2 %>%
  select(other_sum, foundfam_sum) %>% 
  filter(other_sum==0)  %>% 
  group_by(foundfam_sum)  %>% 
  summarise(sum_foundfam = sum(foundfam_sum), sum_other = sum(other_sum), sum_total = sum(foundfam_sum) +sum(other_sum), sum_n = n()) %>% 
  summarise(sum_foundfam = sum(sum_foundfam), sum_other = sum(sum_other), sum_total = sum(sum_total), sum(sum_n)) 

print(tab3)

# Total number of firms
tab4 <- data2 %>%
  select(other_sum, foundfam_sum) %>% 
  group_by(foundfam_sum)  %>% 
  summarise(sum_foundfam = sum(foundfam_sum), sum_other = sum(other_sum), sum_total = sum(foundfam_sum) +sum(other_sum), sum_n = n()) %>% 
  summarise(sum_foundfam = sum(sum_foundfam), sum_other = sum(sum_other), sum_total = sum(sum_total), n= sum(sum_n)) 

print(tab4)

# Ratio (930/2507= 0.371)
tab3$sum_foundfam/tab4$sum_foundfam

#---------------------------------------------------------------------
# AS3 - practice
# create nice data viz on key variables
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------


# Examples of treated firms with no exact match
data2 %>%
  select(industry, countrycode, degree_nm_bins, competition, foundfam_sum, other_sum, n) %>%
  filter(other_sum==0 & n<100)
  
# U.S. food industry
data2 %>%
  select(industry, countrycode, degree_nm_bins, competition, foundfam_sum, other_sum, n) %>%
  filter(countrycode==21 & industry==20)
  

# Remove objects
rm(tab, tab2, tab3, tab4, data2)

################################################
### Crucial assumption: Check common support ###

# Import workfile 
data <- read.csv(paste0(data_out,"Ch21_wms_workfile_xsec.csv"))

# Country, cometition, industry
c1 <- CrossTable(data$foundfam_owned, data$compet_moder, na.rm=T )
c2 <- CrossTable(data$foundfam_owned, data$compet_strong, na.rm=T)
i <- CrossTable(data$foundfam_owned, data$industry, na.rm=T)
c <- CrossTable(data$foundfam_owned, data$countrycode, na.rm=T)

cbind(c1$prop.row, c2$prop.row, i$prop.row, c$prop.row)

# College Degree
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(degree_nm , na.rm=T), 
            max = max(degree_nm , na.rm=T),
            p1 = quantile(degree_nm , probs = 0.01, na.rm=T),
            p5 = quantile(degree_nm , probs = 0.05, na.rm=T),
            p95 = quantile(degree_nm , probs = 0.95, na.rm=T),
            q99 = quantile(degree_nm, probs = 0.99, na.rm=T),
            n = n())

# Employment
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(emp_firm , na.rm=T), 
            max = max(emp_firm , na.rm=T),
            p1 = quantile(emp_firm , probs = 0.01, na.rm=T),
            p5 = quantile(emp_firm, probs = 0.05, na.rm=T),
            p95 = quantile(emp_firm, probs = 0.95, na.rm=T),
            q99 = quantile(emp_firm, probs = 0.99, na.rm=T),
            n = n())

# Histogram comparison
ggplot(data, aes( x = lnemp , group = foundfam_owned ) )+
  geom_histogram( aes(y=..density..) , binwidth=0.25, alpha=0.3, position="identity")+
  labs(x = "Log-Employment",y = "Density")+
  theme_bg() +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  geom_density(alpha=0.8)

rm(data, c1, c2, i, c )

###############
#### TOOLS ####
###############

# Import workfile 
data <- read.csv(paste0(data_out,"Ch21_wms_workfile_xsec.csv"))

# Simple means are missleading in obsevrational studies
ATE_SM <- mean(data$management[data$foundfam_owned==1]) - mean(data$management[data$foundfam_owned==0])
ATE_SM_SE <- sqrt( var(data$management[data$foundfam_owned==1]) + var(data$management[data$foundfam_owned==0]) )

print( c( ATE_SM , ATE_SM_SE ) )

####################################
#  Exact matching estimate of ATE  # 
#  (using successful matches only) #
####################################

# Generate other_owned
data$other_owned = 1-data$foundfam_owned


# Management score if treated vs if untreated
# We will take their average separately when aggregating
data$management1 <- ifelse(data$foundfam_owned == 1, data$management, NA)
data$management0 <- ifelse(data$other_owned == 1, data$management, NA)


# Aggregate by cell
# Count treated and untreated firms, take treated and untreated avg of outcome

data2 <- data %>%
  group_by(degree_nm_bins, competition, industry, countrycode) %>%
  summarise(foundfam_owned = sum(foundfam_owned),
            other_owned = sum(other_owned),
            management0 = mean(management0, na.rm=T),
            management1 = mean(management1, na.rm=T))
    
View(data2)


# Exact matching: keep only those with both founder/family and other

data2 <- data2 %>%
  filter(foundfam_owned>0 & other_owned>0)


# average of treated and untreated outcomes
#  weighted by treated # observations in each cell
data2$n <- 1

summary <- data2 %>% 
  group_by(n) %>% 
  mutate(w = (foundfam_owned + other_owned)/sum(foundfam_owned + other_owned)) %>%
  summarise(management0_Mean = sum(management0 * w),
            management1_Mean = sum(management1 * w),
            obs = n(), 
            management0_Std.Dev. = sd(management0), 
            management1_Std.Dev. = sd(management1),
            management0_Min = min(management0), 
            management1_Min = min(management1),
            management0_Max = max(management0), 
            management1_Max = max(management1),
            weight = sum(foundfam_owned + other_owned))
  

tab <- data.frame(row.names= c("management0", "management1"),
                  Mean = t(select(summary, ends_with("Mean"))),
                  Std.Dev. = t(select(summary, ends_with("Std.Dev."))),
                  Min = t(select(summary, ends_with("Min"))),
                  Max = t(select(summary, ends_with("Max"))),
                  Obs = t(select(summary, ends_with("obs"))),
                  Weight = t(select(summary, ends_with("weight"))))

print(tab)
ATE_EM <- tab[2,1]-tab[1,1]
ATE_EM_SE <- (tab[1,2]^2+tab[2,2]^2)^(1/2)
print( c( ATE_EM , ATE_EM_SE ) )

# Delete objects
rm(data2, tab, summary)


####################################
# Matching on the propensity score #
####################################

# Import workfile 
data <- read.csv(paste0(data_out,"Ch21_wms_workfile_xsec.csv"))

# SOLUTION With replacement --------------------------------------
# a) Except employment

# Function only works with non-missing values!
data2 <- data %>% 
  select(foundfam_owned, management, degree_nm, degree_nm_sq, 
         compet_moder, compet_strong, industry, countrycode) %>%
  na.omit()


# Step 1 - Matching
mod_match <- matchit(foundfam_owned ~ 
                       degree_nm + degree_nm_sq + factor(compet_moder) + 
                       factor(compet_strong) + factor(industry) + factor(countrycode), 
                     data = data2, 
                     method = 'nearest', distance = 'logit', replace=T)

summary(mod_match)

# Step 2 - restrict data to matched 
data_match <- match.data(mod_match)
dim(data_match)

# Step 3 - Estimate treatment effects (6356 matched)
reg_match <- lm(management ~ foundfam_owned, 
                data = data_match)

out1 <- summary(reg_match)

ATE_PSME1 <- out1$coefficients[2]
ATE_PSME1_SE <- out1$coefficients[2,2]

# b) With employment
data2 <- data %>% 
  select(foundfam_owned, management, degree_nm, degree_nm_sq, 
         compet_moder, compet_strong, industry, countrycode, lnemp) %>%
  na.omit()

mod_match <- matchit(foundfam_owned ~ 
                     degree_nm + degree_nm_sq + factor(compet_moder) + 
                     factor(compet_strong) + lnemp + factor(industry) + 
                     factor(countrycode), 
                     data = data2, method = 'nearest', distance = 'logit')

summary(mod_match)

# Step 2 - restrict data to matched
data_match <- match.data(mod_match)
dim(data_match)

# Step 3 - Estimate treatment effects (6356 matched)
reg_match <- lm(management ~ foundfam_owned
                + degree_nm + degree_nm_sq + factor(compet_moder) + 
                  factor(compet_strong) + lnemp + factor(industry) + 
                  factor(countrycode), data = data_match)

out2 <- summary(reg_match)

ATE_PSME2 <- out2$coefficients[2]
ATE_PSME2_SE <- out2$coefficients[2,2]

#---------------------------------------------------------------------
# practice
# create nice data viz on key variables
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------


# Regressions --------------------------------------------------------

# Import workfile 
data <- read.csv(paste0(data_out,"Ch21_wms_workfile_xsec.csv"))


# 1. y on x
reg <- lm(management ~ foundfam_owned, data=data)
reg1 <- coeftest(reg, vcov = sandwich)


# 2.y on x and all z except employment
reg <- lm(management ~ foundfam_owned + degree_nm + degree_nm_sq + compet_moder + 
            compet_strong + factor(industry) + factor(countrycode), data=data)

reg2 <- coeftest(reg, vcov = sandwich)


# 3. y on x and all z incl. employment
reg <- lm(management ~ foundfam_owned + degree_nm + degree_nm_sq + compet_moder + 
            compet_strong + lnemp + factor(industry) + factor(countrycode), data=data)

reg3 <- coeftest(reg, vcov = sandwich)

# Save output
stargazer(reg1, reg2, reg3, out=paste(output,"Foundfam_reg1_R.html",sep=""), digits=2, float = F, no.space = T)


#---------------------------------------------------------------------
# practice
# create nice data viz on key variables
# table should be exported in tex. / graph should be in jpg
#---------------------------------------------------------------------

#########################
## WHAT WE HAVE LEARNT ##
#########################


# Exact Matching is quite good but with lot of compromise and often it is infeasible
# Propensity score matching is a good approximation, but several options to be decided
# OLS is a good linear approximation
# Results are robust if they do not depend on method and on added controls much!
sum_results <- matrix( c(ATE_SM,ATE_SM_SE , ATE_EM, ATE_EM_SE, 
                         ATE_PSME1, ATE_PSME1_SE, ATE_PSME2, ATE_PSME2_SE, 
                         reg1[2,1], reg1[2,2], reg2[2,1], reg2[2,2], reg3[2,1], reg3[2,2] ),
                       nrow = 2 , ncol = 7 ,
                       dimnames = list( c('ATE','SE[ATE]') , c('Simple means','Exact Matching',
                           'PS matching without emp','PS matching with emp',
                           'simple OLS','OLS+controls','OLS+controls+emp' )  ) )
print(sum_results)  
  
  

