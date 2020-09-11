################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 10
# CH10A Understanding the gender difference in earnings
# version 0.9 2020-08-31


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries 
library(tidyverse)
library(arm)
library(lmtest)
library(estimatr)
library(sandwich)
library(segmented)
library(stargazer)
library(cowplot)
library(huxtable)



# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3) 

data_in <- paste(data_dir,"cps-earnings","clean/", sep = "/")
use_case_dir <- "ch10-gender-earnings-understand/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#####################################################################


# load in clean and tidy data and create workfile
cps <- read_csv(paste0(data_in,"morg-2014-emp.csv"))

# SELECT OBSERVATIONS
cps <- cps %>% filter(uhours>=20 & earnwke>0 & age>=24 & age<=64 & grade92>=44)
glimpse(cps)

# CREATE VARIABLES
cps <- cps %>% mutate(female=sex==2,
                      w=earnwke/uhours,
                      lnw=log(w)
)
# Save workfile
write_csv(cps,paste0(data_out,"earnings_multireg.csv",sep=""))               


# DISTRIBUTION OF EARNINGS
cps %>% dplyr::select(earnwke,uhours,w) %>% summary()

cps %>% dplyr::select(earnwke,uhours,w) %>% filter(w>=1) %>% summary()


################################################################
# LN EARNINGS, GENDER, AGE
# robust standard error

reg <- lm_robust(lnw ~ female, data=cps, se_type = "HC1")
reg2 <- lm_robust(lnw ~ female+ age, data=cps, se_type = "HC1")
reg3 <- lm_robust(age ~ female, data=cps, se_type = "HC1")

# stargazer makes nice regression tables. _r makes them neater + robust SE
ht<-huxreg(reg,reg2, reg3,
           statistics = c(N = "nobs", R2 = "r.squared")) 
ht

# not in book
F10_earnings_hist<- ggplot(data = cps, aes (x = age, y = 2*(..count..)/sum(..count..))) +
  geom_histogram(binwidth = 4, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  
                 boundary=0, closed='left',  show.legend=F, na.rm =TRUE) +
  labs(x = "Age (years)", y = "Percent") +
  facet_wrap(~ifelse(female, "Female", "Male"))+
  labs(x = "Age (years)",y = "Percent")+
  scale_x_continuous(limits = c(24,64) , breaks = seq(25, 65, by = 12),) +
  scale_y_continuous(limits=c(0, 0.16), breaks = seq(0, 0.16, by = 0.02), labels = scales::percent_format(accuracy = 5L)) +
  theme_bg()
F10_earnings_hist



F10_earnings_density<- ggplot(data = cps, aes(x=age, y = stat(density), color = female)) +
  geom_density(adjust=1.5, show.legend=F, na.rm =TRUE, size=0.7) +
  labs(x="Age (years)", y="Density", color = "") +
  scale_color_manual(name="", 
                     values=c(color[2],color[1]),
                     labels=c("Male","Female")) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(24, 64), breaks = seq(25, 65, by = 5)) +
  scale_y_continuous(expand = c(0.0, 0.0), limits = c(0, 0.04), breaks = seq(0, 0.04, by = 0.01)) +
  geom_text(aes(x = 55, y = 0.028, label = "Male"), color = color[2], size=2) +
  geom_text(aes(x = 55, y = 0.020, label = "Female"), color = color[1], size=2) +
  theme_bg() 
F10_earnings_density
save_fig("ch10-figure-1-earnings-density", output, "small")



##############################################
# LN EARNINGS, GENDER, AGE

cps <- cps %>% mutate(agesq=age**2,
                      agecu=age**3,
                      agequ=age**4
)


reg4 <- lm_robust(lnw ~ female, data=cps, se_type = "HC1")
reg5 <- lm_robust(lnw ~ female+ age, data=cps, se_type = "HC1")
reg6 <- lm_robust(lnw ~ female + age + agesq, data=cps, se_type = "HC1")
reg7 <- lm_robust(lnw ~ female + age + agesq + agecu + agequ, data=cps, se_type = "HC1")

# show results with robust SE and save them
huxreg(reg4, reg5, reg6, reg7,statistics = c(N = "nobs", R2 = "r.squared"))


##########################################
# LN EARNINGS, EDU CATEG

cps <- cps %>% mutate(ed_MA=as.numeric(grade92==44),
                      ed_Profess = as.numeric(grade92==45),
                      ed_PhD = as.numeric(grade92==46)
)

reg8 <- lm_robust(lnw ~ female, data=cps,se_type = "HC1")
reg9 <- lm_robust(lnw ~ female + ed_Profess + ed_PhD, data=cps, se_type = "HC1")
reg10 <- lm_robust(lnw ~ female + ed_MA + ed_PhD, data=cps, se_type = "HC1")

huxreg(reg8, reg9, reg6, reg10,statistics = c(N = "nobs", R2 = "r.squared"))




#################################################
# SIMPLE INTERACTION: LINEAR AGE WITH GENDER
cps %>% filter(female==1)
reg11 <- lm_robust(lnw ~ age, data=cps %>% filter(female==1), se_type = "HC1")
reg12 <- lm_robust(lnw ~ age, data=cps %>% filter(female==0), se_type = "HC1")
reg13 <- lm_robust(lnw ~ female + age + age*female, data=cps, se_type = "HC1")

huxreg(reg11, reg12, reg13,statistics = c(N = "nobs", R2 = "r.squared"))

#####################################################
# FOR RPEDICTIONL FUNCTIONAL FORMS & INTERACTIONS WITH GENDER

cps <- cps %>% mutate(agesq=age^2,
                      agecu=age^3,
                      agequ=age^4
)


reg14 <- lm_robust(lnw ~ age + agesq + agecu + agequ, data=cps %>% filter(female==1))
reg15 <- lm_robust(lnw ~ age + agesq + agecu + agequ, data=cps %>% filter(female==0))
reg16 <- lm_robust(lnw ~ age + agesq + agecu + agequ + female + female*age + female*agesq + female*agecu + female*agequ, data=cps)

huxreg(reg14, reg15, reg16,statistics = c(N = "nobs", R2 = "r.squared"))

# PREDICTION AND GRAPH LINEAR
data_m <- cps %>% filter(female==0)
pred <- predict(reg13, newdata = data_m, se.fit=T)
data_m <- bind_cols(data_m,as_tibble(pred$fit))
data_m <- data_m %>% mutate(CIup=value+2*pred$se.fit,
                            CIlo=value+-2*pred$se.fit 
)


data_f <- cps %>% filter(female==1)
pred <- predict(reg13, newdata = data_f, se.fit=T)
data_f <- bind_cols(data_f,as_tibble(pred$fit))
data_f <- data_f %>% mutate(CIup=value+2*pred$se.fit,
                            CIlo=value+-2*pred$se.fit 
)

F10_earnings_interact<- ggplot(data=data_m,aes(x=age,y=value))+
  geom_line(colour=color[1],linetype=1, lwd=0.8)+
  geom_line(data=data_m,aes(x=age,y=CIup), colour=color[1], linetype= "dashed", lwd=0.3)+
  geom_line(data=data_m,aes(x=age,y=CIlo), colour=color[1], linetype= "dashed", lwd=0.3)+
  geom_line(data=data_f,aes(x=age,y=value),colour=color[2],lwd=0.8)+
  geom_line(data=data_f,aes(x=age,y=CIup), colour=color[2],  linetype= "dashed", lwd=0.3)+
  geom_line(data=data_f,aes(x=age,y=CIlo), colour=color[2],  linetype= "dashed", lwd=0.3)+
  labs(x = "Age (years)",y = "ln(earnings per hour, US dollars)")+
  scale_x_continuous(expand = c(0.01,0.01), limits = c(24, 65), breaks = seq(25, 65, by = 5)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(2.8, 3.8), breaks = seq(2.8, 3.8, by = 0.1)) +
  theme_bg() 
F10_earnings_interact
#save_fig("F10_earnings_interact_R", output, "small")


# PREDICTION AND GRAPH POLYNOMIAL
data_m <- cps %>% filter(female==0)
pred <- predict(reg16, newdata = data_m, se.fit=T)
data_m <- bind_cols(data_m,as_tibble(pred$fit))
data_m <- data_m %>% mutate(CIup=value+2*pred$se.fit,
                            CIlo=value+-2*pred$se.fit 
)


data_f <- cps %>% filter(female==1)
pred <- predict(reg16, newdata = data_f, se.fit=T)
data_f <- bind_cols(data_f,as_tibble(pred$fit))
data_f <- data_f %>% mutate(CIup=value+2*pred$se.fit,
                            CIlo=value+-2*pred$se.fit 
)

F10_earnings_interact2<- ggplot(data=data_m,aes(x=age,y=value))+
  geom_line(colour=color[1],linetype=1, lwd=0.8)+
  geom_line(data=data_m,aes(x=age,y=CIup), colour=color[1], linetype= "dashed", lwd=0.3)+
  geom_line(data=data_m,aes(x=age,y=CIlo), colour=color[1], linetype= "dashed", lwd=0.3)+
  geom_line(data=data_f,aes(x=age,y=value),colour=color[2],lwd=0.8)+
  geom_line(data=data_f,aes(x=age,y=CIup), colour=color[2],  linetype= "dashed", lwd=0.3)+
  geom_line(data=data_f,aes(x=age,y=CIlo), colour=color[2],  linetype= "dashed", lwd=0.3)+
  labs(x = "Age (years)",y = "ln(earnings per hour, US dollars)")+
  scale_x_continuous(expand = c(0.01,0.01), limits = c(24, 65), breaks = seq(25, 65, by = 5)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(2.8, 3.8), breaks = seq(2.8, 3.8, by = 0.1)) +
  theme_bg() 
F10_earnings_interact2
#save_fig("F10_earnings_interact_R", output, "small")



########################################################################
# Part 2
# TOWARDS CAUSAL ANALYIS - IS IT DISCRIMINATION?

# FILTER DATA -  SELECTION of the sample we need
cps <- cps %>% filter(age>=40 & age<=60)


# Pre-determined demographics
cps <- cps %>% mutate(white=as.numeric(race==1),
                      afram = as.numeric(race==2),
                      asian = as.numeric(race==4),
                      hisp = !is.na(ethnic),
                      othernonw = as.numeric(white==0 & afram==0 & asian==0 & hisp==0),
                      nonUSborn = as.numeric(prcitshp=="Foreign Born, US Cit By Naturalization" | prcitshp=="Foreign Born, Not a US Citizen") 
)


# EDUC
# AGE
cps <- cps %>% mutate(ed_MA = as.numeric(grade92==44),
                      ed_Profess = as.numeric(grade92==45),
                      ed_PhD = as.numeric(grade92==46),
                      agesq =  age**2,
                      agecu =  age**3,
                      agequ =  age**4)




# Potentially endogeneous demographics
cps <- cps %>% mutate(married = as.numeric(marital==1 | marital==2),
                      divorced = as.numeric(marital==3 | marital==5 | marital==6),
                      wirowed = as.numeric(marital==4),
                      nevermar = as.numeric(marital==7),
                      
                      child0 = as.numeric(chldpres==0),
                      child1 = as.numeric(chldpres==1),
                      child2 = as.numeric(chldpres==2),
                      child3 = as.numeric(chldpres==3),
                      child4pl = as.numeric(chldpres>=4))

# Work-related variables
cps <- cps %>% mutate(fedgov = as.numeric(class=="Government - Federal"),
                      stagov = as.numeric(class=="Government - State"),
                      locgov = as.numeric(class=="Government - Local"),
                      nonprof = as.numeric(class=="Private, Nonprofit"),
                      ind2dig = as.integer(as.numeric(as.factor(ind02))/100),
                      occ2dig = as.integer(occ2012/100),
                      union = as.numeric(unionmme=="Yes" | unioncov=="Yes"))



# hours in ploynomial
cps <- cps %>% mutate(uhourssq = uhours^2,
                      uhourscu = uhours^3,
                      uhoursqu = uhours^4)


#### Extended regressions
reg1 <- lm_robust(lnw ~ female, data=cps,se_type = "HC1")

reg2 <- lm_robust(lnw ~ female + age + ed_Profess + ed_PhD, data=cps,se_type = "HC1")

reg3 <- lm_robust(lnw ~ female + age + afram + hisp + asian + othernonw + nonUSborn + ed_Profess + ed_PhD + married + divorced+ wirowed + child1 + child2 + child3 +child4pl + as.factor(stfips) + uhours + fedgov + stagov + locgov + nonprof + union + as.factor(ind2dig) + as.factor(occ2dig), data=cps,se_type = "HC1")

reg4 <- lm_robust(lnw ~ female + age + afram + hisp + asian + othernonw + nonUSborn + ed_Profess + ed_PhD + married + divorced+ wirowed + child1 + child2 + child3 +child4pl + as.factor(stfips) + uhours + fedgov + stagov + locgov + nonprof + union + as.factor(ind2dig) + as.factor(occ2dig) + agesq + agecu + agequ + uhoursqu + uhourscu + uhourssq, data=cps,se_type = "HC1")

huxreg(reg1, reg2, reg3, reg4,statistics = c(N = "nobs", R2 = "r.squared"))

#ch10-table-1-gendergap-reg1
#ch10-table-2-gendergap-reg2
#ch10-table-3-gendergap-reg3
#ch10-table-4-gendergap-reg4
#ch10-table-5-gendergap-reg5
#ch10-table-6-hotel-descr