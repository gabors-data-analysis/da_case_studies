# CH 19 FOOD-HEALTH 

# v1.1 2020-01-09
# v1.2. 2020-01-29, adjust graphs
# v1.3. 2020-01-30, adjust graphs coordinates
# v1.4. 2020-04-23 edit graphs
# v1.5 2020-04-22 names ok
# v1.6 2020-07-01 rerun on corrected data, change var name

# WHAT THIS CODES DOES:
  # data manage, graphs and simple regressions

# clear environment
rm(list=ls())

# PACKAGES
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(Hmisc)
library(estimatr)


# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


data_in <- paste0(dir, "da_data_repo/food-health/clean/")
data_out <- paste0(dir, "da_case_studies/ch19-food-health/")
output <- paste0(dir,"da_case_studies/ch19-food-health/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

# load ggplot theme function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))
options(digits = 3)



# import data
data <- read_csv(paste0(data_in,"food-health.csv"))

# drop observations
workfile <-  data %>%
  filter(age >= 30 & age <60)

# new variables: 
  ## Fruit and vegetables per day (grams)
  ## Blood pressure (systolic+diastolic)

workfile <- workfile %>%
  mutate(fv=veggies_n_fruits_gr,
         bp=blood_pressure)

describe(workfile$fv)
describe(workfile$bp)

workfile <- workfile %>%
  filter(fv<3200) %>%
  drop_na(bp)

# Days per week exercising 
workfile <- workfile %>%
  mutate(exerc=case_when(paq655 <=7 ~ paq655,
                        paq650 ==2 ~ 0))

# Potato chips per day, grams
workfile <-  workfile %>%
  mutate(pchips=gr_potato_chips)



# Descriptive table -------------------------------------------------------------

descr_table <- function(var) {
workfile %>%
  summarise(mean=mean(var),
            median=median(var),
            std_dev=sd(var),
            minimum=min(var),
            maximum=max(var), 
            observations=n())
}


bp_descrtable <- descr_table(workfile$bp)
fv_descrtable <- descr_table(workfile$fv)
variable <- c("Blood pressure (systolic+diastolic)", "Fruit and vegetables per day (grams)")
descrtable <- rbind(bp_descrtable,fv_descrtable)
descrtable <- cbind(descrtable, variable)
descrtable <- descrtable %>%
  select(variable, mean:observations)
descrtable

# SCATTERPLOT AND REGRESSION LINE ------------------------------------------------------------------

# Blood pressure vs amount of fruit and vegetables 

#cor.test(workfile$fv,workfile$bp, method = "pearson")

reg <- lm(bp~fv, data = workfile)
summary(reg)

 # scatterplot and regression line
 fv_bp <-
   ggplot(workfile, aes(fv, bp)) +
   geom_point_da(size=0.3, alpha=0.5) +
   geom_smooth_da(method="lm",color=color[2]) +
   theme_bg() +
   labs(x="Fruit and vegetables per day (grams)",
        y = "Blood pressure (systolic+diastolic)") +
   scale_y_continuous(expand = c(0.01,0.01),breaks = seq(140,280,10)) +
   scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,3000,500)) +
   coord_cartesian(ylim=c(140,280), xlim =c(0,3000), expand=TRUE) 
 fv_bp
 #save_fig("R_19_fv_bp", output, "small")
 save_fig("ch19-figure-8a-fv-bp", output, "small")

 # regression line only
 fv_bp_regrline <-
   ggplot(workfile, aes(fv, bp)) +
   geom_smooth_da(method="lm") +
   theme_bg() +
   labs(x="Fruit and vegetables per day (grams)",
        y = "Blood pressure (systolic+diastolic)") +
   scale_y_continuous(expand = c(0.01,0.01),breaks = seq(180,200,2), minor_breaks = NULL) +
   scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,3000,500), minor_breaks = NULL) +
   coord_cartesian(ylim=c(180,200), xlim =c(0,3000), expand=TRUE) 
 fv_bp_regrline
 #save_fig("R_19_fv_regrlineonly", output, "small")
 save_fig("ch19-figure-8b-fv-reglin", output, "small")


#Log household income per capita vs fruit and vegetables per day,grams

# gen Log household income per capita
workfile <- workfile %>%
 mutate(lninc=log(hh_income_percap))
 
fv_inc <-  
ggplot(workfile, aes(lninc, fv)) +
  geom_point_da( size=0.3, alpha=0.5) +
  geom_smooth_da(method="lm") +
  theme_bg() +
  labs(x="ln(household income per capita, US dollars)",
       y = "Fruit and vegetables per day (grams)") +
  scale_y_continuous(expand = c(0.01,0.01),breaks=c(0,500,1000,1500,2000)) +
  scale_x_continuous(expand = c(0.01,0.01),breaks=c(6,7,8,9,10,11,12)) +
  coord_cartesian(xlim=c(6,12), ylim =c(0,2000), expand=TRUE) 
fv_inc
#save_fig("R_19_fv_inc", output, "small")
save_fig("ch19-figure-9a-fv-inc", output, "small")

# Amount of fruit and vegetables per day, g (fv) vs Days per week vigorous recreational activities (exerc)
workfile %>%
  group_by(exerc) %>%
  summarise(tab_exerc=length(exerc))

fv_exerc <-  
ggplot(workfile, aes(exerc, fv)) +
  geom_point_da(size=0.5, alpha=0.5) +
  geom_smooth_da(method="lm") +
  theme_bg() +
  labs(x="Days per week exercising",
       y = "Fruit and vegetables per day (grams)") +
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,7,1)) +
  scale_y_continuous(expand = c(0.01,0.01),breaks = seq(0,2000,500)) +
  coord_cartesian(xlim=c(0,7), ylim =c(0,2000), expand=TRUE) 
fv_exerc
#save_fig("R_19_fv_exerc", output, "small")
save_fig("ch19-figure-9b-fv-exerc", output, "small")

# potato chips (potato_chips) and amount of fruit and vegetables per day (fv)
pchips <- workfile %>%
  filter(pchips<400)

fv_pchips <-  
  ggplot(workfile, aes(pchips, fv)) +
  geom_point_da(size=0.3, alpha=0.5) +
  geom_smooth_da(method="lm") +
  theme_bg() +
  labs(x="Potato chips per day (grams)",
       y = "Fruit and vegetables per day (grams)") +
  scale_x_continuous(breaks = seq(0,400,50)) +
  scale_y_continuous(breaks = seq(0,3000,500)) +
  coord_cartesian(xlim=c(0,400), ylim =c(0,3000), expand=TRUE) 
fv_pchips
#save_fig("R_19_fv_pchips", output, "small")
save_fig("ch19-figure-10-fv-pchips", output, "small")


# regressions
reg1 <- lm_robust(bp~fv, data = workfile)
summary(reg1)

reg2 <- lm_robust(fv~ lninc  , data = workfile)
summary(reg2)
reg3 <- lm_robust(fv~ exerc, data = workfile)
summary(reg3)
reg4 <- lm_robust(fv ~ pchips, data = workfile)
summary(reg4)



#ch19-table-food-health-destab




