# *************************************************************************
# * Chapter 19
# * Food and Health Case study
# *
# * Relating food habits and health outcomes
# * Comes from the idea by Emily Oster's article in Slate (2018)
# * NHANES data, using aggregated features from Emily Oster
# *

# *************************************************************************

# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(haven)

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3)

data_in <- paste(data_dir,"food-health","clean/", sep = "/")
use_case_dir <- "ch19-food-health/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



# ********************************************************************************************************-
# ********************************************************************************************************-
# *
# * PART I. FEATURE ENGINEERING ---> VARIABLES FOR THE EXERCISE
# *
# ********************************************************************************************************-
# ********************************************************************************************************-

data <- read_dta(paste0(data_in,"food_dataset_pers_extended_09-11-13.dta"))

# **********************************************-
# * FOOD
# **********************************************-

# * create veggies

veggies <- c("other_vegetable", "collard_greens", "dandelion_greens", "romaine_lettuce", 
	"generic_greens", "kale", "mustard_greens", "spinach", "turnip_greens",
	"broccoli", "dark_green_vegetable_soup", "carrots", "pumpkin", "sweet_potato", 
	"squash", "tomato" , "sprouts", "artichoke", "asparagus", "green_beans",   
	"beets", "brussels_sprouts", "cabbage", "cauliflower","celery","corn", 
	"cucumber", "eggplant", "lettuce", "arugula", "mushrooms", "onions",
	"peas",  "peppers", "radish", "snow_peas")

fruits <- c("grapefruit" , "lemon" ,"orange", "apple", "applesauce", "apricot", 
	"avocado", "banana", "cantaloupe", "cherries", "other_fruit", "grapes", 
	"guava", "kiwi", "honeydew", "mango", "nectarine", "papaya", "peach", 
	"pear", "pineapple", "plum", "watermelon", "blackberries", "blueberries", 
	"cranberries", "raspberries", "strawberries")

beef_all <- c("beef", "beef_frozen_meal", "beef_soup", "beef_lean", "beef_with_starch",
	"beef_with_starch_vegetable", "beef_with_vegetable")

red_meat_all <- c("beef", "beef_frozen_meal", "beef_soup", "beef_lean", "beef_with_starch",
 "beef_with_starch_vegetable", "beef_with_vegetable", "pork", "pork_lean", 
 "pork_soup", "pork_with_starch", "pork_with_starch_vegetable", "pork_with_vegetable",
  "lamb", "lamb_lean")

nuts <- c("almonds", "cashews", "nuts_other")


data <- data %>%
	mutate(
		veggies_n_fruits = rowSums(.[c(veggies, fruits)]),
		veggies_n_fruits_gr = rowSums(.[paste0("gr_", c(veggies, fruits))]),
		coffee_espressounit = ((dr1tcaff + dr2tcaff)/2)/60,
		beef_all = rowSums(.[beef_all]),
		red_meat_all = rowSums(.[red_meat_all]),
		nuts = rowSums(.[nuts])
	) %>%
	mutate(
		veggies_n_fruits = ifelse(veggies_n_fruits>11, 11, veggies_n_fruits),
		coffee_espressounit = ifelse(coffee_espressounit>12, 15, coffee_espressounit) 
	)

# **********************************************-
# * SOCIO-ECON, GENDER, AGE
# **********************************************-

	
# * gender, age
data <- data %>%
	mutate(
		gender = factor(data$riagendr, levels = c(1,2), labels = c("male", "female")),
		age = ridageyr,
		age2 = age^2
	) %>%
	filter(age >= 18) %>%
	mutate(
		age_cat = cut(age, breaks = c(18, 30,40,50,60,70, 81), include.lowest= TRUE, 
						labels = c("aged 18-29", "aged 30-39", "aged 40-49", "aged 50-59", "aged 60-69", "aged 70+"))
	)


# * socio-economic
hh_income_usd_recode <- c("1"="2500","2"="7500","3"="12500","4"="17500","5"="22500",
						"6"="30000","7"="40000","8"="50000","9"="60000","10"="70000","12"="30000",
						"13"="10000","14"="85000","15"="150000","77"="40000","99"="40000")

data <- data %>%
	mutate(
		race = ridreth1,
		married = dmdmartl==1 | dmdmartl==6,
		edu = dmdeduc2,
		hh_size = dmdhhsiz,
		hh_income = ifelse( indhhin2==12, 6, hh_size),
		hh_income = ifelse( indhhin2==13, 4, hh_size),
		hh_income = ifelse( indhhin2==14, 12, hh_size),
		hh_income = ifelse( indhhin2==15, 13, hh_size),
		hh_income = ifelse( indhhin2>15, NA, hh_size),
		hh_income_usd = recode(indhhin2, !!!hh_income_usd_recode)
	) %>%
	mutate(
		edu = ifelse(edu>5, NA, edu),
		hh_income_percap = as.numeric(hh_income_usd) /hh_size
	)
 
# * prep for regressions
data <- data %>%
	mutate(
		ln_hh_income_percap = log(hh_income_percap),
		income_cat= cut(hh_income_percap, breaks = c(1000, 10000, 30000, 150000), 
			include.lowest= TRUE, labels = c("low", "mid", "high")),
		work_occupation = ocd241,
		work_type = ocd150,
		work_hs = ocq180
	) %>%
	mutate(
		work_hs = ifelse(work_hs>150, NA, ifelse(work_hs>100, 100, work_hs))
	)

# **********************************************-
# * HEALTH OUTCOMES
# **********************************************-
# *sport

data <- data %>%
	mutate(
		sport_days=paq655,
		walk_cycle_days=paq640
	)	 %>%
	mutate(
		sport_days=ifelse(is.na(sport_days), 0, sport_days),
		walk_cycle_days=ifelse(is.na(walk_cycle_days), 0, walk_cycle_days)
	)

# *smoke
data <- data %>%
	mutate(
		smoker= smq040 ==1 | smq040 ==2
	)	

# *sleep
data <- data %>%
	mutate(
		sleep_hs = sld010h,
		bp_systolic = bpxsy1,
		bp_diastolic = bpxdi1,
		total_cholesterol= lbdhdd,
		hdl = lbxtc,
		weight = bmxwt,
		height = bmxht,
		ldl= lbxtc- lbdhdd
	) %>%
	mutate(
		ldl = ifelse(ldl<60, 60, ifelse(ldl>250, 250, ldl))
	)

# * BMI and normal weight variables
data <- data %>%
	mutate(
		bmi = 10000*weight/(height*height),
		normal_weight = ifelse(bmi < 25, 1, 0) 
	)
	
# * Blood pressure variables
data <- data %>%
	mutate(
		bp_systolic = ifelse(bp_systolic == 0, NA, bp_systolic),
		bp_diastolic = ifelse(bp_diastolic == 0, NA, bp_diastolic),
	) %>%
	mutate(
		bp_systolic= ifelse(bp_systolic<85, 85, bp_systolic),
		bp_diastolic= ifelse(bp_diastolic<40, 40, bp_diastolic)
	) %>%
	mutate(
		bp_systolic= ifelse(bp_systolic>200, 200, bp_systolic),
		bp_diastolic= ifelse(bp_diastolic>100, 100, bp_diastolic)
	)

# * gabor's score
data <- data %>%
	mutate(
		blood_pressure =(bp_systolic + bp_diastolic)
	)	

# * heart health
data <- data %>%
	mutate(
		heart_risk=ldl+blood_pressure 
	)	

data <- data %>%
	select(-c(sld010h, bpxsy1, bpxdi1, lbxtc, lbdhdd, bmxwt, bmxht, dmdmartl))

# **********************************************-
# * SAVE WORK FILE
# **********************************************-

write_csv(data, paste0(data_in,"food-health.csv"))



export delimited using "$data_out/food-health.csv", replace
