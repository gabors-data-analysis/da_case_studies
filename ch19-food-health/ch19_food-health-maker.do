********************************************************************
* Prepared for Gabor's Data Analysis
*
* Data Analysis for Business, Economics, and Policy
* by Gabor Bekes and  Gabor Kezdi
* Cambridge University Press 2021
*
* gabors-data-analysis.com
*
* License: Free to share, modify and use for educational purposes.
* 	Not to be used for commercial purposes.
*
* Chapter 19
* Food and Health Case study
* Relating food habits and health outcomes
* Comes from the idea by Emily Oster's article in Slate (2018)
* NHANES data, using aggregated features from Emily Oster
* creates the food-health work file that ch19-food-health.do reads
* using the food-health dataset
* version 1.1 2026-09-04
*
* STATA VERSION: This code is written for Stata 18
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


********************************************************************
* SETTING UP DIRECTORIES
********************************************************************

* STEP 1: set working directory for da_case_studies
* Example: cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
* NOTE: this script WRITES its output back into the data repository,
* overwriting food-health.dta and food-health.csv there.
global data_in   "${data_dir}/food-health/clean"
global data_out  "${data_dir}/food-health/clean"


********************************************************************************************************-
********************************************************************************************************-
*
* PART I. FEATURE ENGINEERING ---> VARIABLES FOR THE EXERCISE
*
********************************************************************************************************-
********************************************************************************************************-


use "${data_in}/food_dataset_pers_extended_09-11-13.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/8ueht/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 


**********************************************-
* FOOD
**********************************************-

* create veggies
	order gr_*
	egen veggies1 = rowtotal(other_vegetable -tomato  )
	egen veggies2= rowtotal(sprouts- snow_peas)
	generate veggies=veggies1+veggies2
	egen fruits1= rowtotal( grapefruit - orange )
	egen fruits2= rowtotal( apple - strawberries )
	generate fruits=fruits1+fruits2
	drop fruits1 fruits2 veggies1 veggies2

	generate veggies_n_fruits=veggies+fruits
label variable veggies_n_fruits "number of vegetables and fruits
	replace veggies_n_fruits=11 if  veggies_n_fruits>11
	
	egen veggies1 = rowtotal(gr_other_vegetable -gr_tomato  )
	egen veggies2= rowtotal(gr_sprouts- gr_snow_peas)
	generate veggies_gr=veggies1+veggies2
	egen fruits1= rowtotal( gr_grapefruit - gr_orange )
	egen fruits2= rowtotal( gr_apple - gr_strawberries )
	generate fruits_gr=fruits1+fruits2
	drop fruits1 fruits2 veggies1 veggies2

generate veggies_n_fruits_gr=veggies_gr+fruits_gr

* coffee
generate coffee_espressounit= ((dr1tcaff + dr2tcaff)/2)/60
label variable coffee_espressounit "2-day avg caffeine, in espresso units (60mg), dr1tcaff dr2tcaff"
replace coffee_espressounit=15 if coffee_espressounit>12 & coffee_espressounit<.


* alternative food categories
generate beef_all=beef+beef_frozen_meal +beef_soup +beef_lean +beef_with_starch + beef_with_starch_vegetable +beef_with_vegetable
generate red_meat_all=beef+beef_frozen_meal +beef_soup +beef_lean +beef_with_starch + beef_with_starch_vegetable +beef_with_vegetable+pork +pork_lean +pork_soup +pork_with_starch +pork_with_starch_vegetable +pork_with_vegetable +lamb +lamb_lean
generate nuts= almonds + cashews + nuts_other

**********************************************-
* SOCIO-ECON, GENDER, AGE
**********************************************-

	
* gender, age
rename riagendr gender
label variable gender "gender"
label define mf 1 "male" 2 "female"
label value gender mf 

rename ridageyr age
keep if age>=18
label variable age "age, 80 topcoded"
generate age2=age^2

egen age_cat= cut(age), at(18, 30,40,50,60,70, 81)
label variable age_cat "age groups"
label define agc 18 "aged 18-29" 30 "aged 30-39" 40 "aged 40-49"  50 "aged 50-59"  60 "aged 60-69"  70 "aged 70+" 
label value age_cat agc	


* socio-economic
rename ridreth1 race
generate married = dmdmartl==1 | dmdmartl==6

rename dmdeduc2 edu
replace edu=. if edu>5

rename dmdhhsiz hh_size
rename indhhin2  hh_income

generate hh_income_usd=hh_income
replace hh_income=6 if hh_income==12
replace hh_income=4 if hh_income==13
replace hh_income=12 if hh_income==14
replace hh_income=13 if hh_income==15
replace hh_income=. if hh_income>15


recode hh_income_usd ///
		1=2500 /// 
		2=7500 /// 
		3=12500 /// 
		4=17500 /// 
		5=22500 /// 
		6=30000 /// 
		7=40000 /// 
		8=50000 /// 
		9=60000 /// 
		10=70000 /// 
		12=30000 /// 
		13=10000 ///
		14=85000 ///
		15=150000 ///
		77=40000 ///
		99=40000

generate hh_income_percap=hh_income_usd /hh_size
label variable hh_income_percap "Household income mid-point / size"
* prep for regressions
generate ln_hh_income_percap =ln(hh_income_percap )
label variable ln_hh_income_percap "Log Household income mid-point / size"


egen income_cat= cut(hh_income_percap ), at(1000,10000,30000, 150000)
label variable income_cat "Income (per-capita) categories"
label define inc 1000 "low" 10000 "mid" 30000 "high"
label value income_cat inc	

rename ocd241 work_occupation 
label variable work_occupation " Occupation group code: current job"
rename ocd150 work_type
rename ocq180  work_hs
replace work_hs=. if work_hs>150
replace work_hs=100 if work_hs>100 & work_hs<.


**********************************************-
* HEALTH OUTCOMES
**********************************************-
*sport
generate sport_days=paq655
replace sport_days =0 if sport_days ==.
generate walk_cycle_days=paq640
replace walk_cycle_days =0 if walk_cycle_days ==.

*smoke
generate smoker= smq040 ==1 | smq040 ==2

*sleep
generate sleep_hs = sld010h
label variable sleep_hs "How much sleep do you get (hours)? sld010h"

	generate bp_systolic = bpxsy1 
	generate bp_diastolic = bpxdi1
	generate total_cholesterol= lbdhdd 
	generate hdl = lbxtc
	generate weight = bmxwt 
	generate height = bmxht

generate ldl= lbxtc- lbdhdd
replace ldl=60 if ldl<60
replace ldl=250 if ldl>250 & ldl<.
label variable ldl "LDL Cholesterol, Total-HDL, lbxtc lbdhdd "


* BMI and normal weight variables
	generate bmi = 10000*weight/(height*height)
	generate normal_weight = 1 if bmi < 25
	replace normal_weight = 0 if bmi >= 25
	replace normal_weight = . if bmi == .
	
	* Blood pressure variables
	replace bp_systolic = . if bp_systolic == 0
	replace bp_diastolic = . if bp_diastolic == 0
    replace bp_systolic=85 if bp_systolic<85
	replace bp_systolic=200 if bp_systolic>200 & bp_systolic<.
	replace bp_diastolic=40 if bp_diastolic<40
	replace bp_diastolic=100 if bp_diastolic>100 & bp_diastolic<.

* gabor's score	
generate blood_pressure =(bp_systolic + bp_diastolic)

* heart health
generate heart_risk=ldl+blood_pressure 
label variable heart_risk "heart risk - sum of blood pressure and cholesterol"






**********************************************-
* CLEAN-UP
**********************************************-

drop sld010h bpxsy1 bpxdi1 lbxtc lbdhdd bmxwt bmxht dmdmartl

**********************************************-
* SAVE WORK FILE
**********************************************-

save "${data_out}/food-health.dta", replace

export delimited using "${data_out}/food-health.csv", replace
