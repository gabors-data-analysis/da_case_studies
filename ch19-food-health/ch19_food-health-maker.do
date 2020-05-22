*************************************************************************
* Chapter 19
* Food and Health Case study
*
* Relating food habits and health outcomes
* Comes from the idea by Emily Oster's article in Slate (2018)
* NHANES data, using aggregated features from Emily Oster
*
* v1.0. 2019-04-08
*************************************************************************


* set folders
*cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
* define folders 
global data_in			"cases_studies_public\food-health\clean"
global output          	"textbook_work\ch19\food-health\output"
global data_out			"textbook_work\ch19\food-health\"

********************************************************************************************************-
********************************************************************************************************-
*
* PART I. FEATURE ENGINEERING ---> VARIABLES FOR THE EXERCISE
*
********************************************************************************************************-
********************************************************************************************************-


use "$data_in/food_dataset_pers_extended_09-11-13.dta", clear


**********************************************-
* FOOD
**********************************************-

* create veggies
	order gr_*
	egen veggies1 = rowtotal(other_vegetable -tomato  )
	egen veggies2= rowtotal(sprouts- snow_peas)
	gen veggies=veggies1+veggies2
	egen fruits1= rowtotal( grapefruit - orange )
	egen fruits2= rowtotal( apple - strawberries )
	gen fruits=fruits1+fruits2
	drop fruits1 fruits2 veggies1 veggies2

	gen veggies_n_fruits=veggies+fruits
label var veggies_n_fruits "number of vegetables and fruits
	replace veggies_n_fruits=11 if  veggies_n_fruits>11
	
	egen veggies1 = rowtotal(gr_other_vegetable -gr_tomato  )
	egen veggies2= rowtotal(gr_sprouts- gr_snow_peas)
	gen veggies_gr=veggies1+veggies2
	egen fruits1= rowtotal( gr_grapefruit - gr_orange )
	egen fruits2= rowtotal( gr_apple - gr_strawberries )
	gen fruits_gr=fruits1+fruits2
	drop fruits1 fruits2 veggies1 veggies2

gen veggies_n_fruits_gr=veggies_gr+fruits_gr

* coffee
gen coffee_espressounit= ((dr1tcaff + dr2tcaff)/2)/60
label var coffee_espressounit "2-day avg caffeine, in espresso units (60mg), dr1tcaff dr2tcaff"
replace coffee_espressounit=15 if coffee_espressounit>12 & coffee_espressounit<.


* alternative food categories
gen beef_all=beef+beef_frozen_meal +beef_soup +beef_lean +beef_with_starch + beef_with_starch_vegetable +beef_with_vegetable
gen red_meat_all=beef+beef_frozen_meal +beef_soup +beef_lean +beef_with_starch + beef_with_starch_vegetable +beef_with_vegetable+pork +pork_lean +pork_soup +pork_with_starch +pork_with_starch_vegetable +pork_with_vegetable +lamb +lamb_lean
gen nuts= almonds + cashews + nuts_other

**********************************************-
* SOCIO-ECON, GENDER, AGE
**********************************************-

	
* gender, age
rename riagendr gender
label var gender "gender"
label define mf 1 "male" 2 "female"
label value gender mf 

rename ridageyr age
keep if age>=18
label var age "age, 80 topcoded"
gen age2=age^2

egen age_cat= cut(age), at(18, 30,40,50,60,70, 81)
label var age_cat "age groups"
label define agc 18 "aged 18-29" 30 "aged 30-39" 40 "aged 40-49"  50 "aged 50-59"  60 "aged 60-69"  70 "aged 70+" 
label value age_cat agc	


* socio-economic
rename ridreth1 race
gen married = dmdmartl==1 | dmdmartl==6

rename dmdeduc2 edu
replace edu=. if edu>5

rename dmdhhsiz hh_size
rename indhhin2  hh_income

gen hh_income_usd=hh_income
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

gen hh_income_percap=hh_income_usd /hh_size
label var hh_income_percap "Household income mid-point / size"
* prep for regressions
gen ln_hh_income_percap =ln(hh_income_percap )
label var ln_hh_income_percap "Log Household income mid-point / size"


egen income_cat= cut(hh_income_percap ), at(1000,10000,30000, 150000)
label var income_cat "Income (per-capita) categories"
label define inc 1000 "low" 10000 "mid" 30000 "high"
label value income_cat inc	

rename ocd241 work_occupation 
label var work_occupation " Occupation group code: current job"
rename ocd150 work_type
rename ocq180  work_hs
replace work_hs=. if work_hs>150
replace work_hs=100 if work_hs>100 & work_hs<.


**********************************************-
* HEALTH OUTCOMES
**********************************************-
*sport
gen sport_days=paq655
replace sport_days =0 if sport_days ==.
gen walk_cycle_days=paq640
replace walk_cycle_days =0 if walk_cycle_days ==.

*smoke
gen smoker= smq040 ==1 | smq040 ==2

*sleep
gen sleep_hs = sld010h
label var sleep_hs "How much sleep do you get (hours)? sld010h"

	gen bp_systolic = bpxsy1 
	gen bp_diastolic = bpxdi1
	gen total_cholesterol= lbdhdd 
	gen hdl = lbxtc
	gen weight = bmxwt 
	gen height = bmxht

gen ldl= lbxtc- lbdhdd
replace ldl=60 if ldl<60
replace ldl=250 if ldl>250 & ldl<.
label var ldl "LDL Cholesterol, Total-HDL, lbxtc lbdhdd "


* BMI and normal weight variables
	gen bmi = 10000*weight/(height*height)
	gen normal_weight = 1 if bmi < 25
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
gen blood_pressure =(bp_systolic + bp_diastolic)

* heart health
gen heart_risk=ldl+blood_pressure 
label var heart_risk "heart risk - sum of blood pressure and cholesterol"






**********************************************-
* CLEAN-UP
**********************************************-

drop sld010h bpxsy1 bpxdi1 lbxtc lbdhdd bmxwt bmxht dmdmartl

**********************************************-
* SAVE WORK FILE
**********************************************-

save "$data_out/food-health.dta", replace


**********************************************************************-
**********************************************************************-
*
* PART II: ANALYSIS 
*
**********************************************************************-
**********************************************************************-

use "$data_out/food-health.dta", clear

*************************************
* DESCRIPTIVES
*************************************



* T1
tabout age_cat gender income_cat ///
using "$output/food_descrtable1.tex", replace ///
style(tex) font(bold) sum npos(tufte) f(1) ///
c(mean veggies_n_fruits median veggies_n_fruits  p90 veggies_n_fruits mean blood_pressure ) /// 
h2c(3 1) ltrim(1) h2(veggies_and_fruits(n) blood_pressure(sum)))  ///
* h2( & \multicolumn{3}{c}{veggies and fruits(n) } & \multicolumn{1}{c}{blood pressure(sum)} \\ )  ///
clab(mean median top10_% mean ) 
*title(Table 2: Variation by country, firm owner and size) 
*fn(Observation shares in brackets. Considering first buyer only. Gabor_2019-03-27)




*************************************
* REGRESSIONS 
*
* FOOD ON HEALTH
*************************************



reg blood_pressure c.age##gender c.age2##gender days i.year
predict blood_pressure_rel, resid
label var blood_pressure_rel "blood_pressure , relative - compared to age-gender peers"

reg veggies_n_fruits  c.age##gender c.age2##gender days i.year
predict veggies_n_fruits_rel, resid
label var veggies_n_fruits_avg_rel "veggies_n_fruits , relative - compared to age-gender peers"


***** need to adjust it for a subgroup, and blood pressure ---> TODO
*** create 3 grapsh, x-y, z-y, z-x
lpoly blood_pressure veggies_n_fruits , noscat ///
 xtitle("Number of veggies/fruits a day") ///
 ytitle("Blood pressure (sum of two measures)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/bp_veggies.png", as(png) replace

lpoly blood_pressure veggies_n_fruits if age>=30 & age<=60, noscat ///
 xtitle("Number of veggies/fruits a day") ///
 ytitle("Blood pressure (sum of two measures)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/bp_veggies_30_60.png", as(png) replace


lpoly veggies_n_fruits  hh_income if age>=30 & age<=60, noscat ///
 xtitle("Income group category") ///
 ytitle("Number of veggies/fruits a day") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/income_veggies_lpoly_30_60.png", as(png) replace

lpoly blood_pressure hh_income if age>=30 & age<=60, noscat ///
 xtitle("Blood pressure") ///
 ytitle("Income group category") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/blood_income_lpoly_30_60.png", as(png) replace


 

* blood  pressure measure
reg blood_pressure  veggies_n_fruits days i.year  ,r 
outreg2 using "$output/T_bp_veggies.tex", keep(veggies_n_fruits ) excel  tex bdec(2) addtext(Age-gender, NO, Socio-econ, No, Beef, No, Activity, No, Meds, No) nocons replace
reg blood_pressure   c.age##gender c.age2##gender days i.year  veggies_n_fruits,r 
outreg2 using "$output/T_bp_veggies.tex", keep(veggies_n_fruits ) excel  tex bdec(2) addtext(Age-gender, YES, Socio-econ, No, Beef, No, Activity, No, Meds, No) nocons append
reg blood_pressure c.age##gender c.age2##gender days i.year  i.edu i.race i.hh_income i.hh_size veggies_n_fruits 
outreg2 using "$output/T_bp_veggies.tex", keep(veggies_n_fruits ) excel  tex bdec(2) addtext(Age-gender, YES, Socio-econ, YES, Beef, No, Activity, No, Meds, No) nocons append
reg blood_pressure c.age##gender c.age2##gender days i.year  i.edu i.race i.hh_income i.hh_size veggies_n_fruits beef_all smoker sport_days walk_cycle_days
outreg2 using "$output/T_bp_veggies.tex", keep(veggies_n_fruits ) excel  tex bdec(2) addtext(Age-gender, YES, Socio-econ, YES, Beef, YES, Activity, YES, Meds, No) nocons append
reg blood_pressure  c.age##gender c.age2##gender days i.year  i.edu i.race i.hh_income i.hh_size veggies_n_fruits beef_all smoker sport_days walk_cycle_days med_*
outreg2 using "$output/T_bp_veggies.tex", keep(veggies_n_fruits ) excel  tex bdec(2) addtext(Age-gender, YES, Socio-econ, YES, Beef, YES, Activity, YES, Meds, YES) nocons append



vége

*************************************
* COFFEE ON SLEEP	
* total caffeine translated into espresso, at 60mg/cup 
*************************************

reg sleep_hs c.age##gender c.age2##gender
predict sleep_rel, resid
label var sleep_rel "sleep in hs, relative - compared to age-gender peers"
	
	
lpoly sleep_hs	coffee_espressounit , noscat
lpoly sleep_rel coffee_espressounit , noscat
