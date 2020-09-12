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
* Chapter 14
* CH14A Predicting uesd car value: log proces
* using the used-car dataset
* version 0.9 2020-09-12
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/GitHub/da_case_studies"


* STEP 2: * Directory for data
* Option 1: run directory-setting do file
do set-data-directory.do 
							/* this is a one-line do file that should sit in 
							the working directory you have just set up
							this do file has a global definition of your working directory
							more details: gabors-data-analysis.com/howto-stata/   */

* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"


global data_in  "$data_dir/used-cars/clean"
global work  	"ch14-used-cars-log"

cap mkdir 		"$work/output"
global output 	"$work/output"


use "$data_in/used-cars_2cities_prep.dta", clear


* Sample design

drop if Hybrid==1
drop Hybrid

tab fuel
keep if fuel=="gas"

tab condition
drop if condition=="new"
drop if condition=="fair"

* drop very small prices, likely error
drop if price<500
drop if price>25000
drop if odometer>100
drop if price<1000 & (condition=="like new" | age<8)
drop if price==.

tab transmission
drop if transmission =="manual"
drop pricestr

tab type
drop if type=="truck"

tab area
gen chicago=area=="chicago"
keep if chicago==1


* Some feature engineering
* condition
gen cond_excellent = condition=="excellent"
gen cond_good = condition=="good"
gen cond_likenew = condition=="like new" 

* cylinders
gen cylind6 = cylinders=="6 cylinders"

* price: quadratic
gen agesq=age^2
gen agecu=age^3
gen odometersq=odometer^2

save "$work/usedcars_work.dta", replace




* lowess: price, lnprice
lowess price age, mc(navy*0.6) lineopts( lc(green*0.8) lw(vthick)) ///
 ylab(, grid) xlab(, grid) ytitle("Price (US dollars)") xtitle("Age (years)") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch14-figure-2a-p-age-lowess-Stata.png",replace
 
lowess lnprice age, mc(navy*0.6) lineopts( lc(green*0.8) lw(vthick)) ///
 ylab(, grid) xlab(, grid) ytitle("ln(price, US dollars)") xtitle("Age (years)") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch14-figure-2b-lnp-age-lowess-Stata.png",replace



* PREDICTION
* repeat what M3 in ch 13, no in logs
* generate  new observation with features our car

global M3 age agesq odometer odometersq LE cond_excellent cond_good dealer 

* generate  new observation with features our car
local nplus1=_N+1
set obs `nplus1'
replace age=10 if _n==`nplus1'
replace agesq=age^2 if _n==`nplus1'
replace odometer=12 if _n==`nplus1'
replace odometersq=odometer^2 if _n==`nplus1'
replace LE=1 if _n==`nplus1'
replace XLE=0 if _n==`nplus1'
replace SE=0 if _n==`nplus1'
replace cond_likenew=0 if _n==`nplus1'
replace cond_excellent=1 if _n==`nplus1'
replace cond_good=0 if _n==`nplus1'
replace cylind6=0 if _n==`nplus1'
replace dealer=0 if _n==`nplus1'
lis if _n==`nplus1'

 
* ln y
* M3
reg lnprice $M3
 predict lnpM3 if _n==`nplus1'
 gen lnpM3_sig = e(rmse) if _n==`nplus1'
 predict lnpM3_spe if _n==`nplus1', stdf
 gen lnpM3_80PIlow  = lnpM3 - 1.28*lnpM3_spe
 gen lnpM3_80PIhigh = lnpM3 + 1.28*lnpM3_spe
* log correction
gen pM3_log = exp(lnpM3) * exp(lnpM3_sig^2/2)
 gen pM3_log_80PIlow   = exp(lnpM3_80PIlow ) * exp(lnpM3_sig^2/2)
 gen pM3_log_80PIhigh  = exp(lnpM3_80PIhigh) * exp(lnpM3_sig^2/2)

* level y
* M3
reg price $M3
 predict pM3_level if _n==`nplus1'
 predict pM3_level_spe if _n==`nplus1', stdf
 gen pM3_level_80PIlow  = pM3_level - 1.28*pM3_level_spe
 gen pM3_level_80PIhigh = pM3_level + 1.28*pM3_level_spe

 
* Table 14.1
* numbers are slightly different from textbook
* due to differences in degrees-of-freedom corrections in how
* sigma is calculated in R vs Stata
tabstat lnpM3*, c(s)
tabstat pM3_log*, c(s)
tabstat pM3_level*, c(s)
 
 
 
