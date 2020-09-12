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
* Chapter 13
* CH13A Predicting used car value with linear regression
* using the used-cars dataset
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
global work  	"ch13-used-cars-reg"

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


* Explorative data analysis; results are not in the textbook
* histograms not in the textbook
hist price, percent color(navy*0.8) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))

hist lnprice, percent color(navy*0.8) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 
table condition, c(freq mean price)
table drive, c(freq mean price)
table dealer, c(freq mean price)
table area, c(freq mean price)

* Prediction with regressions
 
* models	
local M1 age agesq 
local M2 age agesq odometer 
local M3 age agesq odometer odometersq LE                     cond_excellent cond_good dealer 
local M4 age agesq odometer odometersq LE XLE SE cond_likenew cond_excellent cond_good dealer cylind6 
local M5 age agesq odometer odometersq LE XLE SE cond_likenew cond_excellent cond_good dealer cylind6  c.age##c.odometer c.age##c.odometersq i.LE##c.age i.XLE##c.age i.SE##c.age i.cond_likenew##c.age i.cond_excellent##c.age i.cond_good##c.age i.dealer##c.age i.cylind6##c.age 
forvalue i=1/5 {
	global M`i' `M`i''
}
/* note. we define them as local so Stata can use them in the loops.
but we also define them as global so we remember them even if we stop the code */

* regression table
* Table 13.2
cap rm "$output/ch13-table-2-reg-Stata.tex"
cap rm "$output/ch13-table-2-reg-Stata.txt"

* Models 1 to 4
forval i=1/4 {
quietly reg price `M`i'', robust
 quietly outreg2 using "$output/ch13-table-2-reg-Stata.tex", nose bdec(2) noaster ctitle(Model `v') ///
  tex(frag) append 
}

* point and interval prediction for M1 & M3

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

* M1
reg price $M1
 predict pM1 if _n==`nplus1'
 predict pM1_spe if _n==`nplus1', stdf
 gen pM1_80PIlow  = pM1 - 1.28*pM1_spe
 gen pM1_80PIhigh = pM1 + 1.28*pM1_spe
 gen pM1_95PIlow  = pM1 - 1.96*pM1_spe
 gen pM1_95PIhigh = pM1 + 1.96*pM1_spe
 
* M3
reg price $M3
 predict pM3 if _n==`nplus1'
 predict pM3_spe if _n==`nplus1', stdf
 gen pM3_80PIlow  = pM3 - 1.28*pM3_spe
 gen pM3_80PIhigh = pM3 + 1.28*pM3_spe
 gen pM3_95PIlow  = pM3 - 1.96*pM3_spe
 gen pM3_95PIhigh = pM3 + 1.96*pM3_spe

* Table 13.3
* results would differ because of differences in the degrees-of-freedom corrections
tabstat pM1*, c(s)
tabstat pM3*, c(s)

* Model selection

* Criteria using all original data
* Models 1 to 5
* Table 13.4
forval i=1/5 {
	quietly reg price `M`i'', robust
	local r2_M`i' = e(r2)
	local rmse_M`i'= e(rmse) 
	qui estat ic
	matrix out=r(S)
	local BIC_M`i'=out[1,6]
	dis "r2_M`i'="`r2_M`i'' "   rmse_M`i'="`rmse_M`i'' "   BIC_M`i'="`BIC_M`i''
}

* k-fold cros-validation
set seed 1505
local k=4
qui forval v=1/5 {
	crossfold reg price `M`v'', robust k(`k') loud
	matrix list r(est)
	matrix rmse_M`v'=r(est)
}
* from here you can copy and past the RMSE values into a spreasheet
* square them to get MSE, and take their averages for each model
* to get average MSE, then square root for average cross-validated RMSE

* instead, here we do it in code, using some matrix algebra
matrix rmse_folds= [rmse_M1, rmse_M2, rmse_M3, rmse_M4, rmse_M5]
matrix mse_folds = J(rowsof(rmse_folds),colsof(rmse_folds),0)
forvalue i=1/4 {
	forvalue j=1/5 {
		matrix mse_folds[`i',`j'] = rmse_folds[`i',`j']^2
	}
}
mat mse_avg = J(1,rowsof(mse_folds),1)*mse_folds /4
mat rmse_avg = J(1,5,0)
forvalue j=1/5 {
	matrix rmse_avg[1,`j'] = sqrt(mse_avg[1,`j'])
}

* Table 13.5
* results would differ because of randomization differences 
* and the differences in degrees-of-freedom correction in calculating rmse
mat lis rmse_folds
mat lis rmse_avg

