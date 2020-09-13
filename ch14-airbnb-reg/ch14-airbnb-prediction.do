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
* CH014 Predicting Airbnb apartment prices: selecting a regression model
* using the airbnb dataset
* version 0.9 2020-09-12
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/Github/da_case_studies"

* STEP 2: * Directory for data
* no need for it here: prepare file created workfile in work directory

global work  	"ch14-airbnb-reg"

cap mkdir 		"$work/output"
global output 	"$work/output"

clear
set matsize 1000


use "$work/airbnb_hackney_workfile.dta" , replace
count
keep if price<1000
count

************************
* holdout set, work set
set seed 6411554
* create random number
gen temprand = uniform()
* first 20% to holdout set, remaining 80% to work set
centile temprand, centile(20)
gen holdout = temprand<r(c_1)
gen workset = temprand>=r(c_1)
tab holdout workset, mis cell

/* 
Note: The holdout and work sets are different in different software due to
differences in random number generation. Therefore all results are slightly 
different in different sofware, too.
*/


******************
* models
local M1 n_accommodates 
local M2 `M1' n_beds n_days_since i.f_property_type i.f_room_type i.f_bed_type 
local M3 `M2' i.f_bathroom i.f_cancellation_policy n_review_score d_missing_review_score i.f_number_of_rev 
local M4 `M3' n_accommodates2 n_days_since2 n_days_since3
local M5 `M4' i.f_room_type##i.f_property_type i.f_number_of_rev##i.f_property_type 
local M6 `M5' d_aircond##i.f_property_type d_petsallowed##i.f_property_type 
local M7 `M6' d_hourchec d_doorman d_freeparkingonprem d_internet d_paidparking ///
 d_shampoo d_wheelcha d_doormane d_freeparkingonstr d_iron d_smartlock d_wireless ///
 d_breakfast d_dryer d_gym d_keypad d_petslive d_smokedet ///
 d_buzzerwi d_elevator d_hairdryer d_kitchen d_pool d_smokinga ///
 d_cabletv d_essentials d_hangers d_laptopf d_privatee d_suitable d_carbonmo ///
 d_familyki d_heating d_lockonbe d_privatel d_tv ///
 d_cats d_fireexti d_hottub d_lockbox d_safetycard d_washer ///
 d_dogs d_firstaid d_indoorfi d_otherpets d_selfcheck d_washerdr
local M8 `M6' i.f_pr##i.d_hourchec i.f_pr##i.d_doorman i.f_pr##i.d_freeparkingonprem i.f_pr##i.d_internet i.f_pr##i.d_paidparking ///
 i.f_pr##i.d_shampoo i.f_pr##i.d_wheelcha i.f_pr##i.d_doormane i.f_pr##i.d_freeparkingonstr i.f_pr##i.d_iron i.f_pr##i.d_smartlock i.f_pr##i.d_wireless ///
 i.f_pr##i.d_breakfast i.f_pr##i.d_dryer i.f_pr##i.d_gym i.f_pr##i.d_keypad i.f_pr##i.d_petslive i.f_pr##i.d_smokedet ///
 i.f_pr##i.d_buzzerwi i.f_pr##i.d_elevator i.f_pr##i.d_hairdryer i.f_pr##i.d_kitchen i.f_pr##i.d_pool i.f_pr##i.d_smokinga ///
 i.f_pr##i.d_cabletv i.f_pr##i.d_essentials i.f_pr##i.d_hangers i.f_pr##i.d_laptopf i.f_pr##i.d_privatee i.f_pr##i.d_suitable i.f_pr##i.d_carbonmo ///
 i.f_pr##i.d_familyki i.f_pr##i.d_heating i.f_pr##i.d_lockonbe i.f_pr##i.d_privatel i.f_pr##i.d_tv ///
 i.f_pr##i.d_cats i.f_pr##i.d_fireexti i.f_pr##i.d_hottub i.f_pr##i.d_lockbox i.f_pr##i.d_safetycard i.f_pr##i.d_washer ///
 i.f_pr##i.d_dogs i.f_pr##i.d_firstaid i.f_pr##i.d_indoorfi i.f_pr##i.d_otherpets i.f_pr##i.d_selfcheck i.f_pr##i.d_washerdr ///
 i.f_bed##i.d_hourchec i.f_bed##i.d_doorman i.f_bed##i.d_freeparkingonprem i.f_bed##i.d_internet i.f_bed##i.d_paidparking ///
 i.f_bed##i.d_shampoo i.f_bed##i.d_wheelcha i.f_bed##i.d_doormane i.f_bed##i.d_freeparkingonstr i.f_bed##i.d_iron i.f_bed##i.d_smartlock i.f_bed##i.d_wireless ///
 i.f_bed##i.d_breakfast i.f_bed##i.d_dryer i.f_bed##i.d_gym i.f_bed##i.d_keypad i.f_bed##i.d_petslive i.f_bed##i.d_smokedet ///
 i.f_bed##i.d_buzzerwi i.f_bed##i.d_elevator i.f_bed##i.d_hairdryer i.f_bed##i.d_kitchen i.f_bed##i.d_pool i.f_bed##i.d_smokinga ///
 i.f_bed##i.d_cabletv i.f_bed##i.d_essentials i.f_bed##i.d_hangers i.f_bed##i.d_laptopf i.f_bed##i.d_privatee i.f_bed##i.d_suitable i.f_bed##i.d_carbonmo ///
 i.f_bed##i.d_familyki i.f_bed##i.d_heating i.f_bed##i.d_lockonbe i.f_bed##i.d_privatel i.f_bed##i.d_tv ///
 i.f_bed##i.d_cats i.f_bed##i.d_fireexti i.f_bed##i.d_hottub i.f_bed##i.d_lockbox i.f_bed##i.d_safetycard i.f_bed##i.d_washer ///
 i.f_bed##i.d_dogs i.f_bed##i.d_firstaid i.f_bed##i.d_indoorfi i.f_bed##i.d_otherpets i.f_bed##i.d_selfcheck i.f_bed##i.d_washerdr
*/
 
	
forvalue i=1/8 {
	global M`i' `M`i''
}
/* note. we define them as local so Stata can use them in the loops.
but we also define them as global so we remember it even if we stop the code */


*************************************
* R-SQUARED, BIC on entire work set
preserve
keep if workset==1
count
forval i=1/8 {
	quietly reg price `M`i'' 
	local r2_M`i' = e(r2)
	local rmse_M`i'= e(rmse) 
	qui estat ic
	matrix out=r(S)
	local BIC_M`i'=out[1,6]
	dis "r2_M`i'="`r2_M`i'' "  BIC_M`i'="`BIC_M`i''
}
restore
count


*************************************
* k-fold cros-validation
local k=5
global k=5
set seed 76112181

preserve
keep if workset==1

* create k folds manually
cap drop temprand
gen temprand = uniform()
gen testfold = 0
sort temprand
forvalue i=1/$k {
	replace testfold = `i' if testfold==0 & _n<= (`i'/$k)*_N
}
tab testfold,mis
* 8 regressions
forvalue i=1/8 {
	local msetest_M`i'=0
	local msetrain_M`i'=0
	* k folds
	qui forvalue j=1/$k {
		regress price `M`i'' if testfold!=`j'  /* training set: without the test set */
		predict phat
		gen e2 = (price-phat)^2 
		* test set mse 
		sum e2 if testfold==`j'
		local msetest`j' = r(mean)
		local msetest_M`i' = `msetest_M`i'' + `msetest`j''
		* training set mse (for illustration purposes)
		sum e2 if testfold!=`j'
		local msetrain`j' = r(mean)
		local msetrain_M`i' = `msetrain_M`i'' + `msetrain`j''
		cap drop e2*
		cap drop phat*
	}
	local rmsetest_M`i' = sqrt(`msetest_M`i''/$k)
	local rmsetrain_M`i' = sqrt(`msetrain_M`i''/$k)
	
dis "'Model M`i'" "  training avg RMSE = `rmsetrain_M`i'' " "  test avg RMSE = `rmsetest_M`i'' "
}
* lasso
local msetest_lasso=0
local msetrain_lasso=0
	* k folds
forvalue j=1/$k {
		cvlasso price $M8 if testfold!=`j', alpha(1) lopt nfolds(5)
		predict phat
		gen e2 = (price-phat)^2 
		* test set mse 
		sum e2 if testfold==`j'
		local msetest`j' = r(mean)
		local msetest_lasso = `msetest_lasso' + `msetest`j''
		* training set mse (for illustration purposes)
		sum e2 if testfold!=`j'
		local msetrain`j' = r(mean)
		local msetrain_lasso = `msetrain_lasso' + `msetrain`j''
		cap drop e2*
		cap drop phat*
	}
	local rmsetest_lasso = sqrt(`msetest_lasso'/$k)
	local rmsetrain_lasso = sqrt(`msetrain_lasso'/$k)
	
dis "'Model lasso" "  training avg RMSE = `rmsetrain_lasso' " "  test avg RMSE = `rmsetest_lasso' "
}

restore
*/

*******************************
*** DIAGNOSTICS

qui reg price $M7 if workset==1
predict phat if holdout==1
predict spe if holdout==1, stdf
gen pi80lo = phat - 1.28*spe
gen pi80hi = phat + 1.28*spe

* average predicted price in the holdout set
sum phat if holdout==1
* 80% PI around average predicted price in holdout set
*  (little trick: there is no observation with the exactly average predicted price
*   so we look at observations within a narrow interval)
sum pi80lo pi80hi if holdout==1 & phat>=r(mean)-0.1 & phat<=r(mean)+0.1

* Figure 14.8a
* yhat-y plot
scatter price phat if holdout==1 & price<=350, ms(o) mc(navy*0.6) ///
 || line phat phat if holdout==1 , lc(green*0.8) lw(thick) lp(dash) ///
 xlab(0(50)350, grid) ylab(0(50)350, grid) ///
 xtitle("Predicted price, (US dollars)") ytitle("Price, (US dollars)") ///
 legend(off) 
graph export "$output/ch14-figure-8a-yhat-y-Stata.png", replace

* point and interval predictions by size
qui reg price $M7 if workset==1

* Figure 14.8b
preserve
 keep if holdout==1
 collapse phat pi80lo pi80hi, by(n_accommodates)
 
 graph twoway (bar phat n_accommodates, col(navy*0.8) lcol(white) lw(vthick) ) ///
  (rcap pi80lo pi80hi n_accommodates, lc(green) lw(thick) ) , ///
  legend(off) ///
  xla(1(1)7, grid) yla(0(50)200, grid) ///
  xtitle("Number of guests accommodated") ytitle("Predicted price (US dollars)") 
graph export "$output/ch14-figure-8b-yhat-bars-byaccom-Stata.png", replace
 
restore 
 

