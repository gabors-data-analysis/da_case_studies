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
* version 1.1 2026-09-04
*
* STATA VERSION: This code is written for Stata 18
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
 

* STEP 2: * Directory for data
* no need for it here: prepare file created workfile in work directory

global work  	"ch14-airbnb-reg"

capture mkdir 		"${work}/output"
global output 	"${work}/output"

clear
set matsize 1000
********************
* !!! make sure you have run ch14_airbnb_prepare.do beforehand !!! 
********************
use "${work}/airbnb_hackney_workfile.dta" , replace
count
keep if price<1000
count

************************
* holdout set, work set
set seed 6411554
* create random number
generate temprand = uniform()
* first 20% to holdout set, remaining 80% to work set
centile temprand, centile(20)
generate holdout = temprand<r(c_1)
generate workset = temprand>=r(c_1)
tabulate holdout workset, mis cell

/* 
Note: The holdout and work sets are different in different software due to
differences in random number generation. Therefore all results are slightly 
different in different sofware, too.
*/


******************
* models
local M1 n_accommodates 
local M2 `M1' n_beds n_days_since i.f_property_type i.f_room_type i.f_bed_type 
local M3 `M2' i.f_bathroom i.f_cancellation_policy n_review_scores_rating d_missing_review_scores_rating i.f_number_of_reviews 
local M4 `M3' n_accommodates2 n_days_since2 n_days_since3
local M5 `M4' i.f_room_type##i.f_property_type i.f_number_of_reviews##i.f_property_type 
local M6 `M5' d_airconditioning##i.f_property_type d_petsallowed##i.f_property_type 
local M7 `M6' d_hourcheckin d_doorman d_freeparkingonpremises d_internet d_paidparkingoffpremises ///
 d_shampoo d_wheelchairaccessible d_doormanentry d_freeparkingonstreet d_iron d_smartlock d_wirelessinternet ///
 d_breakfast d_dryer d_gym d_keypad d_petsliveonthisproperty d_smokedetector ///
 d_buzzerwirelessintercom d_elevatorinbuilding d_hairdryer d_kitchen d_pool d_smokingallowed ///
 d_cabletv d_essentials d_hangers d_laptopfriendlyworkspace d_privateentrance d_suitableforevents d_carbonmonoxidedetector ///
 d_familykidfriendly d_heating d_lockonbedroomdoor d_privatelivingroom d_tv ///
 d_cats d_fireextinguisher d_hottub d_lockbox d_safetycard d_washer ///
 d_dogs d_firstaidkit d_indoorfireplace d_otherpets d_selfcheckin d_washerdryer
local M8 `M6' i.f_property_type##i.d_hourcheckin i.f_property_type##i.d_doorman i.f_property_type##i.d_freeparkingonpremises i.f_property_type##i.d_internet i.f_property_type##i.d_paidparkingoffpremises ///
 i.f_property_type##i.d_shampoo i.f_property_type##i.d_wheelchairaccessible i.f_property_type##i.d_doormanentry i.f_property_type##i.d_freeparkingonstreet i.f_property_type##i.d_iron i.f_property_type##i.d_smartlock i.f_property_type##i.d_wirelessinternet ///
 i.f_property_type##i.d_breakfast i.f_property_type##i.d_dryer i.f_property_type##i.d_gym i.f_property_type##i.d_keypad i.f_property_type##i.d_petsliveonthisproperty i.f_property_type##i.d_smokedetector ///
 i.f_property_type##i.d_buzzerwirelessintercom i.f_property_type##i.d_elevatorinbuilding i.f_property_type##i.d_hairdryer i.f_property_type##i.d_kitchen i.f_property_type##i.d_pool i.f_property_type##i.d_smokingallowed ///
 i.f_property_type##i.d_cabletv i.f_property_type##i.d_essentials i.f_property_type##i.d_hangers i.f_property_type##i.d_laptopfriendlyworkspace i.f_property_type##i.d_privateentrance i.f_property_type##i.d_suitableforevents i.f_property_type##i.d_carbonmonoxidedetector ///
 i.f_property_type##i.d_familykidfriendly i.f_property_type##i.d_heating i.f_property_type##i.d_lockonbedroomdoor i.f_property_type##i.d_privatelivingroom i.f_property_type##i.d_tv ///
 i.f_property_type##i.d_cats i.f_property_type##i.d_fireextinguisher i.f_property_type##i.d_hottub i.f_property_type##i.d_lockbox i.f_property_type##i.d_safetycard i.f_property_type##i.d_washer ///
 i.f_property_type##i.d_dogs i.f_property_type##i.d_firstaidkit i.f_property_type##i.d_indoorfireplace i.f_property_type##i.d_otherpets i.f_property_type##i.d_selfcheckin i.f_property_type##i.d_washerdryer ///
 i.f_bed_type##i.d_hourcheckin i.f_bed_type##i.d_doorman i.f_bed_type##i.d_freeparkingonpremises i.f_bed_type##i.d_internet i.f_bed_type##i.d_paidparkingoffpremises ///
 i.f_bed_type##i.d_shampoo i.f_bed_type##i.d_wheelchairaccessible i.f_bed_type##i.d_doormanentry i.f_bed_type##i.d_freeparkingonstreet i.f_bed_type##i.d_iron i.f_bed_type##i.d_smartlock i.f_bed_type##i.d_wirelessinternet ///
 i.f_bed_type##i.d_breakfast i.f_bed_type##i.d_dryer i.f_bed_type##i.d_gym i.f_bed_type##i.d_keypad i.f_bed_type##i.d_petsliveonthisproperty i.f_bed_type##i.d_smokedetector ///
 i.f_bed_type##i.d_buzzerwirelessintercom i.f_bed_type##i.d_elevatorinbuilding i.f_bed_type##i.d_hairdryer i.f_bed_type##i.d_kitchen i.f_bed_type##i.d_pool i.f_bed_type##i.d_smokingallowed ///
 i.f_bed_type##i.d_cabletv i.f_bed_type##i.d_essentials i.f_bed_type##i.d_hangers i.f_bed_type##i.d_laptopfriendlyworkspace i.f_bed_type##i.d_privateentrance i.f_bed_type##i.d_suitableforevents i.f_bed_type##i.d_carbonmonoxidedetector ///
 i.f_bed_type##i.d_familykidfriendly i.f_bed_type##i.d_heating i.f_bed_type##i.d_lockonbedroomdoor i.f_bed_type##i.d_privatelivingroom i.f_bed_type##i.d_tv ///
 i.f_bed_type##i.d_cats i.f_bed_type##i.d_fireextinguisher i.f_bed_type##i.d_hottub i.f_bed_type##i.d_lockbox i.f_bed_type##i.d_safetycard i.f_bed_type##i.d_washer ///
 i.f_bed_type##i.d_dogs i.f_bed_type##i.d_firstaidkit i.f_bed_type##i.d_indoorfireplace i.f_bed_type##i.d_otherpets i.f_bed_type##i.d_selfcheckin i.f_bed_type##i.d_washerdryer
*/
 
	
forvalues i=1/8 {
	global M`i' `M`i''
}
/* note. we define them as local so Stata can use them in the loops.
but we also define them as global so we remember it even if we stop the code */


*************************************
* R-SQUARED, BIC on entire work set
preserve
keep if workset==1
count
forvalues i=1/8 {
	quietly reg price `M`i'' 
	local r2_M`i' = e(r2)
	local rmse_M`i'= e(rmse) 
	quietly estat ic
	matrix out=r(S)
	local BIC_M`i'=out[1,6]
	display "r2_M`i'="`r2_M`i'' "  BIC_M`i'="`BIC_M`i''
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
capture drop temprand
generate temprand = uniform()
generate testfold = 0
sort temprand
forvalues i=1/$k {
	replace testfold = `i' if testfold==0 & _n<= (`i'/$k)*_N
}
tabulate testfold,mis
* 8 regressions
forvalues i=1/8 {
	local msetest_M`i'=0
	local msetrain_M`i'=0
	* k folds
	quietly forvalue j=1/$k {
		regress price `M`i'' if testfold!=`j'  /* training set: without the test set */
		predict phat
		generate e2 = (price-phat)^2 
		* test set mse 
		summarize e2 if testfold==`j'
		local msetest`j' = r(mean)
		local msetest_M`i' = `msetest_M`i'' + `msetest`j''
		* training set mse (for illustration purposes)
		summarize e2 if testfold!=`j'
		local msetrain`j' = r(mean)
		local msetrain_M`i' = `msetrain_M`i'' + `msetrain`j''
		capture drop e2*
		capture drop phat*
	}
	local rmsetest_M`i' = sqrt(`msetest_M`i''/$k)
	local rmsetrain_M`i' = sqrt(`msetrain_M`i''/$k)
	
display "'Model M`i'" "  training avg RMSE = `rmsetrain_M`i'' " "  test avg RMSE = `rmsetest_M`i'' "
}
* lasso
local msetest_lasso=0
local msetrain_lasso=0
	* k folds
forvalues j=1/$k {
		cvlasso price $M8 if testfold!=`j', alpha(1) lopt nfolds(5)
		predict phat
		generate e2 = (price-phat)^2 
		* test set mse 
		summarize e2 if testfold==`j'
		local msetest`j' = r(mean)
		local msetest_lasso = `msetest_lasso' + `msetest`j''
		* training set mse (for illustration purposes)
		summarize e2 if testfold!=`j'
		local msetrain`j' = r(mean)
		local msetrain_lasso = `msetrain_lasso' + `msetrain`j''
		capture drop e2*
		capture drop phat*
	}
	local rmsetest_lasso = sqrt(`msetest_lasso'/$k)
	local rmsetrain_lasso = sqrt(`msetrain_lasso'/$k)
	
display "'Model lasso" "  training avg RMSE = `rmsetrain_lasso' " "  test avg RMSE = `rmsetest_lasso' "
}

restore
*/

*******************************
*** DIAGNOSTICS

quietly reg price $M7 if workset==1
predict phat if holdout==1
predict spe if holdout==1, stdf
generate pi80lo = phat - 1.28*spe
generate pi80hi = phat + 1.28*spe

* average predicted price in the holdout set
summarize phat if holdout==1
* 80% PI around average predicted price in holdout set
*  (little trick: there is no observation with the exactly average predicted price
*   so we look at observations within a narrow interval)
summarize pi80lo pi80hi if holdout==1 & phat>=r(mean)-0.1 & phat<=r(mean)+0.1

* Figure 14.8a
* yhat-y plot
scatter price phat if holdout==1 & price<=350, ms(o) mc(navy*0.6) ///
 || line phat phat if holdout==1 , lc(green*0.8) lw(thick) lp(dash) ///
 xlab(0(50)350, grid) ylab(0(50)350, grid) ///
 xtitle("Predicted price, (US dollars)") ytitle("Price, (US dollars)") ///
 legend(off) 
graph export "${output}/ch14-figure-8a-yhat-y-Stata.png", replace

* point and interval predictions by size
quietly reg price $M7 if workset==1

* Figure 14.8b
preserve
 keep if holdout==1
 collapse phat pi80lo pi80hi, by(n_accommodates)
 
 graph twoway (bar phat n_accommodates, col(navy*0.8) lcol(white) lw(vthick) ) ///
  (rcap pi80lo pi80hi n_accommodates, lc(green) lw(thick) ) , ///
  legend(off) ///
  xla(1(1)7, grid) yla(0(50)200, grid) ///
  xtitle("Number of guests accommodated") ytitle("Predicted price (US dollars)") 
graph export "${output}/ch14-figure-8b-yhat-bars-byaccom-Stata.png", replace
 
restore 
 

