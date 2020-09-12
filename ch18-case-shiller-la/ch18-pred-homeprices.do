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
* Chapter 18
* CH18B Forecasting a home price index
* using the case-shiller-la dataset
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


global data_in  "$data_dir/case-shiller-la/clean"
global work  	"ch18-case-shiller-la"

cap mkdir 		"$work/output"
global output 	"$work/output"


*******************************************
** IMPORT RAW DATA
** MULTIPLE CSV FILES

* home price indices
clear
insheet using "$data_in\houseprices-data-1990-2018.csv", names

gen year=int((_n-1)/12)+1990
gen month = _n - (year-1990)*12
*lis year month date
gen ym = ym(year,month)
format ym %tm
codebook ym
sort ym

gen p = pn
gen u = caur
gen emp = cana

order ym year month date p u emp
keep ym year month date p u emp

keep if year>=2000
tsset ym
gen t=_n
order ym t

gen dp = D.p
gen lnp = ln(p)
gen dlnp = D.lnp
gen du = d.u
gen lnemp = ln(emp)
gen dlnemp = D.lnemp
tab month, gen(mo)


preserve
keep if year>=2000 & year<=2017
save "$work\case-shiller-workfile.dta",replace
restore
preserve
keep if year>=2000 & year<=2018
order ym year month t date p u emp
save "$work\case-shiller-workfile-1990-2018dta",replace
restore

 




**********************************************
* EXPLORE
use "$work\case-shiller-workfile.dta",replace

* Fig 18.8
colorpalette viridis, n(4) select(2) nograph
tsline p, lw(thick) lc(`r(p)') ///
  ylab(50(50)300, grid) tlab(2000m1(36)2018m1, grid) ///
  ytitle("Case-Shiller home price index") xtitle(Date (month))
graph export "$output/ch18-figure-8-price-ts-Stata.png", replace



**********************************************
** FORECASTS, CROSS-VALIDATON
cap rm "$work/forecast1-cv-rmse.dta"

* Model M1
local i=1
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	regress p t i.month if year>=`trainy_lo' & year<=`trainy_hi'
	predict yhat 
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)

* Model M2
local i=2
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	arima dp if year>=`trainy_lo' & year<=`trainy_hi', ar(1) ma(1/2)
	 sum ym if year==`testy' & month==1 /*when the forecast starts */
	 local start = r(mean)
	predict dyhat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen yhat = p[_n-1] + dyhat if year==`testy' & month==1
	 replace yhat = yhat[_n-1] + dyhat if year==`testy' & month>=2 & month<=12
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)
	
* Model M3
local i=3
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	arima dp mo2-mo12 if year>=`trainy_lo' & year<=`trainy_hi', ar(1) ma(1)
	 sum ym if year==`testy' & month==1 /*when the forecast starts */
	 local start = r(mean)
	predict dyhat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen yhat = p[_n-1] + dyhat if year==`testy' & month==1
	 replace yhat = yhat[_n-1] + dyhat if year==`testy' & month>=2 & month<=12
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)
	
	
* Model M4
local i=4
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	arima p t mo2-mo12 if year>=`trainy_lo' & year<=`trainy_hi', ar(1 2) 
	 sum ym if year==`testy' & month==1 /*when the forecast starts */
	 local start = r(mean)
	predict yhat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
aorder
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)
	
* Model M5
local i=5
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	reg dp t mo2-mo12 if year>=`trainy_lo' & year<=`trainy_hi'
	predict dyhat 
	gen yhat = p[_n-1] + dyhat if year==`testy' & month==1
	 replace yhat = yhat[_n-1] + dyhat if year==`testy' & month>=2 & month<=12
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
aorder
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)
	
* Model M6
local i=6
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	gen d2lnp = d.dlnp
	reg d2lnp mo2-mo12 if year>=`trainy_lo' & year<=`trainy_hi'
	predict d2lnyhat
	 gen dlnyhat = dlnp[_n-1] + d2lnyhat if year==`testy' & month==1
	 replace dlnyhat = dlnyhat[_n-1] + d2lnyhat if year==`testy' & month>=2 & month<=12
	 gen lnyhat = lnp[_n-1] + dlnyhat if year==`testy' & month==1
	 replace lnyhat = lnyhat[_n-1] + dlnyhat if year==`testy' & month>=2 & month<=12
	 gen temp = (lnyhat-lnp)^2
	 sum temp if year==`testy'
	 local sig=r(mean)
	gen yhat = exp(lnyhat) * exp(`sig'^2/2) 
	gen sqerr = (p-yhat)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	}
gen cv_mse_M`i'=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_M`i'=sqrt(cv_mse_M`i')
keep if t==1 /* one-obervation file with the forecast statistics */
keep t cv*
cap merge 1:1 t using "$work/forecast1-cv-rmse.dta", nogen
aorder
save "$work/forecast1-cv-rmse.dta", replace
tabstat cv_rmse*, s(mean) col(s)
	

**********************************************
* EVALUATION ON HOLDOUT SET
clear
use "$work\case-shiller-workfile.dta",replace
gen holdout = year==2017 /* holdout set */
gen workset = year!=2017 /* work set */
tabstat holdout workset, by(year) s(sum)
sum ym year month if year==2017 & month==1

* estimate best model on entire work set
* M4
qui reg p L(1/2).p t mo2-mo12 if workset==1
est store m4
forecast create m4holdout, replace
forecast estimates m4
forecast solve, begin(684)
replace f_p=. if workset

* time series graph of point predictions
* Figure 18.9a
colorpalette viridis, n(4) select(2) nograph
tsline p f_p if year>=2015, lw(thick thick) lc(`r(p)') ///
 ylab(, grid) xlab(, grid) ///
 ytitle(Case-Shiller price index) xtitle(Date (month)) ///
 legend(label(1 Actual) label(2 Predicted))
graph export "$output/ch18-figure-9a-p-phat-ts-Stata.png", replace

* time series graph of point and interval predictions
* Figure 18.9b
* no built-in routine in Stata




**********************************************
* VECTOR AUTOREGRESSION
clear
use "$work\case-shiller-workfile.dta",replace
gen holdout = year==2017 /* holdout set */
gen workset = year!=2017 /* work set */
tabstat holdout workset, by(year) s(sum)
sum ym year month if year==2017 & month==1

* time series graphs of unemployment and employment
* Figure 18.10a
colorpalette viridis, n(4) select(2) nograph
tsline u , lw(thick) lc(`r(p)') ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
  ytitle(Unemployment rate (percent)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10a-u-ts-Stata.png", replace

* Figure 18.10a
colorpalette viridis, n(4) select(2) nograph
tsline du , lw(thick) lc(`r(p)') ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Change in unemployment rate (percentage point)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10b-du-ts-Stata.png", replace

 * Figure 18.10c
colorpalette viridis, n(4) select(2) nograph
tsline emp , lw(thick) lc(`r(p)') ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Employment (in thousands)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10c-emp-ts-Stata.png", replace

 * Figure 18.10d
colorpalette viridis, n(4) select(2) nograph
tsline dlnemp , lw(thick) lc(`r(p)') ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Change in ln(employment, in thousands)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10d-dlnemp-ts-Stata.png", replace

 
* VAR(1) + season dummies estimation and forecast
qui forvalue testy = 2013/2016 {
	use "$work\case-shiller-workfile.dta",replace
	drop if year>=2017 /* keep work set */
	local trainy_hi = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_hi'-13 /*first year of training set */
	sum dp du dlnemp if year>=`trainy_lo' & year<=`trainy_hi'
	qui var dp du dlnemp if year>=`trainy_lo' & year<=`trainy_hi', lags(1) exog(mo*) 
	 estimate store var_`testy'
	 forecast create var_`testy', replace
	 forecast estimates var_`testy'
	 forecast exogenous mo2-mo12
	 forecast identity p = L.p + dp
	qui forecast solve, prefix(fvar_) begin(tm(`testy'm1))
	gen f_p = fvar_p if year==`testy'
	gen sqerr = (p-f_p)^2 
	sum sqerr if year==`testy'
	local mse_`testy' = r(mean)
	noisily dis "mse_`testy'  =  "  `mse_`testy''
}
gen cv_mse_var=(`mse_2013'+`mse_2014'+`mse_2015'+`mse_2016')/4
gen cv_rmse_var=sqrt(cv_mse_var)
keep if t==1 /* one-obervation file with the forecast statistics */
sum cv*




 