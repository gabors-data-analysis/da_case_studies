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
* version 0.92 2021-05-01
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/gabors_data_analysis/da_case_studies"
 


* STEP 2: * Directory for data
* Option 1: run directory-setting do file
do set-data-directory.do 
							/* this is a one-line do file that should sit in 
							the working directory you have just set up
							this do file has a global definition of your working directory
							more details: gabors-data-analysis.com/howto-stata/   */

* Option 2: set directory directly here
* for example:
 global data_dir "/Users/vigadam/Dropbox/work/data_book/da_data_repo"


global data_in  "$data_dir/case-shiller-la/clean"
global work  	"ch18-case-shiller-la"

cap mkdir 		"$work/output"
global output 	"$work/output"


******************************************
* SET UP VIRIDIS COLORS
*
colorpalette viridis, n(4)
return list
global p =r(p1) /* dark purple */
global b =r(p2) /* blue */
global g =r(p3) /* green */
global y =r(p4) /* yellow */



*******************************************
** IMPORT TIDY DATA

use "$data_in/homeprices-data-2000-2018.dta", clear
* NOTE
* this data table contains observations up to 2018 
* the 2018 data are used only at the very end of the case stufy
* the main part of the case study ends with 2017
* to reflect that we create two workfiles, one ending with 2017, one with 2018


* generate running t index
gen t=_n
* tell Statat this is time series data
* and define year-month variable
gen ym = ym(year,month)
format ym %tm
order ym t
tsset ym

* define variables we'll work with
* not seasonally adjusted price index
gen p = pn
* seasonally adjusted unemployment rate and total employment
gen u = us
gen emp = emps

order ym t year month date p u emp
keep ym t year month date p u emp

gen dp = D.p
gen lnp = ln(p)
gen dlnp = D.lnp
gen du = d.u
gen lnemp = ln(emp)
gen dlnemp = D.lnemp
qui tab month, gen(mo) /* generate month dummies */


* now save the workfile with data from 2000 through 2018
order ym year month t date p u emp
save "$work\case-shiller-workfile-2000-2018.dta",replace

* and now create and save the workfile with data from 2000 through 2017
keep if year>=2000 & year<=2017

* create work and holdout sets
gen holdout = year==2017 /* holdout set */
gen workset = year!=2017 /* work set */

* create training and test sets for 4 folds
forvalue y = 2013/2016 {
	local fold=`y'-2012
	gen test`fold'=year==`y'
	gen train`fold' = year<=`y'-1 & year>=`y'-13 
	* tabstat test`fold' train`fold', by(year) /* check training-test splits */
}
save "$work\case-shiller-workfile-2000-2017.dta",replace

 




**********************************************
* EXPLORE
use "$work\case-shiller-workfile-2000-2017.dta",replace

* Fig 18.8
tsline p, lw(thick) lc("$b") ///
  ylab(50(50)300, grid) tlab(2000m1(36)2018m1, grid) ///
  ytitle("Case-Shiller home price index") xtitle(Date (month))
graph export "$output/ch18-figure-8-price-ts-Stata.png", replace


**********************************************
** FORECASTS, CROSS-VALIDATON
cap rm "$work/forecast-cv-rmse.dta"

* Model M1
* p on trend & seasonality
local i=1
use "$work\case-shiller-workfile-2000-2017.dta",replace
qui forvalue fold = 1/4 {
	regress p t i.month if train`fold'==1
	predict phat
	* calculate error squared for mse
	gen errsq = (p-phat)^2 
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop phat errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* now create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M`i'"
order model rmse* cv_rmse
save "$work/forecast-cv-rmse.dta", replace

* Model M2
* p ARIMA(1,1,2) 
local i=2
use "$work\case-shiller-workfile-2000-2017.dta",replace
qui forvalue fold = 1/4 {
	arima dp if train`fold'==1, ar(1) ma(1/2) /* this is ARIMA(1,1,2) as left-hand-side is dp */
	 sum ym if test`fold'==1 & month==1 /*when the forecast starts */
	 local start = r(mean)
	 predict dphat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen phat = p[_n-1] + dphat if test`fold'==1 & month==1
	 replace phat = phat[_n-1] + dphat if test`fold'==1 & month>=2 & month<=12
	gen errsq = (p-phat)^2  if test`fold'==1
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop phat dphat errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M`i'"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

* Model M3
* p ARIMA(1,1,0) & seasonality
local i=3
use "$work\case-shiller-workfile-2000-2017.dta",replace
qui forvalue fold = 1/4 {
	arima dp mo2-mo12 if train`fold'==1, ar(1) 
	 sum ym if test`fold'==1 & month==1 /*when the forecast starts */
	 local start = r(mean)
	 predict dphat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen phat = p[_n-1] + dphat if test`fold'==1 & month==1
	 replace phat = phat[_n-1] + dphat if test`fold'==1 & month>=2 & month<=12
	gen errsq = (p-phat)^2 if test`fold'==1
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop phat dphat errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
gen str2 model="M`i'"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

* Model M4
* dp ARIMA(2,0,0) & trend & seasonality (or p ARIMA(2,1,0) & trend & seasonality)
local i=4
use "$work\case-shiller-workfile-2000-2017.dta",replace
qui forvalue fold = 1/4 {
	arima p mo2-mo12 t if train`fold'==1, ar(1/2) 
	sum ym if test`fold'==1 & month==1 /*when the forecast starts */
	 local start = r(mean)
	predict phat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
	gen errsq = (p-phat)^2 
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop phat errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M`i'"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

	
* Model M5
* dp on trend & seasonality (or p (ARIMA(0,1,0) & trend & seasonality)
use "$work\case-shiller-workfile-2000-2017.dta",replace
local i=5
qui forvalue fold = 1/4 {
	reg dp t mo2-mo12  if train`fold'==1
	 predict dphat 
	gen phat = p[_n-1] + dphat if test`fold'==1 & month==1
	 replace phat = phat[_n-1] + dphat if test`fold'==1 & month>=2 & month<=12
	gen errsq = (p-phat)^2 if test`fold'==1
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop phat dphat errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M`i'"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

* Model M6
* lnp ARIMA(0,2,0) $ seasonality (it's twice differenced lnp & seasonality)
local i=6
use "$work\case-shiller-workfile-2000-2017.dta",replace
gen d2lnp = d.dlnp
qui forvalue fold = 1/4 {
	reg d2lnp mo2-mo12 if train`fold'==1
	predict d2lnphat
	 gen dlnphat = dlnp[_n-1] + d2lnphat if test`fold'==1 & month==1
	 replace dlnphat = dlnphat[_n-1] + d2lnphat if test`fold'==1 & month>=2 & month<=12
	 gen lnphat = lnp[_n-1] + dlnphat if test`fold'==1 & month==1
	 replace lnphat = lnphat[_n-1] + dlnphat if test`fold'==1 & month>=2 & month<=12
	 * now the log correction
	 gen temp = (lnphat-lnp)^2 if test`fold'==1
	 sum temp if test`fold'==1
	 local sig=r(mean) 
	gen phat = exp(lnphat) * exp(`sig'^2/2) 
	gen errsq = (p-phat)^2 if test`fold'==1
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop d2lnphat dlnphat lnphat temp phat errsq 
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M`i'"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

**** Cross-validated RMSE statistics for TABLE 18.2
sort model
format cv_rmse %4.1f
lis model cv_rmse, separator(0)


**********************************************
* EVALUATION ON HOLDOUT SET
clear
use "$work\case-shiller-workfile-2000-2017.dta",replace
* tabstat holdout workset, by(year) s(sum) /* check if workset - holdout set are fine */

* estimate best model (M4) on entire work set
arima p mo2-mo12 t if workset==1, ar(1/2) 
 sum ym if holdout==1 & month==1 /*when the forecast starts */
 local start = r(mean)
 predict phat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
  replace phat=. if holdout!=1
  gen errsq = (p-phat)^2 
  sum errsq if holdout==1
 local mse_holdout = r(mean)
 local rmse_holdout = sqrt(`mse_holdout')
dis "Holdout set RMSE = " `rmse_holdout'


* time series graph of point predictions
* Figure 18.9a
tsline p phat if year>=2015, lw(thick thick) lc("$p" "$g") ///
 ylab(, grid) xlab(, grid) ///
 ytitle(Case-Shiller price index) xtitle(Date (month)) ///
 legend(label(1 Actual) label(2 Predicted) ///
  ring(0) position(5) row(1) region(lstyle(none)))
graph export "$output/ch18-figure-9a-p-phat-ts-Stata.png", replace

* time series graph of point and interval predictions
* Figure 18.9b
* no built-in routine in Stata




**********************************************
* VECTOR AUTOREGRESSION
clear
use "$work\case-shiller-workfile-2000-2017.dta",replace

* time series graphs of unemployment and employment
* Figure 18.10a
tsline u , lw(thick) lc("$b") ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
  ytitle(Unemployment rate (percent)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10a-u-ts-Stata.png", replace

* Figure 18.10a
tsline du , lw(thick) lc("$b") ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Change in unemployment rate (percentage point)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10b-du-ts-Stata.png", replace

* Figure 18.10c
tsline emp , lw(thick) lc("$b") ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Employment (in thousands)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10c-emp-ts-Stata.png", replace

* Figure 18.10d
tsline dlnemp , lw(thick) lc("$b") ///
  ylab(, grid) tlab(2000m1(36)2018m1, grid) ///
 ytitle(Change in ln(employment, in thousands)) xtitle(Date (month)) 
 graph export "$output/ch18-figure-10d-dlnemp-ts-Stata.png", replace
*

forvalue fold = 1/4 {
	qui var dp du dlnemp if train`fold'==1, lags(1) exog(mo*) 
	 estimate store var`fold'
	 forecast create var`fold', replace
	 forecast estimates var`fold'
	 forecast exogenous mo2-mo12
	 forecast identity p = L.p + dp
	 sum ym if test`fold'==1 & month==1
	 local start = r(mean) /* to tell Stata where to start the actual forecast */
	qui forecast solve, prefix(yhat) begin(`start')
	gen phat = yhatp
	gen errsq = (p-phat)^2 if test`fold'==1
	sum errsq if test`fold'==1
	gen mse`fold' = r(mean)
	gen rmse`fold' = sqrt(mse`fold')
	drop yhat* phat* errsq
}
* average mse across folds and take square root
gen cv_mse=(mse1+mse2+mse3+mse4)/4
gen cv_rmse=sqrt(cv_mse)
* create data table with one observation that has the mse & rmse stats
keep if t==1 /* keep one obervation only */
keep cv* rmse* mse*
gen str2 model="M7"
order model rmse* cv_rmse
* add this one row to the previously created data table with the mse and rmse statistics
append using "$work/forecast-cv-rmse.dta"
save "$work/forecast-cv-rmse.dta", replace

** TABLE 18.3
sort model
format rmse* %4.2f
format cv_* %4.1f
lis model rmse1 rmse2 rmse3 rmse4 cv_rmse, separator(0)


 
*************************************************
* new holdout set: 2018
* home price indices
use "$work\case-shiller-workfile-2000-2018.dta",replace
gen holdout = year==2018
gen workset = year!=2018
* tabstat holdout workset, by(year) s(sum) /* check if workset - holdout set are fine */

* estimate best model (M4) on entire work set
arima p mo2-mo12 t if workset==1, ar(1/2) 
 sum ym if holdout==1 & month==1 /*when the forecast starts */
 local start = r(mean)
 predict phat , dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
  replace phat=. if holdout!=1
  gen errsq = (p-phat)^2 
  sum errsq if holdout==1
 local mse_holdout = r(mean)
 local rmse_holdout = sqrt(`mse_holdout')
dis "Holdout set RMSE = " `rmse_holdout'


* time series graph of point predictions
* Figure 18.11
tsline p phat if year>=2015, lw(thick thick) lc("$p" "$g") ///
 ylab(, grid) xlab(, grid) ///
 ytitle(Case-Shiller price index) xtitle(Date (month)) ///
 legend(label(1 Actual) label(2 Predicted) ///
  ring(0) position(5) row(1) region(lstyle(none)))
graph export "$output/ch18-figure-11-p-phat-ts-Stata.png", replace

