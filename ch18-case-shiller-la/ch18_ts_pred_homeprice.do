*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 18B
* Forecasting a home price index
*
* using the case-shiller-la dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch18-case-shiller-la"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-europe/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/case-shiller-la/raw"


*******************************************
** IMPORT RAW DATA
** MULTIPLE CSV FILES

* home price indices
clear
insheet using "$data_in\house_monthly_fred.csv", names

keep date_month *_m
gen temp=date(date_month, "YMD")
format temp %td
gen year=year(temp)
gen quarter=quarter(temp)
gen month=month(temp)
drop if year==.
drop if month==.
compress
cap drop temp
order year quarter month
save "$work\ts_homeprices.dta",replace

* monthly statistics on unemployment and employment
clear
insheet using "$data_in\house_monthly_bls.csv", names

keep date_month *_m
gen temp=date(date_month, "YMD")
format temp %td
gen year=year(temp)
gen quarter=quarter(temp)
gen month=month(temp)
drop if year==.
drop if month==.
compress
cap drop temp
order year quarter month
merge 1:1 year month using "$work\ts_homeprices.dta", nogen keep(2 3)

drop nyc* nys*
destring calif_unemp_m, replace

* keep 2000 and after
keep if year>=2000

** FEATURE ENGINEERING
gen ym=ym(year, month)
format ym %tm
tsset ym, monthly

gen p=la_hpinsa
 drop if p==.
 lab var p "Price index"
gen dp = d.p
gen lnp=ln(p)
gen dlnp=d.lnp
 lab var dlnp "Log difference of price index"

sort year month
gen t = _n
 lab var t "t for trend"
tab month, gen(mo)
 
tsset ym 
order ym t
save "$work\ts_homeprices.dta",replace


/*merge m:1 year quarter using ts_homeprices_quarterly, nogen keep(1 3)
merge m:1 year using ts_homeprices_yearly, nogen keep(1 3)
keep if year>=1987
save "$work\ts_homeprices.dta",replace
*/



**********************************************
* EXPLORE
use "$work\ts_homeprices.dta",replace

* Fig 18.8
colorpalette viridis, n(4) select(2) nograph
tsline p, lw(thick) lc(`r(p)') ///
  ylab(50(50)300, grid) xlab(, grid) ///
  ytitle("Case-Shiller home price index") xtitle(Date (month))
graph export "$output/ch18-figure-8-price-ts-Stata.png", replace



**********************************************
** FORECASTS, CROSS-VALIDATON
cap rm "$work/forecast1-cv-rmse.dta"

* Model M1
local i=1
qui forvalue testy = 2013/2016 {
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	regress p t i.month if year>=`trainy_lo' & year<=`trainy_up'
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
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	arima dp if year>=`trainy_lo' & year<=`trainy_up', ar(1) ma(1/2)
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
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	arima dp mo2-mo12 if year>=`trainy_lo' & year<=`trainy_up', ar(1) ma(1)
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
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	arima p t mo2-mo12 if year>=`trainy_lo' & year<=`trainy_up', ar(1 2) 
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
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	reg dp t mo2-mo12 if year>=`trainy_lo' & year<=`trainy_up'
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
	use "$work\ts_homeprices.dta",replace
	drop if year==2017 /* holdout set */
	local trainy_up = `testy'-1 /*last year of training set */
	local trainy_lo = `trainy_up'-13 /*first year of training set */
	gen d2lnp = d.dlnp
	reg d2lnp mo2-mo12 if year>=`trainy_lo' & year<=`trainy_up'
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
use "$work\ts_homeprices.dta",replace
gen holdout = year==2017 /* holdout set */
gen workset = year!=2017 /* work set */
tabstat holdout workset, by(year) s(sum)

* estimate best model on entire work set
* M4
qui arima p t mo2-mo12 if workset==1, ar(1 2) 
 sum ym if holdout==1 & month==1 /*when the forecast starts */
	 local start = r(mean)
	 dis "start ym for prediction is `start'"
 predict yhat, dynamic(`start') /* "dynamic" forecast: further ahead using predicted values */
  replace yhat=. if holdout!=1
  
colorpalette viridis, n(4) select(2) nograph
tsline p yhat if year>=2015, lw(thick thick) lc(`r(p)') ///
 ylab(, grid) xlab(, grid) ///
 ytitle(Case-Shiller price index) xtitle(Date (month)) ///
 legend(label(1 Actual) label(2 Predicted))
 
 

/** select year for test set
local testy 2016
** here we set the first year tos how on the output
local outputtart = `testy'-7

** define train and test sets
gen train = year<`testy'
gen test = year==`testy'
** drop time periods after test year - technicality, for nice output
drop if year>`testy'
** create month binary variables
qui tab month, gen(M)

**********************************************
** DO FORECASTS, MANY MODELS TO CONSIDER
* MODEL 0: JUST A CONSTANT IN DLNP (TREND)
local m m0 
qui reg dlnp if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'

forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: avg log diff"

gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
 graph export "$output/fcast_`m'_`testy'.png",replace

 
* MODEL: SEASONALITY
local m m1 
qui reg dlnp M2-M12 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff + seasonality"

gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
 graph export "$output/fcast_season_`testy'.png",replace


* MODEL: AR(1) + SEASONALITY 
local m ar1
qui 
reg dlnp L.dlnp M2-M12 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12 
forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff AR(1) + seasonality"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_season_`m'_`testy'.png",replace

* regression table with estimates from first three models
* esttab m0 m1 ar1 using Ch18_regtab_1.tex, replace b(%4.3f) se nostar tex


* MODEL: AR(4) + SEASONALITY 
local m ar4
qui reg dlnp L(1/4).dlnp M2-M12 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff AR(4) + seasonality"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_season_`m'_`testy'.png",replace


* MODEL: ARMA(4,4)  
local m arma44
qui arima dlnp if train==1, arima(4,0,4)
 local sig = e(sigma)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff ARMA(4,4)"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_`m'_`testy'.png",replace



* MODEL: AR(12) + SEASONALITY 
local m ar12
qui reg dlnp L(1/12).dlnp M2-M12 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff AR(12) + seasonality"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_season_`m'_`testy'.png",replace



* MODEL: AR(12) NO SEASSONALITY
local m ar12ns
qui reg dlnp L(1/12).dlnp if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'

forecast identity lnp = L.lnp + dlnp
forecast identity p = exp(lnp)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff AR(12) "
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_noseason_`m'_`testy'.png",replace



*** VAR with unemployment rate & level of emp  (CA)
*** create variables
gen u=(calif_unemp/1000)/calif_emp 
 gen du=d.u
 /*
 tsline u, ylabel(, grid) xlabel(, grid) ///
	xtitle(" ") ytitle("Unemployment rate")
 graph export "$output/tseries_u.png",replace
  *more
 tsline du, ylabel(, grid) xlabel(, grid) ///
	xtitle(" ") ytitle("Monthly change in the unemployment rate")
 graph export "$output/tseries_du.png",replace
 */

gen emp=calif_emp
 gen lnemp=ln(emp)
 gen dlnemp=d.lnemp 
 
*
tsline emp, ylabel(, grid) xlabel(, grid) ///
	xtitle(" ") ytitle("Employment") 
 graph export "$output/tseries_emp.png",replace

tsline dlnemp, ylabel(, grid) xlabel(, grid) ///
	xtitle(" ") ytitle("Log change in employment") 
 graph export "$output/tseries_dlnemp.png",replace
*/

*** MODEL: VAR(1) + seasonality

local m var1
qui var dlnp du dlnemp if train==1, exog(M2-M12) lags(1)
 local sig1 = e(rmse_1)
 local sig2 = e(rmse_2)
 local sig3 = e(rmse_3)
 gen coeffs_`m' = e(k)
 gen trainobs_`m' = e(obs_1)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
 forecast identity p = exp(lnp)*exp(`sig1' ^2 /2)
forecast identity u = L.u + du
forecast identity lnemp = L.lnemp + dlnemp
 forecast identity emp = exp(lnemp)*exp(`sig3' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff VAR(1) w/ du, dlnemp + seas."
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

 
tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_var1_`testy'.png",replace

 
** MODEL: VAR(2) + seasonality

local m var2
qui var dlnp du dlnemp if train==1, exog(M2-M12) lags(1/2)
 local sig1 = e(rmse_1)
 local sig2 = e(rmse_2)
 local sig3 = e(rmse_3)
 gen coeffs_`m' = e(k)
 gen trainobs_`m' = e(obs_1)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
 forecast identity p = exp(lnp)*exp(`sig1' ^2 /2)
forecast identity u = L.u + du
forecast identity lnemp = L.lnemp + dlnemp
 forecast identity emp = exp(lnemp)*exp(`sig3' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff VAR(4) w/ du, dlnemp + seasonality"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)
 
tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
more 
*/

*** MODEL: VAR(4) + seasonality

local m var4
qui var dlnp du dlnemp if train==1, exog(M2-M12) lags(1/4)
 local sig1 = e(rmse_1)
 local sig2 = e(rmse_2)
 local sig3 = e(rmse_3)
 gen coeffs_`m' = e(k)
 gen trainobs_`m' = e(obs_1)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
 forecast identity p = exp(lnp)*exp(`sig1' ^2 /2)
forecast identity u = L.u + du
forecast identity lnemp = L.lnemp + dlnemp
 forecast identity emp = exp(lnemp)*exp(`sig3' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff VAR(4) w/ du, dlnemp + seas."
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)
 
tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
 graph export "$output/fcast_var4_`testy'.png",replace

 
*** MODEL: VAR(12) + seasonality

local m var12
qui var dlnp du dlnemp if train==1, exog(M2-M12) lags(1/12)
 local sig1 = e(rmse_1)
 local sig2 = e(rmse_2)
 local sig3 = e(rmse_3)
 gen coeffs_`m' = e(k)
 gen trainobs_`m' = e(obs_1)
 
 
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity lnp = L.lnp + dlnp
 forecast identity p = exp(lnp)*exp(`sig1' ^2 /2)
forecast identity u = L.u + du
forecast identity lnemp = L.lnemp + dlnemp
 forecast identity emp = exp(lnemp)*exp(`sig3' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff VAR(12) w/ du, dlnemp + seas."
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)


tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_var12_season_`testy'.png",replace


** MODEL: VAR(12) , no seasonality

local m var12ns
qui var dlnp du dlnemp if train==1, lags(1/12)
 local sig1 = e(rmse_1)
 local sig2 = e(rmse_2)
 local sig3 = e(rmse_3)
 gen coeffs_`m' = e(k)
 gen trainobs_`m' = e(obs_1)
est store `m'
forecast create `m', replace
forecast estimates `m'

forecast identity lnp = L.lnp + dlnp
 forecast identity p = exp(lnp)*exp(`sig1' ^2 /2)
forecast identity u = L.u + du
forecast identity lnemp = L.lnemp + dlnemp
 forecast identity emp = exp(lnemp)*exp(`sig3' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(tm(`testy'm1))
 lab var f`m'_p "Forecast. Model: log diff VAR(12) w/ du, dlnemp"
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

tsline p f`m'_p if year>=`outputtart', ylab(, grid) xlab(,grid) ///
 lw(thick thick) lp(solid dash) lc(navy blue)
graph export "$output/fcast_var12_season_`testy'.png",replace


** ENSEMBLE OF trend+season, arma(4,4), VAR(4)+season, VAR(12)+season
local m ensemble
egen f`m'_p = rowmean(fm1_p farma44_p fvar4_p fvar12_p)
gen temp`m' = (p-f`m'_p)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

 
**********************************************
** COMPARE RMSE ON TEST SET
dis " test year = `testy'"
tabstat trainobs*, col(s) varw(20) format(%4.0f)
tabstat coeffs*, col(s) varw(20) format(%4.0f)

tabstat RMSE*, col(s) varw(20) format(%4.2f)

