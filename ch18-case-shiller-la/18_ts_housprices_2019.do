*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CASE STUDY FOR CHAPTER 18
*
* DATA Case Schiller
*********************************************************************

* WHAT THIS CODES DOES:

* 
*****
********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
*cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
*cd "C:\Users\viktoriakonya\Dropbox\bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in   "cases_studies_public/case-schiller-la/raw"
global data_out	 "textbook_work/ch18/case-schiller-la" /*data_out*/
global output    "textbook_work/ch18/case-schiller-la/output" /*output*/



clear all


set scheme s1color
*******************************************
** IMPORT RAW DATA
** MULTIPLE CSV FILES

* house price indices
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
save "$data_out\ts_houseprices.dta",replace

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
merge 1:1 year month using "$data_out\ts_houseprices.dta", nogen keep(2 3)

drop nyc* nys*
destring calif_unemp_m, replace

save "$data_out\ts_houseprices.dta",replace


/*merge m:1 year quarter using ts_houseprices_quarterly, nogen keep(1 3)
merge m:1 year using ts_houseprices_yearly, nogen keep(1 3)
keep if year>=1987
save "$data_out\ts_houseprices.dta",replace
*/



**********************************************
** FEATURE ENGINEERING

use "$data_out\ts_houseprices.dta",replace
gen ym=ym(year, month)
format ym %tm
tsset ym, monthly

gen p=la_hpinsa
 drop if p==.
 lab var p "Price index"
gen lnp=ln(p)
gen dlnp=d.lnp
 lab var dlnp "Log difference of price index"


tsline p, ylab(, grid) xlab(, grid)
 graph export "$output/p_tseries.png", replace
 more
tsline dlnp, ylab(, grid) xlab(, grid)
 graph export "$output/dlnp_tseries.png", replace
 more






**********************************************
** PREPARE FORECASTING

** select year for test set
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

