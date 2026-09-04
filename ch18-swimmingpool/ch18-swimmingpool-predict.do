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
* CH18A Forecasting daily ticket volumes for a swimming pool
* using the swim-transactions dataset
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
* Option 1: run directory-setting do file
capture do set-data-directory.do
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"


global data_in  "${data_dir}/swim-transactions/raw"
global work  	"ch18-swimmingpool"

capture mkdir 		"${work}/output"
global output 	"${work}/output"





***********************************************************
* DATA, SAMPLE DESIGN, AGGREGATION TO DAILY, FEATURES
***********************************************************
* NOTE ON THE RAW DATA
* This case study starts from the raw transaction-level file. That file is
* about 180 MB, so it is NOT part of the da_data_repo download - only the
* metadata pdf sits in swim-transactions/raw. Option 2 fetches it from OSF.
* The R and Python versions skip the aggregation below and read the
* already-aggregated swim-transactions/clean/swim_work.csv instead.

* Option 1: raw file in the local data repository
capture import delimited "${data_in}/SwimmingPoolAdmissionsCABQ-en-us.csv", varnames(1) encoding(utf8) clear

* Option 2: download the raw file directly from OSF (about 180 MB)
if _rc {
	display as text "raw file not found locally - downloading from OSF (about 180 MB)"
	copy "https://osf.io/download/48qts/" "workfile.csv"
	import delimited "workfile.csv", varnames(1) encoding(utf8) clear
	erase "workfile.csv"
}
keep if location =="AQSP01" /* a single outdoor pool Sunport*/

* inspect category
table category, statistic(median quantity) statistic(min quantity) statistic(max quantity)
keep if category=="ADMISTIER1" | category=="ADMISTIER2"

* inspect item
* TOT means toddlers. PM means an evening ticket. 
* based on feedback, we do not need additional adjustment

table item, statistic(median quantity) statistic(min quantity) statistic(max quantity)
generate core1 =inlist(item, "ADULT     " , "SENIOR    " ,"TEEN      " ,"CHILD     ", "TOT       ")
generate core2 =inlist(item, "CHILD PM  ","ADULT PM  ","SENIOR PM ", "TOT PM    ", "TEEN PN   "  )

generate item2=item
replace item2="other" if core1==0 & core2==0

tabout item2  ///
using "${output}/items.tex", replace ///
style(tex) font(bold)    npos(lab) sum oneway format( 2 0 0 ) ///
c( mean quantity min quantity  max quantity ) /// 
 clab( Mean Min max ) 

keep if core1==1 | core2==1

generate n=1
generate timestamp = clock(date_time, "YMDhms", 2020)
format timestamp %tc

generate date=dofc(timestamp)
collapse (sum) n quantity, by(date)
label variable quantity "Daily ticket sales"

format date %td
tsset date
tsfill, full
replace quantity=0 if quantity==.

generate year=year(date)
generate month=month(date)
generate week=week(date)
generate day=day(date)
generate dayofweek=dow(date)
 recode dayofweek 0 = 7 /* sunday is 7 */
label variable date "Date of operation"
label def mo 1 Jan 2 Feb 3 mar 4 Apr 5 May 6 Jun 7 Jul 8 Aug 9 Sep 10 Oct 11 Nov 12 Dec
 label val month mo


* Subsample of complete years
keep if year>=2010 & year<=2016

* generate trend variable
capture drop t
sort year month day
capture drop t
generate t=_n


* national holidays
generate natholiday = 0
 replace natholiday = 1 if month==1 & day==1
 replace natholiday = 2 if month==7 & day==14
 replace natholiday = 3 if month==11 & day==11
 replace natholiday = 4 if month==12 & day==25
 replace natholiday = 5 if month==1 & dayofweek==1 & day>=15 & day<=21 
 replace natholiday = 6 if month==2 & dayofweek==1 & day>=15 & day<=21 
 replace natholiday = 7 if month==5 & dayofweek==1 & day>=25 
 replace natholiday = 8 if month==9 & dayofweek==1 & day<=7
 replace natholiday = 9 if month==10 & dayofweek==1 & day>=8 & day<=14 
 replace natholiday = 10 if month==11 & dayofweek==4 & day>=25 
******************************************************************  
* NOTE
* In the R code the holidays are defined using a built-in function called holidayNYSE
* no such fundtion exists in Stata
* instead, as you see, we created the holidays by hand
* these turn out to be a little different 
* as a result, the CV RMSE results are a little different in Stata 
* from what's in R and what's published in the book (Table 18.1)
* but the differences are small and don't affect the conclusions
******************************************************************  
 

* when school is off
generate school_off=0
	replace school_off = 1 if  (day>15 & month==5 & day <=30)
	replace school_off = 1 if (month==6 |  month==7)
	replace school_off = 1 if (day<15 & month==8 )
	replace school_off = 1 if (day>20 & month==12 )
tabulate school_off

save "${work}/swim-daily-workfile.dta", replace



**************************************************************************
* EDA
**************************************************************************
use "${work}/swim-daily-workfile.dta", replace


* Fig 18.3a
tsline quantity if year==2015, lc(navy*0.8) lw(thick) ///
 tlabel(#4, labsize(small) angle(forty_five)) tmtick(##11) ///
 ylab(, grid) xlab(, grid) ttitle("Date (day)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch18-figure-3a-tsline2015-Stata.png", as(png) replace

* Fig 18.3b
tsline quantity if year<=2015, lcolor(navy*0.8) lw(medthick) ///
 tlabel(, labsize(small) angle(forty_five)) tmtick(##11) ///
 ylab(, grid) xlab(, grid) ttitle("Date (day)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch18-figure-3b-tsline-Stata.png", as(png) replace


* BOX PLOTS
* Fig 18.4a
graph box quantity, over(month) box(1, lcol(navy*0.8) fcol(none) lw(thick) ) ///
 alsize(0) marker(1, mc(green*0.6) ) yla(, grid) title("Month")
graph export "${output}/ch18-figure-4a-boxmonth-Stata.png", as(png) replace

* Fig 18.4b
graph box quantity, over(dayofweek) box(1, lcol(navy*0.8) fcol(none) lw(thick) ) ///
 alsize(0) marker(1, mc(green*0.6) ) yla(, grid) title("Day of the week")
graph export "${output}/ch18-figure-4b-boxday-Stata.png", as(png) replace

* HEATMAP
* heatplot is installed by ch00-tech-prep/ch00_install_libraries.do
heatplot quantity month dayofweek , stat(mean) ybins(12) xbins(7) /// 
 col(viridis, reverse) xlab(1(1)7) ylab(1(1)12) 
graph export "${output}/ch18-figure-5-heatmap-Stata.png", as(png) replace




**********************************************
** CROSS-VALIDATED MODEL SELECTION 
**********************************************
* work set =2010-2015
* holdout = 2016
* 6-fold cross-validation with test sets 2010, 2011,..., 2015

*** 5 MODELS WITH QUANTITY AS TARGET VARIABLE (M1-M5)
local M1 t i.month
local M2 t i.month i.dayofweek
local M3 t i.month i.dayofweek i.natholiday
local M4 t i.month i.dayofweek i.natholiday i.school_off##i.dayofweek
local M5 t i.month i.dayofweek i.natholiday i.school_off##i.dayofweek i.month##i.dayofweek

forvalues i=1/5 {
	forvalues y=2010/2015 {
		quietly use "${work}/swim-daily-workfile.dta", replace
		display ""
		display "**********************************************"
		display "Model M`i'"
		display "test year: `y'"
		display "training years:"
		tabulate year if year!=`y'
		regress quantity `M`i'' if year!=`y'
		quietly predict yhat 
		quietly gen sq_error = (quantity - yhat)^2 
		quietly sum sq_error if year==`y'
		local mse_`y' = r(mean)
	}
	generate  cv_mse_M`i' = (`mse_2010'+`mse_2011'+`mse_2012'+`mse_2013'+`mse_2014'+`mse_2015')/6
	generate cv_rmse_M`i' = sqrt(cv_mse_M`i')
	keep if t==1 /* one-obervation file with the forecast statistics */
	keep t cv*
	capture merge 1:1 t using "${work}/swim-daily-forecasts.dta", nogen
	save "${work}/swim-daily-forecasts.dta", replace
}

*** +1 MODEL (M6)
* LN(QUANTITY) AS TARGET VARIABLE
* DEPENDENT VARIABLES ARE THE SAME AS IN M4
forvalues y=2010/2015 {
	use "${work}/swim-daily-workfile.dta", replace
	capture gen lnq = ln(quantity)
	keep if year>=2010 & year<=2015
	quietly reg lnq `M4' if year!=`y'
	quietly predict yhat 
	local sig = e(rmse)
	replace yhat = exp(yhat) * exp(`sig'^2/2)
	quietly gen sq_error = (quantity - yhat)^2 
	quietly sum sq_error if year==`y'
	local mse_`y' = r(mean)
}
generate cv_mse_M6 = (`mse_2010'+`mse_2011'+`mse_2012'+`mse_2013'+`mse_2014'+`mse_2015')/6
generate cv_rmse_M6 = sqrt(cv_mse_M6)
capture merge 1:1 t using "${work}/swim-daily-forecasts.dta", nogen
 aorder
 order t
 save "${work}/swim-daily-forecasts.dta", replace

tabstat cv_rmse_M*, col(s) format(%4.2f)


**********************************************
** EVALUATION OF BEST MODEL
**********************************************
* work set = 2010-2015
* holdout = 2016
* best model is M5
* estimate model on entire work set
use "${work}/swim-daily-workfile.dta", replace
regress quantity `M5' if year>=2010 & year<=2015

* prediction for holdout set
capture drop yhat
predict yhat if year==2016

* time-series plot of actual and predicted
* Fig 18.6
tsline quantity yhat if year==2016, ///
 lcolor(navy*0.8 green*0.8) lw(medthick medthick) lp(solid dash) ///
 tlabel(, grid) tmtick(##11) ///
 ylab(, grid) ttitle("Date (day)") ytitle("Daily ticket sales") ///
 legend(lab(1 "Actual") lab(2 "Predicted")) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch18-figure-6-pred-Stata.png", as(png) replace

* time-series plot of actual and predicted in August
* Fig 18.7a
tsline quantity yhat if year==2016 & month==8, ///
 lcolor(navy*0.8 green*0.8) lw(medthick medthick) lp(solid dash) ///
 tlabel(01Aug2016(7)29Aug2016, grid)  ///
 ylab(, grid) ttitle("Date (day)") ytitle("Daily ticket sales") ///
 legend(lab(1 "Actual") lab(2 "Predicted")) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch18-figure-7a-predAug-Stata.png", as(png) replace


* monthly RMSE in holdout set
generate sqerror = (quantity - yhat)^2 if year==2016
egen mse_mo = mean(sqerror) if year==2016, by(month)
egen q_mo = mean(quantity) if year==2016, by(month)
generate rmse_mo_norm = sqrt(mse_mo)/q_mo

* Fig 18.7b
graph bar (mean) rmse_mo_norm, over(month) ///
 bar(1, fc(navy*0.8) lc(navy*0.8) lw(vthick)) ///
 yla(, grid) ytitle("RMSE (normalized by monthly sales")
graph export "${output}/ch18-figure-7b-rmse-by-mo-Stata.png", as(png) replace


