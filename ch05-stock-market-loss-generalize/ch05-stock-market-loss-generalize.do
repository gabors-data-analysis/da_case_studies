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
* Chapter 05
* CH05A What likelihood of loss to expect on a stock portfolio?
* using the sp500 dataset
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

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


global data_in  "$data_dir/sp500/clean"
global work  	"ch05-stock-market-loss-generalize"

cap mkdir 		"$work/output"
global output 	"$work/output"

cap mkdir 		"$work/temp"
global temp 	"$work/temp"



* set level of loss to inquire (in percent)
global loss 5

***************************************************************
* load data 
use "$data_in/SP500_2006_16_data.dta", clear

* create gap variable 
gen gap=date-date[_n-1]-1

* label variables
lab var value "Value of the S&P500"
lab var datestr "Date, in string format (YMD)"
lab var date "Date"
lab var gap "Gap between observations, in days"

* create variable for each year and each month
* for later use
gen year =year(date)
gen month=month(date)

* order variables
order date datestr year month gap

save "$work/work.dta", replace


************************************************
* Exaploratory Data Analysis

use "$work/work.dta", clear

* time series graphs not in textbook
tsset date
tsline value, lw(thick) lc(navy*0.8) xla(, grid) yla(,grid)

sort date
gen pct_return=(value-value[_n-1])/value[_n-1]*100
tsline pct_return, lc(navy*0.8) xla(, grid) yla(,grid) yline(0)


collapse (mean) value, by (year )
tsset year
tsline value, lw(thick) lc(navy*0.8) xla(, grid) yla(,grid)

sort year
gen pct_return=(value-value[_n-1])/value[_n-1]*100
tsline pct_return, lw(thick) lc(navy*0.8) xla(, grid) yla(,grid) yline(0)



use "$work/work.dta", clear

* create percent daily returns
sort date
gen pct_return=(value-value[_n-1])/value[_n-1]*100
lab var pct_return "Percent daily return"

* histogram
colorpalette viridis, n(4) select(2) nograph
histogram pct_return, freq start(-10) width(0.25) bstyle(background) ///
	fcol(`r(p)') xlab(-10 -5 -2 0 2 5 10, grid) ylab(, grid) ///
	xline(-$loss, lc(green) lstyle(foreground) lw(thick)) ///
	text(220 -7 "5% loss -->")
graph export "$output/ch05-figure-1-returns-histogram-Stata.png",replace

keep date pct_return
save "$temp/sp500_pctreturns.dta", replace

***************************************
* REPEATED SAMPLES
* samples of 500 and 1000 observations (2 & 4 years approximately)

clear
set matsize 10000
set seed 573164
global M=10000

foreach n in 500 1000 {

	mat def Results = J($M,10,0)

qui forvalues i=1/$M {
	use "$temp/sp500_pctreturns.dta",replace
	sample `n', count
	forvalue j=1/10 {
		gen loss`j'p=100*(pct_return<-`j')
		sum loss`j'p
		mat Results[`i',`j']=r(mean)
	}
}
clear
svmat Results, names(loss)
tabstat loss*, s(mean median min max sd n)
save "$temp/repsamples`n'",replace
}


use "$temp/repsamples1000",replace
tabstat loss$loss, s(mean median min max sd n)
lab var loss$loss "Percent of days with losses of $loss% or more"

* Figure 2
qui sum loss$loss
local mean_resamples1000 = r(mean)
histogram loss$loss, freq width(0.099) bstyle(background) ///
	fcol(navy*0.8) xlab(0(0.25)1.5, grid) ylab(0(500)2500, grid) ///
	xline(`mean_resamples1000 ', lc(green*0.8) lstyle(foreground) lw(thick)) ///
	text(2400 0.65 "<---- mean")
graph export "$output/ch05-figure-2-resample1000-Stata.png",replace



clear

use "$temp/repsamples500",replace
 keep loss$loss
 rename loss$loss loss_n500
merge 1:1 _n using "$temp/repsamples1000", nogen
 keep loss$loss* loss_n*
 rename loss$loss loss_n1000

sum loss* 

* Figure 3
twoway histogram loss_n500, percent width(0.2) ///
	fcolor(navy*0.6) lcol(navy*1.2) lw(thick)  ///
  || histogram loss_n1000, percent width(0.2) ///
	fcolor(none) lcol(green*0.8) lw(thick) ///
  xlab(0(0.2)1.6, grid) ylab(0(10)50, grid) ///
  xtitle("Percent of days with losses over $loss%") ytitle("Percent") ///
  legend(rows(1) position(2) ring(0) region(lp(blank)) ///
  order(2 "n500" 1 "n1000" )) 
graph export "$output/ch05-figure-3-resample-hist-Stata.png",replace




***************************************
* BOOTSRTAP SAMPLES

* WARNING!!!!!! 
* IF using DROPBOX/BOX - need to switch off sync!!!!


clear
set seed 573164
global M=10000 /* # bootstrap draws*/

set obs 1
gen loss_$loss=.
save "$temp/sp500_bootstrap.dta",replace

qui forvalues i=1/$M {
	use "$temp/sp500_pctreturns.dta",replace
	bsample 
	gen loss_$loss=100*(pct_return<-$loss)
	collapse loss
	append using "$temp/sp500_bootstrap.dta"
	save "$temp/sp500_bootstrap.dta",replace
}

lab var loss "Percent of days with losses of $loss% or more"
tabstat loss, s(mean sd median min max n)


* Figure 5
histogram loss, freq width(0.04) ///
	fcol(navy*0.8) lcol(white) ///
	xlab(0(0.1)1.2, grid) ylab(0(200)1200, grid) 
graph export "$output/ch05-figure-5-bootstrap-Stata.png",replace



***************************************
* SE FORMULA

use "$temp/sp500_pctreturns.dta", replace

gen loss_$loss=100*(pct_return<-$loss)
sum loss

local sd=r(sd)
local n=r(N)

dis `sd'
dis `n'

local SE=1/sqrt(`n') * `sd'
dis `SE'


