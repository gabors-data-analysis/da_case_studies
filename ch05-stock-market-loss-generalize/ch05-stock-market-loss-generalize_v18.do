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
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


********************************************************************
* SETTING UP DIRECTORIES
********************************************************************

* STEP 1: set working directory for da_case_studies
* Example: cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global data_in  "${data_dir}/sp500/clean"
global work     "ch05-stock-market-loss-generalize"
global output   "${work}/output"
global temp     "${work}/temp"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"
capture mkdir "${temp}"


********************************************************************
* PARAMETER SETTINGS
********************************************************************

* Set level of loss to inquire (in percent)
global loss 5


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/SP500_2006_16_data.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile sp500_data
copy "https://osf.io/download/gds5t/" `sp500_data'
use `sp500_data', clear
*/


********************************************************************
* DATA PREPARATION
********************************************************************

* Create gap variable (days between observations)
generate gap = date - date[_n-1] - 1

* Label variables
label variable value "Value of the S&P500"
label variable datestr "Date, in string format (YMD)"
label variable date "Date"
label variable gap "Gap between observations, in days"

* Create year and month variables
generate year = year(date)
generate month = month(date)

* Order variables
order date datestr year month gap

* Save work file
save "${work}/work.dta", replace


********************************************************************
* EXPLORATORY DATA ANALYSIS
********************************************************************

use "${work}/work.dta", clear

* Time series graphs (not in textbook)
tsset date
tsline value, lw(thick) lc(navy*0.8) xla(, grid) yla(, grid)

* Calculate daily percent returns
sort date
generate pct_return = (value - value[_n-1]) / value[_n-1] * 100
tsline pct_return, lc(navy*0.8) xla(, grid) yla(, grid) yline(0)

* Collapse to yearly level
collapse (mean) value, by(year)
tsset year
tsline value, lw(thick) lc(navy*0.8) xla(, grid) yla(, grid)

* Calculate yearly percent returns
sort year
generate pct_return = (value - value[_n-1]) / value[_n-1] * 100
tsline pct_return, lw(thick) lc(navy*0.8) xla(, grid) yla(, grid) yline(0)


********************************************************************
* FIGURE 5.1: HISTOGRAM OF DAILY RETURNS
********************************************************************

use "${work}/work.dta", clear

* Create percent daily returns
sort date
generate pct_return = (value - value[_n-1]) / value[_n-1] * 100
label variable pct_return "Percent daily return"

* Set up viridis color
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

* Create histogram
histogram pct_return, ///
	freq start(-10) width(0.25) bstyle(background) ///
	fcol(`color1') ///
	xlab(-10 -5 -2 0 2 5 10, grid) ylab(, grid) ///
	xline(-${loss}, lc(green) lstyle(foreground) lw(thick)) ///
	text(220 -7 "5% loss -->") ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch05-figure-1-returns-histogram-Stata.png", replace

* Keep only necessary variables
keep date pct_return
save "${temp}/sp500_pctreturns.dta", replace


********************************************************************
* REPEATED SAMPLES ANALYSIS
********************************************************************

* Samples of 500 and 1000 observations (approximately 2 & 4 years)
clear
set matsize 10000
set seed 573164
global M = 10000

foreach n in 500 1000 {
	
	matrix define Results = J(${M}, 10, 0)
	
	quietly forvalues i = 1/${M} {
		use "${temp}/sp500_pctreturns.dta", replace
		sample `n', count
		forvalue j = 1/10 {
			generate loss`j'p = 100 * (pct_return < -`j')
			summarize loss`j'p
			matrix Results[`i', `j'] = r(mean)
		}
	}
	
	clear
	svmat Results, names(loss)
	tabstat loss*, s(mean median min max sd n)
	save "${temp}/repsamples`n'", replace
}


********************************************************************
* FIGURE 5.2: DISTRIBUTION OF LOSS ESTIMATES (n=1000)
********************************************************************

use "${temp}/repsamples1000", replace
tabstat loss${loss}, s(mean median min max sd n)
label variable loss${loss} "Percent of days with losses of ${loss}% or more"

* Calculate mean for reference line
quietly summarize loss${loss}
local mean_resamples1000 = r(mean)

* Create histogram
histogram loss${loss}, ///
	freq width(0.099) bstyle(background) ///
	fcol(navy*0.8) ///
	xlab(0(0.25)1.5, grid) ylab(0(500)2500, grid) ///
	xline(`mean_resamples1000', lc(green*0.8) lstyle(foreground) lw(thick)) ///
	text(2400 0.65 "<---- mean") ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch05-figure-2-resample1000-Stata.png", replace


********************************************************************
* FIGURE 5.3: COMPARISON OF n=500 vs n=1000
********************************************************************

clear

* Merge both sample sizes
use "${temp}/repsamples500", replace
keep loss${loss}
rename loss${loss} loss_n500

merge 1:1 _n using "${temp}/repsamples1000", nogen
keep loss${loss}* loss_n*
rename loss${loss} loss_n1000

summarize loss*

* Create overlayed histograms
twoway histogram loss_n500, percent width(0.2) ///
	fcolor(navy*0.6) lcol(navy*1.2) lw(thick) ///
	|| histogram loss_n1000, percent width(0.2) ///
	fcolor(none) lcol(green*0.8) lw(thick) ///
	xlab(0(0.2)1.6, grid) ylab(0(10)50, grid) ///
	xtitle("Percent of days with losses over ${loss}%") ytitle("Percent") ///
	legend(rows(1) position(2) ring(0) region(lp(blank)) ///
	order(2 "n500" 1 "n1000")) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch05-figure-3-resample-hist-Stata.png", replace


********************************************************************
* BOOTSTRAP SAMPLES
********************************************************************

* WARNING: If using DROPBOX/BOX - need to switch off sync!

clear
set seed 573164
global M = 10000

* Initialize bootstrap results file
set obs 1
generate loss_${loss} = .
save "${temp}/sp500_bootstrap.dta", replace

* Run bootstrap iterations
quietly forvalues i = 1/${M} {
	use "${temp}/sp500_pctreturns.dta", replace
	bsample
	generate loss_${loss} = 100 * (pct_return < -${loss})
	collapse loss
	append using "${temp}/sp500_bootstrap.dta"
	save "${temp}/sp500_bootstrap.dta", replace
}

label variable loss "Percent of days with losses of ${loss}% or more"
tabstat loss, s(mean sd median min max n)


********************************************************************
* FIGURE 5.5: BOOTSTRAP DISTRIBUTION
********************************************************************

histogram loss, ///
	freq width(0.04) ///
	fcol(navy*0.8) lcol(white) ///
	xlab(0(0.1)1.2, grid) ylab(0(200)1200, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch05-figure-5-bootstrap-Stata.png", replace


********************************************************************
* STANDARD ERROR FORMULA
********************************************************************

use "${temp}/sp500_pctreturns.dta", replace

* Calculate loss indicator
generate loss_${loss} = 100 * (pct_return < -${loss})
summarize loss

* Extract statistics
local sd = r(sd)
local n = r(N)

display `sd'
display `n'

* Calculate standard error
local SE = 1 / sqrt(`n') * `sd'
display `SE'
