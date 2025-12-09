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
* Chapter 08
* CH08C Measurement error in hotel ratings
* using the hotels-vienna dataset
* version 1.1 2025-12-09
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
global data_in  "${data_dir}/hotels-vienna/clean"
global work     "ch08-hotels-measurement-error"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/hotels-vienna.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile hotels_data
copy "https://osf.io/download/dn8je/" `hotels_data'
use `hotels_data', clear
*/


********************************************************************
* SAMPLE SELECTION
********************************************************************

* 3 to 4-star hotels (including 3.5 stars)
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
label variable distance "Distance to city center, miles"
drop if price>600 	/* likely error */

* Drop hotels not really in Vienna
tabulate city_actual 
keep if city_actual=="Vienna"


********************************************************************
* FEATURE ENGINEERING
********************************************************************

generate lnprice = ln(price)
label variable lnprice "ln(Price)"


********************************************************************
* DESCRIPTIVE STATISTICS
********************************************************************

summarize rating_count, d


********************************************************************
* DEFINE CUTOFFS FOR RATING COUNT
********************************************************************

* Define cutoffs
local k1 100
local k2 200

summarize rating_count rating if rating_count <`k1'
summarize rating_count rating if rating_count >=`k1' & rating_count <`k2' 
summarize rating_count rating if rating_count >=`k2'
 

********************************************************************
* REGRESSION ANALYSIS BY RATING COUNT GROUPS
********************************************************************

* Regression for low rating count group
regress lnprice rating if rating_count <`k1'
predict yhat1
label variable yhat1 "more noisy x: # ratings <`k1'"

* Regression for medium rating count group
regress lnprice rating if rating_count >=`k1' & rating_count <`k2' 
capture predict yhat2
capture label variable yhat2 "`k1' <= # ratings <`k2' "

* Regression for high rating count group
regress lnprice rating if rating_count >=`k2' 
predict yhat3
label variable yhat3 "less noisy x: # ratings >`k2'"


********************************************************************
* FIGURE 8.8: MEASUREMENT ERROR IN RATINGS
********************************************************************

* Set up viridis colors
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p1)'
colorpalette viridis, n(4) select(3) nograph
local color2 `r(p1)'

line yhat1 yhat3 rating, ///
	lw(vthick vthick) ///
	lc("`color1'" "`color2'") ///
	lp(solid solid) ///
	xtitle("Average rating") ///
	ytitle("ln(Hotel price, US dollars)") ///
	ylab(3.5(0.5)5.0, grid) ///
	xlab(2.0(0.5)5, grid) ///
	legend(off) ///
	text(4.3 2.55 "More noisy: n. of ratings<100", col("`color1'")) ///
	text(3.7 3.1 "Less noisy: n. of ratings>200", col("`color2'")) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch08-figure-8-hotels-measerror-Stata.png", replace
