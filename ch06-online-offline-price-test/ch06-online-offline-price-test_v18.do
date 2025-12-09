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
* Chapter 06
* CH06A Comparing online and offline prices: testing the difference
* using the billion-prices dataset
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
global data_in  "${data_dir}/billion-prices/clean"
global work     "ch06-online-offline-price-test"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/online_offline_ALL_clean.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile prices_data
copy "https://osf.io/download/wm6ge/" `prices_data'
use `prices_data', clear
*/


********************************************************************
* SAMPLE SELECTION
********************************************************************

* Check country distribution
tabulate COUNTRY

* Keep only USA
keep if COUNTRY == "USA"

* Keep only regular prices (not promotional)
keep if PRICETYPE == "Regular Price"

* Drop items on sale online
drop if sale_online == 1

* Drop observations with missing prices
drop if price==.
drop if price_online==.

* Check price distribution
summarize price*, d

* Drop obvious errors (3 observations with price > $1000)
drop if price>1000

* Final price summary
summarize price*

count
display as text "Final sample size: " as result r(N) " items"


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* Calculate price difference (online - offline)
generate pd = price_online - price
label variable pd "Online - offline price difference (USD)"

* Summary statistics for price difference
tabstat pd, s(mean sd min median max n)

* Count different types of price differences
count if pd==0
display as text "Items with same price online and offline: " as result r(N)

count if pd>0
display as text "Items more expensive online: " as result r(N)

count if pd<0
display as text "Items less expensive online: " as result r(N)

count if pd>=-1 & pd<=1
display as text "Items with price difference within $1: " as result r(N)


********************************************************************
* FIGURE 6.1: DISTRIBUTION OF PRICE DIFFERENCES
********************************************************************

* Set up viridis color
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'


* Figure 6.1a - Full distribution of price differences
histogram pd, ///
	freq start(-400) width(5) ///
	col("`color1'") ///
	xlab(-400(100)400, grid) ///
	ylab(, grid) ///
	xtitle("Online - offline price difference (USD)") ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch06-figure-1a-pricediff-Stata.png", replace


* Figure 6.1b - Zoomed-in distribution (±$5)
histogram pd if pd>-5 & pd<5, ///
	freq start(-5) width(0.5) ///
	col("`color1'") lcol(white) ///
	xlab(-5(1)5, grid) ///
	ylab(0(1000)5000, grid) ///
	xtitle("Online - offline price difference (USD)") ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch06-figure-1b-pricediff-Stata.png", replace


********************************************************************
* HYPOTHESIS TESTING
********************************************************************

* Test whether price difference is zero (H0: pd = 0)
ttest pd = 0 	

display as text _newline "T-test results:"
display as text "Mean price difference: " as result %6.3f r(mu_1)
display as text "t-statistic: " as result %6.3f r(t)
display as text "p-value: " as result %6.4f r(p)


********************************************************************
* MULTIPLE HYPOTHESIS TESTS BY RETAILER
********************************************************************

* Test price difference by retailer
sort retailer
by retailer: ttest pd = 0 
