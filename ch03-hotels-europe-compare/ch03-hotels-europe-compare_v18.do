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
* Chapter 03
* CH03B Comparing hotel prices in Europe: Vienna vs. London
* using the hotels-europe dataset
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
global data_in  "${data_dir}/hotels-europe/clean"
global work     "ch03-hotels-europe-compare"
global output   "${work}/output"
global temp     "${work}/temp"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"
capture mkdir "${temp}"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/hotels-europe_price", clear
merge m:m hotel_id using "${data_in}/hotels-europe_features.dta", nogen

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile prices_data
copy "https://osf.io/download/hz4gw/" `prices_data'
use `prices_data', clear

tempfile features_data
copy "https://osf.io/download/j9mkf/" `features_data'
merge m:m hotel_id using `features_data', nogen
*/


********************************************************************
* SAMPLE SELECTION
********************************************************************

* Keep November 2017 weekend data, 3-4 star hotels
keep if year==2017 & month==11 & weekend==0
keep if city== "Vienna" | city=="London"
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4

* Keep only hotels in actual city (removes extreme distances)
keep if city_actual== "Vienna" | city_actual=="London"

* Drop erroneous price observations
keep if price<1000

* Save working file
save "${temp}/hotels-vienna-london.dta", replace

* Check city distribution
tab city


********************************************************************
* FIGURE 3.6a: PRICE DISTRIBUTION - VIENNA
********************************************************************

* Set up viridis color
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

histogram price if city=="Vienna", ///
	width(20) percent ///
	xtitle("Price (US dollars)") ///
	color(`color1') lcol(white) lw(vthin) ///
	xlabel(0(50)500, grid) ylabel(0(10)30, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch03-figure-6a-hist-price-Vienna-Stata.png", replace


********************************************************************
* FIGURE 3.6b: PRICE DISTRIBUTION - LONDON
********************************************************************

colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

histogram price if city=="London", ///
	width(20) percent ///
	xtitle("Price (US dollars)") ///
	color(`color1') lcol(white) lw(vthin) ///
	xlabel(0(50)500, grid) ylabel(0(10)30, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch03-figure-6b-hist-price-London-Stata.png", replace


********************************************************************
* FIGURE 3.7: OVERLAYED DENSITY PLOTS
********************************************************************

* Generate kernel density estimates
kdensity price if city=="Vienna", gen(xV yV) nograph
kdensity price if city=="London", gen(xL yL) nograph

* Set up viridis color
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

* Create overlayed density plot
line yV yL xL, ///
	lc(`color1' `color1') lw(thick thick) ///
	xtitle("Price (US dollars)") ///
	xlabel(0(100)500, grid) ylabel(, grid) ///
	legend(off) ///
	text(0.007 180 "Vienna" 0.0025 340 "London") ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch03-figure-7-densities-price-ViennaLondon-Stata.png", replace


********************************************************************
* TABLE 3.6: SUMMARY STATISTICS BY CITY
********************************************************************

* Summary statistics by city
tabstat price, s(n mean median min max sd) by(city) format(%4.2f) save

* Calculate mean-median skewness statistic
* London
quietly summarize price if city=="London", detail
display (r(mean) - r(p50)) / r(sd)

* Vienna
quietly summarize price if city=="Vienna", detail
display (r(mean) - r(p50)) / r(sd)

* Export to LaTeX format
* Replace built-in skewness with (mean-median)/sd measure
tabout city using "${output}/ch03-table-6-summary-ViennaLondon-Stata.tex", ///
	replace style(tex) sum ///
	c(count price mean price median price min price max price sd price skewness price)
