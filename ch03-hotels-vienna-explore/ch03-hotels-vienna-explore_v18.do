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
* CH03A Finding a good deal among hotels: data exploration
* using the hotels-vienna dataset
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off  // Require full variable names for clarity


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
global work     "ch03-hotels-vienna-explore"
global output   "${work}/output"

* Create output directory if it doesn't exist
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

* Keep only hotels in Vienna
keep if city == "Vienna" 
keep if accommodation_type == "Hotel"

* Check sample size
count
display as text "Sample size: " as result r(N) " hotels"


********************************************************************
* FIGURE 3.1: DISTRIBUTION OF STAR RATINGS
********************************************************************

* Figure 3.1a - Percentage distribution of star ratings
colorpalette viridis, n(4) select(2) nograph

histogram stars, ///
	discrete percent ///
	xtitle("Star rating (number of stars)")  ///
	xlabel(1(0.5)5, grid format(%3.1f)) ///
	ylabel(0(10)50, grid)  ///
	fcolor(`r(p)') ///
	gap(5) ///
	lcolor(white) ///
	lwidth(vthin) ///
	addlabel ///
	addlabopts(mlabsize(medium) yvarformat(%3.1f))  ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-1a-hist-stars-Stata.png", replace

 
* Figure 3.1b - Frequency distribution
colorpalette viridis, n(4) select(2) nograph

histogram stars, ///
	discrete frequency ///
	xtitle("Star rating (number of stars)")  ///
	fcolor(`r(p)') ///
	gap(5) ///
	lcolor(white) ///
	lwidth(vthin) ///
	xlabel(1(0.5)5, grid) ///
	ylabel(0(20)140, grid)  ///
	addlabel ///
	addlabopts(mlabsize(medium) yvarformat(%3.0f))  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-1b-hist-stars-Stata.png", replace


********************************************************************
* FIGURE 3.2 & 3.3: PRICE DISTRIBUTIONS
********************************************************************

* Reload data for price analysis
use "${data_in}/hotels-vienna.dta", clear

* Alternative: Download from OSF (uncomment if needed)
/*
tempfile hotels_data
copy "https://osf.io/download/dn8je/" `hotels_data'
use `hotels_data', clear
*/

* Sample selection: 3-4 star hotels, exclude extreme prices
keep if accommodation_type == "Hotel"
keep if stars >= 3 & stars <= 4
keep if price < 1000

* Check sample size after filters
qui count
display as text "Sample size after filters: " as result r(N)

* Descriptive statistics
tab city
tab stars 

* To export table to Word/Excel/LaTeX (Stata 17+ only):
* collect style cell, nformat(%9.0fc)
* collect export "${output}/ch03-table-city-stars.docx", replace


* Figure 3.2a - Price histogram, binwidth=1
colorpalette viridis, n(4) select(2) nograph

histogram price, ///
	discrete frequency ///
	xtitle("Price (US dollars)")  ///
	color(`r(p)') ///
	xlabel(0(50)500, grid) ///
	ylabel(0(2)8, grid)  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-2a-hist-price-Stata.png", replace


* Figure 3.2b - Price histogram, binwidth=10
colorpalette viridis, n(4) select(2) nograph

histogram price, ///
	width(10) frequency ///
	xtitle("Price (US dollars)")  ///
	color(`r(p)') ///
	lcolor(white) ///
	lwidth(vthin) ///
	xlabel(0(50)500, grid) ///
	ylabel(0(5)40, grid)  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-2b-hist-price-Stata.png", replace


* Figure 3.3a - Price histogram, binwidth=40
colorpalette viridis, n(4) select(2) nograph

histogram price, ///
	width(40) start(40) frequency ///
	xtitle("Price (US dollars)")  ///
	color(`r(p)') ///
	lcolor(white) ///
	lwidth(vthin) ///
	xlabel(0(80)500, grid) ///
	ylabel(0(20)120, grid)  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-3a-hist-price-Stata.png", replace


* Figure 3.3b - Price histogram, binwidth=80
colorpalette viridis, n(4) select(2) nograph

histogram price, ///
	width(80) start(0) frequency ///
	xtitle("Price (US dollars)")  ///
	color(`r(p)') ///
	lcolor(white) ///
	lwidth(vthin) ///
	xlabel(0(80)500, grid) ///
	ylabel(0(50)150, grid)  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-3b-hist-price-Stata.png", replace


********************************************************************
* FIGURE 3.4 & 3.5: DISTANCE TO CITY CENTER
********************************************************************

* Figure 3.4 - Distance histogram
colorpalette viridis, n(4) select(2) nograph

histogram distance, ///
	width(0.5) frequency ///
	xtitle("Distance to city center (miles)")  ///
	color(`r(p)') ///
	lcolor(white) ///
	lwidth(vthin) ///
	xlabel(0(2)14, grid) ///
	ylabel(0(10)60, grid)  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch03-figure-4-hist-dist-Stata.png", replace


* Identify hotels too far from center
count if distance > 8
display as text "Hotels more than 8 miles from center: " as result r(N)

* Drop distant hotels and verify city location
drop if distance > 8

* Check city distribution
tab city_actual  

* To export this table (Stata 17+ only):
* collect export "${output}/ch03-table-city-actual.docx", replace

keep if city_actual == "Vienna"
count
display as text "Final sample size: " as result r(N) " hotels"


********************************************************************
* END OF SCRIPT
********************************************************************

* Log results
display as text _newline(2) "Analysis complete!"
display as text "Output saved to: ${output}"
display as text "Stata version: " as result c(stata_version)
display as text "Date: " as result c(current_date)
