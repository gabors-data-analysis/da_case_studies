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
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Applied viridis colors instead of green/navy
*   - Already had as(png) in original
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


global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch08-hotels-measurement-error"

cap mkdir 		"$work/output"
global output 	"$work/output"




* Load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/dn8je/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

*** SAMPLE SELECTION

*** 3 to 4-star hotels (incl 3.5 stars)
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
label var distance "Distance to city center, miles"
drop if price>600 	/* likely error */


*** Drop hotels not really in Vienna
tab city_actual 
keep if city_actual=="Vienna"


gen lnprice = ln(price)
lab var lnprice "ln(Price)"

sum rating_count, d

* Define cutoffs
local k1 100
local k2 200

sum rating_count rating if rating_count <`k1'
sum rating_count rating if rating_count >=`k1' & rating_count <`k2' 
sum rating_count rating if rating_count >=`k2'
 
* FIGURE
reg lnprice rating if rating_count <`k1'
predict yhat1
lab var yhat1 "more noisy x: # ratings <`k1'"

reg lnprice rating if rating_count >=`k1' & rating_count <`k2' 
cap predict yhat2
cap lab var yhat2 "`k1' <= # ratings <`k2' "

reg lnprice rating if rating_count >=`k2' 
predict yhat3
lab var yhat3 "less noisy x: # ratings >`k2'"

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

graph export "$output/ch08-figure-8-hotels-measerror-Stata.png", as(png) replace

