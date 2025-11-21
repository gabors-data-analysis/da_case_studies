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
* CH08A Finding a good deal among hotels with nonlinear function
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
global work     "ch08-hotels-nonlinear"
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

* SAMPLE SELECTION

* 3 to 4-star hotels (incl 3.5 stars)
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
label var distance "Distance to city center, miles"
drop if price>600 	/* likely error */


* Drop hotels not really in Vienna
tab city_actual 
keep if city_actual=="Vienna"


********************************************************************
* SAVE WORK FILE
********************************************************************

* Save work file
save "${work}/hotels_work.dta", replace





********************************************************************
* FEATURE ENGINEERING
********************************************************************

* SCATTERPLOT + REGRESSION LINE


gen lnprice = ln(price)
lab var lnprice "ln(price)"
count
count if distance==0
gen lndistance = ln(distance)
replace lndistance = ln(distance+0.05) if distance==0
lab var lndistance "ln(distance to city center)"

		

********************************************************************
* REGRESSION MODELS COMPARISON
********************************************************************

* Run and compare regressions	
reg price distance, r
outreg2 using "${output}/T08_reg1.tex", label bdec(2) tex(frag) nose noaster replace

reg lnprice distance, r
outreg2 using "${output}/T08_reg1.tex", label bdec(2) tex(frag) nose noaster append

reg price lndistance, r
outreg2 using "${output}/T08_reg1.tex", label bdec(2) tex(frag) nose noaster append

reg lnprice lndistance
outreg2 using "${output}/T08_reg1.tex", label bdec(2) tex(frag) nose noaster append


********************************************************************
* FIGURE 8.1: SCATTERPLOTS WITH LINEAR FIT
********************************************************************

* Set up viridis colors
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p1)'
colorpalette viridis, n(4) select(3) nograph
local color2 `r(p1)'
		
scatter price distance, ///
 ms(O) msize(small) mlw(thick) mcolor("`color1'") ///
 xlab(0(1)7, grid) ///
 ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price(US dollars)") ///
 || lfit price distance, ///
 lw(thick) lc("`color2'") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch08-figure-1a-hotel-levlev-Stata", as(png) replace


scatter lnprice distance, ///
 ms(O) msize(small) mlw(thick) mcolor("`color1'") ///
 xlab(0(1)7, grid) ///
 ylab(3.5(0.50)6, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("ln(hotel price in US dollars)") ///
 || lfit lnprice distance, ///
 lw(thick) lc("`color2'") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch08-figure-1b-hotel-loglev-Stata", as(png) replace

scatter price lndistance, ///
 ms(O) msize(small) mlw(thick) mcolor("`color1'") ///
 xlab(-2.5(0.5)2, grid) ///
 ylab(000(50)400, grid) ///
 xtitle("ln(distance to city center, miles)") ///
 ytitle("Hotel price (US dollars) ") ///
 || lfit price lndistance, ///
 lw(thick) lc("`color2'") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch08-figure-1c-hotel-levlog-Stata", as(png) replace


scatter lnprice lndistance, ///
 ms(O) msize(small) mlw(thick) mcolor("`color1'") ///
 xlab(-3(0.5)2, grid) ///
 ylab(3.5(0.50)6, grid) ///
 xtitle("ln(distance to city center, miles)") ///
 ytitle("ln(hotel price in US dollars)") ///
 || lfit lnprice lndistance, ///
 lw(thick) lc("`color2'") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch08-figure-1d-hotel-loglog-Stata", as(png) replace

