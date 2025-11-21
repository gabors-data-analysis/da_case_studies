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
* Chapter 10
* CH10A Finding a good deal among hotels with multiple regression
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
global work     "ch10-hotels-multiple-reg"
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

* Keep 3 to 4-star hotels (including 3.5 stars)
keep if stars >= 3 & stars <= 4
keep if accommodation_type == "Hotel"
label var distance "Distance to city center, miles"

* Drop likely error (price > $600)
drop if price > 600

* Keep only hotels actually in Vienna
tab city_actual

keep if city_actual == "Vienna"

count
display as text "Final sample size: " as result r(N) " hotels"


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* Log price
gen lnprice = ln(price)
label var lnprice "ln(Price)"

* Piecewise linear spline for distance (knots at 1 and 4 miles)
mkspline distsp1 1 distsp2 4 distsp3 = distance

* Piecewise linear spline for rating (knot at 3.5)
mkspline ratingsp1 3.5 ratingsp2 = rating

* Star rating: binary indicators
gen star35 = (stars == 3.5)
gen star4 = (stars == 4)

* Summary statistics
tabstat price distance lnprice, ///
    statistics(mean sd min p25 p50 p75 max n) columns(statistics)


********************************************************************
* BASIC REGRESSIONS
********************************************************************

* Distance only
reg lnprice distance, robust
outreg2 using "${output}/ch10-table-1-hotels-basic-Stata.tex", ///
    tex(fragment) excel bdec(3) replace

* Rating only  
reg lnprice rating, robust
outreg2 using "${output}/ch10-table-1-hotels-basic-Stata.tex", ///
    tex(fragment) excel bdec(3) append

* Distance and rating
reg lnprice distance rating, robust
outreg2 using "${output}/ch10-table-1-hotels-basic-Stata.tex", ///
    tex(fragment) excel bdec(2) append

* Auxiliary regression: distance on rating
reg distance rating, robust
outreg2 using "${output}/ch10-table-1-hotels-basic-Stata.tex", ///
    tex(fragment) excel bdec(2) append


********************************************************************
* MULTIPLE REGRESSION WITH SPLINES
********************************************************************

* Full model with splines and star indicators
reg lnprice distsp1 distsp2 distsp3 star35 star4 ratingsp1 ratingsp2, robust
predict lnprice_hat
predict lnprice_resid, residual

* Compare R-squared: distance splines only vs. full model
reg lnprice distsp1 distsp2 distsp3, robust


********************************************************************
* IDENTIFY BEST DEALS
********************************************************************

* Sort by residuals (lowest = best deals)
sort lnprice_resid

* Format for display
format lnprice_resid %5.3f
format distance %3.1f

* List top 5 best deals
display as text _newline "Top 5 Best Hotel Deals:"
list hotel_id price lnprice_resid distance stars rating if _n <= 5

* Export to LaTeX table
listtex hotel_id price lnprice_resid distance stars rating ///
    using "${output}/ch10-table-6-hotels-good-deals-Stata.tex" ///
    if _n <= 5, replace ///
    headlines("\begin{tabular}{l c c c c c}" ///
    "\hline" ///
    "Hotel name & Price & Residual & Distance & Stars & Rating \\" ///
    "\hline") ///
    footlines("\hline" "\end{tabular}") ///
    rstyle(tabular)


********************************************************************
* FIGURE 3: PREDICTED VS ACTUAL (Y-HAT VS Y)
********************************************************************

* Create scatter plot with best deals highlighted
sort lnprice_resid

scatter lnprice lnprice_hat if _n > 5, ///
    msize(medium) mcolor(navy*0.6) ///
    || scatter lnprice lnprice_hat if _n <= 5, ///
    msize(medium) mcolor(black) ///
    || line lnprice lnprice, sort lwidth(thick) lcolor(green*0.6) ///
    lpattern(dash) ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    xlabel(3.75(0.25)6, grid) ylabel(3.75(0.25)6, grid) ///
    legend(off) ///
    ytitle("ln(price, US dollars)") ///
    xtitle("Predicted ln(price, US dollars)") ///
    text(4.1 4.84 "Best deal")
graph export "${output}/ch10-figure-3-hotels-yhat-y-Stata.png", replace


