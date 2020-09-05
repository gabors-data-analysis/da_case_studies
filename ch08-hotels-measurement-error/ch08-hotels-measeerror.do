*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 08C
* Hotel ratings and measurement error
*
* using the hotels-vienna dataset
* 
* License: Free to share, modify and use for educational purposes. 
* Not to be used for commercial purposes
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch08-hotels-measurement-error"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-vienna/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/hotels-vienna/clean"


* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear



*** SAMPLE SELECTION

*** 3 to 4-star hotels (incl 3.5 stars)
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
label var distance "Distance to city center, miles"
drop if price>600 	/* likely error */


*** drop hotels not really in Vienna
tab city_actual 
keep if city_actual=="Vienna"


gen lnprice=ln(price)
	lab var lnprice "ln(Price)"

sum rating_count ,d

* define cutoffs
local k1 100
local k2 200

sum rating_count  rating if rating_count <`k1'
sum rating_count  rating if rating_count >=`k1' & rating_count <`k2' 
sum rating_count  rating if rating_count >=`k2'
 
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

line yhat1 yhat3 rating, lw(vthick vthick) lc(green*0.8 navy*0.8) lp(solid solid)  ///
	xtitle("Average rating") ytitle("ln(Hotel price, US dollars)") ///
	ylab(3.5(0.5)5.0, grid) xlab(2.0(0.5)5, grid) ///
	legend(off) ///
	text(4.3 2.55 "More noisy: n. of ratings<100", col(green*0.8)) ///
	text(3.7 3.1 "Less noisy: n. of ratings>200", col(navy*0.8)) ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-8-hotels-measerror-Stata.png", as(png) replace

