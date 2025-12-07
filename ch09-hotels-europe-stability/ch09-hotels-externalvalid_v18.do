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
* Chapter 09
* CH09B External validity: Comparing hotel price models across cities
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
global work     "ch09-hotels-europe-stability"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD AND MERGE DATA
********************************************************************


* Load in clean and tidy data and create workfile
use "${data_in}/hotels-europe_price", clear
merge m:m hotel_id using "${data_in}/hotels-europe_features.dta"

* Or download directly from OSF:
/*
copy "https://osf.io/download/hz4gw/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
preserve
	copy "https://osf.io/download/j9mkf/" "workfile.dta"
	use "workfile.dta", clear
	erase "workfile.dta"
	tempfile hotels_features
	save `hotels_features'
restore
merge m:m hotel_id using `hotels_features', nogen
*/

drop _merge
label variable distance "Distance to city center, miles"

********************************************************************
* SAMPLE SELECTION
********************************************************************

* Filter a few cities
* keep if city_actual==city
keep if inlist(city_actual, "Vienna", "Amsterdam", "Barcelona")
keep if inlist(accommodation_type, "Hotel", "Apartment")

* Drop long stay, 1000E+
drop if nnights==4
drop if price>1000

* Check for duplicates
duplicates report
duplicates drop



********************************************************************
* CREATE DATE VARIABLE
********************************************************************

generate date = ""
replace date = "2017-NOV-weekday" if month==11 & weekend==0
replace date = "2017-NOV-weekend" if month==11 & weekend==1
replace date = "2017-DEC-holiday" if month==12 & holiday==1
replace date = "2018-JUNE-weekend" if month==6 & weekend==1
drop if date==""

count

tabulate city
tabulate accommodation_type city
tabulate date 



********************************************************************
* FEATURE ENGINEERING
********************************************************************

generate lnprice = ln(price)
label variable lnprice "ln(Price)"

keep hotel_id date city accommodation_type stars rating distance price lnprice


********************************************************************
* SAVE WORK FILE
********************************************************************

* Save work file
saveold "${work}/hotels_work.dta", replace


********************************************************
********************************************************************
* EXTERNAL VALIDITY BY TIME
********************************************************************
use "${work}/hotels_work.dta", replace
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
keep if city=="Vienna"
tabulate date

tabstat distance, s(min max p50 mean n) by(date)
tabstat price, s(min max p50 mean n) by(date) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(city) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


* Regressions with three dates for textbook
* Original regression
regress lnprice dist_0_2 dist_2_7 if date=="2017-NOV-weekday", robust
outreg2 using "${output}/hotels_extval_time1", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes replace

* Other dates 
foreach d in "2017-NOV-weekend" "2017-DEC-holiday" "2018-JUNE-weekend" { 
	reg lnprice dist_0_2 dist_2_7 if date=="`d'", robust
	outreg2 using "${output}/hotels_extval_time1", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes append
}


** Same with hotels restricted to be the same
* First create variable that counts the number of times a hotel is in the data
egen hotelcount = count(price), by(hotel_id)
tabulate hotelcount
keep if hotelcount==4

* Original regression
regress lnprice dist_0_2 dist_2_7 if date=="2017-NOV-weekday", robust
outreg2 using "${output}/hotels_extval_time2", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes replace

* Other dates 
foreach d in "2017-NOV-weekend" "2017-DEC-holiday" "2018-JUNE-weekend" { 
	regress lnprice dist_0_2 dist_2_7 if date=="`d'", robust
	outreg2 using "${output}/hotels_extval_time2", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes append
}

* Check interaction term p value
keep if date=="2017-NOV-weekday" | date=="2017-NOV-weekend"
generate we = date=="2017-NOV-weekend"
tabulate we
regress lnprice we c.dist_0_2##we c.dist_2_7##we, robust

 
********************************************************
* External validity by city
use "${work}/hotels_work.dta", replace
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
keep if date=="2017-NOV-weekday"

tabstat distance, s(min max p50 mean n) by(city)
tabstat price, s(min max p50 mean n) by(city) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(city) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


* Regressions for three cities
* Original regression: Vienna
regress lnprice dist_0_2 dist_2_7 if city=="Vienna", r
outreg2 using "${output}/hotels_extval_city", se 2aster bdec(2) ctitle("Vienna") tex(frag) nonotes replace

* Two other cities
foreach c in Amsterdam Barcelona {
	regress lnprice dist_0_2 dist_2_7 if city=="`c'", r
	outreg2 using "${output}/hotels_extval_city", se 2aster bdec(2) ctitle("`c'") tex(frag) nonotes append
}

		
		
********************************************************
* External validity by accommodation type: hotels vs apartments
use "${work}/hotels_work.dta", replace
keep if city=="Vienna"
keep if date=="2017-NOV-weekday"
keep if stars>=3 & stars<=4
tabulate accommodation_type stars



tabstat distance, s(min max p50 mean n) by(stars)
tabstat price, s(min max p50 mean n) by(stars) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(stars) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


regress lnprice dist_0_2 dist_2_7 if accommodation_type=="Hotel"
outreg2 using "${output}/hotels_extval_type", se 2aster bdec(2) ctitle("Hotels") tex(frag) nonotes replace

regress lnprice dist_0_2 dist_2_7 if accommodation_type=="Apartment"
outreg2 using "${output}/hotels_extval_type", se 2aster bdec(2) ctitle("Apartments") tex(frag) nonotes append

