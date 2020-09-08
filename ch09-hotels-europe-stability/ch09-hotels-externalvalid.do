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
* CH09B How stable is the hotel price - distance to city center relathionship?
* using the hotels-europe dataset
* version 0.9 2020-09-06
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


global data_in  "$data_dir/hotels-europe/clean"
global work  	"ch09-hotels-europe-stability"

cap mkdir 		"$work/output"
global output 	"$work/output"



 
* Vienna vs London

* load in clean and tidy data and create workfile
use "$data_in/hotels-europe_price", clear
merge m:m hotel_id using "$data_in/hotels-europe_features.dta"
drop _m
label var distance "Distance to city center, miles"

* filter a few cities
* keep if city_actual==city
keep if inlist(city_actual, "Vienna", "Amsterdam", "Barcelona")
keep if inlist(accommodation_type, "Hotel", "Apartment")

	* drop long stay , 1000E+
drop if nnights==4
drop if price>1000

* check for duplicates
duplicates report
duplicates drop


gen date=""
replace date = "2017-NOV-weekday" if month==11 & weekend==0
replace date = "2017-NOV-weekend" if month==11 & weekend==1
replace date = "2017-DEC-holiday" if month==12 & holiday==1
replace date = "2018-JUNE-weekend" if month==6 & weekend==1
drop if date==""

count

tab city
tab accommodation_type city
tab date 


gen lnprice=ln(price)
lab var lnprice "ln(Price)"

keep hotel_id date city accommodation_type stars rating distance price lnprice

* save work file
saveold "$work/hotels_work.dta", replace


**********************************************************
* External validity by time
use "$work/hotels_work.dta",replace
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
keep if city=="Vienna"
tab date

tabstat distance , s(min max p50 mean n) by(date)
tabstat price, s(min max p50 mean n) by(date) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(city) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


*** Regressions with three dates for textbook
* original regression
reg lnprice dist_0_2 dist_2_7 if date=="2017-NOV-weekday", robust
 outreg2 using "$output/hotels_extval_time1", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes  replace
* other dates 
foreach d in "2017-NOV-weekend"  "2017-DEC-holiday" "2018-JUNE-weekend" { 
	reg lnprice dist_0_2 dist_2_7 if date=="`d'", robust
 outreg2 using "$output/hotels_extval_time1", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes  append
}


** same with hotels restricted to be the same
* first create variable that counts the number of times a hotel is in the data
egen hotelcount = count(price), by(hotel_id)
tab hotelcount
keep if hotelcount==4
* original regression
reg lnprice dist_0_2 dist_2_7 if date=="2017-NOV-weekday", robust
 outreg2 using "$output/hotels_extval_time2", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes  replace
* other dates 
foreach d in "2017-NOV-weekend" "2017-DEC-holiday" "2018-JUNE-weekend" { 
	reg lnprice dist_0_2 dist_2_7 if date=="`d'", robust
 outreg2 using "$output/hotels_extval_time2", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes  append
}

* check interaction term p value
keep if date=="2017-NOV-weekday" | date=="2017-NOV-weekend"
gen we= date=="2017-NOV-weekend"
tab we
reg lnprice we c.dist_0_2##we c.dist_2_7##we , robust

 
**********************************************************
* External validity by city
use "$work/hotels_work.dta",replace
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
keep if date=="2017-NOV-weekday"

tabstat distance , s(min max p50 mean n) by(city)
tabstat price, s(min max p50 mean n) by(city) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(city) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


*** Regressions for three cities
* original regression: Vienna
reg lnprice dist_0_2 dist_2_7 if city=="Vienna" , r
	outreg2 using "$output/hotels_extval_city", se 2aster bdec(2) ctitle("Vienna")  tex(frag) nonotes replace
* two other cities
foreach c in Amsterdam Barcelona{
	reg lnprice dist_0_2 dist_2_7 if city=="`c'" , r
	outreg2 using "$output/hotels_extval_city", se 2aster bdec(2) ctitle("`c'")  tex(frag) nonotes append
}

	
	
**********************************************************
* External validity by accommodation type: hotels vs apartments
use "$work/hotels_work.dta",replace
keep if city=="Vienna"
keep if date=="2017-NOV-weekday"
keep if stars>=3 & stars<=4
tab accommodation_type stars



tabstat distance , s(min max p50 mean n) by(stars)
tabstat price, s(min max p50 mean n) by(stars) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(stars) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


reg lnprice dist_0_2 dist_2_7 if accommodation=="Hotel"
	outreg2 using "$output/hotels_extval_type", se 2aster bdec(2) ctitle("Hotels") tex(frag) nonotes  replace
reg lnprice dist_0_2 dist_2_7 if  accommodation=="Apartment"
	outreg2 using "$output/hotels_extval_type", se 2aster bdec(2) ctitle("Apartments") tex(frag) nonotes  append



