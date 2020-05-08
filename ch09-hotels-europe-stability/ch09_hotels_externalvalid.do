*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* FUNDAMENTALS OF REGRESSION ANALYSIS
* ILLUSTRATION STUDY
* Hotel prices and distance to city center
*
* data downloaded from a hotels price comparison site on Oct 29, 2017

* 2.0 2019-11-02
* 2.1. 2020-02-04 added interaction term testb
*********************************************************************

* WHAT THIS CODES DOES:
* sets up data for some tasks
* input vienna_2017nov21.dta
* output textbook_ch2_hotels_Vienna_v2017.dta



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
*cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 
 *location folders
global data_in   	"da_data_repo/hotels-europe/clean"
global data_out  	"da_case_studies/ch09-hotels-europe-stability"
global output 		"da_case_studies/ch09-hotels-europe-stability/output"
 
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
saveold "$data_out/hotels_work.dta", replace


**********************************************************
* External validity by time
use "$data_out/hotels_work.dta",replace
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
 outreg2 using "$output/hotels_extval_time1", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes word replace
* other dates 
foreach d in "2017-NOV-weekend"  "2017-DEC-holiday" "2018-JUNE-weekend" { 
	reg lnprice dist_0_2 dist_2_7 if date=="`d'", robust
 outreg2 using "$output/hotels_extval_time1", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes word append
}


** same with hotels restricted to be the same
* first create variable that counts the number of times a hotel is in the data
egen hotelcount = count(price), by(hotel_id)
tab hotelcount
keep if hotelcount==4
* original regression
reg lnprice dist_0_2 dist_2_7 if date=="2017-NOV-weekday", robust
 outreg2 using "$output/hotels_extval_time2", se 2aster bdec(2) ctitle("2017-NOV-weekday") tex(frag) nonotes word replace
* other dates 
foreach d in "2017-NOV-weekend" "2017-DEC-holiday" "2018-JUNE-weekend" { 
	reg lnprice dist_0_2 dist_2_7 if date=="`d'", robust
 outreg2 using "$output/hotels_extval_time2", se 2aster bdec(2) ctitle("`d'") tex(frag) nonotes word append
}

* check interaction term p value
keep if date=="2017-NOV-weekday" | date=="2017-NOV-weekend"
gen we= date=="2017-NOV-weekend"
tab we
reg lnprice we c.dist_0_2##we c.dist_2_7##we , robust

 
**********************************************************
* External validity by city
use "$data_out/hotels_work.dta",replace
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
	outreg2 using "$output/hotels_extval_city1", se 2aster bdec(2) ctitle("Vienna") word tex(frag) nonotes replace
* two other cities
foreach c in Amsterdam Barcelona{
	reg lnprice dist_0_2 dist_2_7 if city=="`c'" , r
	outreg2 using "$output/hotels_extval_city1", se 2aster bdec(2) ctitle("`c'") word tex(frag) nonotes append
}

	
	
**********************************************************
* External validity by accommodation type: hotels vs apartments
use "$data_out/hotels_work.dta",replace
keep if city=="Vienna"
keep if date=="2017-NOV-weekday"
keep if stars>=3 & stars<=4
tab accommodation_type stars



tabstat distance , s(min max p50 mean n) by(stars)
tabstat price, s(min max p50 mean n) by(stars) format(%4.1f)
tabstat lnprice, s(min max p50 mean n) by(stars) format(%4.1f)

mkspline dist_0_2 2 dist_2_7 = distance


reg lnprice dist_0_2 dist_2_7 if accommodation=="Hotel"
	outreg2 using "$output/hotels_extval_type", se 2aster bdec(2) ctitle("Hotels") tex(frag) nonotes word replace
reg lnprice dist_0_2 dist_2_7 if  accommodation=="Apartment"
	outreg2 using "$output/hotels_extval_type", se 2aster bdec(2) ctitle("Apartments") tex(frag) nonotes word append



