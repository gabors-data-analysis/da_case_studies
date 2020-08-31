*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 03D
* Distributions of body height and income
*
* using the height-income-distributions dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch02-hotels-data-prep"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-vienna"
*global data_in   	"$da_data_repo/hotels-vienna"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/hotels-vienna"

* we'll use both clean and raw files in this case study
* separate data_in directpories for these two
* same for options 1 and 2 (once you have set $data_in)
global data_in_clean "$data_in/clean"
global data_in_raw "$data_in/raw"


*********************************************************************
* load in clean and tidy data and create workfile
use "$data_in_clean/hotels-vienna.dta", clear
keep hotel_id price accommodation_type distance stars rating rating_count

* look at accomodation types
tab accom



**********************************************
* Table 2.2
**********************************************

sort hotel_id
lis hotel_id price accommodation_type distance stars rating rating_count if _n==2


listtab hotel_id price accommodation_type distance stars rating rating_count if _n==2 ///
 using "$output/ch02-table2-hotel-list-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Hotel ID & Price & Accomodation &Distance & stars & rating & rating count \\"') ///
 foot("\end{tabular}")


**********************************************
* Table 2.3
**********************************************

keep if accommodation_type=="Hotel"
lis hotel_id price distance if _n<=3


listtab hotel_id price distance if _n<=3 ///
 using "$output/ch02-table3-hotel-simpletidy-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrr}" ///
 `"Hotel ID & Price  & Distance  \\"') ///
 foot("\end{tabular}")



*********************************************************
* PART B: repeat of the cleaning code
* includes some additional output
*********************************************************

*** IMPORT AND PREPARE DATA

* variables downoaded as string, often in form that is not helpful
* need to transform then to numbers that we can use

import delimited using "$data_in_raw/hotelbookingdata-vienna.csv", clear


* generate numerical variable of rating variable from string variable
*  trick: ignore charecters listed in option

*  distance to center entered as string in miles with one decimal
destring center1distance , generate(distance) ignore(" miles")
lab var distance "Distance to city center (miles)
destring center2distance , generate(distance_alter) ignore(" miles")
lab var distance "Distance to second center (miles)


split accommodationtype , p("@")
drop accommodationtype1 accommodationtype
rename accommodationtype2 accommodation_type

gen nnights=1
replace nnights = 4 if price_night=="price for 4 nights"
 lab var nnights "Number of nights"

 
 * ratings
* generate numerical variable of rating variable from string variable

gen rating = substr(guestreviewsrating,1,strpos(guestreviewsrating," ")-1)
destring rating, replace


* check: frequency table of all values incl. missing varlues as recignized by Stata
*  note same as in string format previously, except # missing here = # NA there

tab rating, missing
 
 tab rating_reviewcount, miss
 destring rating_reviewcount, gen(rating_count) force
 su rating_count

* RENAME VARIABLES
rename rating2_ta ratingta
rename rating2_ta_reviewcount ratingta_count
rename addresscountryname country
rename s_city city

 * look at key vars
rename starrating stars
tab stars
replace stars=. if stars==0

tab rating
codebook hotel_id
drop if hotel_id==.

drop center2distance center1distance price_night guestreviewsrating rating_reviewcount 

* Look for perfect duplicates

duplicates report
sort hotel_id
lis hotel_id if hotel_id==hotel_id[_n-1]

*********************************
* Table 2.10
*********************************
lis hotel_id accommodation_type price  distance stars rating rating_count ///
 if hotel_id==22050 | hotel_id==22185

 
listtab hotel_id accommodation_type price  distance stars rating rating_count if hotel_id==22050 | hotel_id==22185 ///
 using "$output/ch02-table10-hotel-duplicates-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Hotel ID & Price & Accomodation &Distance & stars & rating & rating count \\"') ///
 foot("\end{tabular}")

 
* these are perfect duplicates of the observation in the previous row
duplicates drop

drop if hotel_id==. | price==.
order hotel_id 



**********************************************
* Missing values in text
***********************************************
sum
count if rating_count ==.
gen misrating = rating_count ==.
tab misrating

tab accommodation_type  misrating
tab accommodation_type  misrating, sum(price) mean
lis hotel_id accommodation_type price  distance stars rating rating_count if misrating==1 & accommodation_type =="Hotel"


