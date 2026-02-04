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
* Chapter 02
* CH02A Finding a good deal among hotels: data preparation
* using the hotels-vienna dataset
* version 1.1 2025-12-09
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
global data_in  "${data_dir}/hotels-vienna"
global work     "ch02-hotels-data-prep"
global output   "${work}/output"

* We'll use both clean and raw files in this case study
* Separate data_in directories for these two
global data_in_clean "${data_in}/clean"
global data_in_raw "${data_in}/raw"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"



*********************************************************************
* PART A: Load clean data and create workfile
*********************************************************************

use "${data_in_clean}/hotels-vienna.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/dn8je/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

keep hotel_id price accommodation_type distance stars rating rating_count

* Look at accommodation types
tabulate accommodation_type


**********************************************
* Table 2.2: Single hotel observation example
**********************************************

sort hotel_id
list hotel_id price accommodation_type distance stars rating rating_count if _n == 2


listtab hotel_id price accommodation_type distance stars rating rating_count if _n == 2 ///
 using "${output}/ch02-table2-hotel-list-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Hotel ID & Price & Accomodation &Distance & stars & rating & rating count \\"') ///
 foot("\end{tabular}")


**********************************************
* Table 2.3: First three hotels after filtering
**********************************************

keep if accommodation_type == "Hotel"
count
list hotel_id price distance if _n <= 3

listtab hotel_id price distance if _n <= 3 ///
 using "${output}/ch02-table3-hotel-simpletidy-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrr}" ///
 `"Hotel ID & Price  & Distance  \\"') ///
 foot("\end{tabular}")



*********************************************************
* PART B: Repeat part of the cleaning code
* Using the raw CSV data file
*********************************************************

* Variables downloaded as string, often in form that is not helpful
* Need to transform them to numbers that we can use

import delimited using "${data_in_raw}/hotelbookingdata-vienna.csv", clear

* Or download directly from OSF:

/*
copy "https://osf.io/g5dmw/download" "workfile.csv"
import delimited "workfile.csv", clear
erase "workfile.csv"
*/

* Distance to center entered as string in miles with one decimal
destring center1distance, generate(distance) ignore(" miles")
label variable distance "Distance to city center (miles)"
destring center2distance, generate(distance_alter) ignore(" miles")
label variable distance_alter "Distance to second center (miles)"

* Parse accommodation type (format: "something@Hotel")
split accommodationtype, p("@")
drop accommodationtype1 accommodationtype
rename accommodationtype2 accommodation_type

* Number of nights
generate nnights = 1
replace nnights = 4 if price_night=="price for 4 nights"
label variable nnights "Number of nights"

 
* Generate numerical variable of rating from string variable
generate rating = substr(guestreviewsrating, 1, strpos(guestreviewsrating, " ") - 1)
destring rating, replace

* Check: frequency table of all values including missing values
tabulate rating, missing
 
tabulate rating_reviewcount, miss
destring rating_reviewcount, gen(rating_count) force
su rating_count

* Rename variables
rename rating2_ta ratingta
rename rating2_ta_reviewcount ratingta_count
rename addresscountryname country
rename s_city city

* Clean star ratings
rename starrating stars
tabulate stars
replace stars = . if stars==0

* Check and clean hotel IDs
tabulate rating
codebook hotel_id
drop if hotel_id==.

* Drop original string variables that have been converted
drop center2distance center1distance price_night guestreviewsrating rating_reviewcount 

* Look for perfect duplicates
duplicates report
sort hotel_id
list hotel_id if hotel_id==hotel_id[_n-1]

*********************************
* Table 2.10: Example duplicates
*********************************
list hotel_id accommodation_type price  distance stars rating rating_count ///
 if hotel_id == 22050 | hotel_id == 22185

 
listtab hotel_id accommodation_type price  distance stars rating rating_count if hotel_id == 22050 | hotel_id == 22185 ///
 using "${output}/ch02-table10-hotel-duplicates-Stata.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Hotel ID & Price & Accomodation &Distance & stars & rating & rating count \\"') ///
 foot("\end{tabular}")

 
* These are perfect duplicates of the observation in the previous row
duplicates drop

drop if hotel_id==. | price==.
order hotel_id 



**********************************************
* Missing values analysis
***********************************************
summarize
count if rating_count ==.
generate misrating = rating_count ==.
tabulate misrating

tabulate accommodation_type misrating
tabulate accommodation_type misrating, sum(price) mean
list hotel_id accommodation_type price distance stars rating rating_count ///
     if misrating==1 & accommodation_type =="Hotel"

