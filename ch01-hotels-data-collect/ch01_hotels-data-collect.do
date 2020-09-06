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
* Chapter 01
* CH01A Finding a good deal among hotels: data collection
* using the hotels-vienna dataset
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

global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch01-hotels-data-collect"

cap mkdir 		"$work/output"
global output 	"$work/output"


* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear
order hotel_id accommodation_type country city city_actual neighbourhood center1label distance center2label distance_alter stars rating rating_count ratingta ratingta_count year month weekend holiday nnights price scarce_room offer offer_cat
sum

* export list
export excel hotel_id accommodation_type country city city_actual center1label distance stars rating price using "$output\hotel_listobs.xls" in 1/5, firstrow(variables) replace

