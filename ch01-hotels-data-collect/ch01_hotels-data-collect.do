**********************************************
* Chapter 01
*
* hotels-vienna
* v1.2 
**********************************************


* set the path
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

*location folders
global data_in   "da_data_repo/hotels-vienna/clean"
global data_out  "da_case_studies/ch01-hotels-data-collect"
global output    "da_case_studies/ch01-hotels-data-collect/output"

* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear
order hotel_id accommodation_type country city city_actual neighbourhood center1label distance center2label distance_alter stars rating rating_count ratingta ratingta_count year month weekend holiday nnights price scarce_room offer offer_cat
sum

* export list
export excel hotel_id accommodation_type country city city_actual center1label distance stars rating price using "$output\hotel_listobs.xls" in 1/5, firstrow(variables) replace

