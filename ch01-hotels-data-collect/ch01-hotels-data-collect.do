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
global data_in  "${data_dir}/hotels-vienna/clean"
global work     "ch01-hotels-data-collect"
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
tempfile workfile
copy "https://osf.io/download/dn8je/" `workfile'
use `workfile', clear
*/


********************************************************************
* DATA STRUCTURE
********************************************************************

* Order variables for better overview
order hotel_id accommodation_type country city city_actual neighbourhood ///
      center1label distance center2label distance_alter stars rating ///
      rating_count ratingta ratingta_count year month weekend holiday ///
      nnights price scarce_room offer offer_cat

* Display summary statistics
summarize


********************************************************************
* EXPORT SAMPLE DATA
********************************************************************

* Export first 5 observations to Excel
export excel hotel_id accommodation_type country city city_actual ///
             center1label distance stars rating price ///
             using "${output}/hotel_listobs.xls" in 1/5, ///
             firstrow(variables) replace
