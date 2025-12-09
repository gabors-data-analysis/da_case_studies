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
* Chapter 11
* CH11A Predicting rainfall in Australia
* using the australia-weather-forecasts dataset
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
********************************************************************
* LOAD DATA
********************************************************************

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
global data_in  "${data_dir}/australia-weather-forecasts/clean"
global work     "ch11-australia-rainfall-predict"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"





clear
import delimited "${data_in}/rainfall_australia.csv"

* Or download directly from OSF:
/*
copy "https://osf.io/download/kdva8/" "workfile.csv"
import delimited "workfile.csv"
erase "workfile.csv"
*/ 

*drop if station_name =="MARREE AERO"
keep if station_name=="DARWIN AIRPORT"
compress

tabulate bd_fc_before_start
keep if bd_fc_before_start == 39
generate pred_time = "2-day forecast"

* Darwin well calibrated overall
su prob daily_sum 
generate rain_prob_fc = prob/100
generate rain = daily_sum

egen bin = cut(rain_prob_fc), at(0(0.1)1)
replace bin = bin + 0.05
format bin %3.2f
replace bin = 0 if rain_prob_fc==0
tabstat rain_prob_fc, by(bin) s(min max n)

format bin %3.1f
collapse rain rain_prob_fc, by(bin)

scatter rain bin bin, ///
 connect(l l) ///
 lw(thick medium) ///
 lc(navy*0.8 green*0.6) ///
 mcolor(navy*0.8) ///
 msize(medium) ///
 msymbol(O none) ///
 xla(0(0.1)1, grid) ///
 yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probabilities") ///
 ytitle("Proportion rainy days") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch11-figure-6-weather-calib-Stata.png", replace as(png)

