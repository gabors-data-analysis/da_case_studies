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
* CH11B Are Australian weather forecasts well calibrated?
* using the australia-weather-forecasts dataset
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


global data_in  "$data_dir/australia-weather-forecasts/clean"
global work  	"ch11-australia-rainfall-predict"

cap mkdir 		"$work/output"
global output 	"$work/output"




clear
import delimited "$data_in/rainfall_australia.csv"
*drop if station_name =="MARREE AERO"
keep if station_name=="DARWIN AIRPORT"
compress

tab bd_fc_before_start
keep if bd_fc_before_start == 39
gen pred_time="2-day forecast"

* Darwin well calibrated overall
su prob daily_sum 
gen rain_prob_fc=prob/100
gen rain=daily_sum

egen bin = cut(rain_prob_fc), at(0(0.1)1)
replace bin = bin+0.05
 format bin %3.2f
replace bin=0 if rain_prob_fc==0
tabstat rain_prob_fc, by(bin) s(min max n)

 format bin %3.1f
collapse rain rain_prob_fc, by(bin)

scatter rain bin  bin, connect(l l) lw(thick medium) lc(navy*0.8 green*0.6) ///
 mcolor(navy*0.8) msize(medium) msymbol(O none) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ytitle("Proportion rainy days") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-6-weather-calib-Stata.png", replace

