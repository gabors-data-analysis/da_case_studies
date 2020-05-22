*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH 11 PROBABILITY MODELS
* weather australia
*

* v 1.0 2019-11-24 
* v 1.1 2020-03-10 change bins

*********************************************************************

* WHAT THIS CODES DOES:

* selects a single station, shows calibration curve

***

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * tex(frag) tbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in   "da_data_repo/australia-weather-forecasts/clean" 
global data_out	 "da_case_studies/ch11-australia-rainfall-predict" 
global output    "da_case_studies/ch11-australia-rainfall-predict/output" 


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

 line rain_prob_fc bin  bin,  lw(medium thin) lc(navy green) ///
 recast(connected) mcolor(%80) msize(medium) msymbol(circle none) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ///
 ytitle("Proportion rainy days") ///
 legend(label(1 "proportion rainy days") label(2 "45 degree line") rows(1)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\ch11-weather-calib.png", replace

