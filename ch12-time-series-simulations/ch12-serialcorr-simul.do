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
* Chapter 12
* Simulating time series with various levels of serial correlation
* no actual data used
* version 0.9 2020-09-12
********************************************************************


* SETTING UP DIRECTORIES

* set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch12-time-series-simulations"

cap mkdir 		"$work/output"
global output 	"$work/output"

clear
set seed 2016
set obs 100
global rho=0.8
global sde=0.5

gen t=_n
tsset t

** serially uncorrelated series
gen y1=rnormal(0,$sde)
 lab var y1 "simulated time series, no serial correlation"

tsline y1, lw(thick) lc(navy*0.8) ///
 yline(0) xlabel(, grid) ylabel(, grid) xtitle("Time period") ///
 graphregion(fcolor(white) ifcolor(none))
graph export "$output\ch12-figure-9a-serialcorr-Stata.png", replace


** serially correlated series
gen y2=0 if t==1
 lab var y2 "simulated time series, serial correlation = 0$rho"
 replace y2=$rho*y2[_n-1] + rnormal(0,$sde) if t>1

tsline y2, lw(thick) lc(navy*0.8) ///
 yline(0) xlabel(, grid) ylabel(, grid) xtitle("Time period") ///
 graphregion(fcolor(white) ifcolor(none))
graph export "$output\ch12-figure-9b-serialcorr-Stata.png", replace

