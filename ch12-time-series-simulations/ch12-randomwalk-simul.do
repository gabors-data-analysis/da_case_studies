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
* Simulating random walk time series
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
set seed 201806
set obs 100
global sde=1
global ystart=0

gen t=_n
tsset t

forvalue i=1/5 {
	gen y`i' = $ystart if t==1
	replace y`i' = y`i'[_n-1] + rnormal(0,$sde) if t>1
}

tsline y1 y2 y3 y4 y5, xlab(, grid) ylab(,grid) legend(off) ///
 lw(thick thick thick thick thick) lp(solid dash shortdash longdash dash) ///
 lc(olive dkgreen blue red gs10) ///
graphregion(fcolor(white) ifcolor(none)) 
graph export "$output\ch12-figure-1-randomwalks-Stata.png", replace

