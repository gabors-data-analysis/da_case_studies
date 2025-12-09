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
* version 1.0 2025-01-04
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

* Set up paths
global data_in  "${data_dir}/hotels-vienna/clean"
global work     "ch12-time-series-simulations"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* SIMULATION PARAMETERS
********************************************************************

* Set random seed for reproducibility
clear
set seed 201806

* Create time series of 100 observations
set obs 100

* Simulation parameters
global sde = 1       /* Standard deviation of errors */
global ystart = 0    /* Starting value */


********************************************************************
* GENERATE RANDOM WALKS
********************************************************************

* Create time variable
generate t = _n
tsset t

* Generate 5 random walk series
forvalue i = 1/5 {
	generate y`i' = ${ystart} if t==1
	replace y`i' = y`i'[_n-1] + rnormal(0, ${sde}) if t>1
}


********************************************************************
* FIGURE 12.1: PLOT RANDOM WALKS
********************************************************************

tsline y1 y2 y3 y4 y5, ///
	xlab(, grid) ylab(, grid) ///
	legend(off) ///
	lw(thick thick thick thick thick) ///
	lp(solid dash shortdash longdash dash) ///
	lc(olive dkgreen blue red gs10) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch12-figure-1-randomwalks-Stata.png", replace
