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
* Chapter 07
* Simulating simple linear regression estimated by OLS
* No actual data used - simulations only
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

* Set up paths
global work     "ch07-ols-simulation"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* SIMULATION SETUP
********************************************************************

* Set random seed for reproducibility
set seed 1458

* Define sample size
global N = 100
set obs ${N}



********************************************************************
* GENERATE SIMULATED DATA
********************************************************************

* Generate independent variable: uniformly distributed x on [0,4]
generate x = runiform(0, 4)
label variable x "Simulated x variable (uniform [0,4])"

* Define population parameters for y = a + b*x + u
local a = 2        // Intercept
local b = 0.5      // Slope
local sigmau = 0.7 // Standard deviation of error term


* Generate dependent variable with normal errors
generate y = `a' + `b'*x + rnormal(0, `sigmau')
label variable y "Simulated y variable (y = a + b*x + u)"


********************************************************************
* DESCRIPTIVE STATISTICS
********************************************************************

* Calculate means for reference lines in plot
summarize y
local meany = r(mean)

summarize x
local meanx = r(mean)

* Display summary statistics
summarize x y


********************************************************************
* OLS ESTIMATION
********************************************************************

* Estimate OLS regression
regress y x



********************************************************************
* FIGURE 7.4: SCATTERPLOT WITH OLS REGRESSION LINE
********************************************************************

* Set up viridis colors for scatter and line
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p1)'

colorpalette viridis, n(4) select(3) nograph
local color2 `r(p1)'

* Create scatterplot with OLS fit
* Show average x and average y with dashed lines
scatter y x, ///
 mc("`color1'") ms(O) msize(small) mlw(thick) ///
 || lfit y x, ///
 legend(off) ///
 lc("`color2'") lw(thick) ///
 ylabel(0(0.5)6, grid) ///
 xlabel(0(0.5)4, grid) ///
 ytitle("Simulated y variable") ///
 xtitle("Simulated x variable") ///
 yline(`meany', lc(black) lp(dash)) ///
 xline(`meanx', lc(black) lp(dash)) ///
 text(5 1.6 "Average x") ///
 text(3.2 0.4 "Average y") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch07-figure-4-olsfit-Stata.png", replace
