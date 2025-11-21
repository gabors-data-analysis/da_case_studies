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
* Chapter 03
* Simulating the density function (histograms) of theoretical distributions
* No actual data used - simulations only
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

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global work     "ch03-simulations"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"



* Clear environment
clear

* Set the seed
set seed 16460

* Sample size
global N = 100000
set obs $N



* Set up viridis color for all distributions
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

* Bernoulli
gen bernoulli = rbinomial(1, 0.7) 
hist bernoulli, ///
 xtitle("") ///
 ytitle("Percent") ///
 xlab(0 1) ///
 color("`color1'") ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-Bernoulli-Stata.png", replace as(png)
 
* Binomial
* With smaller sample 
global Nbinom = 20
gen rbinomial = rbinomial($Nbinom, .4)
hist rbinomial, ///
 disc width(0.5) ///
 xtitle("") ///
 ytitle("Percent") ///
 xlab("") ///
 color("`color1'") ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-binomial-Stata.png", replace as(png)

* Uniform [0,1]
gen runif = runiform(0, 1)
hist runif, ///
 xtitle("") ///
 ytitle("Percent") ///
 fcolor("`color1'") ///
 lcolor(white) ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-uniform-Stata.png", replace as(png)
 
* Normal
gen rnormal = rnormal(0, 1)
hist rnormal, ///
 xtitle("") ///
 ytitle("Percent") ///
 xlab("") ///
 fcolor("`color1'") ///
 lcolor(white) ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-normal-Stata.png", replace as(png)
 
* Lognormal
* Take the exponential of the randomly generated normal above 
generate lognormal = exp(rnormal)
hist lognormal if lognormal <10, ///
 xtitle("") ///
 ytitle("Percent") ///
 xlab("") ///
 fcolor("`color1'") ///
 lcolor(white) ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-lognormal-Stata.png", replace as(png)

 
* Power-law
global alpha = 6
global xmin = 1
cap gen x = _n
cap drop powerlaw
generate powerlaw = $xmin*x^(-$alpha)
sum powerlaw, d
replace powerlaw = powerlaw/r(sum)
local histrange = r(p75)

hist powerlaw if powerlaw < `histrange', ///
 xtitle("") ///
 ytitle("Percent") ///
 xlab("") ///
 fcolor("`color1'") ///
 lcolor(white) ///
 percent ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "${output}/dist-powerlaw-Stata.png", replace as(png)
 
sum powerlaw, d

