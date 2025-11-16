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
* Simulated simple linear regression estimated by OLS
* No actual dataset used
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Applied viridis colors instead of navy/green
*   - Fixed path separator for cross-platform compatibility
*   - Updated graph export syntax
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

global work  	"ch07-ols-simulation"

cap mkdir 		"$work/output"
global output 	"$work/output"



* No real data is used 


* Clear environment
clear

* Set the seed
set seed 1458

* Sample size
global N = 100
set obs $N

* Uniformly distributed x, [0,4]
gen x = runiform(0, 4)

* y = a + bx + u (u normally distributed)
local a = 2
local b = 0.5
local sigmau = 0.7

gen y = `a' + `b'*x + rnormal(0, `sigmau')

summarize y
local meany = r(mean)
summarize x
local meanx = r(mean)

* Scatterplot and OLS regression line
* Average y and average x shown

* Set up viridis colors
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p1)'

colorpalette viridis, n(4) select(3) nograph
local color2 `r(p1)'


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

graph export "$output/ch07-figure-4-olsfit-Stata.png", replace as(png)

