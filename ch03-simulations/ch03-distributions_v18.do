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
* No actual data used here
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Applied viridis colors instead of navy
*   - Updated graph export syntax
*   - Removed interactive more commands
*   - Fixed typos in comments
********************************************************************


* SETTING UP DIRECTORIES

* Set working directory for da_case_studies.
* For example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

global work  	"ch03-simulations"

cap mkdir 		"$work/output"
global output 	"$work/output"


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

graph export "$output/dist-Bernoulli-Stata.png", replace as(png)
 
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

graph export "$output/dist-binomial-Stata.png", replace as(png)

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

graph export "$output/dist-uniform-Stata.png", replace as(png)
 
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

graph export "$output/dist-normal-Stata.png", replace as(png)
 
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

graph export "$output/dist-lognormal-Stata.png", replace as(png)

 
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

graph export "$output/dist-powerlaw-Stata.png", replace as(png)
 
sum powerlaw, d

