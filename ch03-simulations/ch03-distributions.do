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
* noa ctual data used here
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/Github/da_case_studies"

global work  	"ch03-simulations"

cap mkdir 		"$work/output"
global output 	"$work/output"


*clear environment
clear

*set the seed
set seed 16460

*sample size
global N=100000
set obs $N




* Bernoulli
gen bernoulli=rbinomial(1,0.7) 
hist bernoulli, ///
 xtitle("") ytitle("Percent") xlab(0 1) color(navy*0.8) percent
 graph export "$output/dist-Bernoulli-Stata.png", replace
 more
 
* Binomial
* with smaller sample 
global Nbinom = 20
gen rbinomial=rbinomial($Nbinom,.4)
hist rbinomial, disc width(0.5)  ///
 xtitle("") ytitle("Percent") xlab("") color(navy*0.8) percent
 graph export "$output/dist-binomial-Stata.png", replace
 more

* uniform [0,1]
gen runif=runiform(0,1)
hist runif,   ///
 xtitle("") ytitle("Percent") fcolor(navy*0.8) lcolor(white) percent
 graph export "$output/dist-uniform-Stata.png", replace
 more
 
* noromal
gen rnormal=rnormal(0,1)
hist rnormal,  ///
 xtitle("") ytitle("Percent") xlab("") fcolor(navy*0.8) lcolor(white) percent
 graph export "$output/dist-normal-Stata.png", replace
 more
 
* lognoromal
* take the exponential of the randomly generated normal above 
generate lognormal = exp(rnormal)
hist lognormal  if lognormal <10 , ///
 xtitle("") ytitle("Percent") xlab("") fcolor(navy*0.8) lcolor(white) percent
 graph export "$output/dist-lognormal-Stata.png", replace
 more

 
* power-law
global alpha = 6
global xmin = 1
cap gen x = _n
cap drop powerlaw
generate powerlaw = $xmin*x^(-$alpha)
 sum powerlaw, d
 replace powerlaw = powerlaw/r(sum)
 local histrange = r(p75)
hist powerlaw if powerlaw < `histrange', ///
 xtitle("") ytitle("Percent") xlab("") fcolor(navy*0.8) lcolor(white) percent
 graph export "$output/dist-powerlaw-Stata.png", replace
 
 sum powerlaw,d


