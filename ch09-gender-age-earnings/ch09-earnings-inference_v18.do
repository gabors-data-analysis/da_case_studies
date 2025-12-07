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
* Chapter 09
* CH09A Estimating gender and age differences in earnings
* using the cps-earnings dataset
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
global data_in  "${data_dir}/cps-earnings/clean"
global work     "ch09-gender-age-earnings"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/morg-2014-emp.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile cps_data
copy "https://osf.io/download/rtmga/" `cps_data'
use `cps_data', clear
*/

count
display as text "Total observations loaded: " as result r(N)


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* Create binary female indicator
generate female = (sex == 2)

* Label variables
label variable earnwke "Earnings per week"
label variable uhours "Usual hours per week"

* Calculate hourly wage
generate w = earnwke / uhours
label variable w "Earnings per hour"

* Log hourly wage
generate lnw = ln(w)
label variable lnw "ln earnings per hour"


********************************************************************
* SAMPLE SELECTION BY OCCUPATION
********************************************************************

* Create sample indicator
generate sample = 0
replace sample = 1 if occ2012 == 0735  /* Market research analysts */
replace sample = 2 if occ2012 >= 1005 & occ2012 <= 1240  /* Computer/Math */

label define sample_lbl 1 "Market research analysts" ///
                        2 "Computer science occupations"
label values sample sample_lbl

* Check sample distribution
tabulate sample

* Select working sample (change to =2 for computer occupations)
keep if sample == 1

* Order and compress
order hhid-stfips weight earnwke uhours w lnw female age ind02 occ2012
compress

* Save working file
save "${work}/earnings_inference.dta", replace
count
display as text "Sample size after selection: " as result r(N)


********************************************************************
* DESCRIPTIVE STATISTICS
********************************************************************

use "${work}/earnings_inference.dta", clear

* Summary statistics for key variables
tabstat earnwke uhours w, ///
    statistics(mean min p5 p50 p95 max n) columns(statistics)
    
tabstat earnwke uhours w if w >= 1, ///
    statistics(mean min p5 p50 p95 max n) columns(statistics)


********************************************************************
* GENDER WAGE GAP ANALYSIS
********************************************************************

* Check distribution by gender
tabulate female

tabulate occ2012 female

* Simple regression: gender wage gap
regress lnw female 
outreg2 using "${output}/ch09-table-1-gender-reg-Stata.tex", ///
    2aster tex(fragment) nonotes bdec(2) replace

* With robust standard errors
regress lnw female, robust
outreg2 using "${output}/ch09-table-1-gender-reg-Stata.tex", ///
    2aster tex(fragment) nonotes bdec(2) append


********************************************************************
* FIGURE 1: BOOTSTRAP DISTRIBUTION
********************************************************************

* Bootstrap the gender coefficient
set seed 201711
bootstrap, reps(1000) ///
    saving("${output}/b_earnings_female", replace): ///
    reg lnw female if sample == 1

* Load bootstrap results and create histogram
use "${output}/b_earnings_female", clear
histogram _b_female, percent width(0.025) ///
    color(navy*0.8) lcolor(white) ///
    xline(-0.11, lwidth(vthick) extend) ///
    text(18 -0.09 "mean") ///
    xlabel(-0.3(0.1)0.1, grid) ///
    xtitle("Slope coefficients from bootstrap samples") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-1-bootstrap-hist-Stata.png", replace


********************************************************************
* AGE-EARNINGS ANALYSIS
********************************************************************

use "${work}/earnings_inference.dta", clear

* Create age squared
generate agesq = age^2

* Create age splines (knots at 30 and 40)
mkspline agesp1 30 agesp2 40 agesp3 = age

* Label variables for regression output
label variable age "Age"
label variable agesq "Age squared"
label variable lnw "ln wage"
label variable agesp1 "Age spline <30"
label variable agesp2 "Age spline 30-40"
label variable agesp3 "Age spline 40<"


********************************************************************
* FIGURE 2: CONFIDENCE AND PREDICTION INTERVALS
********************************************************************

* Figure 2a - Confidence interval for linear model
regress lnw age if sample == 1, robust
capture drop SE
predict SE, stdp

graph twoway ///
    (lfitci lnw age if lnw < 4.4 & lnw > 2, lcolor(green*0.8) stdp) ///
    (scatter lnw age if lnw < 4.4 & lnw > 2, sort mcolor(navy*0.6)), ///
    xlabel(20(10)60, grid) ylabel(2(0.4)4.4, grid) ///
    legend(off) ///
    xtitle("Age (years)") ytitle("ln earnings per hour") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-2a-wage-age-ci-Stata.png", replace


* Figure 2b - Prediction interval for linear model
regress lnw age if sample == 1
capture drop SE
predict SE, stdf

graph twoway ///
    (lfitci lnw age if lnw < 4.4 & lnw > 2, lcolor(green*0.8) stdf) ///
    (scatter lnw age if lnw < 4.4 & lnw > 2, sort mcolor(navy*0.6)), ///
    xlabel(20(10)60, grid) ylabel(2(0.4)4.4, grid) ///
    legend(off) ///
    xtitle("Age (years)") ytitle("ln earnings per hour") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-2b-wage-age-pi-Stata.png", replace


********************************************************************
* FIGURE 3: LOWESS AND REGRESSION COMPARISONS
********************************************************************

* Figure 3a - Lowess nonparametric regression
lowess lnw age if sample == 1, ///
    lineopts(lwidth(vthick) lcolor(dkgreen)) ///
    mcolor(navy) msize(small) ///
    xlabel(20(10)60, grid) ylabel(1.5(0.5)4.5, grid) ///
	xtitle("Age (years)") ytitle("ln earnings per hour") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-3a-wage-lowess-Stata.png", replace


********************************************************************
* TABLE 2: REGRESSION MODELS COMPARISON
********************************************************************

* Run different age specifications
regress lnw age, robust
outreg2 using "${output}/ch09-table-2-age-models-Stata.tex", ///
    label 2aster bdec(3) tex(fragment) nonotes replace

regress lnw age agesq, robust
outreg2 using "${output}/ch09-table-2-age-models-Stata.tex", ///
    label 2aster tex(fragment) nonotes bdec(3) append

regress lnw agesp*, robust
outreg2 using "${output}/ch09-table-2-age-models-Stata.tex", ///
    label 2aster sortvar(age agesq agesp*) ///
    tex(fragment) nonotes bdec(3) append


********************************************************************
* GENERATE PREDICTIONS FOR PLOTTING
********************************************************************

* Quadratic model predictions
regress lnw age agesq, robust
predict lnwpred_ageq
label variable lnwpred_ageq "Quadratic"

predict lnwpred_ageqSE, stdp
capture generate lnwpred_ageqCIUP = lnwpred_ageq + 2 * lnwpred_ageqSE
capture generate lnwpred_ageqCILO = lnwpred_ageq - 2 * lnwpred_ageqSE

* Spline model predictions
regress lnw agesp*, robust
predict lnwpred_agesp
label variable lnwpred_agesp "Piecewise linear spline"

predict lnwpred_agespSE, stdp
capture generate lnwpred_agespCIUP = lnwpred_agesp + 2 * lnwpred_agespSE
capture generate lnwpred_agespCILO = lnwpred_agesp - 2 * lnwpred_agespSE

* Lowess predictions
lowess lnw age, nograph generate(lnwpred_agel)
label variable lnwpred_agel "Lowess"


********************************************************************
* FIGURE 3B: COMPARE THREE MODELS (NO CI)
********************************************************************

line lnwpred_agel lnwpred_agesp lnwpred_ageq age, sort ///
    lpattern(solid dash shortdash) ///
    lwidth(vthick thick thick) ///
    lcolor(navy*0.8 green*0.8 black) ///
    xlabel(20(10)60, grid) ylabel(2.6(0.2)3.4, grid) ///
    legend(rows(1)) ///
    ytitle("ln earnings per hour") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-3b-wage-various-Stata.png", replace


********************************************************************
* FIGURE 4: THREE MODELS WITH CONFIDENCE INTERVALS
********************************************************************

line lnwpred_agel ///
     lnwpred_agesp lnwpred_agespCIUP lnwpred_agespCILO ///
     lnwpred_ageq lnwpred_ageqCIUP lnwpred_ageqCILO ///
     age, sort ///
    lpattern(solid dash dash dash shortdash shortdash shortdash) ///
    lwidth(vthick thick thin thin thick thin thin) ///
    lcolor(navy*0.8 green*0.8 green*0.8 green*0.8 black black black) ///
    xlabel(20(10)60, grid) ylabel(2.6(0.2)3.4, grid) ///
    ytitle("ln earnings per hour") ///
    legend(rows(1) order(1 2 5)) ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch09-figure-4-wage-age-reg-ci-Stata.png", replace


