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
* Chapter 11
* CH11A Does smoking pose a health risk?
* using the share-health dataset
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

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global data_in  "${data_dir}/share-health/clean"
global work     "ch11-smoking-health-risk"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"



********************************************************************
* LOAD DATA
********************************************************************

import delimited "$data_in/share-health.csv", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/kbjzp/" "workfile.csv"
import delimited "workfile.csv"
erase "workfile.csv"
*/ 


********************************************************************
* DEFINE OUTCOME AND SAMPLE SELECTION
********************************************************************

* create outcome
generate healthy = sphus==1 | sphus==2 if sphus>0 & sphus<=5
tabulate healthy,mis
drop if healthy==.

* baseline: wave 4; endline: wave 6
generate baseline = wave==4
generate endline  = wave==6
tabulate baseline 
tabulate endline

* define staying healthy at endline
generate temp = healthy==1 if endline==1
egen stayshealthy = max(temp), by(mergeid)
tabulate stayshealthy
drop temp

* keep if endline health outcome non-missing
keep if stayshealthy==1 | stayshealthy==0
label variable stayshealthy "Stays healthy"

* keep baseline observations (endline outcome already defined for them)
keep if baseline==1

* keep age 50-60 at baseline
keep if age>=50 & age<=60

* keep healthy individuals at baseline
keep if healthy==1

* keep those with non-missing observations for smoking at baseline
* and re-define smoking to be 0-1
recode smoking 5=0
keep if smoking==1 | smoking==0
recode ever_smoked 5=0
keep if ever_smoked ==1 | ever_smoked ==0
label variable smoking "Current smoker"
label variable ever_smoked "Ever smoked"



********************************************************************
* FEATURE ENGINEERING
********************************************************************

* other variables
capture rename eduyears_mod eduyears
generate exerc = br015==1 if br015>0
replace bmi=. if bmi<0
summarize bmi
rename income_pct_w4 income10
generate married = mar_stat==1 | mar_stat==2 
replace eduyears=. if eduyears<0
summarize eduyears

compress

********************************************************************
* SAVE WORK FILE
********************************************************************

save "${work}/ch11_share.dta", replace


* Summary stats
use "${work}/ch11_share.dta", clear
summarize stayshealthy smoking ever_smoked female age income10 eduyears bmi exerc 
tabulate country stayshealthy,mis
drop if bmi==.
drop if eduyears==.
drop if exerc==.
summarize stayshealthy smoking ever_smoked female age income10 eduyears bmi exerc 
tabulate country stayshealthy,mis


* Linear probability models of good health at endline and smoking

* current smoker on RHS
regress stayshealthy smoking, robust
 outreg2 using "${output}/ch11-table-1-reg-Stata.tex", label tex(frag)  dec(3) 2aster nor2 replace 

* visualize this regression
* Figure 11.1 - using viridis colors
quietly reg stayshealthy smoking
predict ypred
 label variable ypred "Predicted probability of staying healthy"
egen obs_by_cell = count(ypred), by(stayshealthy smoking)
scatter stayshealthy smoking [w=obs_by_cell], ms(O) mc(green*0.6) ///
 || scatter ypred smoking, ms(O) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
  ylab(0(0.1)1, grid) xlab(0 1) ///
  ytitle("Staying healthy / predicted probability of staying healthy") ///
  legend(off)  
graph export "${output}/ch11-figure-1-reg-Stata.png", as(png) replace 
 
 
* current smoker and ever smoked on RHS
regress stayshealthy smoking ever_smoked, robust
 outreg2 using "${output}/ch11-table-1-reg-Stata.tex", label tex(frag)  dec(3) 2aster nor2 append

 
* adding other right-hand-side variables
* first check some functional forms

* age, not in textbook
lowess stayshealthy age, ms(i) ///
 lineopts(lw(thick) lcolor(navy*0.8)) ///
 ylab(0(0.1)1, grid) xlab(, grid) ///
 ytitle("Probability of staying healthy") xtitle("Age (years)") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))

* education
* Figure 11.2a
lowess stayshealthy eduyears, ms(i) ///
 lineopts(lw(thick) lcolor(navy*0.8)) ///
 ylab(0(0.1)1, grid) xlab(0(4)24, grid) ///
 ytitle("Probability of staying healthy") xtitle("Years of education") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-2a-edu-lowess-Stata.png", as(png) replace 

* income group
* Figure 11.2b
lowess stayshealthy income10, ms(i) ///
 lineopts(lw(thick) lcolor(navy*0.8)) ///
 ylab(0(0.1)1, grid) xlab(1(1)10, grid) ///
 ytitle("Probability of staying healthy") xtitle("Income group") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-2b-inc-lowess-Stata.png", as(png) replace 

* BMI, not in textbook
lowess stayshealthy bmi, ms(i) ///
 lineopts(lw(thick) lcolor(navy*0.8)) ///
 ylab(0(0.1)1, grid) xlab(, grid) ///
 ytitle("Probability of staying healthy") xtitle("BMI") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))


* creating piecewise linear spline variables from education and bmi
mkspline eduy_08 8 eduy_818 18 eduy_18p = eduyears
mkspline bmi_1635 35 bmi_3545 = bmi

* regressions with other variables (age, gender, education, income bmi, exercising, country)
label variable female "Female"
label variable age "Age"

label variable stayshealthy "Staying healthy"
label variable eduy_08 "Years of education (if $<8$)"
label variable eduy_818 "Years of education (if $>=8$ and $<18$)"
label variable eduy_18p "Years of education (if $>=18) $"
label variable income10 "Income group"
label variable bmi_1635 "BMI (if $<35$)"
label variable bmi_3545 "BMI (if $>=35$)"
label variable exerc "Exercises regularly"

regress stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country, robust
 outreg2 using "${output}/ch11-table-2-reg-Stata.tex", label tex(frag)  dec(3) ///
 keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon replace

* predicted probabilities 
predict p_lpm
label variable p_lpm "Predicted probability of staying healthy (LPM)"
summarize p_lpm,d

* Distribution of predicted probabilities
* Figure 11.3
histogram p_lpm, width(0.02) start(0) percent lc(white) lw(vthin) fc(navy*0.8) ///
 ylabel(0(1)7, grid) xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-3-hist-predprob-lpm-Stata.png", as(png) replace
more


********************************************************************
* EXAMINE TOP AND BOTTOM PERCENTILES
********************************************************************

* List top 1% and bottom 1%
drop if p_lpm==.  /* Check if missing */

xtile q100_lpm = p_lpm, nquantiles(100)
summarize smoking ever_smoked female age eduyears income10 bmi exerc if q100_lpm==100
summarize smoking ever_smoked female age eduyears income10 bmi exerc if q100_lpm==1


********************************************************************
* TABLE 11.3: LPM VERSUS LOGIT AND PROBIT
********************************************************************

* LPM (repeating the previous regression)
regress stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
	income10 bmi_1635 bmi_3545 exerc i.country, robust
outreg2 using "${output}/ch11-table-3-reg-Stata.tex", label tex(frag) dec(3) ///
	keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
	income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") ///
	2aster nor2 nocon replace

* Logit coefficients
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
	income10 bmi_1635 bmi_3545 exerc i.country
outreg2 using "${output}/ch11-table-3-reg-Stata", label ctitle("logit coeffs") ///
	tex(frag) dec(3) ///
	keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
	income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") ///
	2aster nor2 nocon append

* predicted probabilities 
predict p_logit
label variable p_logit "Predicted probability of staying healthy (logit)"
summarize p_logit,d
 
* logit marginal differences
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "${output}/ch11-table-3-reg-Stata", label ctitle("logit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 
* probit coefficients
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 outreg2 using "${output}/ch11-table-3-reg-Stata", label ctitle("probit coeffs") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 * predicted probabilities 
predict p_probit
label variable p_probit "Predicted probability of staying healthy (probit)"
summarize p_probit,d

* probit marginal differences
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "${output}/ch11-table-3-reg-Stata", label ctitle("probit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 
* lpm, logit and probit predicted probabilities
* Figure 11.5 - using viridis colors
scatter p_logit p_probit p_lpm , ///
xlab(, grid) ylab(, grid) ms(o o) mc(navy*0.8 green*0.6) msize(small small) ///
 || line p_lpm p_lpm, ytitle("Predicted probability") lwidth(medthick) lcolor(black) ///
 legend(rows(1) lab(1 "logit") lab(2 "probit") lab(3 "45 degree line")) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-5-predprob-3models-Stata.png", as(png) replace

 
* DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
* re-estimate the simplest lpm for benchmark
regress stayshealthy smoking
predict p_lpmbase

capture drop x* y*

* LPM simple model
* Figure 11.7a - using viridis colors
twoway histogram p_lpmbase if stayshealthy==1, percent width(0.05) disc ///
 fcolor(navy*0.8) lcolor(navy*0.8) ///
 || histogram p_lpmbase if stayshealthy==0, percent width(0.05) disc ///
 fcolor(none) lcolor(green*0.6) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1, grid) ylab(0(20)80, grid) ytitle("Relative frequency")  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-7a-predprob-by-y-lpmsimple-Stata.png", as(png) replace

* LPM rich model
* Figure 11.7b - using viridis colors
twoway histogram p_lpm if stayshealthy==1, percent width(0.05) start(0) ///
 fcolor(navy*0.8) lcolor(navy*0.8) ///
 || histogram p_lpm if stayshealthy==0, percent width(0.05) start(0) ///
 fcolor(none) lcolor(green*0.6) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1, grid) ylab(0(5)20, grid) ytitle("Relative frequency")  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-7b-predprob-by-y-lpm-Stata.png", as(png) replace

* SUMMARY STATS OF PREDICTED PROBABILITIES BY OUTCOME
label variable p_lpmbase "Simple LPM"
label variable p_lpm "LPM"
label variable p_logit "Logit"
label variable p_probit "Probit"
estpost tabstat p_lpmbase p_lpm p_logit p_probit, by(stayshealthy) s(mean)

* MEAN table (using collect, only compatible with Stata 17+)
collect clear
table stayshealthy, statistic(mean p_lpmbase p_lpm p_logit p_probit)
collect style cell result[mean], nformat(%9.3f)
collect style header result, level(hide)
collect export "${output}/ch11-table-4-pred-mean-Stata.tex", as(tex) replace tableonly

* MEDIAN table (using collect, only compatible with Stata 17+)
collect clear
table stayshealthy, statistic(median p_lpmbase p_lpm p_logit p_probit)
collect style cell result[median], nformat(%9.3f)
collect style header result, level(hide)
collect export "${output}/ch11-table-4-pred-median-Stata.tex", as(tex) replace tableonly
count if p_lpm!=.

 

********************************************************************
* CONFOUNDING AND CONTROLS
********************************************************************

* CALIBRATION CURVES

* LPM rich model
egen p_lpm_bins = cut(p_lpm), at(0 0.2(0.05)0.85 1.05)
tabstat p_lpm , by(p_lpm_bins) s(min max mean n)
egen temp=mean(p_lpm), by(p_lpm_bins)
replace p_lpm_bins = temp /* attach value to bin: avg pred value */
tabstat p_lpm, by(p_lpm_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_lpm_bins)
 * Figure 11.8a - using viridis colors
 twoway scatter stayshealthy p_lpm_bins, ms(o) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
 || line p_lpm_bins p_lpm_bins,  lw(medium ) lc(green*0.6) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probabilities") ///
 ytitle("Proportion staying healthy") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "${output}/ch11-figure-8a-calib-lpm-Stata.png", as(png) replace
restore


* logit rich model
egen p_logit_bins = cut(p_logit), at(0 0.2(0.05)0.85 1.05)
tabstat p_logit, by(p_logit_bins) s(min max mean n)
capture drop temp
egen temp=mean(p_logit), by(p_logit_bins)
replace p_logit_bins = temp /* attach value to bin: avg pred value */
tabstat p_logit, by(p_logit_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_logit_bins)
 * Figure 11.8b - using viridis colors
 twoway scatter stayshealthy p_logit_bins, ms(o) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
 || line p_logit_bins p_logit_bins,  lw(medium ) lc(green*0.6) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probabilities") ///
 ytitle("Proportion staying healthy") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "${output}/ch11-figure-8b-calib-logit-Stata.png", as(png) replace
restore


* GOODNESS OF FIT STATISTICS

* LPM
quietly reg stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
* R-squared 
local R2_lpm = e(r2)
* Brier
generate sqd_lpm = (stayshealthy - p_lpm)^2
* log-loss
generate logl_lpm = stayshealthy*ln(p_lpm) + (1-stayshealthy)*ln(1-p_lpm)

* logit
* R-squared & Brier
quietly summarize stayshealthy
generate var_stayshealthy=r(sd)^2
generate sqd_logit = (stayshealthy - p_logit)^2
quietly summarize sqd_logit
generate r2_logit = 1 - (r(mean) / var_stayshealthy)
* Pseudo R-squared
quietly logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
local pseudo_R2_logit = e(r2_p)
* log-loss
generate logl_logit = stayshealthy*ln(p_logit) + (1-stayshealthy)*ln(1-p_logit)


* probit
* R-squared and Brier
generate sqd_probit = (stayshealthy - p_probit)^2
quietly summarize sqd_probit
generate r2_probit = 1 - (r(mean) / var_stayshealthy)
summarize r2_probit

* Pseudo R-squared
quietly probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
local pseudo_R2_probit = e(r2_p)
* log-loss
generate logl_probit = stayshealthy*ln(p_probit) + (1-stayshealthy)*ln(1-p_probit)

* R-squares
display "R2_lpm = `R2_lpm'"
tabstat r2_*
* Brier scores
tabstat sqd_*
* Pseudo-R-squares
display "pseudo_R2_logit = `pseudo_R2_logit'    " "pseudo_R2_probit = `pseudo_R2_probit'"
* log-loss
tabstat logl_*
 



********************************************************************
* LOGIT MODELS
********************************************************************

* ILLUSTRATION LOGIT AND PROBIT CURVES
use "${work}/ch11_share.dta", replace
drop if bmi==.
drop if eduyears==.
drop if exerc==.
 
mkspline eduy_08 8 eduy_818 16 eduy_18p = eduyears
mkspline bmi_1635 35 bmi_3545 = bmi

* estimate logit, predict bx instead of p
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
predict bx_logit, xb
label variable bx_logit "z for logit"
generate illustr_logit=logistic(bx_logit)
 label variable illustr_logit "logit"

* estimate probit, predict bx instead of p
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
predict bx_probit, xb
 label variable bx_probit "z for probit"
generate illustr_probit=normal(bx_probit)
 label variable illustr_probit "probit"

* Figure 11.4 - using viridis colors
line illustr_logit illustr_probit bx_logit, sort ///
  lw(thick thick) lc(navy*0.8 green*0.6) ///
  ylab(, grid) xlab(, grid) ///
  ytitle("Probability") xtitle("z values") ///
  legend(rows(1) lab(1 "logit") lab(2 "probit") ) ///
  graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch11-figure-4-illustrate-logit-probit-Stata.png", as(png) replace
