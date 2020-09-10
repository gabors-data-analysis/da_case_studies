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
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/GitHub/da_case_studies"

* STEP 2: * Directory for data
* Option 1: run directory-setting do file
do set-data-directory.do 
							/* this is a one-line do file that should sit in 
							the working directory you have just set up
							this do file has a global definition of your working directory
							more details: gabors-data-analysis.com/howto-stata/   */

* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"


global data_in  "$data_dir/share-health/clean"
global work  	"ch11-smoking-health-risk"

cap mkdir 		"$work/output"
global output 	"$work/output"




********************************************************************

** IMPORT AND SELECT DATA
use "$data_in/share-health.dta", clear

* create outcome
gen healthy = sphus==1 | sphus==2 if sphus>0 & sphus<=5
tab healthy,mis
drop if healthy==.

* baseline: wave 4; endline: wave 6
gen baseline = wave==4
gen endline  = wave==6
tab baseline 
tab endline

* define staying healthy at endline
gen temp = healthy==1 if endline==1
egen stayshealthy = max(temp), by(mergeid)
tab stayshealthy
drop temp

* keep if endline health outcome non-missing
keep if stayshealthy==1 | stayshealthy==0
lab var stayshealthy "Stays healthy"

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
lab var smoking "Current smoker"
lab var ever_smoked "Ever smoked"


* other variables
cap rename eduyears_mod eduyears
gen exerc = br015==1 if br015>0
replace bmi=. if bmi<0
summ bmi
rename income_pct income10
gen married = mar_stat==1 | mar_stat==2 
replace eduyears=. if eduyears<0
summ eduyears

compress
save "$work/ch11_share.dta", replace


*** Summary stats
use "$work/ch11_share.dta", clear
sum stayshealthy smoking ever_smoked female age income10 eduyears bmi exerc 
tab country stayshealthy,mis
drop if bmi==.
drop if eduyears==.
drop if exerc==.
sum stayshealthy smoking ever_smoked female age income10 eduyears bmi exerc 
tab country stayshealthy,mis

more

*** Linear probability models of good health at endline and smoking

* current smoker on RHS
regress stayshealthy smoking, robust
 outreg2 using "$output/ch11-table-1-reg-Stata.tex", label tex(frag)  dec(3) 2aster nor2 replace 

* visualize this regression
* Figure 11.1
qui reg stayshealthy smoking
predict ypred
 lab var ypred "Predicted probability of staying healthy"
egen obs_by_cell = count(ypred), by(stayshealthy smoking)
scatter stayshealthy smoking [w=obs_by_cell], ms(O) mc(green*0.8) ///
 || scatter ypred smoking, ms(O) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
  ylab(0(0.1)1, grid) xlab(0 1) ///
  ytitle("Staying healthy / predicted probability of staying healthy") ///
  legend(off)  
graph export "$output\ch11-figure-1-reg-Stata.png",replace 
 
 
* current smoker and ever smoked on RHS
regress stayshealthy smoking ever_smoked, robust
 outreg2 using "$output/ch11-table-1-reg-Stata.tex", label tex(frag)  dec(3) 2aster nor2 append

 
*** adding other right-hand-side variables
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
graph export "$output\ch11-figure-2a-edu-lowess-Stata.png",replace 

* income group
* Figure 11.2b
lowess stayshealthy income10, ms(i) ///
 lineopts(lw(thick) lcolor(navy*0.8)) ///
 ylab(0(0.1)1, grid) xlab(1(1)10, grid) ///
 ytitle("Probability of staying healthy") xtitle("Income group") ///
 title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-2b-inc-lowess-Stata.png",replace 

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

*** regressions with other variables (age, gender, education, income bmi, exercising, country)
label var female "Female"
label var age "Age"

label var stayshealthy "Staying healthy"
label var eduy_08 "Years of education (if $<8$)"
label var eduy_818 "Years of education (if $>=8$ and $<18$)"
label var eduy_18p "Years of education (if $>=18) $"
label var income10 "Income group"
label var bmi_1635 "BMI (if $<35$)"
label var bmi_3545 "BMI (if $>=35$)"
label var exerc "Exercises regularly"
regress stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country, robust
 outreg2 using "$output/ch11-table-2-reg-Stata.tex", label tex(frag)  dec(3) ///
 keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon replace

* predicted probabilities 
predict p_lpm
 lab var p_lpm "Predicted probability of staying healthy (LPM)"
sum p_lpm,d

* Distribution of predicted probabilities
* Figure 11.3
histogram p_lpm, width(0.02) start(0) percent lc(white) lw(vthin) fc(navy*0.8) ///
 ylabel(0(1)7, grid) xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-3-predprob-hist-Stata.png",replace 
more

* list top 1% and bottom 1%
drop if p_lpm==. /* check if missing */

xtile q100_lpm = p_lpm,  nquantiles(100)
sum smoking ever_smoked female age eduyears income10 bmi exerc if q100_lpm==100
sum smoking ever_smoked female age eduyears income10 bmi exerc if q100_lpm==1


**************************************************************
* lpm versus logit and probit
* with all right-hand-side variables

* lpm (repeating the previous regression)
regress stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country, robust
outreg2 using "$output/ch11-table-3-reg-Stata.tex", label tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon replace

* logit coefficients
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 outreg2 using "$output/ch11-table-3-reg-Stata.tex", label ctitle("logit coeffs") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

* predicted probabilities 
predict p_logit
 lab var p_logit "Predicted probability of staying healthy (logit)"
sum p_logit,d
 
* logit marginal differences
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "$output/ch11-table-3-reg-Stata", label ctitle("logit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 
* probit coefficients
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 outreg2 using "$output/ch11-table-3-reg-Stata", label ctitle("probit coeffs") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 * predicted probabilities 
predict p_probit
 lab var p_probit "Predicted probability of staying healthy (probit)"
sum p_probit,d

* probit marginal differences
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "$output/ch11-table-3-reg-Stata", label ctitle("probit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 
* lpm, logit and probit predicted probabilities
scatter p_logit p_probit p_lpm , ///
xlab(, grid) ylab(, grid) ms(o o) mc(navy*0.8 green*0.6) msize(small small) ///
 || line p_lpm p_lpm, ytitle("Predicted probability") lwidth(medthick) lcolor(black) ///
 legend(rows(1) lab(1 "logit") lab(2 "probit") lab(3 "45 degree line")) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-5-predprob-3models-Stata.png",replace

 
* DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
* re-estimate the simplest lpm for benchmark
reg stayshealthy smoking
predict p_lpmbase

cap drop x* y*

* LPM simple model
* Figure 11.7a
twoway histogram p_lpmbase if stayshealthy==1, percent width(0.05) disc ///
 fcolor(navy*0.8) lcolor(navy) ///
 || histogram p_lpmbase if stayshealthy==0, percent width(0.05) disc ///
 fcolor(none) lcolor(green*0.8) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1, grid) ylab(0(20)80, grid) ytitle("Relative frequency")  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-7a-predprob-by-y-lpmsimple-Stata.png",replace

* LPM rich model
* Figure 11.7b
twoway histogram p_lpm if stayshealthy==1, percent width(0.05) start(0) ///
 fcolor(navy*0.8) lcolor(navy) ///
 || histogram p_lpm if stayshealthy==0, percent width(0.05) start(0) ///
 fcolor(none) lcolor(green*0.8) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1, grid) ylab(0(5)20, grid) ytitle("Relative frequency")  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-7b-predprob-by-y-lpm-Stata.png",replace

* SUMMARY STATS OF PREDICTED PROBABILITIES BY OUTCOME
label var p_lpmbase "Simple LPM"
label var p_lpm "LPM"
label var p_logit "Logit"
label var p_probit "Probit"
estpost tabstat p_lpmbase p_lpm p_logit p_probit, by(stayshealthy) s(mean)
esttab using "$output/ch11-table-4-pred-mean-Stata.tex", replace noobs label  tex  ///
 cells(" p_lpmbase(fmt(3)) p_lpm(fmt(3)) p_logit(fmt(3)) p_probit(fmt(3))")  nonumbers

estpost tabstat p_lpmbase p_lpm p_logit p_probit, by(stayshealthy) s(median) 
esttab using "$output/ch11-table-4-pred-median-Stata.tex", replace noobs label  tex ///
 cells(" p_lpmbase(fmt(3)) p_lpm(fmt(3)) p_logit(fmt(3)) p_probit(fmt(3))")  nonumbers 

count if p_lpm!=.

 
*********************************************
*** CALIBRATION CURVES

* LPM rich model
egen p_lpm_bins = cut(p_lpm), at(0 0.2(0.05)0.85 1.05)
tabstat p_lpm , by(p_lpm_bins) s(min max mean n)
egen temp=mean(p_lpm), by(p_lpm_bins)
replace p_lpm_bins = temp /* attach value to bin: avg pred value */
tabstat p_lpm, by(p_lpm_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_lpm_bins)
 twoway scatter stayshealthy p_lpm_bins, ms(o) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
 || line p_lpm_bins p_lpm_bins,  lw(medium ) lc(green*0.8) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ///
 ytitle("Proportion staying healthy") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\ch11-figure-8a-calib-lpm-Stata.png",replace
restore


* logit rich model
egen p_logit_bins = cut(p_logit), at(0 0.2(0.05)0.85 1.05)
tabstat p_logit, by(p_logit_bins) s(min max mean n)
cap drop temp
egen temp=mean(p_logit), by(p_logit_bins)
replace p_logit_bins = temp /* attach value to bin: avg pred value */
tabstat p_logit, by(p_logit_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_logit_bins)
 twoway scatter stayshealthy p_logit_bins, ms(o) mc(navy*0.8) c(l) lw(thick) lc(navy*0.8) ///
 || line p_logit_bins p_logit_bins,  lw(medium ) lc(green*0.8) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ///
 ytitle("Proportion staying healthy") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\ch11-figure-8b-calib-logit-Stata.png",replace
restore


* GOODNESS OF FIT STATISTICS

* LPM
qui reg stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
* R-squared 
local R2_lpm = e(r2)
* Brier
gen sqd_lpm = (stayshealthy - p_lpm)^2
* log-loss
gen logl_lpm = stayshealthy*ln(p_lpm) + (1-stayshealthy)*ln(1-p_lpm)

* logit
* R-squared & Brier
qui sum stayshealthy
gen var_stayshealthy=r(sd)^2
gen sqd_logit = (stayshealthy - p_logit)^2
qui sum sqd_logit
gen r2_logit = 1 - (r(mean) / var_stayshealthy)
* Pseudo R-squared
qui logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
local pseudo_R2_logit = e(r2_p)
* log-loss
gen logl_logit = stayshealthy*ln(p_logit) + (1-stayshealthy)*ln(1-p_logit)


* probit
* R-squared and Brier
gen sqd_probit = (stayshealthy - p_probit)^2
qui sum sqd_probit
gen r2_probit = 1 - (r(mean) / var_stayshealthy)
sum r2_probit

* Pseudo R-squared
qui probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
local pseudo_R2_probit = e(r2_p)
* log-loss
gen logl_probit = stayshealthy*ln(p_probit) + (1-stayshealthy)*ln(1-p_probit)

* R-squares
dis "R2_lpm = `R2_lpm'"
tabstat r2_*
* Brier scores
tabstat sqd_*
* Pseudo-R-squares
dis "pseudo_R2_logit = `pseudo_R2_logit'    " "pseudo_R2_probit = `pseudo_R2_probit'"
* log-loss
tabstat logl_*
 


*********************************************
* ILLUSTRATION LOGIT AND PROBIT CURVES
use "$work\ch11_share.dta", replace
drop if bmi==.
drop if eduyears==.
drop if exerc==.
 
mkspline eduy_08 8 eduy_818 16 eduy_18p = eduyears
mkspline bmi_1635 35 bmi_3545 = bmi

* estimate logit, predict bx instead of p
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
predict bx_logit, xb
 lab var bx_logit "z for logit"
gen illustr_logit=logistic(bx_logit)
 lab var illustr_logit "logit"

* estimate probit, predict bx instead of p
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
predict bx_probit, xb
 lab var bx_probit "z for probit"
gen illustr_probit=normal(bx_probit)
 lab var illustr_probit "probit"

* Figure 11.4 
line illustr_logit illustr_probit bx_logit, sort ///
  lw(thick thick) lc(navy*0.8 green*0.5) ///
  ylab(, grid) xlab(, grid) ///
  ytitle("Probability") xtitle("z values") ///
  legend(rows(1) lab(1 "logit") lab(2 "probit") ) ///
  graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\ch11-figure-4-illustrate-logit-probit-Stata.png",replace
