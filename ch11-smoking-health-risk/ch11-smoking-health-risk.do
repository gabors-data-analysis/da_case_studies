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

* stays healthy at endline
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
outreg2 using "$output/T11_reg3.tex", label tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon replace

* logit coefficients
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 outreg2 using "$output/T11_reg3.tex", label ctitle("logit coeffs") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

* predicted probabilities 
predict p_logit
 lab var p_logit "Predicted probability of staying healthy (logit)"
sum p_logit,d
histogram p_logit, width(0.025) percent ///
ylabel(, grid) lcolor(white) lwidth(vthin) fcolor(navy%80) fintensity(80) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 
 graph export "$output\pred_histogram_logit.png",replace
 
* logit marginal differences
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "$output/T11_reg3.tex", label ctitle("logit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 
* probit coefficients
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 outreg2 using "$output/T11_reg3.tex", label ctitle("probit coeffs") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

 * predicted probabilities 
predict p_probit
 lab var p_probit "Predicted probability of staying healthy (probit)"
sum p_probit,d
histogram p_probit, percent ///
ylabel(, grid) lcolor(white) lwidth(vthin) fcolor(navy%80) fintensity(80) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
  graph export "$output\pred_histogram_probit.png",replace
 graph export "$output\pred_histogram_probit.eps",replace

* probit marginal differences
probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
 margins, dydx(_all) post
 outreg2 using "$output/T11_reg3.tex", label ctitle("probit marginals") tex(frag)  dec(3) ///
keep(stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc) addtext("Country indicators", "YES") 2aster nor2 nocon append

scatter p_logit p_probit p_lpm , ///
xlab(, grid) ylab(, grid) ms(o +) mc(navy green) msize(tiny vtiny) ///
 || line p_lpm p_lpm, ytitle("Predicted probability") lwidth(vthin) lcolor(grey) ///
 legend(rows(1) lab(1 "logit") lab(2 "probit") lab(3 "45 degree line")) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
  graph export "$output\pred_scatter_3models.png",replace
 graph export "$output\pred_scatter_3models.eps",replace

 
*********************************************
*********************************************
* GOODNESS OF FIT




***************************************************************
*** DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
***************************************************************
* re-estimate the simplest lpm for benchmark
reg stayshealthy smoking
predict p_lpmbase

cap drop x* y*


* LPM simple model
twoway histogram p_lpmbase if stayshealthy==1, fraction width(0.05) disc fcolor(navy%80) ///
 || histogram p_lpmbase if stayshealthy==0, fraction width(0.05) disc fcolor(none) lcolor(black) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1) ytitle("Relative frequency")  ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\pred_hist_byoutcome_lpmbase.png", replace


 
* LPM rich model
twoway histogram p_lpm if stayshealthy==1, fraction width(0.05) fcolor(navy%80) lcolor(gs12) ///
 || histogram p_lpm if stayshealthy==0,  fraction width(0.05) fcolor(none) lcolor(black) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1) ytitle("Relative frequency") ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output\pred_hist_byoutcome_lpm.png", replace


* logit rich model
twoway histogram p_logit if stayshealthy==1, fraction width(0.05) fcolor(navy%80) lcolor(gs12) ///
 || histogram p_logit if stayshealthy==0, fraction width(0.05) fcolor(none) lcolor(black) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1) ytitle("Relative frequency")  ylab(0) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\pred_hist_byoutcome_logit_notused.png", replace

* probit rich model
twoway histogram p_probit if stayshealthy==1, fraction width(0.05) fcolor(navy%80) lcolor(gs12) ///
 || histogram p_probit if stayshealthy==0, fraction width(0.05) fcolor(none) lcolor(black) ///
 legend(order(2 "Did not stay healthy" 1 "Stayed healthy" )) ///
 xlab(0(0.2)1) ytitle("Relative frequency")  ylab(0) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\pred_hist_byoutcome_probit.png", replace

 
 * or: density plots
kdensity p_logit if stayshealthy==1, nogra gen(xlogit1 ylogit1)
kdensity p_logit if stayshealthy==0, nogra gen(xlogit0 ylogit0)
lab var ylogit1 "Stayed healthy"
lab var ylogit0 "Did not stay healthy"
line ylogit0 ylogit1 xlogit1, lc(green navy) lp(dash solid) lw(thick thick) ///
 ytitle("Density") ylab(, grid) xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\pred_density_byoutcome_logit.png", replace

 *
 

*********************************************
*** SUMMARY STATS OF PREDICTED PROBABILITIES BY OUTCOME
label var p_lpmbase "Simple LPM"
label var p_lpm "LPM"
label var p_logit "Logit"
label var p_probit "Probit"
estpost tabstat p_lpmbase p_lpm p_logit p_probit, by(stayshealthy) s(mean)
esttab using "$output/T11_mean_medtest.tex", replace noobs label  tex  ///
 cells(" p_lpmbase(fmt(3)) p_lpm(fmt(3)) p_logit(fmt(3)) p_probit(fmt(2))")  nonumbers

tabstat p_lpmbase p_lpm p_logit p_probit, by(stayshealthy) s(median) 
esttab using "$output/T11_mean_medtest2.tex", replace noobs label  tex ///
 cells(" p_lpmbase(fmt(3)) p_lpm(fmt(3)) p_logit(fmt(3)) p_probit(fmt(2))")  nonumbers 

 
*********************************************
*** CALIBRATION CURVES

* LPM rich model
egen p_lpm_bins = cut(p_lpm), at(0(0.05)1.05)
 replace p_lpm_bins = int(p_lpm_bins*20)/20
 replace p_lpm_bins = 0.15 if p_lpm_bins<0.15
 replace p_lpm_bins = 0.85 if p_lpm_bins>0.85
tabstat p_lpm, by(p_lpm_bins) s(min max mean n)
egen temp=mean(p_lpm), by(p_lpm_bins)
replace p_lpm_bins = temp /* attach value to bin: avg pred value */
tabstat p_lpm, by(p_lpm_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_lpm_bins)
 line stayshealthy p_lpm_bins p_lpm_bins,  lw(thick medium ) lc(navy green) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ///
 ytitle("Proportion staying healthy") ///
 legend(label(1 "proportion staying healthy") label(2 "45 degree line") rows(1)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
  graph export "$output\calib_lpm.png", replace
restore
x

* logit rich model
egen p_logit_bins = cut(p_logit), at(0(0.05)1.05)
 replace p_logit_bins = int(p_logit_bins*20)/20
 replace p_logit_bins = 0.15 if p_logit_bins<0.15
 replace p_logit_bins = 0.85 if p_logit_bins>0.85
tabstat p_logit, by(p_logit_bins) s(min max mean n)
cap drop temp
egen temp=mean(p_logit), by(p_logit_bins)
replace p_logit_bins = temp /* attach value to bin: avg pred value */
tabstat p_logit, by(p_logit_bins) s(min max mean n)

preserve
 collapse stayshealthy, by(p_logit_bins)
 line stayshealthy p_logit_bins p_logit_bins,  lw(thick medium ) lc(navy green) ///
 xla(0(0.1)1, grid) yla(0(0.1)1, grid) ///
 xtitle("Bins of predicted probaiblities") ///
 ytitle("Proportion staying healthy") ///
 legend(label(1 "proportion staying healthy") label(2 "45 degree line") rows(1)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 
 graph export "$output\calib_logit.png", replace
restore




********************************************
*** GOODNESS OF FIT STATISTICS
********************************************


* LPM
qui reg stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
* R-squared
dis e(r2)
* Brier
gen sqd_lpm = (stayshealthy - p_lpm)^2
tabstat sqd_lpm
* log-loss
gen logl_lpm = stayshealthy*ln(p_lpm) + (1-stayshealthy)*ln(1-p_lpm)
tabstat logl_lpm

* logit
* R-squared
qui sum stayshealthy
gen var_stayshealthy=r(sd)^2
gen sqd_logit = (stayshealthy - p_logit)^2
qui sum sqd_logit
gen r2_logit = 1 - (r(mean) / var_stayshealthy)
sum r2_logit

* Brier
tabstat sqd_logit
* Pseudo R-squared
qui logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
dis e(r2_p)
* log-loss
gen logl_logit = stayshealthy*ln(p_logit) + (1-stayshealthy)*ln(1-p_logit)
tabstat logl_logit


* probit
* R-squared
gen sqd_probit = (stayshealthy - p_probit)^2
qui sum sqd_probit
gen r2_probit = 1 - (r(mean) / var_stayshealthy)
sum r2_probit

* Brier
tabstat sqd_probit
* Pseudo R-squared
qui probit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p ///
 income10 bmi_1635 bmi_3545 exerc i.country
dis e(r2_p)
* log-loss
gen logl_probit = stayshealthy*ln(p_probit) + (1-stayshealthy)*ln(1-p_probit)
tabstat logl_probit


 
*** CLASSIFICATION, CONFUSION TABLES
* for any model
gen y_actual = stayshealthy
foreach x in lpmbase lpm logit probit {
	gen y_`x'=p_`x'>0.5 if p_`x'<=1
	tab y_`x' y_actual, cell nofre
	tab y_`x' y_actual, col nofre
	more
}


*Very easy table with logit
logit stayshealthy smoking ever_smoked female age  , robust
lstat
logit stayshealthy smoking ever_smoked female age eduy_08 eduy_818 eduy_18p income10 bmi_1635 bmi_3545 exerc i.country, robust
lstat
*

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

 
line illustr_logit bx_logit, sort lw(thick) lc(red) ylab(, grid) ///
  ytitle("Probability") ///
  || line illustr_probit bx_probit, sort lw(thick) xaxis(2) lc(navy green) lp(dash) ///
  ylab(, grid) xlab(, grid) ytitle("Probability") xlabel(-1 1, axis(2)) ///
  legend(rows(1) lab(1 "logit") lab(2 "probit") ) ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\logit_probit_curves.png",replace
 graph export "$output\logit_probit_curves.eps",replace
 
 
* check if simply plotting against bx_logit works, and it does

line illustr_logit illustr_probit bx_logit, sort

