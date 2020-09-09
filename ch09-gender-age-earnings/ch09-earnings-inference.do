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


global data_in  "$data_dir/cps-earnings/clean"
global work  	"ch09-gender-age-earnings"

cap mkdir 		"$work/output"
global output 	"$work/output"



* loead data
clear
use "$data_in/morg-2014-emp.dta"
count

* new variables
gen female=sex==2
lab var earnwke "Earnings per week"
lab var uhours "Usual hours per week"
gen w = earnwke/uhours
lab var w "Earnings per hour"

gen lnw=ln(w)
lab var lnw "ln earnings per hour"

** SELECT OCCUPATION
gen sample=0
replace sample=1 if occ2012==0735 /* Market research analysts and marketing specialists */
replace sample=2 if occ2012>=1005 & occ2012<=1240 /* ** Computer and Mathematical Occupations */
label define sa 1 "market research analysts" 2 "computer sci occupations"
label value sample sa

tab sample

order hhid-stfips weight earnwke uhours w lnw female age ind occ
compress

* set sample here
keep if sample==1 /* change to =2 */
save "$work/earnings_inference.dta",replace
count


use "$work/earnings_inference.dta" ,clear
cap erase "$output/earnings_female.tex"

** DISTRIBUTION OF EARNINGS

tabstat earnwke uhours w , s(mean min p5 p50 p95 max n) col(s)
tabstat earnwke uhours w if w>=1, s(mean min p5 p50 p95 max n) col(s)

** LN EARNINGS AND GENDER

tab female 
tab occ female 

reg lnw female 
reg lnw female , robust
 outreg2 using "$output/T09_reg1.tex", 2aster tex(frag) nonotes bdec(2)  replace 

* bootstrap
set seed 201711
bootstrap, reps(1000) saving("$output\b_earnings_female",replace): reg lnw female if sample==1
use "$output\b_earnings_female",replace
hist _b_female, percent width(0.025) color(navy*0.8) lcolor(white) ///
 xline(-0.11, lw(vthick) extend) ///
 text(18 -0.09 "mean") ///
 xlab(-0.3(0.1)0.1, grid) xtitle("Slope coefficients from bootstrap samples") ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch09-figure-1-bootstrap-hist-Stata.png",replace


* CI for linear
* Figure 2a
use "$work/earnings_inference.dta" ,replace
reg lnw age if sample==1, r
 cap drop SE
 predict SE, stdp
graph twoway lfitci lnw age if  lnw<4.4 & lnw>2, lc(green*0.8) stdp ///
 || scatter lnw age if  lnw<4.4 & lnw>2, sort mc(navy*0.6) ///
 xlab(20(10)60, grid) ylab(2(0.4)4.4, grid) legend(rows(1)) ///
 xtitle("Age (years)")  ytitle("ln earnings per hour") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch09-figure-2a-wage-age-ci-Stata.png", replace

* PI for linear
* Figure 2b
reg lnw age if sample==1
 cap drop SE
 predict SE, stdf
graph twoway lfitci lnw age if  lnw<4.4 & lnw>2, lc(green*0.8) stdf ///
 || scatter lnw age if  lnw<4.4 & lnw>2, sort mc(navy*0.6) ///
 xlab(20(10)60, grid) ylab(2(0.4)4.4, grid) legend(rows(1)) ///
 xtitle("Age (years)")  ytitle("ln earnings per hour") ///
 legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch09-figure-2b-wage-age-pi-Stata.png", replace
 

** LN EARNINGS AND AGE
* lowess nonparametric regression
lowess lnw age if sample==1, lineop( lw(vthick) lc(dkgreen) ) ///
	 mcolor(navy) msize(small) ///
	 xlab(20(10)60, grid) ylab(1.5(0.5)4.5, grid) ///
	 graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output/ch09-figure-3a-wage-lowess-Stata.png", replace

 
* splines, quadratic
mkspline agesp1 30 agesp2 40 agesp3 = age 
gen agesq = age^2

label var age "age"
label var agesq "age squared" 
label var lnw "ln wage"
label var agesp1 "age spline <30" 
label var agesp2 "age spline 30-40" 
label var agesp3 "age spline 40<"
	
reg lnw age , r
 outreg2 using "$output/T09_reg2.tex", label 2aster bdec(3) tex(frag) nonotes replace
reg lnw age agesq , r
 outreg2 using "$output/T09_reg2.tex", label 2aster tex(frag) nonotes bdec(3) append
reg lnw agesp* , r
 outreg2 using "$output/T09_reg2.tex", label 2aster sortvar(age agesq agesp*) tex(frag) nonotes bdec(3) append
	  

* intervals

reg lnw age agesq , r
 predict lnwpred_ageq
  lab var lnwpred_ageq "quadratic"
 predict lnwpred_ageqSE , stdp
 cap gen lnwpred_ageqCIUP = .
 replace lnwpred_ageqCIUP = lnwpred_ageq + 2*lnwpred_ageqSE
 cap gen lnwpred_ageqCILO = .
 replace lnwpred_ageqCILO = lnwpred_ageq - 2*lnwpred_ageqSE

reg lnw agesp* , r
 predict lnwpred_agesp 
  lab var lnwpred_agesp "piecewise linear spline"
 predict lnwpred_agespSE , stdp
 cap gen lnwpred_agespCIUP = .
 replace lnwpred_agespCIUP = lnwpred_agesp + 2*lnwpred_agespSE
 cap gen lnwpred_agespCILO = .
 replace lnwpred_agespCILO = lnwpred_agesp - 2*lnwpred_agespSE

* lowess reg
lowess lnw age,  nogra gen(lnwpred_agel)
 lab var lnwpred_agel "lowess"

* all three visualized w/o CI 
* Figure 3b
line lnwpred_agel lnwpred_agesp lnwpred_ageq age,  sort ///
 lp(solid dash shortdash) lw(vthick thick thick) lc(navy*0.8 green*0.8 black) ///
 xlab(20(10)60, grid) ylab(2.6(0.2)3.4, grid) legend(rows(1)) ytitle("ln earnings per hour") ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch09-figure-3b-wage-various-Stata.png", replace

* all three visualized, parametric ones with CI 
* Figure 4
line lnwpred_agel lnwpred_agesp lnwpred_agespCIUP lnwpred_agespCILO ///
 lnwpred_ageq lnwpred_ageqCIUP lnwpred_ageqCILO age , sort ///
 lp(solid dash dash dash shortdash shortdash shortdash) ///
 lw(vthick thick thin thin thick thin thin) ///
 lc(navy*0.8 green*0.8 green*0.8 green*0.8 black black black) ///
 xlab(20(10)60, grid) ylab(2.6(0.2)3.4, grid) ytitle("ln earnings per hour") ///
  legend(rows(1)  order(1 2 5) ) ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output/ch09-figure-4-wage-age-reg-ci-Stata.png", replace




