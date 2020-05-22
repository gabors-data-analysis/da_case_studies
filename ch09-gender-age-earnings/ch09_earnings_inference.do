*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* FUNDAMENTALS OF REGRESSION ANALYSIS
* ILLUSTRATION STUDY FOR CHAPTER 9
*
* DATA US CPS 2014
*********************************************************************

* WHAT THIS CODES DOES:

* Loads the data csv
* Filter the dataset and save a sample used in the analysis
* Transforms variables
* shows level and log regressions
* shows non-linear models such as splines
* shows CI and PI
* bootstrap
* v1.9 - 2019-11-05

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/"

 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 
global data_in	 "da_data_repo/cps-earnings/clean" 
global data_out  "da_case_studies/ch09-gender-age-earnings/"
global output    "da_case_studies/ch09-gender-age-earnings/output"

clear
use "$data_in/morg-2014-emp.dta"


gen female=sex==2
lab var earnwke "Earnings per week"
lab var uhours "Usual hours per week"
gen w = earnwke/uhours
lab var w "Earnings per hour"

gen lnw=ln(w)
lab var lnw "ln earnings per hour"

* full sample regression
reg lnw female 



** SELECT OCCUPATION
gen sample=0
replace sample=1 if occ2012==0735 /* Market research analysts and marketing specialists */
replace sample=2 if occ2012>=1005 & occ2012<=1240 /* ** Computer and Mathematical Occupations */
label define sa 1 "market research analysts" 2 "computer sci occupations"
label value sample sa

keep if sample==1 | sample==2
tab sample


order hhid-stfips weight earnwke uhours w lnw female age ind occ
compress

* set sample here
keep if sample==1 /* change to =2 */
saveold "$data_out/earnings_inference.dta",replace

use     "$data_out/earnings_inference.dta" ,clear
	cap erase "$output/earnings_female.tex"

	*******************************************
	** DISTRIBUTION OF EARNINGS
	*******************************************

	tabstat earnwke uhours w , s(mean min p5 p50 p95 max n) col(s)
	tabstat earnwke uhours w if w>=1, s(mean min p5 p50 p95 max n) col(s)

	*hist w , width(5) percent
	*hist lnw , width(0.2) percent

	*******************************************
	** LN EARNINGS AND GENDER
	*******************************************

	tab female 
	tab occ female 

	reg lnw female 
	reg lnw female , robust
	 outreg2 using "$output/T09_reg1.tex", 2aster tex(frag) nonotes bdec(2)  replace 



	*******************************************
	** LN EARNINGS AND AGE
	*******************************************
	 use "$data_out/earnings_inference.dta" ,replace
	 

	lowess lnw age if sample==1, lineop( lw(vthick) lc(dkgreen) ) ///
	 mcolor(navy) msize(small) ///
	 xlab(20(10)60, grid) ylab(1.5(0.5)4.5, grid) ///
	 graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) 
	 graph export "$output/earnings_age_lowess.png", replace


	
	* create splines
	mkspline agesp1 30 agesp2 40 agesp3 = age 
	gen agesq = age^2

	 * label the variables e.g. agesq as age square
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
	  
  	**********************************
 	* linear regressions - intervals
	**********************************

	 predict lnwpred_ageq
	  la var lnwpred_ageq "quadratic"
	 predict lnwpred_ageqSE , stdp
	 cap gen lnwpred_ageqCIUP = .
	 replace lnwpred_ageqCIUP = lnwpred_ageq + 2*lnwpred_ageqSE
	 cap gen lnwpred_ageqCILO = .
	 replace lnwpred_ageqCILO = lnwpred_ageq - 2*lnwpred_ageqSE

	  
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
	line lnwpred_agel lnwpred_agesp lnwpred_ageq age,  sort ///
	 lp(solid dash shortdash) lw(vthick thick thick) lc(blue red black) ///
	 xlab(20(10)60, grid) ylab(2.6(0.2)3.4, grid) legend(rows(1)) ytitle("ln earnings per hour") ///
	   graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white))
	 graph export "$output/earnings_age_functions_noCI.png", replace
	more
	 
	* all three visualized, parametric ones with CI 
	line lnwpred_agel lnwpred_agesp lnwpred_agespCIUP lnwpred_agespCILO ///
	 lnwpred_ageq lnwpred_ageqCIUP lnwpred_ageqCILO age , sort ///
	 lp(solid dash dash dash shortdash shortdash shortdash) ///
	 lw(vthick thick thin thin thick thin thin) ///
	 lc(blue red red red black black black) ///
	 xlab(20(10)60, grid) ylab(2.6(0.2)3.4, grid) ytitle("ln earnings per hour") ///
	  legend(rows(1)  order(1 2 5) ) ///
	   graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) 
	 graph export "$output/earnings_age_functions_CI.png", replace
	more

*******************************************
* CI and PI
* run for sample 1 and 2
*******************************************


 * add CI for a linear
  reg lnw age if sample==1, r
  cap drop SE
  predict SE, stdp
  graph twoway lfitci lnw age if  lnw<4.4 & lnw>2, stdp || scatter lnw age if  lnw<4.4 & lnw>2, sort ///
  mcolor(navy) ytitle("age") title("Confidence bands for predicted value") ///
   xlab(20(10)60, grid) ylab(2(0.4)4.4, grid) legend(rows(1)) ytitle("ln earnings per hour") ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/F9_earnings_51notused.png", replace

 * add PI for a linear
 reg lnw age if sample==1
  cap drop SEyhat
  predict SEyhat, stdf
  graph twoway lfitci lnw age if lnw<4.4 & lnw>2, stdf || scatter lnw age if  lnw<4.4 & lnw>2, sort ///
  mcolor(navy) ytitle("age") title("Confidence bands for individual case predictions in our data") ///
   xlab(20(10)60, grid) ylab(2(0.4)4.4, grid) legend(rows(1)) ytitle("ln earnings per hour") ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/F9_earnings_61notused.png", replace




*******************************************
* bootstrap
*******************************************
* Market research occ
use "$data_out/earnings_inference.dta" ,replace
set seed 201711
bootstrap, reps(1000) saving("$output\b_earnings_female",replace): reg lnw female if sample==1
use "$output\b_earnings_female",replace
hist _b_female, percent width(0.025) addlabel ///
 color(ltblue) lcolor(navy) xline(-0.11, lw(vthick) ) ///
 xlab(-0.23 -0.11 0.01, grid) xtitle("Slope coefficients from bootstrap samples") ///
   graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/earnings_gender_bootstrap.png",replace

 
