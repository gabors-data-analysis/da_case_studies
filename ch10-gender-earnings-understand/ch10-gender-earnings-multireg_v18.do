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
* Chapter 10
* CH10B Understanding gender differences in earnings
* using the cps-earnings dataset
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
********************************************************************
* LOAD DATA
********************************************************************

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
global work     "ch10-gender-earnings-understand"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"



clear
use "${data_in}/morg-2014-emp.dta"
* Or download directly from OSF:
/*
copy "https://osf.io/download/rtmga/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

count


********************************************************************
* SAMPLE SELECTION
********************************************************************

* for this exercise - postgrad only
keep if grade92>=44 /* MA or professional degree of doctorate */
keep if age>=24
keep if uhours>=20
count


********************************************************************
* FEATURE ENGINEERING
********************************************************************

generate female=sex==2
label variable earnwke "Earnings per week"
label variable uhours "Usual hours per week"
generate w = earnwke/uhours
label variable w "Earnings per hour"

generate lnw=ln(w)
label variable lnw "ln earnings per hour"

compress

********************************************************************
* SAVE WORK FILE
********************************************************************

save "${work}/earnings_multireg.dta",replace



*****************************************

********************************************************************
* DISTRIBUTION OF EARNINGS
********************************************************************
use "${work}/earnings_multireg.dta",replace

tabstat earnwke uhours w , s(mean min p5 p50 p95 max n) col(s)
tabstat earnwke uhours w if  w>=1, s(mean min p5 p50 p95 max n) col(s)

*****************************************

********************************************************************
* BASIC REGRESSIONS: GENDER AND AGE
********************************************************************
*lpoly lnw age, nosca


regress lnw female , robust
 outreg2 using "${output}/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
 
regress lnw female age, robust
 outreg2 using "${output}/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
 
regress age female , robust
 outreg2 using "${output}/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append


* age distribution by gender
* histogram, not in textbook 
label define female 0 male 1 female
label value female female 
histogram age, by(female) start(20) width(5) fcol(navy*0.8) lcol(white) percent ///
 xla(20(10)60, grid) yla(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))

* density plots
kdensity age if female==0, gen(x0 y0) nogra
kdensity age if female==1, gen(x1 y1) nogra
line y1 y0 x0, lc(navy*0.8 green*0.5) lw(thick thick) ///
 xla(20(10)60, grid) yla(, grid) ///
 xtitle("Age (years)") ytitle("Density") ///
 legend(off) ///
 text(0.02 55 "Women") text(0.029 55 "Men")
graph export "${output}/ch10-figure-1-density-age-gender.png", as(png) replace
 


*****************************************

********************************************************************
* BASIC REGRESSIONS: GENDER AND AGE
********************************************************************

generate agesq = age^2
generate agecu = age^3
generate agequ = age^4

regress lnw female , robust
 outreg2 using "${output}/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
 
regress lnw female age , robust
 outreg2 using "${output}/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
 
regress lnw female age agesq , robust
 outreg2 using "${output}/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
 
regress lnw female age* , robust
 outreg2 using "${output}/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append

*****************************************
** LN EARNINGS, EDU CATEG
use "${work}/earnings_multireg.dta",replace

generate ed_MA = grade92==44
generate ed_Profess = grade92==45
generate ed_PhD = grade92==46

regress lnw female , robust
 outreg2 using "${output}/ch10-table-3-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
 
regress lnw female ed_Profess ed_PhD, robust
 outreg2 using "${output}/ch10-table-3-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
 
regress lnw female ed_MA ed_Profess, robust
 outreg2 using "${output}/ch10-table-3-reg2-Stata.tex", 2aster tex(frag) nonotes bdec(3) append

*****************************************
** SIMPLE INTERACTION: LINEAR AGE WITH GENDER

generate fXage = female*age
label variable fXage "female X age"
label variable lnw "lnw"

regress lnw age if female==1, robust
 outreg2 using "${output}/ch10-table-4-reg-Stata.tex", label  ctitle("WOMEN", "lnw") 2aster tex(frag) nonotes bdec(3) replace
 
regress lnw age if female==0, robust
 outreg2 using "${output}/ch10-table-4-reg-Stata.tex", label ctitle("MEN", "lnw") 2aster tex(frag) nonotes bdec(3) append
 
regress lnw age female fXage , robust
 outreg2 using "${output}/ch10-table-4-reg-Stata.tex", label ctitle("ALL", "lnw") sortvar(female age fXage) 2aster tex(frag) nonotes bdec(3) append

 
 
** LINEAR IN AGE INTERACTED WITH GENDER

regress lnw age female fX* , robust
capture drop lnwhat*
predict lnwhat_m if female==0 
predict lnwhatse_m if female==0, stdp
predict lnwhat_f if female==1 
predict lnwhatse_f if female==1 , stdp
generate lnwhat_mCIup = lnwhat_m + 2*lnwhatse_m 
generate lnwhat_mCIlo = lnwhat_m - 2*lnwhatse_m 
generate lnwhat_fCIup = lnwhat_f + 2*lnwhatse_f 
generate lnwhat_fCIlo = lnwhat_f - 2*lnwhatse_f 

* Figure 2a - Linear age model
line lnwhat_f lnwhat_fCIup lnwhat_fCIlo lnwhat_m lnwhat_mCIup lnwhat_mCIlo age, ///
	sort lw(thick medium medium thick medium medium) ///
	lcolor(green*0.5 green*0.5  green*0.5 navy*0.8 navy*0.8 navy*0.8) ///
	lp(solid dash dash solid dash dash) ///
	ylab(2.8(0.1)3.8, grid) xlab(25(5)65, grid) ///
	ytitle("ln(hourly earnings, US dollars)") xtitle(Age (years)) ///
	legend(off) ///
	text(3.24 42 "Women") text( 3.55 42 "Men") ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white)) 
graph export "${output}/ch10-figure-2a-reg-age-gender-lin-Stata.png", as(png) replace


** QUARTIC FUNCTIONAL FORM IN AGE INTERACTED WITH GENDER

generate agesq = age^2
generate agecu = age^3
generate agequ = age^4

*label fXage 
generate fXagesq = female*agesq
generate fXagecu  = female*agecu
generate fXagequ  = female*agequ

capture drop lnwhat*

regress lnw age agesq agecu agequ if female==1, robust
regress lnw age agesq agecu agequ if female==0, robust
regress lnw age agesq agecu agequ female fX* , robust

predict lnwhat_m if female==0 
predict lnwhatse_m if female==0, stdp
predict lnwhat_f if female==1 
predict lnwhatse_f if female==1 , stdp
generate lnwhat_mCIup = lnwhat_m + 2*lnwhatse_m 
generate lnwhat_mCIlo = lnwhat_m - 2*lnwhatse_m 
generate lnwhat_fCIup = lnwhat_f + 2*lnwhatse_f 
generate lnwhat_fCIlo = lnwhat_f - 2*lnwhatse_f 

* Figure 2b - Quartic age model
line lnwhat_f lnwhat_fCIup lnwhat_fCIlo lnwhat_m lnwhat_mCIup lnwhat_mCIlo age, ///
	sort lw(thick medium medium thick medium medium) ///
	lcolor(green*0.5 green*0.5  green*0.5 navy*0.8 navy*0.8 navy*0.8) ///
	lp(solid dash dash solid dash dash) ///
	ylab(2.8(0.1)3.8, grid) xlab(25(5)65, grid) ///
	ytitle("ln(hourly earnings, US dollars)") xtitle(Age (years)) ///
	legend(off) ///
	text(3.3 42 "Women") text(3.65 42 "Men") ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white)) 
graph export "${output}/ch10-figure-2b-reg-age-gender-nonlin-Stata.png", as(png) replace


* CAUSAL ANALYSIS
* focus on middle-aged
use "${work}/earnings_multireg.dta",replace
keep if age>=40 & age<=60

**encoding class94 to numerical values:
encode class, gen(temp)
drop class
rename temp class
drop if class>=6 /* self-employed or without pay */


* Pre-determined demographics
generate white = race==1
generate afram = race==2
generate asian = race==4
generate hisp  = ethnic>=1 & ethnic<=8
generate othernonw = white==0 & afram==0 & asian==0 & hisp==0
summarize white-othernonw
**encoding class94 to numerical values:
encode prcitshp, gen(a)
drop prcitshp
rename a prcitshp
generate nonUSborn = prcitshp==4 | prcitshp==5
**
generate edMA = grade92==44
generate edProf = grade92==45
generate edPhd = grade92==46

global DEMOG age afram hisp asian othernonw nonUSborn edProf edPhd 

* age in polynomial
generate agesq = age^2
generate agecu = age^3
generate agequ = age^4
  
* Potentially endogenous demographics
generate married = marital==1 | marital==2
generate divorced = marital==3 | marital==5 | marital==6
generate widowed = marital==4
generate nevermar = marital==7

generate child0 = chldpres==0
generate child1 = chldpres==1
generate child2 = chldpres==2
generate child3 = chldpres==3
generate child4pl = chldpres>=4

**encoding class94 to factors:
encode stfips, gen(b)
drop stfips
rename b stfips
**
global FAMILY married divorced widowed child1-child4pl  i.stfips 

* Work-related variables
generate hours = uhours
generate fedgov = class==1
generate stagov = class==2
generate locgov = class==3
generate nonprof = class==5
**encoding ind02 to factors:
encode ind02, gen(c)
drop ind02
rename c ind02
generate ind2dig = int(ind02/100)
**
generate occ2dig = int(occ2012/100)

**create a unified union variable =1 if union member
generate union= (unioncov=="Yes" | unionmme=="Yes")
tabulate union

**
global WORK hours fedgov stagov locgov nonprof union i.ind2dig i.occ2dig

* hours in polynomial
generate hourssq = hours^2
generate hourscu = hours^3
generate hoursqu = hours^4

  
* Table 10.5
* simple way, edited afterwards
regress lnw female , robust
  outreg2 using "${output}/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) replace
   
regress lnw female age edProf edPhd, robust
  outreg2 using "${output}/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append
   
regress lnw female $DEMOG $FAMILY $WORK, robust
  outreg2 using "${output}/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append
   
regress lnw female $DEMOG $FAMILY $WORK agesq-agequ hourssq-hoursqu, robust
  outreg2 using "${output}/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append
