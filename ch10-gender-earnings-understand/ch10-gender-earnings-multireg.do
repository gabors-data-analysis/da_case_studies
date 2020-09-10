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
* CH10A Understanding the gender difference in earnings
* using the cps-earnings dataset
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

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
global work  	"ch10-gender-earnings-understand"

cap mkdir 		"$work/output"
global output 	"$work/output"



clear
use "$data_in/morg-2014-emp.dta"
count

count

* for this exercise - postgrad only
keep if grade92>=44 /* MA or professional degree of doctorate */
keep if age>=24
keep if uhours>=20
count

gen female=sex==2
lab var earnwke "Earnings per week"
lab var uhours "Usual hours per week"
gen w = earnwke/uhours
lab var w "Earnings per hour"

gen lnw=ln(w)
lab var lnw "ln earnings per hour"

compress
save "$work/earnings_multireg.dta",replace



*******************************************
** DISTRIBUTION OF EARNINGS
use "$work/earnings_multireg.dta",replace

tabstat earnwke uhours w , s(mean min p5 p50 p95 max n) col(s)
tabstat earnwke uhours w if  w>=1, s(mean min p5 p50 p95 max n) col(s)

*******************************************
** LN EARNINGS, GENDER, AGE
*lpoly lnw age, nosca


reg lnw female , robust
 outreg2 using "$output/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female age, robust
 outreg2 using "$output/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
reg age female , robust
 outreg2 using "$output/ch10-table-1-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append


* age distribution by gender
* histogram, not in textbook 
lab def female 0 male 1 female
lab value female female 
hist age, by(female) start(20) width(5) fcol(navy*0.8) lcol(white) percent ///
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
graph export "$output/ch10-figure-1-density-age-gender.png", replace
 


*******************************************
** LN EARNINGS, GENDER, AGE,

gen agesq = age^2
gen agecu = age^3
gen agequ = age^4

reg lnw female , robust
 outreg2 using "$output/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female age , robust
 outreg2 using "$output/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female age agesq , robust
 outreg2 using "$output/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female age* , robust
 outreg2 using "$output/ch10-table-2-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append

*******************************************
** LN EARNINGS, EDU CATEG
use "$work/earnings_multireg.dta",replace

gen ed_MA = grade==44
gen ed_Profess = grade==45
gen ed_PhD = grade==46

reg lnw female , robust
 outreg2 using "$output/ch10-table-3-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female ed_Prof ed_PhD, robust
 outreg2 using "$output/ch10-table-3-reg-Stata.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female ed_MA ed_Prof, robust
 outreg2 using "$output/ch10-table-3-reg2-Stata.tex", 2aster tex(frag) nonotes bdec(3) append

*******************************************
** SIMPLE INTERACTION: LINEAR AGE WITH GENDER

gen fXage = female*age
label var fXage "female X age"
label var lnw "lnw"
reg lnw age if female==1, robust
 outreg2 using "$output/ch10-table-4-reg-Stata.tex", label  ctitle("WOMEN", "lnw") 2aster tex(frag) nonotes bdec(3) replace
reg lnw age if female==0, robust
 outreg2 using "$output/ch10-table-4-reg-Stata.tex", label ctitle("MEN", "lnw") 2aster tex(frag) nonotes bdec(3) append
reg lnw age female fXage , robust
 outreg2 using "$output/ch10-table-4-reg-Stata.tex", label ctitle("ALL", "lnw") sortvar(female age fXage) 2aster tex(frag) nonotes bdec(3) append

 
 
 
** LINEAR IN AGE INTERACTED WITH GENDER

reg lnw age female fX* , robust
cap drop lnwhat*
predict lnwhat_m if female==0 
 predict lnwhatse_m if female==0, stdp
predict lnwhat_f if female==1 
 predict lnwhatse_f if female==1 , stdp
gen lnwhat_mCIup = lnwhat_m + 2*lnwhatse_m 
gen lnwhat_mCIlo = lnwhat_m - 2*lnwhatse_m 
gen lnwhat_fCIup = lnwhat_f + 2*lnwhatse_f 
gen lnwhat_fCIlo = lnwhat_f - 2*lnwhatse_f 

* Figure 2a
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
graph export "$output/ch10-figure-2a-reg-age-gender-lin-Stata.png", replace


** QUARTIC FUNCTIONAL FORM IN AGE INTERACTED WITH GENDER

gen agesq = age^2
gen agecu = age^3
gen agequ = age^4

*label fXage 
gen fXagesq = female*agesq
gen fXagecu  = female*agecu
gen fXagequ  = female*agequ

cap drop lnwhat*

reg lnw age agesq agecu agequ if female==1, robust
reg lnw age agesq agecu agequ if female==0, robust
reg lnw age agesq agecu agequ female fX* , robust

predict lnwhat_m if female==0 
 predict lnwhatse_m if female==0, stdp
predict lnwhat_f if female==1 
 predict lnwhatse_f if female==1 , stdp
gen lnwhat_mCIup = lnwhat_m + 2*lnwhatse_m 
gen lnwhat_mCIlo = lnwhat_m - 2*lnwhatse_m 
gen lnwhat_fCIup = lnwhat_f + 2*lnwhatse_f 
gen lnwhat_fCIlo = lnwhat_f - 2*lnwhatse_f 

* Figure 2b
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
graph export "$output/ch10-figure-2b-reg-age-gender-nonlin-Stata.png", replace


* CAUSAL ANALYSIS
* focus on niddle-aged
use "$work/earnings_multireg.dta",replace
keep if age>=40 & age<=60

**encoding class94 to numberical values:
encode class, gen(temp)
drop class
rename temp class
drop if class>=6 /* self-employed or without pay */


* Pre-determined demographics
gen white = race==1
gen afram = race==2
gen asian = race==4
gen hisp  = ethn>=1 & ethn<=8
gen othernonw = white==0 & afram==0 & asian==0 & hisp==0
sum white-othernonw
**encoding class94 to numberical values:
encode prcitshp, gen(a)
drop prcitshp
rename a prcitshp
gen nonUSborn = prcitshp==4 | prcitshp==5
**
gen edMA = grade==44
gen edProf = grade==45
gen edPhd = grade==46

global DEMOG age afram hisp asian othernonw nonUSborn edProf edPhd 

* age in polynomial
gen agesq = age^2
gen agecu = age^3
gen agequ = age^4
  
* Potentially endogeneous demographics
gen married = marital==1 | marital==2
gen divorced = marital==3 | marital==5 | marital==6
gen wirowed = marital==4
gen nevermar = marital==7

gen child0 = chldpres==0
gen child1 = chldpres==1
gen child2 = chldpres==2
gen child3 = chldpres==3
gen child4pl = chldpres>=4

**encoding class94 to factors:
encode stfips, gen(b)
drop stfips
rename b stfips
**
global FAMILY married divorced wirowed child1-child4  i.stfips 

* Work-related variables
gen hours = uhours
gen fedgov = class==1
gen stagov = class==2
gen locgov = class==3
gen nonprof = class==5
**encoding ind02 to factors:
encode ind02, gen(c)
drop ind02
rename c ind02
gen ind2dig = int(ind02/100)
**
gen occ2dig = int(occ2012/100)

**create a unified union variable =1 if union member
gen union= (unioncov=="Yes" | unionmme=="Yes")
tab union

**
global WORK hours fedgov stagov locgov nonprof union i.ind2dig i.occ2dig

* hours in ploynomial
gen hourssq = hours^2
gen hourscu = hours^3
gen hoursqu = hours^4

  
* Table 10.5
* simple way, edited afterwards
reg lnw female , robust
  outreg2 using "$output/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) replace
reg lnw female age edProf edPhd, robust
  outreg2 using "$output/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append
reg lnw female $DEMOG $FAMILY $WORK, robust
  outreg2 using "$output/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append
reg lnw female $DEMOG $FAMILY $WORK agesq-agequ hourssq-hoursqu, robust
  outreg2 using "$output/ch10-table-5-earnings-causal-Stata.tex", 2aster tex(frag) ///
   keep(female) nonotes bdec(3) append

