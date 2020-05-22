*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* MULTIVARIATE REGRESSION MODEL
* ILLUSTRATION STUDY FOR CHAPTER 10
*
* DATA US CPS 2014
*********************************************************************

* WHAT THIS CODES DOES:

* Loads the data csv
* Filter the dataset and save a sample used in the analysis
* Transforms variables
* Check distribution of variables
* Run regression with multiple explanatory variables
* Run regression with interactions

*****
********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
* cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
* cd "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in	 "da_data_repo/cps-earnings/clean" 
global data_out  "da_case_studies/ch10-gender-earning-understand/"
global output    "da_case_studies/ch10-gender-earning-understand/output"

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
save "$data_out/earnings_multireg.dta",replace



*******************************************
** DISTRIBUTION OF EARNINGS
use "$data_out/earnings_multireg.dta",replace

tabstat earnwke uhours w , s(mean min p5 p50 p95 max n) col(s)
tabstat earnwke uhours w if  w>=1, s(mean min p5 p50 p95 max n) col(s)

*******************************************
** LN EARNINGS, GENDER, AGE
*lpoly lnw age, nosca


reg lnw female , robust
 outreg2 using "$output/T10_reg1.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female age, robust
 outreg2 using "$output/T10_reg1.tex", 2aster tex(frag) nonotes bdec(3) append
reg age female , robust
 outreg2 using "$output/T10_reg1.tex", 2aster tex(frag) nonotes bdec(3) append

 
lab def female 0 male 1 female
lab value female female 
hist age, by(female) width(5) fcol(ltblue) lcol(navy) percent ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F10_earnings_hist.png",replace

*******************************************
** LN EARNINGS, GENDER, AGE,

gen agesq = age^2
gen agecu = age^3
gen agequ = age^4

reg lnw female , robust
 outreg2 using "$output/T10_reg2.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female age , robust
 outreg2 using "$output/T10_reg2.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female age agesq , robust
 outreg2 using "$output/T10_reg2.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female age* , robust
 outreg2 using "$output/T10_reg2.tex", 2aster tex(frag) nonotes bdec(3) append

*******************************************
** LN EARNINGS, EDU CATEG
use "$data_out/earnings_multireg.dta",replace

gen ed_MA = grade==44
gen ed_Profess = grade==45
gen ed_PhD = grade==46

reg lnw female , robust
 outreg2 using "$output/T10_reg3.tex", 2aster tex(frag) nonotes bdec(3) replace 
reg lnw female ed_Prof ed_PhD, robust
 outreg2 using "$output/T10_reg3.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw female ed_MA ed_Prof, robust
 outreg2 using "$output/T10_reg3.tex", 2aster tex(frag) nonotes bdec(3) append

*******************************************
** SIMPLE INTERACTION: LINEAR AGE WITH GENDER

gen fXage = female*age
label var fXage "female X age"
label var lnw "lnw"
reg lnw age if female==1, robust
 outreg2 using "$output/T10_reg4.tex", label  ctitle("WOMEN", "lnw") 2aster tex(frag) nonotes bdec(3) replace
reg lnw age if female==0, robust
 outreg2 using "$output/T10_reg4.tex", label ctitle("MEN", "lnw") 2aster tex(frag) nonotes bdec(3) append
reg lnw age female fXage , robust
 outreg2 using "$output/T10_reg4.tex", label ctitle("ALL", "lnw") sortvar(female age fXage) 2aster tex(frag) nonotes bdec(3) append

 
 
 
 
 
*******************************************
** FOR RPEDICTIONL LINEAR & INTERACTIONS WITH GENDER

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

lab var lnwhat_m "Men"
lab var lnwhat_f "Women"

line lnwhat_m lnwhat_mCIup lnwhat_mCIlo lnwhat_f lnwhat_fCIup lnwhat_fCIlo age, ///
	sort lw(thick medium medium thick medium medium) ///
	lcolor("green" "green" "green" "navy" "navy" "navy") ///
	lp(dash dash dash solid solid solid) ///
	ylab(2.8(0.1)3.8, grid) xlab(25(5)65, grid) ytitle("Log hourly earnings") ///
	legend(order(1 4)) ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white)) 

	graph export "$output/F10_earnings_interact0.png", replace


 
 
 
 
 
 
*******************************************
** FOR RPEDICTIONL FUNCTIONAL FORMS & INTERACTIONS WITH GENDER

gen agesq = age^2
gen agecu = age^3
gen agequ = age^4

*label fXage 
gen fXagesq = female*agesq
gen fXagecu  = female*agecu
gen fXagequ  = female*agequ

cap drop lnwhat*

reg lnw age agesq agecu agequ if female==1, robust
 outreg2 using "$output/T10_regnotused.tex", 2aster tex(frag) nonotes bdec(3) replace
reg lnw age agesq agecu agequ if female==0, robust
 outreg2 using "$output/T10_regnotused.tex", 2aster tex(frag) nonotes bdec(3) append
reg lnw age agesq agecu agequ female fX* , robust
 outreg2 using "$output/T10_regnotused.tex", 2aster tex(frag) nonotes bdec(3) append

predict lnwhat_m if female==0 
 predict lnwhatse_m if female==0, stdp
predict lnwhat_f if female==1 
 predict lnwhatse_f if female==1 , stdp
gen lnwhat_mCIup = lnwhat_m + 2*lnwhatse_m 
gen lnwhat_mCIlo = lnwhat_m - 2*lnwhatse_m 
gen lnwhat_fCIup = lnwhat_f + 2*lnwhatse_f 
gen lnwhat_fCIlo = lnwhat_f - 2*lnwhatse_f 

lab var lnwhat_m "Men"
lab var lnwhat_f "Women"

line lnwhat_m lnwhat_mCIup lnwhat_mCIlo lnwhat_f lnwhat_fCIup lnwhat_fCIlo age, ///
	sort lw(thick medium medium thick medium medium) ///
	lcolor("green" "green" "green" "navy" "navy" "navy") ///
	lp(dash dash dash solid solid solid) ///
	ylab(2.8(0.1)3.8, grid) xlab(25(5)65, grid) ytitle("Log hourly earnings") ///
	legend(order(1 4)) ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white)) 
	 graph export "$output/F10_earnings_interact.png", replace



 
	 
	 

**********************************************
* PART II
* CAUSAL ANALYIS - IS IT DISCRIMINATION?
**********************************************

use "$data_out/earnings_multireg.dta",replace

*******************************************
* focus on homogenous age group
keep if age>=40 & age<=60

**encoding class94 to factors:
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
**encoding class94 to factors:
encode prcitshp, gen(a)
drop prcitshp
rename a prcitshp
gen nonUSborn = prcitshp==4 | prcitshp==5
**
gen edMA = grade==44
gen edProf = grade==45
gen edPhd = grade==46

global DEMOG1 age afram hisp asian othernonw nonUSborn edProf edPhd 

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
global DEMOG2 married divorced wirowed child1-child4  i.stfips 

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

  
 **** SHORT TABLE FOR TEXTBOOK

*simple way, edited afterwards
reg lnw female , robust
  outreg2 using "$output/earnings_causal_short.tex", 2aster tex(frag) nonotes excel bdec(3) replace
reg lnw female age edProf edPhd, robust
  outreg2 using "$output/earnings_causal_short.tex", 2aster tex(frag) nonotes excel bdec(3) append
reg lnw female $DEMOG1 $DEMOG2 $WORK, robust
  outreg2 using "$output/earnings_causal_short.tex", 2aster tex(frag) nonotes excel bdec(3) append
reg lnw female $DEMOG1 $DEMOG2 $WORK agesq-agequ hourssq-hoursqu, robust
  outreg2 using "$output/earnings_causal_short.tex", 2aster tex(frag) nonotes excel bdec(3) append

 
 
 * nicer way
 label var lnw "lnw"
reg lnw female , robust
  outreg2 using "$output/T10_reg5.tex", label nocon 2aster tex(frag) nonotes excel bdec(3) replace
label var age "Age and educ"
reg lnw female age edProf edPhd, robust
  outreg2 using "$output/T10_reg5.tex", label keep(female age) nocon ststr(replace coef="YES" if varname=="age", replace se="" if varname=="age") 2aster tex(frag) nonotes excel bdec(3)  append
label var afram "Family background"
label var stfip "State of residence"
label var hours "Hours worked"
label var nonprof "Government or private"
label var union "Union member"
label var ind2dig "Industry"
label var occ2dig "Occupation"
label var agesq "Age in polynomial"
label var hourssq "Hours in polynomial"
reg lnw female $DEMOG1 $DEMOG2 $WORK, robust
  outreg2 using "$output/T10_reg5.tex", label keep(female age afram stfip hours nonprof union ind2dig ind2dig occ2dig) ///
  nocon ststr(replace coef="YES" if varname!="female", replace se="" if varname!="female") ///
  sortvar(female age afram noUSborn hours nonprof union ind2dig ind2dig occ2dig) 2aster tex(frag) nonotes excel bdec(3) append
reg lnw female $DEMOG1 $DEMOG2 $WORK agesq-agequ hourssq-hoursqu, robust
  outreg2 using "$output/T10_reg5.tex", label keep(female age afram nonUSborn hours nonprof union ind2dig ind2dig occ2dig agesq hourssq) ///
  nocon ststr(replace coef="YES" if varname!="female", replace se="" if varname!="female") ///
  sortvar(female age afram noUSborn hours nonprof union ind2dig ind2dig occ2dig) 2aster tex(frag) nonotes excel bdec(3) append
