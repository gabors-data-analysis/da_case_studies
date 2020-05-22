
***************************************************************
* Case Study: Food and health
* Data: food-health


***************************************************************

* v 1.0 2019-08-10 new version first design
* v 1.1 2019-10-20 second run
* v 1.2 2019-12-10 folders adjusted


* WHAT THIS CODES DOES:
* data manage, graphs and simple regressions

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
* cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in	  "da_data_repo/food-health/clean" 
global data_out	  "da_case_studies/ch19-food-health"
global output     "da_case_studies/ch19-food-health/output"



* need to alter all file locations
use "$data_in/food-health.dta", clear
keep if age>=30 & age<60


gen fv = veggies_n_fruit_g
 lab var fv "Fruit and vegetables per day, grams"
*drop if fv==0
 
gen bp = blood_p
 lab var bp "Blood pressure (systolic+diastolic)"

sum fv,d
drop if fv>3200
drop if bp==.

gen exerc = paq655 if paq655 <=7 
 replace exerc =0  if paq650==2
 lab var exerc "Days per week exercising"
tab exerc,mis
lab var hh_income_percap "Household income per capita (dollars)"

gen pchips = gr_potato_chips
 lab var pchips "Potato chips per day, grams"

****************************
* Descriptive table
eststo clear
estpost sum bp fv ,d
esttab using "$output/food-health-destab.tex", replace ///
 cells("mean(fmt(0)) p50(fmt(0)) sd(fmt(0)) min(fmt(0)) max(fmt(0)) count(fmt(0))") ///
 label noobs nonum ///
 collabels(Mean Median Std.Dev. Minimum Maximum Observations)


*********************************
* Scatterplot and regression line:
* blood pressure on fruit and vegetables
*
scatter bp fv, ms(oh) mc(mint) ///
 || lfit bp fv, lw(thick) lc(navy) ///
    xlab(0(500)3000, grid) ylab(120(20)260, grid) legend(off) ///
	ytitle("Blood pressure (systolic+diastolic)")
 graph export "$output/fv-bp-1.png",replace
 
twoway lfit bp fv, lw(thick) lc(navy) ///
    xlab(0(500)3000, grid) ylab(182(2)198, grid) legend(off) ///
	ytitle("Blood pressure (systolic+diastolic)")
 graph export "$output/fv-bp-2.png",replace


*lowess bp fv
*lowess bp fv if fv<1000

*********************************
* Scatterplot and regression line:
* fruit and vegetables on ln income, exercising, potato chips
*

gen lninc=ln(hh_income_per)
 lab var lninc "Log household income per capita"

scatter fv lninc, ms(oh) mc(mint) ///
 || lfit fv lninc, lw(thick) lc(navy) ///
    xlab(6(1)12, grid) ylab(0(500)3000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day, grams")
 graph export "$output/fv-inc.png",replace

scatter fv exerc, ms(oh) mc(mint) ///
 || lfit fv exerc, lw(thick) lc(navy) ///
    xlab(0(1)7, grid) ylab(0(500)3000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day, grams")
 graph export "$output/fv-exerc.png",replace

scatter fv pchips if pchips<400, ms(oh) mc(mint) ///
 || lfit fv pchips if pchips<400, lw(thick) lc(navy) ///
    xlab(, grid) ylab(0(500)3000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day, grams")
 graph export "$output/fv-pchips.png",replace


reg bp fv 

/* by 3 groups of income

egen inc3groups = cut(hh_income_per), group(3)
tabstat inc, by(inc3groups) s(min max n)

reg bp fv if inc3==0, nohead
 predict bp_i1 if e(sample)
reg bp fv if inc3==1, nohead
 predict bp_i2 if e(sample)
reg bp fv if inc3==2, nohead
 predict bp_i3 if e(sample)
line bp_i1 fv, lw(thick) lc(ltblue) ///
 || line bp_i2 fv, lw(thick) lc(blue)  ///
 || line bp_i3 fv, lw(thick) lc(navy) ///
 ylab(184(2)196, grid) xlab(0(500)2000, grid) ///
 ytitle("Blood pressure (systolic+diastolic)") ///
 legend(label(1 "lowest thrid") lab(2 "middle third") ///
 lab(3 "highest third") rows(1))
graph export "$output/fv-bp-inc.png",replace
more

* smoking

reg bp fv if smoke==0, nohead
 predict bp_s0 if e(sample)
reg bp fv if smoke==1, nohead
 predict bp_s1 if e(sample)
line bp_s1 fv, lw(thick) lc(ltblue) ///
 || line bp_s0 fv, lw(thick) lc(navy)  ///
 ylab(184(2)196, grid) xlab(0(500)2000, grid) ///
 ytitle("Blood pressure (systolic+diastolic)") ///
 legend(label(1 "smoker") label(2 "nonsmoker") rows(1))
graph export "$output/fv-bp-smoke.png",replace
more

* exercising

reg bp fv if exerc==0, nohead
 predict bp_e0 if e(sample)
reg bp fv if exerc==1, nohead
 predict bp_e1 if e(sample)
line bp_s1 fv, lw(thick) lc(ltblue) ///
 || line bp_s0 fv, lw(thick) lc(navy)  ///
 ylab(184(2)196, grid) xlab(0(500)2000, grid) ///
 ytitle("Blood pressure (systolic+diastolic)") ///
 legend(label(1 "doesn't exercise") label(2 "exercises") rows(1))
graph export "$output/fv-bp-exerc.png",replace





