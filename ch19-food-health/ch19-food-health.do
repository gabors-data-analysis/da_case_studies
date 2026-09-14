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
* Chapter 19
* CH19A Food and health
* using the food-health dataset
* version 1.1 2026-09-04
*
* STATA VERSION: This code is written for Stata 18
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"



* STEP 2: * Directory for data
* Option 1: run directory-setting do file
capture do set-data-directory.do
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"


global data_in  "${data_dir}/food-health/clean"
global work  	"ch19-food-health"

capture mkdir 		"${work}/output"
global output 	"${work}/output"

* load clean data
use "${data_in}/food-health.dta", clear
* Or download directly from OSF:
/*
copy "https://osf.io/download/wtybg/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* simpler variable names
* fruit and vegetables
generate fv= veggies_n_fruits_gr
 label variable fv "Fruit and vegetables per day (grams)"

* blood pressure
generate bp = blood_pressure
 label variable bp "Blood pressure (systolic+diastolic)"
summarize fv,d

* exercising
generate exerc = paq655 if paq655 <=7 
 replace exerc =0  if paq650==2
 label variable exerc "Days per week exercising"
tabulate exerc,mis
label variable hh_income_percap "Household income per capita (US dollars)"

* potato chips
generate pchips = gr_potato_chips
 label variable pchips "Potato chips per day (grams)"

* sample design
keep if age>=30 & age<60
drop if fv>3200
drop if bp==.
count


****************************
* Descriptive table
eststo clear
estpost sum bp fv ,d
esttab using "${output}/ch19-table-1-des-Stata.tex", replace ///
 cells("mean(fmt(0)) p50(fmt(0)) sd(fmt(0)) min(fmt(0)) max(fmt(0)) count(fmt(0))") ///
 label noobs nonum ///
 collabels(Mean Median Std.Dev. Minimum Maximum Observations)


*********************************
* Scatterplot and regression line:
* blood pressure on fruit and vegetables

scatter bp fv, ms(o) mc(navy*0.6) msize(small) ///
 || lfit bp fv, lw(thick) lc(green) ///
    xlab(0(500)3000, grid) ylab(140(20)280, grid) legend(off) ///
	ytitle("Blood pressure (systolic+diastolic)")
graph export "${output}/ch19-figure-8a-fv-bp-Stata.png",replace

twoway lfit bp fv, lw(thick) lc(green) ///
    xlab(0(500)3000, grid) ylab(180(2)200, grid) legend(off) ///
	ytitle("Blood pressure (systolic+diastolic)")
graph export "${output}/ch19-figure-8a-fv-bp-reg-Stata.png",replace


*lowess bp fv
*lowess bp fv if fv<1000

*********************************
* Scatterplot and regression line:
* fruit and vegetables on ln income, exercising, potato chips

generate lninc=ln(hh_income_percap)
 label variable lninc "Log household income per capita"

scatter fv lninc, ms(o) mc(navy*0.6) msize(small) ///
 || lfit fv lninc, lw(thick) lc(green) ///
    xlab(6(1)12, grid) ylab(0(500)2000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day (grams)")
graph export "${output}/ch19-figure-9a-fv-inc-Stata.png",replace

scatter fv exerc, ms(o) mc(navy*0.6) msize(small) ///
 || lfit fv exerc, lw(thick) lc(green) ///
    xlab(0(1)7, grid) ylab(0(500)3000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day (grams)")
graph export "${output}/ch19-figure-9a-fv-exerc-Stata.png",replace

scatter fv pchips , ms(o) mc(navy*0.6) msize(small) ///
 || lfit fv pchips , lw(thick) lc(green) ///
    xlab(, grid) ylab(0(500)3000, grid) legend(off) ///
	ytitle("Fruit and vegetables per day (grams)")
graph export "${output}/ch19-figure-10-fv-pchips-Stata.png",replace

