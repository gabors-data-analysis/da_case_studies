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
* Chapter 20
* CH20A Working from home and employee performance
* using the working-from-home dataset
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


global data_in  "${data_dir}/working-from-home/clean"
global work  	"ch20-working-from-home"

capture mkdir 		"${work}/output"
global output 	"${work}/output"




*******************************************************
* Create workfile from clean tidy data
* same as tidy person-level data but variables ordered 
* so balance table is easier to make

use "${data_in}/wfh_tidy_person", clear
* Or download directly from OSF:
/*
copy "https://osf.io/download/jrydb/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

order personid treatment ordertaker type quitjob phonecalls0 phonecalls1 ///
  perform10 perform11 age male second_technical high_school tertiary_technical university ///
  prior_experience tenure married children ageyoungestchild rental costofcommute ///
  bedroom internet basewage bonus grosswage 


save "${work}/ch20-wfh-workfile", replace


*******************************************************
* Analysis

* Balance

use "${work}/ch20-wfh-workfile", replace

*des perform10 age-grosswage

replace ageyoungestchild = . if children==0


* Table 20.1
* here produced from bits and pieces

* First part: table with means and sd
* tabstat in Stata, copied to Excel, 
* to laTex used https://www.latex-tables.com/
tabstat perform10 age-grosswage ordertaker  if treatment==1, c(s) format(%5.2f)
tabstat perform10 age-grosswage ordertaker  if treatment==0, c(s) format(%5.2f)
tabstat perform10 age-grosswage ordertaker, s(sd) c(s) format(%5.2f)

* Second part: t-tests for equal means (we do them by regression for simplicity)
* need to enter p-values one by one to LaTex or Excel table
foreach z of varlist perform10 age-grosswage ordertaker {
	regress treatment `z', robust nohead
}



* outcomes: 
* quit firm during 8 months of experiment
* # phone calls worked, for order takers

des quitjob phonecalls1


tabstat quitjob , by(treatment) s(mean sd n)
tabstat phonecalls1 if ordertaker==1, by(treatment) s(mean sd n)

* Bar chart for quit rates
generate quit_pct = quitjob*100
generate stayed_pct = (1-quitjob)*100
label def treatment 0 "Working from office" 1 "Working from home"
label val treatment treatment
graph bar (mean) stayed_pct quit_pct, over(treatment) stack ///
 bar(1, col(navy*0.8)) bar(2, col(green*0.8)) ///
 ytitle("Percent of employees") ylabel(0(25)100) ///
 legend(label(1 "stayed") label(2 "quit"))
graph export "${output}/ch20-figure-1-wfh-quitrates-Stata.png", replace


 
* Regression 1: ATE estimates, no covariates
label variable treatment "Treatment group"
label variable quitjob "Quit job "
label variable phonecalls1 "Phone calls (thousand)"
la var married "Married"
label variable children "Children"
label variable internet "Internet at home"


regress quitjob treatment , robust
 outreg2 using "${output}/ch20-table-3-wfh-reg1-Stata", bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

regress phonecalls1 treatment if ordertaker==1, robust
 outreg2 using "${output}/ch20-table-3-wfh-reg1-Stata", bdec(2) sdec(2) 2aster tex(frag) nonotes label append


 * Regression 2: ATE estimates, with covariates of some unbalance

regress quitjob treatment married children internet, robust
 outreg2 using "${output}/ch20-table-4-wfh-reg2-Stata", bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

regress phonecalls1 treatment married children if ordertaker==1, robust
 outreg2 using "${output}/ch20-table-4-wfh-reg2-Stata", bdec(2) sdec(2) 2aster tex(frag) nonotes label append
 
 
