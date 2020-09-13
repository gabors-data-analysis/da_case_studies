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
* version 0.9 2020-09-13
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


global data_in  "$data_dir/working-from-home/clean"
global work  	"ch20-working-from-home"

cap mkdir 		"$work/output"
global output 	"$work/output"




*******************************************************
* Create workfile from clean tidy data
* same as tidy person-level data but variables ordered 
* so balance table is easier to make

use "$data_in/wfh_tidy_person", clear
order personid treatment ordertaker type quitjob phonecalls0 phonecalls1 ///
  perform10 perform11 age male second_techn high_school tertiary_tec university ///
  prior_experi tenure married children ageyoungestc rental costofcommut ///
  bedroom internet basewage bonus grosswage 


save "$work/ch20-wfh-workfile", replace


*******************************************************
* Analysis

* Balance

use "$work/ch20-wfh-workfile", replace

*des perform10 age-grosswage

replace ageyoungest = . if children==0


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
	reg treatment `z', robust nohead
}



* outcomes: 
* quit firm during 8 months of experiment
* # phone calls worked, for order takers

des quit phonecalls1


tabstat quit , by(treatment) s(mean sd n)
tabstat phonecalls1 if ordertaker==1, by(treatment) s(mean sd n)

* Bar chart for quit rates
gen quit_pct = quitjob*100
gen stayed_pct = (1-quitjob)*100
lab def treatment 0 "Working from office" 1 "Working from home"
lab val treatment treatment
graph bar (mean) stayed_pct quit_pct, over(treatment) stack ///
 bar(1, col(navy*0.8)) bar(2, col(green*0.8)) ///
 ytitle("Percent of employees") ylabel(0(25)100) ///
 legend(label(1 "stayed") label(2 "quit"))
graph export "$output/ch20-figure-1-wfh-quitrates-Stata.png", replace


 
* Regression 1: ATE estimates, no covariates
lab var treatment "Treatment group"
lab var quitjob "Quit job "
lab var phonecalls1 "Phone calls (thousand)"
la var married "Married"
lab var children "Children"
lab var internet "Internet at home"


reg quitjob treatment , robust
 outreg2 using "$output/ch20-table-3-wfh-reg1-Stata", bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

reg phonecalls1 treatment if ordertaker==1, robust
 outreg2 using "$output/ch20-table-3-wfh-reg1-Stata", bdec(2) sdec(2) 2aster tex(frag) nonotes label append


 * Regression 2: ATE estimates, with covariates of some unbalance

reg quitjob treatment married children internet, robust
 outreg2 using "$output/ch20-table-4-wfh-reg2-Stata", bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

reg phonecalls1 treatment married children if ordertaker==1, robust
 outreg2 using "$output/ch20-table-4-wfh-reg2-Stata", bdec(2) sdec(2) 2aster tex(frag) nonotes label append
 
 