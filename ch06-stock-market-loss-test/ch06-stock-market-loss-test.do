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
* Chapter 06
* CH06B Testing the likelihood of loss on a stock portfolio?
* using the sp500 dataset
* version 0.9 2021-10-20
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


global data_in  "$data_dir/sp500/clean"
global work  	"ch06-stock-market-loss-test"

cap mkdir 		"$work/output"
global output 	"$work/output"

cap mkdir 		"$work/temp"
global temp 	"$work/temp"



* set level of loss to inquire (in percent)
global loss 5

***************************************************************
* load data 
use "$data_in/SP500_2006_16_data.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/wm6ge/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* create gap variable 
gen gap=date-date[_n-1]-1

* label variables
lab var value "Value of the S&P500"
lab var datestr "Date, in string format (YMD)"
lab var date "Date"
lab var gap "Gap between observations, in days"

* create variable for each year and each month
* for later use
gen year =year(date)
gen month=month(date)


* create percent daily returns
sort date
gen pct_return=(value-value[_n-1])/value[_n-1]*100
lab var pct_return "Percent daily return"

gen loss_$loss=100*(pct_return<-$loss)
sum loss

* t-test
ttest loss_5=1
di `r(p)'
di `r(p_l)'

