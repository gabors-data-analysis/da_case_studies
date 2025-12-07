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
* CH06B Testing the likelihood of loss on a stock portfolio
* using the sp500 dataset
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
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
global data_in  "${data_dir}/sp500/clean"
global work     "ch06-stock-market-loss-test"
global output   "${work}/output"
global temp     "${work}/temp"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"
capture mkdir "${temp}"


********************************************************************
* PARAMETER SETUP
********************************************************************

* Set level of loss to inquire (in percent)
global loss 5

display as text "Testing for losses greater than " as result ${loss} "%"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/SP500_2006_16_data.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile sp500_data
copy "https://osf.io/download/wm6ge/" `sp500_data'
use `sp500_data', clear
*/


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* Create gap variable (days between observations)
generate gap = date - date[_n-1] - 1

* Label variables
label variable value "Value of the S&P500"
label variable datestring "Date, in string format (YMD)"
label variable date "Date"
label variable gap "Gap between observations, in days"

* Create year and month variables (for later use)
generate year = year(date)
generate month = month(date)
label variable year "Year"
label variable month "Month"

* Check for gaps in data
summarize gap, d
count if gap > 3
display as text "Trading days with gaps > 3 days: " as result r(N)


********************************************************************
* CALCULATE DAILY RETURNS
********************************************************************

* Create percent daily returns
sort date
generate pct_return = (value - value[_n-1])/value[_n-1]*100
label variable pct_return "Percent daily return"

* Summary statistics for returns
summarize pct_return, d
display as text _newline "Daily Return Statistics:"
display as text "Mean: " as result %6.3f r(mean) "%"
display as text "Std Dev: " as result %6.3f r(sd) "%"
display as text "Min: " as result %6.3f r(min) "%"
display as text "Max: " as result %6.3f r(max) "%"


********************************************************************
* CREATE LOSS INDICATOR
********************************************************************

* Create binary indicator for loss exceeding threshold
generate loss_${loss} = 100*(pct_return < -${loss})
label variable loss_${loss} "Loss indicator (>5% daily loss)"

* Summary of losses
summarize loss_${loss}
count if loss_${loss} == 100
display as text "Days with loss > ${loss}%: " as result r(N)


********************************************************************
* HYPOTHESIS TESTING
********************************************************************

* Test whether likelihood of loss exceeds 1%
* H0: P(loss > 5%) = 1%
* Ha: P(loss > 5%) > 1%

ttest loss_${loss} = 1

display as text _newline "Hypothesis Test Results:"
display as text "Null hypothesis: P(loss > ${loss}%) = 1%"
display as text "Mean observed: " as result %6.3f r(mu_1) "%"
display as text "t-statistic: " as result %6.3f r(t)
display as text "Two-sided p-value: " as result %6.4f r(p)
display as text "Lower-tailed p-value: " as result %6.4f r(p_l)
