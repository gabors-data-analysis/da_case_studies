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
* CH06A Comparing online and offline prices: testing the differences
* using the billion-prices dataset
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


global data_in  "$data_dir/billion-prices/clean"
global work  	"ch06-online-offline-price-test"

cap mkdir 		"$work/output"
global output 	"$work/output"





clear


use "$data_in/online_offline_ALL_clean.dta",replace

* filter dataset
tab country
keep if country == "USA"
keep if PRICETYPE == "Regular Price"
drop if sale_online == 1
drop if price==.
drop if price_online==.

sum price*,d
drop if price>1000 /* 3 observations with obvious error */
sum price*

* PRICE

gen pd = price_online - price
 lab var pd "Online - offline price difference (USD)"

tabstat pd, s(mean sd min median max n)
count
count if pd==0
count if pd>0
count if pd<0
count if pd>=-1 & pd<=1

* Figure 6.1 (a)
histogram pd , freq start(-400) width(5) ///
	col(navy*0.8) xlab(-400(100)400, grid) ylab(, grid)
graph export "$output/ch06-figure-1a-pricediff-Stata.png",replace

* Figure 6.1 (b)
histogram pd if pd>-5 & pd<5, freq start(-5) width(0.5) ///
	col(navy*0.8) lcol(white) xlab(-5(1)5, grid) ylab(0(1000)5000, grid)
graph export "$output/ch06-figure-1b-pricediff-Stata.png",replace


** HYPOTHESIS
ttest pd = 0 	

** MULTIPLE HYPOTHESES
sort retailer
by retailer: ttest pd = 0 




