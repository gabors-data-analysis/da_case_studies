*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 06A
* Comparing online and offline prices: testing the difference
*
* using the billion-prices dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch05-stock-market-loss-generalize"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/billion-prices/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/billion-prices/clean"



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




