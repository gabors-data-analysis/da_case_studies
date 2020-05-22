***************************************************************
* Stock market price and index
* sp-500
*
* Loading and cleaning data
***************************************************************



* WHAT THIS CODES DOES:

* Loads the csv file 
* Clean the dataset
* Generate new variables
* Generate samples by resampling to derive confidence intervals
* Generate samples by bootstraping to derive confidence intervals

***

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

*location folders
global data_in   	"da_data_repo/sp500/clean"
global data_out  	"da_case_studies/ch05-stock-market-loss-generalize"
global output 		"da_case_studies/ch05-stock-market-loss-generalize/output"



global temp      "$data_out"

* set level of loss to inquire
global loss 5

***************************************************************
* load data 
use "$data_in/SP500_2006_16_data.dta", clear

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

* order variables
order date datestr year month gap

save "$data_out/work.dta", replace


************************************************
* first look
************************************************


use "$data_out/work.dta", clear

tsset date
tsline value
graph export "$output/sp500_daily.png", as(png) replace

sort date
gen pct_return=(value-value[_n-1])/value[_n-1]*100
tsline pct_return
graph export "$output/sp500_return_daily.png", as(png) replace


collapse (mean) value, by (year )
tsset year
tsline value
graph export "$output/sp500_year.png", as(png) replace
sort year
gen pct_return=(value-value[_n-1])/value[_n-1]*100
tsline pct_return
graph export "$output/sp500_return_year.png", as(png) replace


***************************************
* Analysis
* 
***************************************

use "$data_out/work.dta", clear

* create percent daily returns
sort date
gen pct_return=(value-value[_n-1])/value[_n-1]*100
lab var pct_return "Percent daily return"

* histogram
histogram pct_return, freq start(-10) width(0.25) bstyle(background) ///
	fcol(blue) xlab(-10 -5 -2 0 2 5 10) ///
	xline(-$loss, style(extended) lstyle(foreground) lw(thick))
graph export "$output/returns_histogram.png",replace

keep date pct_return
save "$data_out/sp500_pctreturns.dta", replace

***************************************
* REPEATED SAMPLES
* samples of 150, 300 and 600 observations (1/2, 1 & 2 years approximately)

clear
set matsize 10000
set seed 573164
global M=10000

foreach n in 450 900 {

	mat def Results = J($M,10,0)

qui forvalues i=1/$M {
	use "$data_out/sp500_pctreturns.dta",replace
	sample `n', count
	forvalue j=1/10 {
		gen loss`j'p=100*(pct_return<-`j')
		sum loss`j'p
		mat Results[`i',`j']=r(mean)
	}
}
clear
svmat Results, names(loss)
tabstat loss*, s(mean median min max sd n)
save "$data_out/repsamples`n'",replace
}


foreach n in 450 900 {
	use "$data_out/repsamples`n'",replace
	tabstat loss$loss, s(mean median min max sd n)
	lab var loss$loss "Percent of days with losses of $loss% or more"
	histogram loss$loss, freq width(0.1) fcol(mint) xlab(0(0.2)2)
	more
	graph export "$data_out/output/resample`n'.png",replace
}

** DENSITY PLOTS WITH DIFFERENT SAMPLE SIZES

clear

use "$data_out/repsamples450",replace
 keep loss$loss
 rename loss$loss loss_n450
merge 1:1 _n using "$data_out/repsamples900", nogen
 keep loss$loss* loss_n*
 rename loss$loss loss_n900

sum loss* 
 
foreach n in 450 900 {
	kdensity loss_n`n' if loss_n`n'>=0, nogra gen(x`n' y`n') bw(0.5)
	lab var y`n' "n=`n'"
}

* NEW
line y450 x450 , lw(thick) lc(green) lp(shortdash) ///
 || line y900 x900, lw(vthick) lc( blue) ///
 ytitle("Frequency") xtitle("percent of days with losses above 5%") ///
  xline(0.5, lc(dkgrey) ) xline(0, lw(thin) lc(grey) lp(dash) ) ///
  xlab(0(0.5)2.5, grid)  ylab(0, grid) legend(rows(1)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
  graph export "$output/resample_densities.png",replace




***************************************
* BOOTSRTAP SAMPLES
***************************************

* WARNING!!!!!! 
* IF using DROPBOX/BOX - need to switch off sync!!!!


clear
set seed 573164
global M=10000 /* # bootstrap draws*/

set obs 1
gen loss_$loss=.
save "$temp/sp500_bootstrap.dta",replace

qui forvalues i=1/$M {
	use "$temp/sp500_pctreturns.dta",replace
	bsample 
	gen loss_$loss=100*(pct_return<-$loss)
	collapse loss
	append using "$temp/sp500_bootstrap.dta"
	save "$temp/sp500_bootstrap.dta",replace
}

lab var loss "Percent of days with losses of $loss% or more"
tabstat loss, s(mean sd median min max n)
histogram loss, freq width(0.04) fcol(mint) xlab(0(0.1)1.2)
graph export "$data_out/output/bootstrap.png",replace


***************************************
* SE FORMULA

use "$temp/sp500_pctreturns.dta", replace

gen loss_$loss=100*(pct_return<-$loss)
sum loss

local sd=r(sd)
local n=r(N)

dis `sd'
dis `n'

local SE=1/sqrt(`n') * `sd'
dis `SE'


