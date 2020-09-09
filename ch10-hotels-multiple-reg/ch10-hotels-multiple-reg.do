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
* Chapter 10
* CH10A Finding a good deal among hotels with multiple regression
* using the hotels-vienna dataset
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


global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch10-hotels-multiple-reg"

cap mkdir 		"$work/output"
global output 	"$work/output"




* add user written library
ssc install listtex

*********************************************************************
*** LOAD and PREP DATA



* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear



*** SAMPLE SELECTION

*** 3 to 4-star hotels (incl 3.5 stars)
keep if stars>=3 & stars<=4
keep if accommodation_type=="Hotel"
label var distance "Distance to city center, miles"
drop if price>600 	/* likely error */


*** drop hotels not really in Vienna
tab city_actual 
keep if city_actual=="Vienna"

*** take log price
gen lnprice=ln(price)
 lab var lnprice "ln(Price)"

*** piecewise lins pline of distance
mkspline distsp1 1 distsp2 4 distsp3 = distance

*** piecewise lins pline rating
mkspline ratingsp1 3.5 ratingsp2  = rating

*** stars: binary indicators
gen star35 = stars==3.5
gen star4 = stars==4

tabstat price distance lnprice, s(mean sd min p25 p50 p75 max n) col(s)
 

*********************************************************************
*regressions
*********************************************************************

reg lnprice distance, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(3) replace 
reg lnprice rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(3) append
reg lnprice distance rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(2) append
reg distance rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(2) append




* basic
reg lnprice distance rating, robust  


* predicted values 
reg lnprice distsp1 distsp2 distsp3 star35 star4 ratingsp1 ratingsp2, robust  
predict lnprice_hat
predict lnprice_resid, resid

* compare R-sqared with distance only
reg lnprice distsp1 distsp2 distsp3,r 

* list of 5 best deals
sort lnprice_resid

format lnprice_resid %5.3f
format distance %3.1f
list hotel_id price lnprice_resid distance stars rating if _n<=5
* outputing the list in a LaTex format
listtex hotel_id price lnprice_resid distance stars rating ///
 using "$output\ch10-table-6-hotels-good-deals-Stata.tex" if _n<=5, replace ///  
 headlines( "\begin{tabular}{l c c c c c})" ///
 \hline "Hotel name & price & residual in ln(price) & distance & stars & rating \\" \hline) ///
 footlines(\hline \end{tabular}) rstyle(tabular) 
 /* the tabular style makes the table LaTex friendly */
 /* headlines defines the way the table layed out */

* yhat - y graph
* two scatterplot commants, one for best 5 deals, one for rest
* Figure 10.3
sort lnprice_resid
scatter lnprice lnprice_hat if _n>5, ms(O) mc(navy*0.6) ///
 || scatter lnprice lnprice_hat if _n<=5, ms(O) mc(black) ///
 || line lnprice lnprice, sort lw(thick) lc(green*0.6) lp(dash)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xlab(3.75(0.25)6, grid) ylab(3.75(0.25)6, grid) legend(off) ///
 ytitle("ln(price, US dollars)") xtitle("predicted ln(price, US dollars)") ///
 text(4.1 4.84 "Best deal") 
graph export "$output\ch10-figure-3-hitels-yhat-y.png", replace
 
