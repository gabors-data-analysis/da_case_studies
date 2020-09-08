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
* Chapter 07
* CH07A Finding a good deal among hotels with simple regression
* using the hotels-vienna dataset
* version 0.9 2020-09-06
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


global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch07-hotels-simple-reg"

cap mkdir 		"$work/output"
global output 	"$work/output"




*********************************************************************
* DATA WRANGLING

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

* save work file
save "$work/hotels_work.dta", replace




*********************************************************************
* SUMMARY STATISTICS ON PRICE AND DISTANCE
codebook price
tabstat price, s(mean sd min p50 p95 max n)
tabstat distance, s(mean sd min p50 max n)
more



*********************************************************************
*** NONPARAMETRIC REGRESSION 1: CLOSE VS FAR
gen dist_2groups=distance>=2 
 lab def dist_2groups 0 close 1 far
 lab val dist_2groups dist_2groups 
 lab var dist_2groups "Distance to city center: close vs. far"
egen Eprice_cat2=mean(price), by(dist_2groups)
 format Eprice_cat2 %3.0f
tabstat distance, by(dist_2groups) s(mean sd min max n)
tabstat price, by(dist_2groups) s(mean sd min p25 p50 p75 max n)

* FIGURE 7.1.a
scatter Eprice_cat2 dist_2groups, ///
 ms(O) msize(vlarge) mcolor(navy*0.8) mlabel(Eprice_cat2) ///
 xscale(range(-0.5 1.5)) yscale(range(0 400)) ///
 xlab(0 1, valuelabel grid) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (categories)") ///
 ytitle("Average price (US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch07-figure-1a-scatter-nonpar1-Stata.png", as(png) replace


 
*********************************************************************
*** NONPARAMETRIC REGRESSION 2: 4 DISTANCE CATEGORIES
cap drop dist4groups Eprice_cat4
egen dist4groups=cut(distance), at(0 1 2 3 7)
replace dist4groups=dist4groups+0.5
replace dist4groups=5 if dist4groups==3.5 
tabstat distance, by(dist4groups) s(min mean max p95 n) 
egen Eprice_cat4=mean(price), by(dist4groups)
 format Eprice_cat4 %3.0f
 lab var Eprice_cat4 "Average price in 4 distance categ."
tabstat distance, by(dist4groups) s(mean sd min max n)
tabstat price, by(dist4groups) s(mean sd min p25 p50 p75 p95 max n)


* FIGURE 7.1.b 
* PLOT MEAN VALUES BY CLOSE VS FAR -- 4 values
scatter Eprice_cat4 dist4groups, ///
 ms(O) msize(vlarge) mcolor(navy*0.8) mlabel(Eprice_cat4) ///
  xscale(range(0 7)) xlab(1(2)7, grid) yscale(range(0 400)) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Average price (US dollars))") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch07-figure-1b-scatter-nonpar2-Stata.png", as(png) replace
 
 
*********************************************************************
*** SCATTERPLOT + NONPARAMETRIC REGRESSION AS STEP FUNCTION

*** (4 distance categories)
egen price4steps = mean(price), by(dist4groups)
 gen p4s_1 = price4steps if dist4groups==0.5
 gen p4s_2 = price4steps if dist4groups==1.5
 gen p4s_3 = price4steps if dist4groups==2.5
 gen p4s_4 = price4steps if dist4groups==5


* Figure 7.2.a
scatter price distance, ///
 ms(O) mlw(small) mcolor(navy*0.7)  ///
 xlab(0(1)7, grid) yscale(range(0 400)) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
|| line p4s_* distance, ///
 lp(solid solid solid solid) lcolor(green green green green)  ///
 lw(thick thick thick thick) legend(off)
graph export "$output/ch07-figure-2a-stepfn4cat-Stata.png", as(png) replace



* 7 bins

* create 7 bins of distance
egen dist7groups = cut(distance), at(0(1)7)
 tabstat distance, by(dist7g) s(min max n)
* create variable with average price within distance bins
egen price7steps = mean(price), by(dist7groups)
forvalue i=1/7 {
	gen p7s_`i' = price7steps if dist7groups==`i'-1
}

* scatterplot with step function
scatter price distance, ///
 ms(O) mlw(small) mcolor(navy*0.7)  ///
 xlab(0(1)7, grid) yscale(range(0 400)) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
|| line p7s_* distance, ///
 lp(solid solid solid solid solid solid solid) lcolor(green green green green green green green)  ///
 lw(thick thick thick thick thick thick thick) legend(off)

* cosmetics: last two bins have one obs only, 
* step function line doesn't show there.
* cosmetic solution:
* create four fake observations, each for the two ends of the last two bins

* first create new distance variable for step function
*  so scattrplot doesn't show them as real observations
gen distfake = distance
* add 4 nes observations
set obs `=_N+4'
count
* set fake distance variable to ends of last bins
replace distfake = 5.1 if _n==_N-3
replace distfake = 5.9 if _n==_N-2
replace distfake = 6.1 if _n==_N-1
replace distfake = 6.9 if _n==_N
* assign average prices to new observations
sum price if dist7groups==5
replace p7s_6 = r(mean) if distfake>5 & distfake<6
sum price if dist7groups==6
replace p7s_7 = r(mean) if distfake>6 & distfake<7

* redo graph so that step fucntion shows in last two bins, too
* Figure 7.2.b
scatter price distance, ///
 ms(O) mlw(small) mcolor(navy*0.7)  ///
 xlab(0(1)7, grid) yscale(range(0 400)) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
|| line p7s_* distfake, ///
 lp(solid solid solid solid solid solid solid) lcolor(green green green green green green green)  ///
 lw(thick thick thick thick thick thick thick) legend(off)
graph export "$output/ch07-figure-2b-stepfn7cat-Stata.png", as(png) replace


*********************************************************************
** lowess

* Figure 7.3
*** lowess with scatterplot

lowess price distance , bwidth(0.8) ///
 lineopts(lw(thick) lc(green)) ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.7)  ///
 xlab(0(1)7, grid) yscale(range(0 300)) ylab(50(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch07-figure-3-lowess-Stata.png", as(png) replace
 
 
********************************************************************
*** LINEAR REGRESSIONS
regress price distance 

* Fig 7.5
*** SCATTERPLOT + REGRESSION LINE

scatter price distance , ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.8) ///
 xlab(0(1)7, grid) ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price(US dollars)") ///
 || lfit price distance, lw(thick) lc(green) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch07-figure-5-linreg-Stata.png", as(png) replace

 
* Figure 7.6a
* same graph as 7.5 with extra annotation
 
 
* Figure 7.6b
* histogram of residuals
regress price distance 
predict pred_price
predict e ,resid 
hist e, bin(20) percent fcol(navy*0.8) lcol(white) ///
 xlab(-100(100)300, grid) ylab(0(5)30, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch07-figure-6b-resid-hist-Stata.png", as(png) replace

 
* list best deal hotels
sort e
list hotel_id distance price pred_price e if _n<=5

* Figure 7.7
* same graph as 7.5 with extra annotation


sum price , det
local p_avg =`r(mean)'
sum distance , det
local d_avg =`r(mean)'
