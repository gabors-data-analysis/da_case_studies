******************************************************************************************
* Prepared for the textbook:
* Data Analysis for Business, Economics, and Policy
* by Gabor BEKES (Central US dollarsopen University) and  Gabor KEZDI (University of Michigan)
* Cambridge University Press 2020

* License: Free to share, modify and use for educational purposes. 
* Not to be used for business purposes
* 
*
******************************************************************************************

*
* CHAPTER 7
* Regression 
* Hotels dataset
*********************************************************************

* v2.0. 2019-10-31
* v2.1. 2020-05-08 adjusted for miles and dollars 

* TODO needs a review based on R code

* WHAT THIS CODES DOES:
* Imports price, stars and distance data to Stata
* Manages data to get a clean dataset to work with
* Describes data
* Performs regression analysis 
* Creates graphs

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/


 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

*location folders
global data_in   	"da_data_repo/hotels-vienna/clean"
global data_out  	"da_case_studies/ch07-hotel-simple-reg"
global output 		"da_case_studies/ch07-hotel-simple-reg/output"


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
saveold "$data_out/hotels_work.dta", replace




* SUMMARY STATISTICS ON PRICE AND DISTANCE
codebook price
tabstat price, s(mean sd min p50 p95 max n)
tabstat distance, s(mean sd min p50 max n)
more



*********************************************************************
*** REGRESSION 1: CLOSE VS FAR
gen dist_2groups=distance>=2 
 lab def dist_2groups 0 close 1 far
 lab val dist_2groups dist_2groups 
 lab var dist_2groups "Distance to city center: close vs. far"
egen Eprice_cat2=mean(price), by(dist_2groups)
 format Eprice_cat2 %3.0f
tabstat distance, by(dist_2groups) s(mean sd min max n)
tabstat price, by(dist_2groups) s(mean sd min p25 p50 p75 max n)

* FIGURE 7.1.a
* PLOT MEAN VALUES BY CLOSE VS FAR
* TO DO
* ADD values 



scatter Eprice_cat2 dist_2groups, ///
 ms(diamond) msize(large) mcolor(blue) mlabel(Eprice_cat2) ///
 xscale(range(-0.5 1.5)) yscale(range(0 400)) ///
 xlab(0 1, valuelabel grid) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (categories)") ///
 ytitle("Average price (US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_1a.png", as(png) replace


 
*********************************************************************
*** REGRESSION 2: 4 DISTANCE CATEGORIES
cap drop dist_4groups Eprice_cat4
egen dist_4groups=cut(distance), at(0 1 2 3 7)
replace dist_4groups=dist_4groups+0.5
replace dist_4groups=5 if dist_4groups==3.5 
tabstat distance, by(dist_4groups) s(min mean max p95 n) 
egen Eprice_cat4=mean(price), by(dist_4groups)
 format Eprice_cat4 %3.0f
 lab var Eprice_cat4 "Average price in 4 distance categ."
tabstat distance, by(dist_4groups) s(mean sd min max n)
tabstat price, by(dist_4groups) s(mean sd min p25 p50 p75 p95 max n)


* FIGURE 7.1.b 
* PLOT MEAN VALUES BY CLOSE VS FAR -- 4 values


scatter Eprice_cat4 dist_4groups, ///
 ms(diamond) msize(large) mcolor(blue) mlabel(Eprice_cat4) ///
  xscale(range(0 7)) xlab(1(2)7, grid) yscale(range(0 400)) ylab(0(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Average price (US dollars))") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_1b.png", as(png) replace /* not used*/
 
 
* Figure 7.2
 * BOX PLOT BY CLOSE VS FAR
 
local ymax=300
graph box price, over(dist_2groups) noouts ///
 medtype(marker) medm(ms(O) msize(large) mcolor(dkgreen)) ///
 ylab(0(100)400)  ///
 ytitle("Average price (US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_2.png", as(png) replace

 

*********************************************************************
*** SCATTERPLOT + REGRESSION (2nd regression w/ 4 distance categories)

* Figure 7.3.a


 scatter price distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) ///
 xlab(0(1)7, grid) yscale(range(0 400)) ylab(50(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
|| scatter Eprice_cat4 dist_4groups , ///
 ms(diamond) msize(large) mcolor(dkgreen)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_3a.png", as(png) replace




*********************************************************************
** COMPARE NONPARAMETRIC REGRESSIONS
*** bins: 7 bins (distance catgories)
egen dist_7groups=cut(distance), at(0(1)7)
replace dist_7groups=dist_7groups+0.5
tabstat distance, by(dist_7groups) s(min mean max n) 
egen Eprice_cat7=mean(price), by(dist_7groups)

* Figure 7.3.b

 scatter price distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) ///
 xlab(0(1)7, grid) yscale(range(0 400)) ylab(50(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
|| scatter Eprice_cat7 dist_7groups , ///
 ms(diamond) msize(large) mcolor(dkgreen)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_3b.png", as(png) replace
 
*lowess price distance, bwidth(0.8) ///
 *lineopts(lw(vthick) lc(dkgreen)) ///

 * Figure 7.4
*** lowess with scatterplot

lowess price distance , bwidth(0.8) ///
 lineopts(lw(thick) lc(dkgreen)) ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)7, grid) yscale(range(0 300)) ylab(50(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_5.png", as(png) replace
 
 
********************************************************************
*** LINEAR REGRESSIONS
regress price distance 

* Fig 7.6
*** SCATTERPLOT + REGRESSION LINE

scatter price distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)7, grid) ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price(US dollars)") ///
 || lfit price distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_6.png", as(png) replace
 * to add a residual
 
 *Figure 7.7
* same graph with resudal
 
 
 
* Figure 7.8
regress price distance 
 predict pred_price_ols
predict e ,resid 
 format price pred_price_ols e %3.0f
hist e, bin(20) percent ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_8.png", as(png) replace
 
* list best deal hotels
sort e
list hotel_id distance price pred_price_ols e if _n<=5
* task: create a nice table as tex output


sum price , det
local p_avg =`r(mean)'
sum distance , det
local d_avg =`r(mean)'
 
* Fig 7.10.a
*** THE LINEAR REGRESSION GOES THROUGH THE AVERAGES
*** SCATTERPLOT + REGRESSION LINE + LINES FOR AVERAGES

scatter price distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)8) ylab(100(100)400) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Price (US dollars)") ///
 || lfit price distance , lw(thick) lc(dkgreen) legend(off) ///
	xline(`d_avg') yline(`p_avg') ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F07_notused7.png", as(png) replace

* Fig 7.10.b
* TO DO MISSING




* get R2 of lowess
lowess price distance, gen (ylowess)
gen s2=(ylowess-price)^2
su s2


 
