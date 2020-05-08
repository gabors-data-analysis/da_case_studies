*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH03
* Describe hotels-vienna
* 
********************************************************************

* WHAT THIS CODES DOES:
* Focus on histograms


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"
 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

 *location folders
global data_in   	"da_data_repo/hotels-europe/clean"
global data_out  	"da_case_studies/ch03-hotels-europe-compare"
global output 		"da_case_studies/ch03-hotels-europe-compare/output"
 
* Vienna vs London

* load in clean and tidy data and create workfile
use "$data_in/hotels-europe_price", clear
merge m:m hotel_id using "$data_in/hotels-europe_features.dta"
drop _m

* apply filters
* KEEP NOV 2017 weekend, 3-4 stars less than 8km from center
keep if year==2017 & month==11 & weekend==0
keep if city== "Vienna" | city=="London"
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4


*filter
keep if city_actual== "Vienna" | city_actual=="London"
keep if price<600

save "$data_out/hotels-vienna-london.dta", replace


  * Figure 3.6 a) and b)

hist price if city=="Vienna", width(20) percent ///
 ylabel(0(10)30, grid) xlabel(0(100)800) lcolor(black) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histprice_Vienna5.png", replace
hist price if city=="London", width(20) percent ///
 ylabel(0(10)30, grid) xlabel(0(100)800) lcolor(black) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histprice_London.png", replace
 
 
* kernel density plots
kdensity price if city=="Vienna",  gen (xV yV) lcolor(blue) lwidth(thin) lcolor(blue) fintensity(80)
more
kdensity price if city=="London",  gen (xL yL) lcolor(blue) lwidth(thin) lcolor(blue) fintensity(80)
more

* Figure 3.6

line yV xV, lw(thick) lc(black)  || line yL xL, lw(thick) lc(blue) lp(dash) ////
 legend(lab(1 Vienna) lab(2 London)) 
 graph export "$output/kdens_ViennaLondon.png", replace

********************************************************************
*** SUMMARY STATISTICS
***********************************************
 
 * Table 3.1
tabstat price, s(mean median min max sd n) by(city) format(%3.0f)
// --> tabout
estpost tabstat price, s(mean median min max sd n) by(city) 
esttab using "$output/ch03_summary-vienna-london.tex", replace compress nolabel noobs nogaps wide nomtitles ///
nonumber collabels(mean median min max sd n ,lhs("City")) cells(" mean(fmt(0)) p50(fmt(0)) min(fmt(0)) max(fmt(0)) sd(fmt(0)) count(fmt(0)) ")
