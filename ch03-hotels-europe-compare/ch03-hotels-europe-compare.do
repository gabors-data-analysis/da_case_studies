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
* Chapter 03
* CH03B Comparing hotel prices in Europe: Vienna vs. London
* using the hotels-europe dataset
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

global data_in  "$data_dir/hotels-europe/clean"
global work  	"ch03-hotels-europe-compare"

cap mkdir 		"$work/output"
global output 	"$work/output"

cap mkdir 		"$work/temp"
global temp 	"$work/temp"



* Vienna vs London

* load in clean and tidy data and create workfile
use "$data_in/hotels-europe_price", clear
merge m:m hotel_id using "$data_in/hotels-europe_features.dta", nogen

* sample design
* KEEP NOV 2017 weekend, 3-4 star hotels
keep if year==2017 & month==11 & weekend==0
keep if city== "Vienna" | city=="London"
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4

* in actual city (takes care of extreme distances, too)
keep if city_actual== "Vienna" | city_actual=="London"
* drop Vienna hotel with erroneous price 
keep if price<1000

save "$temp/hotels-vienna-london.dta", replace
tab city

* distribution of price

* Figure 3.6a Vienna
colorpalette viridis, n(4) select(2) nograph
return list
hist price if city=="Vienna", ///
	width(20) percent ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(50)500 , grid) ylabel(0(10)30, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-6a-hist-price-Vienna-Stata.png", replace

* Figure 3.6b London
colorpalette viridis, n(4) select(2) nograph
return list
hist price if city=="London", ///
	width(20) percent ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(50)500 , grid) ylabel(0(10)30, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-6b-hist-price-London-Stata.png", replace

* two density plots overlayed
* Figure 3.7
kdensity price if city=="Vienna",  gen (xV yV) nograph
kdensity price if city=="London",  gen (xL yL) nograph
colorpalette viridis, n(4) select(2) nograph
return list
line yV yL xL, lc(`r(p)') lw(thick thick) ///
	xtitle(Price (US dollars))  ///
	xlabel(0(100)500 , grid) ylabel(, grid )  ///
	legend(off) ///
	text(0.007 180 "Vienna" 0.0025 340 "London")  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-7-densities-price-ViennaLondon-Stata.png", replace


 
* Table 3.6
tabstat price, s(n mean median min max sd ) by(city) format(%4.2f) save
* calculate mean-median skewness statistic
* London
qui sum price if city=="London",d
dis (r(mean) - r(p50)) / r(sd)
* Vienna
qui sum price if city=="Vienna",d
dis (r(mean) - r(p50)) / r(sd)

* into .tex format
* replace the built-in skewness measure with our measure computed above: (mean-median)/sd
tabout city using "$output/ch03-table-6-summary-ViennaLondon-Stata.tex" ///
 , replace style(tex) sum ///
 c(count price mean price median price min price max price sd price skewness price)
