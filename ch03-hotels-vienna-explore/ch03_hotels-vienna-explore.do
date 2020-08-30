*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 03A 
* Finding a Good Deal among Hotels: Data Exploration
*
* using the hotels-vienna dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch03-hotels-vienna-explore"
cap mkdir "$work/output"
global output "$work/output"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-vienna/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/hotels-vienna/clean"


* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear


**********************************************
* DISTRIBUTIONS
***********************************************

* sample design
* KEEP hotels in Vienna 
keep if city== "Vienna" 
keep if accommodation_type== "Hotel"


* Stars

* Figure 3.1a
colorpalette viridis, n(4) select(2) nograph
return list
histogram stars, ///
	discrete percent ///
	xtitle(Star rating (number of stars))  ///
	xlabel(1(0.5)5, grid format(%3.1f)) ylabel(0(10)50, grid)  ///
	fcolor(`r(p)') gap(5) lcolor(white) lwidth(vthin) ///
	addlabel addlabopts(mlabsize(medium) yvarformat(%3.1f))  ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-1a-hist-stars-Stata.png", replace
 
* Figure 3.1b
colorpalette viridis, n(4) select(2) nograph
return list
hist stars , ///
	discrete start(0) frequency ///
	xtitle(Star rating (number of stars))  ///
	fcolor(`r(p)') gap(5) lcolor(white) lwidth(vthin) ///
	xlabel(1(0.5)5 , grid) ylabel(0(20)140, grid )  ///
	addlabel addlabopts(mlabsize(medium) yvarformat(%3.0f))  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-1b-hist-stars-Stata.png", replace


* Price

 use "$data_in/hotels-vienna.dta", clear
* sample design
* KEEP hotels in Vienna, stars 3 to 4
* DROP observation with erroneous price variable (price>1000)
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4
keep if price<1000

* brief look at data
tab city
tab stars

* Figure 3.2a
colorpalette viridis, n(4) select(2) nograph
return list
hist price , ///
	discrete start(0) frequency ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') ///
	xlabel(0(50)500 , grid) ylabel(0(2)8, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-2a-hist-price-Stata.png", replace

* Figure 3.2b
colorpalette viridis, n(4) select(2) nograph
return list
hist price , ///
	width(10) start(-5) frequency ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(50)500 , grid) ylabel(0(5)45, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-2b-hist-price-Stata.png", replace


* Figure 3.3a
colorpalette viridis, n(4) select(2) nograph
return list
hist price , ///
	width(40) start(-20) frequency ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(80)500 , grid) ylabel(0(20)120, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-3a-hist-price-Stata.png", replace

* Figure 3.3b
colorpalette viridis, n(4) select(2) nograph
return list
hist price , ///
	width(80) start(40) frequency ///
	xtitle(Price (US dollars))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(80)500 , grid) ylabel(0(50)150, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-3b-hist-price-Stata.png", replace


* Distance to city center
 
* Figure 3.4
* also Figure 3.5 (it's the same with some annotation)
colorpalette viridis, n(4) select(2) nograph
return list
hist distance, ///
	width(0.5)  frequency ///
	xtitle(Distance to city center (miles))  ///
	color(`r(p)') lcol(white) lw(vthin) ///
	xlabel(0(2)14 , grid) ylabel(0(10)60, grid )  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch03-figure-4-hist-dist-Stata.png", replace

count if distance>8
drop if distance>8

tab city_actual
keep if city_actual=="Vienna"
count

