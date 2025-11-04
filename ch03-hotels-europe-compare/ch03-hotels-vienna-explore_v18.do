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
* CH03A Finding a good deal among hotels: data exploration
* using the hotels-vienna dataset
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Applied viridis colors properly
*   - Updated graph export syntax
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
global work  	"ch03-hotels-vienna-explore"

cap mkdir 		"$work/output"
global output 	"$work/output"



* Load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/dn8je/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* DISTRIBUTIONS

* Sample design
* KEEP hotels in Vienna 
keep if city== "Vienna" 
keep if accommodation_type== "Hotel"


* Stars

* Figure 3.1a
colorpalette viridis, n(1) nograph
local color1 `r(p)'

histogram stars, ///
	discrete percent ///
	xtitle(Star rating (number of stars)) ///
	xlabel(1(0.5)5, grid format(%3.1f)) ///
	ylabel(0(10)50, grid) ///
	fcolor("`color1'") gap(5) lcolor(white) lwidth(vthin) ///
	addlabel addlabopts(mlabsize(medium) yvarformat(%3.1f)) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-1a-hist-stars-Stata.png", replace as(png)
 
* Figure 3.1b
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist stars, ///
	discrete frequency ///
	xtitle(Star rating (number of stars)) ///
	fcolor("`color1'") gap(5) lcolor(white) lwidth(vthin) ///
	xlabel(1(0.5)5, grid) ///
	ylabel(0(20)140, grid) ///
	addlabel addlabopts(mlabsize(medium) yvarformat(%3.0f)) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-1b-hist-stars-Stata.png", replace as(png)


* Price

use "$data_in/hotels-vienna.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/dn8je/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* Sample design
* KEEP hotels in Vienna, stars 3 to 4
* DROP observation with erroneous price variable (price>1000)
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4
keep if price<1000

* Brief look at data
tab city
tab stars

* Figure 3.2a
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist price, ///
	discrete frequency ///
	xtitle(Price (US dollars)) ///
	color("`color1'") ///
	xlabel(0(50)500, grid) ///
	ylabel(0(2)8, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-2a-hist-price-Stata.png", replace as(png)

* Figure 3.2b
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist price, ///
	width(10) frequency ///
	xtitle(Price (US dollars)) ///
	color("`color1'") lcol(white) lw(vthin) ///
	xlabel(0(50)500, grid) ///
	ylabel(0(5)40, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-2b-hist-price-Stata.png", replace as(png)


* Figure 3.3a
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist price, ///
	width(40) start(40) frequency ///
	xtitle(Price (US dollars)) ///
	color("`color1'") lcol(white) lw(vthin) ///
	xlabel(0(80)500, grid) ///
	ylabel(0(20)120, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-3a-hist-price-Stata.png", replace as(png)

* Figure 3.3b
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist price, ///
	width(80) start(0) frequency ///
	xtitle(Price (US dollars)) ///
	color("`color1'") lcol(white) lw(vthin) ///
	xlabel(0(80)500, grid) ///
	ylabel(0(50)150, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-3b-hist-price-Stata.png", replace as(png)


* Distance to city center
 
* Figure 3.4
* Also Figure 3.5 (it's the same with some annotation)
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist distance, ///
	width(0.5) frequency ///
	xtitle(Distance to city center (miles)) ///
	color("`color1'") lcol(white) lw(vthin) ///
	xlabel(0(2)14, grid) ///
	ylabel(0(10)60, grid) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-4-hist-dist-Stata.png", replace as(png)

count if distance>8
drop if distance>8

tab city_actual
keep if city_actual=="Vienna"
count

