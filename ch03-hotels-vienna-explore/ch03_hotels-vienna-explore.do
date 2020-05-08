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
cd "...." /*set your dir*/
 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

*location folders
global data_in   	"da_data_repo/hotels-vienna/clean"
global data_out  	"da_case_studies/ch03-hotels-vienna-explore"
global output 		"da_case_studies/ch03-hotels-vienna-explore/output"


* load in clean and tidy data and create workfile
use "$data_in/hotels-vienna.dta", clear



**********************************************
* DISTRIBUTIONS
***********************************************

* apply filters
* KEEP 3-4 stars less than 8km from center
keep if city== "Vienna" 
keep if accommodation_type== "Hotel"


**********************************************
* Figure 3.1
**********************************************


colorpalette viridis, n(4) select(2) nograph
return list
histogram stars, ///
	discrete percent horizontal   ///
	xlabel(, grid format(%9.0f)) ylabel(1 1.5 2 2.5 3 3.5 4 4.5 5, grid)  ///
	fcolor(`r(p)') gap(5) ///
	lcolor(white) lwidth(vthin) addlabel addlabopts(mlabsize(medium)) legend(colgap(small)) ///
	graphregion(fcolor(white) ifcolor(none))   plotregion(fcolor(white) ifcolor(white))
 graph export "$output/histstars_Vienna.png", replace

 
 
colorpalette viridis, n(4) select(2) nograph
return list
hist stars , ///
	discrete start(0) frequency addlabel  horizontal ///
	xtitle(stars) ytitle(count of) ///
	fcolor(`r(p)') gap(5) ///
	xlabel( ,format(%9.0g)) ///
	ylabel(1(0.5)5 1 "*" 1.5 "*+" 2 "**" 2.5 "**+" 3 "***" 3.5 "***+" 4 "****"  4.5 "****+" 5 "*****", valuelabel  angle(horizontal)) xmtick(2(0.5)5) ///
	graphregion(fcolor(white) ifcolor(none))  plotregion(fcolor(white) ifcolor(white))
graph export "$output/histstars_Vienna2.png", replace

 

 use "$data_in/hotels-vienna.dta", clear
* apply filters
* KEEP 3-4 stars less than 8km from center, below 1000 euro
keep if accommodation_type== "Hotel"
keep if stars>=3 & stars<=4
keep if price<1000

* brief look at data
tab city
tab stars

 * Figure 3.2 a) and b)

hist price if city=="Vienna", disc freq ylabel(0 1 2 3 4 5 6 7 8 9 10, grid) color(blue) fintensity(80)
 graph export "$output/histprice_Vienna1.png", replace


hist price if city=="Vienna", width(20) freq ylabel(, grid) lcolor(black) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histprice_Vienna2.png", replace
 
 * Figure 3.3 a) and b)

hist price if city=="Vienna", width(40) freq ylabel(, grid) lcolor(black) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histprice_Vienna3.png", replace


hist price if city=="Vienna", width(80) freq ylabel(, grid) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histprice_Vienna4.png", replace

*  Figure 3.4
hist distance if city=="Vienna",  freq ylabel(, grid) lwidth(vthin) fcolor(blue) fintensity(80)
 graph export "$output/histdist_Vienna.png", replace
 
 * could add annotations

tab city_actual
