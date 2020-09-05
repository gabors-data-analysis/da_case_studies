*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 08A
* Finding a good deal among hotels with nonlinear function
*
* using the hotels-vienna dataset
* 
* License: Free to share, modify and use for educational purposes. 
* Not to be used for commercial purposes
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch08-hotels-non-linear"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-vienna/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/hotels-vienna/clean"



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




* Fig 8.1
*** SCATTERPLOT + REGRESSION LINE


gen lnprice=ln(price)
	lab var lnprice "ln(price)"
count
count if distance==0
gen lndistance=ln(distance)
	replace lndistance = ln(distance+0.05) if distance==0
	lab var lndistance "ln(distance to city center)"

	
* run and compare regressions	
reg price distance, r
	outreg2 using "$output/T08_reg1.tex", label bdec(2) tex(frag) nose noaster replace
reg lnprice distance, r
	 outreg2 using "$output/T08_reg1.tex", label bdec(2) tex(frag) nose noaster  append
reg price lndistance, r
	outreg2 using "$output/T08_reg1.tex", label bdec(2) tex(frag)  nose noaster  append
reg lnprice lndistance
	outreg2 using "$output/T08_reg1.tex", label bdec(2) tex(frag) nose noaster   append
	
* create graphs	
scatter price distance , ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.6) ///
 xlab(0(1)7, grid) ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price(US dollars)") ///
 || lfit price distance, lw(thick) lc(green) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-1a-hotel-levlev-Stata", as(png) replace


scatter lnprice distance , ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.6) ///
 xlab(0(1)7, grid) ylab(3.5(0.50)6, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("ln(hotel price in US dollars)") ///
 || lfit lnprice distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-1b-hotel-loglev-Stata", as(png) replace

scatter price lndistance , ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.6) ///
 xlab(-2.5(0.5)2, grid) ylab(000(50)400, grid) ///
 xtitle("ln(distance to city center, miles)") ///
 ytitle("Hotel price (US dollars) ") ///
 || lfit price lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-1c-hotel-levlog-Stata", as(png) replace


scatter lnprice lndistance , ///
 ms(O) msize(small) mlw(thick) mcolor(navy*0.6) ///
 xlab(-3(0.5)2, grid) ylab(3.5(0.50)6, grid) ///
 xtitle("ln(distance to city center, miles)") ///
 ytitle("ln(hotel price in US dollars)") ///
 || lfit lnprice lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-1d-hotel-loglog-Stata", as(png) replace
