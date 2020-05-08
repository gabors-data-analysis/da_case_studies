*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH08 Case Study B: Hotels
*
*
*********************************************************************

* WHAT THIS CODES DOES:
* log-level combinations


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"



*location folders
global data_in   	"da_data_repo/hotels-vienna/clean"
global data_out  	"da_case_studies/ch08-hotels-non-linear"
global output 		"da_case_studies/ch08-hotels-non-linear/output"


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
x	
* create graphs	
scatter price distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)7, grid) ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price(EUR)") ///
 || lfit price distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F08_1a.png", as(png) replace


scatter lnprice distance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)7, grid) ylab(3.5(0.50)6, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price (EUR) in log") ///
 || lfit lnprice distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F08_1b.png", as(png) replace

scatter price lndistance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(-2.5(0.5)2, grid) ylab(000(50)400, grid) ///
 xtitle("Distance to city center (miles) in log") ///
 ytitle("Hotel price (EUR) ") ///
 || lfit price lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F08_1c.png", as(png) replace


scatter lnprice lndistance , ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(-2.5(0.5)2, grid) ylab(3.5(0.50)6, grid) ///
 xtitle("Distance to city center (miles) in log") ///
 ytitle("Hotel price (EUR) in log") ///
 || lfit lnprice lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F08_1d.png", as(png) replace
