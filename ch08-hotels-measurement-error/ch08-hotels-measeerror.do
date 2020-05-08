*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH08 Case Study B: Hotels
*
*
*********************************************************************

* WHAT THIS CODES DOES:


*location folders
global data_in   	"da_data_repo/hotels-vienna/clean"
global data_out  	"da_case_studies/ch08-hotels-measurement-error"
global output 		"da_case_studies/ch08-hotels-measurement-error/output"


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


gen lnprice=ln(price)
	lab var lnprice "ln(Price)"

sum rating_count ,d

* define cutoffs
local k1 100
local k2 200

sum rating_count  rating if rating_count <`k1'
sum rating_count  rating if rating_count >=`k1' & rating_count <`k2' 
sum rating_count  rating if rating_count >=`k2'
 
* FIGURE
reg lnprice rating if rating_count <`k1'
 predict yhat1
 lab var yhat1 "more noisy x: # ratings <`k1'"
reg lnprice rating if rating_count >=`k1' & rating_count <`k2' 
 cap predict yhat2
 cap lab var yhat2 "`k1' <= # ratings <`k2' "
reg lnprice rating if rating_count >=`k2' 
 predict yhat3
 lab var yhat3 "less noisy x: # ratings >`k2'"

line yhat1 yhat3 rating , lw(vthick thick) lc(ltblue navy) lpattern(dot solid)  ///
	xtitle("Average rating") ytitle("ln price") ///
	ylab(3.6(0.4)5.2, grid) xlab(2.5(0.5)5, grid) ///
	graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch08_hotels_measerror.png",replace

	

line yhat1 yhat3 rating , lw(vthick thick) lc(ltblue navy) lpattern(dot solid) ///
	xtitle("Average rating") ytitle("ln price") ///
	ylab(3.6(0.4)5.2, grid) xlab(2.5(0.5)5, grid) ///
 || scatter lnprice rating if rating_count <`k1', ms(O) msize(small) mcolor(%50)  mc(ltblue) ///
 || scatter lnprice rating if rating_count >=`k2', ms(O) msize(small) mcolor(%50)  mc(navy) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
  legend(order (1 2))
 graph export "$output/ch08_hotels_measerror_scatter.png",replace

	
	


