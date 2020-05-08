*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* MULTIVARIATE REGRESSION MODEL
* ILLUSTRATION STUDY FOR CHAPTER 10
*
* DATA HOTELS 
*********************************************************************

* WHAT THIS CODES DOES:

* Loads the data csv
* Filter the dataset and save a sample used in the analysis
* Transforms variables
* Check distribution of variables
* Run regression with multiple explanatory variables
* Run regression with interactions

*****
********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
* cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
* cd "C:\Users\Viki\Dropbox\bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"
 

global data_in   "da_data_repo/hotels-vienna/clean" /*data_in*/
global data_out	 "da_case_studies/ch10-hotels-multiple-reg" /*data_out*/
global output    "da_case_studies/ch10-hotels-multiple-reg/output" /*output*/


* add user written library
ssc install listtex

*********************************************************************
*** LOAD and PREP DATA



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

*** take log price
gen lnprice=ln(price)
 lab var lnprice "ln(Price)"

*** piecewise lins pline of distance
mkspline distsp1 1 distsp2 4 distsp3 = distance

*** piecewise lins pline rating
mkspline ratingsp1 3.5 ratingsp2  = rating

*** stars: binary indicators
gen star35 = stars==3.5
gen star4 = stars==4

tabstat price distance lnprice, s(mean sd min p25 p50 p75 max n) col(s)
 

*********************************************************************
*regressions
*********************************************************************

reg lnprice distance, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(3) replace 
reg lnprice rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(3) append
reg lnprice distance rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(2) append
reg distance rating, robust  
 outreg2 using "$output/T10_hotels.tex",  tex(frag) excel bdec(2) append




* basic
reg lnprice distance rating, robust  


* predicted values 
reg lnprice distsp1 distsp2 distsp3 star35 star4 ratingsp1 ratingsp2, robust  
predict lnprice_hat
predict lnprice_resid, resid

* compare R-sqared with distance only
reg lnprice distsp1 distsp2 distsp3,r 

* list of 5 best deals
sort lnprice_resid

format lnprice_resid %5.3f
format distance %3.1f
list hotel_id price lnprice_resid distance stars rating if _n<=5
* outputing the list in a LaTex format
listtex hotel_id price lnprice_resid distance stars rating using "$output\T10_hotel_descr.tex" if _n<=5, replace /// 
headlines( "\begin{tabular}{l c c c c c}" \hline "Hotel name & price & residual in ln(price) & distance & stars & rating \\" \hline) /// /* headlines defines the way the table layed out/
footlines(\hline \end{tabular}) rstyle(tabular) /* the tabular style makes the table LaTex friendly */


* y - yhat graph
scatter lnprice lnprice_hat, ms(o) mc(navy) mc(%50) ///
 || lfit lnprice lnprice_hat, lw(thick) lc(dkgreen) ///
 || line lnprice lnprice, sort ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xlab(3.75(0.25)6, grid) ylab(3.75(0.25)6, grid) legend(off) ///
 ytitle("Ln price") xtitle("Ln price predicted value")
 graph export "$output\y_yhat_hotels.png", replace
 
 * not in textbook
 * residual - yhat graph
scatter lnprice_resid lnprice_hat, ms(D) mc(blue) ///
 || lfit lnprice_resid lnprice_hat, lw(thick) lc(black) graphregion(fcolor(white) ifcolor(none)) ///
 xlab(, grid) ylab(, grid) legend(off) ytitle("Ln predicted price") xtitle("Ln price predicted value")
 graph export "$output\res_yhat_hotels_notused.png", replace

 

 

