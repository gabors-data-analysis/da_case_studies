*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* PREDICTION
* Used car data for LA and Chicago
* prep - cleans the data, makes it ready for work
* v1.0 2018 Dec
********************************************************************
*
*********************************************************************

* WHAT THIS CODES DOES:

* Level vs log




********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cap cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cap cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
cap cd "C:/Users/viktoriakonya/Dropbox/bekes_kezdi_textbook/"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in	 "textbook_work\ch13\used-cars"
global output    "textbook_work\ch14\used-cars\output"
global data_out  "textbook_work\ch14\used-cars"


* same work data as in Chapter 13
use "$data_in/usedcars_work.dta", clear




*********************************************
* compare graphs
* lowess: price, lnprice
*********************************************
lowess price age, lineopts( lc(orange) lw(vthick)) ylab(, grid) xlab(, grid) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/Ch14_p_age_lowess.png" ,replace
 
lowess lnprice age, lineopts( lc(blue) lw(vthick)) ylab(, grid) xlab(, grid) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/Ch14_lnp_age_lowess.png",replace



*********************************************
* MULTIPLE REGRESSION 
* price, quadratic in age & odometer
reg price age agesq odometer odometersq SE cond_likenew cond_excellent cond_good dealer, robust
 local sigmahat = e(rmse)
 outreg2 using "$output/Ch14_multireg.tex", se bdec(0) noaster ///
 adds(sigmahat, `sigmahat') tex(frag) replace
 
reg lnprice age odometer SE cond_likenew cond_excellent cond_good dealer, robust
 local sigmahat = e(rmse)
 outreg2 using "$output/Ch14_multireg.tex", se bdec(3) noaster ///
	adds(sigmahat, `sigmahat') tex(frag) append
 cap drop e
 predict e, resid 
 hist e, percent normal ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/Ch14_lnp_multireg_resid.png",replace

 
 
 

*********************************************
* PREDICTIONS SUMMARY
* repeat what we did in ch 13, no in logs
* generate  new observation with features our car

local nplus1=_N+1
set obs `nplus1'
replace age=10 if _n==`nplus1'
replace agesq=10^2 if _n==`nplus1'
replace odometer=12 if _n==`nplus1'
replace odometersq=12^2 if _n==`nplus1'
replace LE=1 if _n==`nplus1'
replace XLE=0 if _n==`nplus1'
replace SE=0 if _n==`nplus1'
replace cond_likenew=0 if _n==`nplus1'
replace cond_excellent=1 if _n==`nplus1'
replace cond_good=0 if _n==`nplus1'
replace cylind6=0 if _n==`nplus1'
replace dealer=0 if _n==`nplus1'

lis if _n==`nplus1'



 * now, in logs 

reg lnprice age odometer SE cond_likenew cond_excellent cond_good dealer
local sig=e(rmse)
dis `sig'
cap drop yin
cap drop lnp2*
 predict lnp2
 gen lnplev=exp(lnp2)*exp(`sig'^2/2)

 predict lnp2_pse, stdf
 gen lnp2_PIlow  = lnp2 - 1.96*lnp2_pse
 gen lnp2_PIhigh = lnp2 + 1.96*lnp2_pse
 gen lnplev_PIlow  =exp(lnp2_PIlow)*exp(`sig'^2/2)
 gen lnplev_PIhigh =exp(lnp2_PIhigh)*exp(`sig'^2/2)

sum lnplev* if _n==`nplus1'
