
***************************************************************
* Case Study: Electricity consumption and temperature
* Data: arizona-electricity


* Arizona, USA - climate (temperature) and household electricity consumption
***************************************************************

* version 1.7 2018-11-30 Finalized data, set of graphs, regressions
* version 1.8.2019-08-11 Clean close, drop unused bits
* version 1.9 2019-11-28 Minor changes


* WHAT THIS CODES DOES:
* daily-->monthly aggregation
* data merge
* describe
* run TS regressions with lags

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook"
 

global data_in	  "da_data_repo/arizona-electricity/raw" /* change this to where u download raw data from moodle */
global data_out	  "da_case_studies/ch12-electrictiy-temperature"
global output     "da_case_studies/ch12-electrictiy-temperature/output"




*****************************************
* TIDY DATA 
* source 1: electricity consumption, by month

clear
insheet using "$data_in\electricity_resid_AZ.csv", comma 
rename my mystring
gen date=date(mystring,"MY", 2020)
format date %td
gen month=month(date)
gen year=year(date)
gen ym = ym(year,month)
format ym %tm
order ym year month 
drop mystring date

rename q Q
 lab var Q "Residential electricity consumption, GWh"
gen lnQ=ln(Q)
  lab var lnQ "Residential electricity consumption, GWh, in logs"

compress
save "$data_out\electricity_resid_AZ",replace

* source 2: "climate" (cooling degree days etc, by month)

clear

insheet using "$data_in\climate_Phoenix_AZ.csv" 
gen tempdate=date(date,"YM")
format tempdate %td
gen year=year(tempdate)
gen month=month(tempdate)
gen ym=ym(year,month)
format ym %tm
rename cldd cd
rename htdd hd
order ym year month cd hd 
drop date tempdate 
drop station name

* from sums to averages
foreach x in cd hd dx70 dx90 {
	gen `x'avg = `x'/30
	replace `x'avg=`x'/31 if inlist(month, 1, 3, 5, 7, 8, 10, 12)
	replace `x'avg=`x'/28 if month==2
}
lab var cdavg "Cooling degrees, daily avg."
lab var hdavg "Heating degrees, daily avg."
lab var dx70avg "Fraction days > 70F"
lab var dx90avg "Fraction days > 90F"

summ cdavg hdavg dx70avg dx90avg

compress
save "$data_out\climate_Phoenix", replace

* create workfile

merge 1:1 ym using "$data_out\electricity_resid_AZ", nogen

keep if year>=2001
keep if year<=2017

tsset ym

save "$data_out\electricity_AZ_workfile", replace


***********************************************************
*** DATA EXPLORATION
use "$data_out\electricity_AZ_workfile",replace
sum year if Q!=.
sum year if cd!=.

tabstat Q lnQ cdavg hdavg, s(n min max mean median sd) col(s)

*** PLOT THE TIME SERIES


foreach x in Q lnQ cdavg hdavg  {
	tsline `x' , lw(thick) lc(navy) ylab(, grid) xtitle("") ///
	tmtick(##4, grid) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\electricity_ts_`x'.png" , replace 
}
*



* only 2014-2017
foreach x in Q lnQ cdavg hdavg  {
	tsline `x' if year>=2014, lw(thick) lc(navy) ylab(, grid) xlab(, grid) xtitle("") ///
	tlab(2014m1 2014m7  2015m1 2015m7  2016m1 2016m7  2017m1 2017m7 ) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\electricity_ts_`x'4y.png" , replace 
}
*

***********************************************************
**** TS REGRESSIONS

use "$data_out\electricity_AZ_workfile",replace

****************
* create differences
gen dlnQ=d.lnQ
gen dcd=d.cdavg
gen dhd=d.hdavg
gen ddx90avg=d.dx90avg
lab var dlnQ "Log monthly electricity consumption, first difference"
lab var dcd "Cooling degrees, monthly avg, first difference"
lab var dhd "Heating degrees, monthly avg, first difference"
lab var ddx90avg "Fraction of days >90F, first difference"



lab var dlnQ "$\Delta \ln Q$"
lab var dcd "$\Delta CD$"
lab var dhd "$\Delta HD$"
label define mon_name 1 January 2 February 3 Mrach 4 April 5 May 6 June 7 July 8 August 9 September 10 October 11 November 12 December
label values month mon_name
lab var ddx90avg "Fraction of days >90F, first difference"


* REGRESSION TABLE 1

* simple reg, Newey-West SE
newey dlnQ dcd dhd, lag(12) 
 outreg2 using "$output\electricity_AZ_reg1.tex", dec(3) label 2aster tex(frag) replace
* + seasonality
newey dlnQ dcd dhd i.month, lag(12) 
 outreg2 using "$output\electricity_AZ_reg1.tex", dec(3) label 2aster tex(frag) append


 *lowess regressions - to check functional form
lowess dlnQ dcd, lineopts(lw(thick) lc(dkgreen)) ///
 ylab(, grid) xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))

lowess dlnQ dhd, lineopts(lw(thick) lc(dkgreen)) ///
 ylab(, grid) xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))

 
 
* REGRESSION TABLE 2: ILLUSTRATING SERIAL CORRELATION ROBUST SE
* simple reg + seasonality
* add 4 digits to see differences in SE
* 1 simple SE 
reg dlnQ dcd dhd i.month
 outreg2 using "$output\electricity_AZ_reg2.tex", dec(4) label 2aster tex(frag) ///
  keep(dcd dhd) nocons replace
* 2 Newey-West SE 
newey dlnQ dcd dhd i.month, lag(12) 
 outreg2 using "$output\electricity_AZ_reg2.tex", dec(4) label 2aster tex(frag) ///
  keep(dcd dhd) nocons append
* 3 Lagged y, simple SE 
gen dlnQlag = L.dlnQ
lab var dlnQlag "Lag of $\Delta \ln Q$"
reg dlnQ dcd dhd dlnQlag i.month
 outreg2 using "$output\electricity_AZ_reg2.tex", dec(4) label 2aster tex(frag) ///
  keep(dcd dhd dlnQlag ) nocons append

* serial corr in dlnQ
corr dlnQ L.dlnQ

 
 
* REGRESSION TABLE 3: LAGGED ASSOCIATIONS.
* RHS variables in lags + seasonality
* create lagged variables (displayed nicer in table if created separeately)
cap gen ldcd=L.dcd
cap gen l2dcd=L2.dcd
cap gen ldhd=L.dhd
cap gen l2dhd=L2.dhd
lab var ldcd "$\Delta CD$ 1st lag"
lab var l2dcd "$\Delta CD$ 2nd lag"
lab var ldhd "$\Delta HD$ 1st lag"
lab var l2dhd "$\Delta HD$ 2nd lag"

reg dlnQ dcd ldcd l2dcd dhd ldhd l2dhd i.month
 outreg2 using "$output\electricity_AZ_reg3.tex", dec(3) nocon label ///
 keep(dcd ldcd l2dcd dhd ldhd l2dhd) addtext("Month binary variables" , "Yes") ///
 2aster tex(frag) replace

* long-run associations
* create variables
cap gen dcd_cumul=L2.dcd
cap gen dhd_cumul=L2.dhd
cap gen ddcd = D.dcd
cap gen ddhd = D.dhd
lab var dcd_cumul "$\Delta CD$ cumulative coeff"
lab var dhd_cumul "$\Delta HD$ cumulative coeff"
lab var ddcd "$\Delta (\Delta CD)$ "
lab var ddhd "$\Delta (\Delta HD)$ "

reg dlnQ dcd_cumul dhd_cumul L(0/1).ddcd L(0/1).ddhd i.month
 outreg2 using "$output\electricity_AZ_reg3", dec(3) nocon label ///
 keep(dcd_cumul dhd_cumul ) ///
 addtext("Month binary variables" , "Yes") 2aster tex(frag) append


