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
* Chapter 12
* CH12B Electricity consumption and temperature
* using the arizona-electricity dataset
* version 0.9 2020-09-10
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


global data_in  "$data_dir/arizona-electricity/raw"
global work  	"ch12-electrictiy-temperature"

cap mkdir 		"$work/output"
global output 	"$work/output"




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
save "$work\electricity_resid_AZ",replace

* source 2: temperature data (cooling degree days etc, by month)
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
save "$work\climate_Phoenix", replace

* create workfile

merge 1:1 ym using "$work\electricity_resid_AZ", nogen

keep if year>=2001
keep if year<=2017

tsset ym

save "$work\electricity_AZ_workfile", replace


*** DATA EXPLORATION
use "$work\electricity_AZ_workfile",replace
sum year if Q!=.
sum year if cd!=.

tabstat Q lnQ cdavg hdavg, s(n min max mean median sd) col(s)

*** PLOT THE TIME SERIES
foreach x in Q lnQ cdavg hdavg  {
	tsline `x' , lw(thick) lc(navy) ylab(, grid) xtitle("") ///
	tmtick(##4, grid) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output\ch12-figure-electricity_ts_`x'-Stata.png" , replace 
}

**** TIME-SERIES REGRESSIONS

* create change variables (first differences)
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
 outreg2 using "$output\ch12-table-4-electricity_AZ_reg-Stata.tex", ///
  dec(3) label 2aster tex(frag) replace
* + seasonality
newey dlnQ dcd dhd i.month, lag(12) 
 outreg2 using "$output\ch12-table-4-electricity_AZ_reg-Stata.tex", ///
  dec(3) label 2aster tex(frag) append

 
* REGRESSION TABLE 2: ILLUSTRATING SERIAL CORRELATION ROBUST SE
* simple reg + seasonality
* add 4 digits to see differences in SE
* 1 simple SE 
reg dlnQ dcd dhd i.month
 outreg2 using "$output\ch12-table-5-electricity_AZ_reg-Stata.tex", ///
  dec(3) keep(dcd dhd) label 2aster tex(frag) replace
* 2 Newey-West SE 
newey dlnQ dcd dhd i.month, lag(12) 
 outreg2 using "$output\ch12-table-5-electricity_AZ_reg-Stata.tex", ///
  dec(3) keep(dcd dhd) label 2aster tex(frag) append
* 3 Lagged y, simple SE 
gen dlnQlag = L.dlnQ
lab var dlnQlag "Lag of $\Delta \ln Q$"
reg dlnQ dcd dhd dlnQlag i.month
 outreg2 using "$output\ch12-table-5-electricity_AZ_reg-Stata.tex", ///
  dec(3) keep(dcd dhd dlnQlag) label 2aster tex(frag) append

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
 outreg2 using "$output\ch12-table-6-electricity_AZ_reg.tex", dec(3) nocon label ///
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
 outreg2 using "$output\ch12-table-6-electricity_AZ_reg.tex", dec(3) nocon label ///
 keep(dcd_cumul dhd_cumul ) ///
 addtext("Month binary variables" , "Yes") 2aster tex(frag) append


