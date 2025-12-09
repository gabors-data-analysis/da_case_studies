********************************************************************
* LOAD AND PROCESS DATA - SOURCE 1: ELECTRICITY CONSUMPTION
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
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


********************************************************************
* SETTING UP DIRECTORIES
********************************************************************

* STEP 1: set working directory for da_case_studies
* Example: cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global data_in  "${data_dir}/arizona-electricity/clean"
global work     "ch12-electricity-temperature"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD ELECTRICITY DATA
********************************************************************

clear
import delimited using "${data_in}/electricity_resid_AZ.csv", clear

* Or download directly from OSF:
/*
tempfile electricity_data
copy "https://osf.io/download/wbef4/" `electricity_data'
import delimited using `electricity_data', clear
*/

rename my mystring
generate date = date(mystring, "MY", 2020)
format date %td
generate month = month(date)
generate year = year(date)
generate ym = ym(year, month)
format ym %tm
order ym year month
drop mystring date

rename q Q
label variable Q "Residential electricity consumption, GWh"
generate lnQ = ln(Q)
label variable lnQ "Residential electricity consumption, GWh, in logs"

compress


********************************************************************
* LOAD AND PROCESS DATA - SOURCE 2: TEMPERATURE
********************************************************************

save "${work}/electricity_resid_AZ", replace

* Source 2: temperature data (cooling degree days etc, by month)
clear

import delimited using "${data_in}/climate_Phoenix_AZ.csv", clear

* Or download directly from OSF:
/*
tempfile climate_data
copy "https://osf.io/download/g3tj7/" `climate_data'
import delimited using `climate_data', clear
*/

generate tempdate = date(date, "YM")
format tempdate %td
generate year = year(tempdate)
generate month = month(tempdate)
generate ym = ym(year, month)
format ym %tm
rename cldd cd
rename htdd hd
order ym year month cd hd
drop date tempdate
drop station name

* From sums to averages
foreach x in cd hd dx70 dx90 {
	generate `x'avg = `x' / 30
	replace `x'avg = `x' / 31 if inlist(month, 1, 3, 5, 7, 8, 10, 12)
	replace `x'avg = `x' / 28 if month==2
}
label variable cdavg "Cooling degrees, daily avg."
label variable hdavg "Heating degrees, daily avg."
label variable dx70avg "Fraction days > 70F"
label variable dx90avg "Fraction days > 90F"

summarize cdavg hdavg dx70avg dx90avg

compress
save "${work}/climate_Phoenix", replace


********************************************************************
* MERGE DATA AND CREATE WORKFILE
********************************************************************

* Create workfile
merge 1:1 ym using "${work}/electricity_resid_AZ", nogen

keep if year>=2001
keep if year<=2017

tsset ym

save "${work}/electricity_AZ_workfile", replace


********************************************************************
* DATA EXPLORATION
********************************************************************

use "${work}/electricity_AZ_workfile", replace
summarize year if Q!=.
summarize year if cd!=.

tabstat Q lnQ cdavg hdavg, s(n min max mean median sd) col(s)


********************************************************************
* PLOT THE TIME SERIES
********************************************************************

foreach x in Q lnQ cdavg hdavg {
	tsline `x', lw(thick) lc(navy) ylab(, grid) xtitle("") ///
		tmtick(##4, grid) ///
		graphregion(fcolor(white) ifcolor(none)) ///
		plotregion(fcolor(white) ifcolor(white))
	graph export "${output}/ch12-figure-electricity_ts_`x'-Stata.png", as(png) replace
}


********************************************************************
* TIME-SERIES REGRESSIONS
********************************************************************

* Create change variables (first differences)
generate dlnQ = d.lnQ
generate dcd = d.cdavg
generate dhd = d.hdavg
generate ddx90avg = d.dx90avg
label variable dlnQ "Log monthly electricity consumption, first difference"
label variable dcd "Cooling degrees, monthly avg, first difference"
label variable dhd "Heating degrees, monthly avg, first difference"
label variable ddx90avg "Fraction of days >90F, first difference"

label variable dlnQ "$\Delta \ln Q$"
label variable dcd "$\Delta CD$"
label variable dhd "$\Delta HD$"
label define mon_name 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"
label values month mon_name
label variable ddx90avg "Fraction of days >90F, first difference"


********************************************************************
* TABLE 12.4: REGRESSION WITH NEWEY-WEST SE
********************************************************************

* Simple regression with Newey-West SE
newey dlnQ dcd dhd, lag(12)
outreg2 using "${output}/ch12-table-4-electricity_AZ_reg-Stata.tex", ///
	dec(3) label 2aster tex(frag) replace

* Add seasonality
newey dlnQ dcd dhd i.month, lag(12)
outreg2 using "${output}/ch12-table-4-electricity_AZ_reg-Stata.tex", ///
	dec(3) label 2aster tex(frag) append


********************************************************************
* TABLE 12.5: ILLUSTRATING SERIAL CORRELATION ROBUST SE
********************************************************************

* Simple regression + seasonality
* Add 4 digits to see differences in SE

* 1. Simple SE
regress dlnQ dcd dhd i.month
outreg2 using "${output}/ch12-table-5-electricity_AZ_reg-Stata.tex", ///
	dec(3) keep(dcd dhd) label 2aster tex(frag) replace

* 2. Newey-West SE
newey dlnQ dcd dhd i.month, lag(12)
outreg2 using "${output}/ch12-table-5-electricity_AZ_reg-Stata.tex", ///
	dec(3) keep(dcd dhd) label 2aster tex(frag) append

* 3. Lagged y, simple SE
generate dlnQlag = L.dlnQ
label variable dlnQlag "Lag of $\Delta \ln Q$"
regress dlnQ dcd dhd dlnQlag i.month
outreg2 using "${output}/ch12-table-5-electricity_AZ_reg-Stata.tex", ///
	dec(3) keep(dcd dhd dlnQlag) label 2aster tex(frag) append

* Serial correlation in dlnQ
correlate dlnQ L.dlnQ


********************************************************************
* TABLE 12.6: LAGGED ASSOCIATIONS
********************************************************************

* RHS variables in lags + seasonality
* Create lagged variables (displayed nicer in table if created separately)
capture generate ldcd = L.dcd
capture generate l2dcd = L2.dcd
capture generate ldhd = L.dhd
capture generate l2dhd = L2.dhd
label variable ldcd "$\Delta CD$ 1st lag"
label variable l2dcd "$\Delta CD$ 2nd lag"
label variable ldhd "$\Delta HD$ 1st lag"
label variable l2dhd "$\Delta HD$ 2nd lag"

regress dlnQ dcd ldcd l2dcd dhd ldhd l2dhd i.month
outreg2 using "${output}/ch12-table-6-electricity_AZ_reg.tex", dec(3) nocon label ///
	keep(dcd ldcd l2dcd dhd ldhd l2dhd) addtext("Month binary variables", "Yes") ///
	2aster tex(frag) replace

* Long-run associations
* Create variables
capture generate dcd_cumul = L2.dcd
capture generate dhd_cumul = L2.dhd
capture generate ddcd = D.dcd
capture generate ddhd = D.dhd
label variable dcd_cumul "$\Delta CD$ cumulative coeff"
label variable dhd_cumul "$\Delta HD$ cumulative coeff"
label variable ddcd "$\Delta (\Delta CD)$ "
label variable ddhd "$\Delta (\Delta HD)$ "

regress dlnQ dcd_cumul dhd_cumul L(0/1).ddcd L(0/1).ddhd i.month
outreg2 using "${output}/ch12-table-6-electricity_AZ_reg.tex", dec(3) nocon label ///
	keep(dcd_cumul dhd_cumul) ///
	addtext("Month binary variables", "Yes") 2aster tex(frag) append
