*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* PREDICTION
* Time series
* Transaction to daily 
* Focus on seasonality
* 
* FOCUS ON OUTDOOR POOL
* v1.1 2019 JUNE
********************************************************************
*
*********************************************************************

* WHAT THIS CODES DOES:

* reads in transaction level data, creates daily sum of tickets
* predict based on seaonsl patterns

* download data from http://data.cabq.gov/community/swimmingpooladmissions/



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/

cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

*global data_in	 "cases_studies_public" 
global data_in	 "cases_studies_public\swim-transactions/raw" 
global output    "textbook_work\ch18\swim-transactions/output"
global data_out  "textbook_work\ch18\swim-transactions"


/*

******************************************
* Sample design
******************************************
import delimited "$data_in/SwimmingPoolAdmissionsCABQ-en-us.csv", varnames(1) encoding(utf8) clear


* keep sunport pool (open)
* https://www.cabq.gov/parksandrecreation/recreation/swimming/outdoor-pools/sunport-pool/sunport-pool
keep if location =="AQSP01" /* a single outdoor pool Sunport*/



* alternative indoor pool
* keep highland pool (indoor) 
* https://www.cabq.gov/parksandrecreation/recreation/swimming/indoor-pools/highland-pool/highland-pool
*gen indoor_highland= location =="AQHP01"


***********************************************************
* Look at raw data
***********************************************************

gen n=1
gen timestamp = clock(date_time, "YMDhms", 2020)
format timestamp %tc
gen daily=dofc(timestamp)
collapse (sum) n quantity, by(daily )
label var quantity "outdoor pool Sunport - all quantity (daily sum)"
format daily %td
tsset daily
gen t=_n 



* all outdoor including weird stuff.
tsline quantity, lcolor(blue) tlabel(, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_all.png", as(png) replace

*/

***********************************************************
* START AGAIN AND FILTER DATA
***********************************************************
import delimited "$data_in/SwimmingPoolAdmissionsCABQ-en-us.csv", varnames(1) encoding(utf8) clear
keep if location =="AQSP01" /* a single outdoor pool Sunport*/


* inspect category
table category, c(median quantity min quantity max quantity )
keep if category=="ADMISTIER1" | category=="ADMISTIER2"

* inspect item
* TOT means toddlers. PM means an evening ticket. 
* based on feedback, we do not need additional adjustment

table item, c(median quantity min quantity max quantity )
gen core1 =inlist(item, "ADULT     " , "SENIOR    " ,"TEEN      " ,"CHILD     ", "TOT       ")
gen core2 =inlist(item, "CHILD PM  ","ADULT PM  ","SENIOR PM ", "TOT PM    ", "TEEN PN   "  )

gen item2=item
replace item2="other" if core1==0 & core2==0

tabout item2  ///
using "$output/items.tex", replace ///
style(tex) font(bold)    npos(lab) sum oneway format( 2 0 0 ) ///
c( mean quantity min quantity  max quantity ) /// 
 clab( Mean Min max ) 


keep if core1==1 | core2==1



gen n=1
gen timestamp = clock(date_time, "YMDhms", 2020)
format timestamp %tc
gen daily=dofc(timestamp)
collapse (sum) n quantity, by(daily )
label var quantity "Daily ticket sales"
* outdoor pool

format daily %td
tsset daily
save "$data_out/swim-transactions.dta", replace
*/

use "$data_out/swim-transactions.dta", clear
tsset daily
tsfill, full
replace quantity=0 if quantity==.


gen year=year(daily)
gen month=month(daily)
gen week=week(daily)
gen day=day(daily)
gen day1=dow(daily)
gen dayofweek=7-day1 /* sunday is 7 */
drop day1
label var daily "days of operation"
gen t=_n 


* filter on more reliable data
keep if year>=2010

***************************
* school period
* 

gen school_off=0
	replace school_off = 1 if  (day>15 & month==5 & day <=30)
	replace school_off = 1 if (month==6 |  month==7)
	replace school_off = 1 if (day<15 & month==8 )
	replace school_off = 1 if (day>20 & month==12 )
tab school_off

**************************************************************************
* LOOK AT GRAPHS
*
* baseline year is 2015
* test period is 2010-2015
**************************************************************************



tsline quantity if year<=2015, lcolor(blue) tlabel(, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_core.png", as(png) replace


tsline quantity if year==2015, lcolor(blue) ///
tlabel(#12, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_2015.png", as(png) replace
graph close







* getting rid of monthly seasonality
* 1 dummies
reg quantity i.month
predict monthly_avg
predict quantity_deseason_1, resid
label var monthly_avg "monthly average"
label var quantity_deseason_1 "quantity, de-seasonal"

tsline quantity monthly_avg if year ==2015, lcolor(blue) tlabel(#12, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_month_2015.png", as(png) replace

tsline quantity quantity_deseason_1 if year ==2015, ///
lcolor(blue) ///
tlabel(#12, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_desason_2015.png", as(png) replace
graph close


* getting rid of monthly seasonality
* 1 dummies
cap drop daily_avg
reg quantity i.dayofweek if month==4
predict daily_avg
label var daily_avg "day of the week average (April)"
tsline quantity daily_avg if year ==2015 & month==4, lcolor(blue) tlabel(#12, labsize(small) angle(forty_five)) tmtick(##11) ///
ylab(, grid) xlab(, grid) ///
color(emidblue) lcolor(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/Ch18_swimmingpool_daily_2015.png", as(png) replace





**********************************************
** PREPARE FORECASTING
**********************************************
* train=2010-2015
* test= 2016


** select year for test set
local testy 2016
** here we set the first year tos how on the output
local outputtart = `testy'-5

** define train and test sets
gen train = year<`testy'
gen test = year==`testy'
** drop time periods after test year - technicality, for nice output
drop if year>`testy'



** create month binary variables
tab month, gen(M)
tab dayofweek, gen(D)

forval j=2/7{
gen school_`j'=school_off*D`j'
}

gen q= quantity
gen ln_q =ln(quantity +1)

ac q
graph export "$output/q_ac.png",replace
pac q
graph export "$output/q_pac.png",replace


local testy 2016
local outputtart = `testy'-5



* MODEL: SEASONALITY MONTHS
local m season1
reg q M2-M12  if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12 
*forecast identity q = l.q
qui forecast solve, prefix(f`m'_) begin(td(02jan`testy'))
 lab var f`m'_q "Forecast. Model: monthly seasonality"
gen temp`m' = (q-f`m'_q)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

 


* MODEL: SEASONALITY MONTHS DOW School
local m season2
reg q M2-M12 D2-D7 school_off school_2 - school_7 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12 D2-D7 school_off school_2 - school_7 
*forecast identity q = l.q
qui forecast solve, prefix(f`m'_) begin(td(02jan`testy'))
 lab var f`m'_q "Forecast. Model: month, days, school seasonality"
gen temp`m' = (q-f`m'_q)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

 
 
cap drop *season3* 
local testy 2016
local outputtart = `testy'-5
 
 
* MODEL: SEASONALITY MONTHS DOW School 
* LOG!!
local m season3
reg ln_q M2-M12 D2-D7 school_off school_2 - school_7 if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12 D2-D7 school_off school_2 - school_7 
forecast identity q = exp(ln_q)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(td(02jan`testy'))
 lab var f`m'_q "Forecast. Model: log(q): month, days, school seasonality"
gen temp`m' = (q-f`m'_q)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)

 
 /* TODO
 
gen dln_q=d.ln_q
cap drop *season4* 
local testy 2016
local outputtart = `testy'-5
 
 * MODEL: SEASONALITY
local m season4
qui reg dln_q M2-M12 D2-D7 school_off school_2 - school_7  if train==1
 local sig = e(rmse)
 gen coeffs_`m' = e(df_m)+1
 gen trainobs_`m' = e(N)
est store `m'
forecast create `m', replace
forecast estimates `m'
forecast exogenous M2-M12
forecast identity ln_q = L.ln_q + dln_q
forecast identity q = exp(ln_q)*exp(`sig' ^2 /2)
qui forecast solve, prefix(f`m'_) begin(td(02jan`testy'))
 lab var f`m'_q "Forecast. Model: log diff(q): month, days, school seasonality"
gen temp`m' = (q-f`m'_q)^2
 egen MSE_`m'_test =  mean(temp`m') if test==1
 gen RMSE_`m'_test =  sqrt(MSE_`m'_test)
*/
 
 
 
label var q "actual daily quantity" 
tsline q fseason1_q fseason2_q  if year==2016 & year<., ///
ylab(, grid) xlab(,grid) ///
 legend(size(vsmall)) ///
lw(medium thin medium) lp(dot dash solid ) lc(blue navy edkblue ) ///
color(emidblue)  ///
graphregion(fcolor(white) ifcolor(none))  ///
plotregion(fcolor(white) ifcolor(white))
graph export "$output/fcast_season_2016.png",replace
