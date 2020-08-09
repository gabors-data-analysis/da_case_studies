
***************************************************************
* Risk and returns
* sandp-stocks

* data is 21 years: 29dec1997 to 31dec2018. 
* monthly yield is based on last day of the month: 
* 253 end-of-month figures; 252 yields
***************************************************************

* version 2.0.2019-11-24 Redo from scratch
* version 2.1 2019-11-27 some changes re labels


* WHAT THIS CODES DOES:
* creates graphs
* runs simple regressions


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
*cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data


*/

global data_in   	"da_data_repo\stocks-sp500\clean"
global data_out  	"da_case_studies/ch12-stock-returns-risk"
global output 		"da_case_studies/ch12-stock-returns-risk/output"



************************************
* PART I: graphs
************************************

use "$data_in/stock-prices-daily.dta",clear
tsset date

************************************************************
* EXPLORING DAILY TIME SERIES 
************************************************************

* prices
tsline p_MSFT, lcolor(navy) lwidth(medium) /// 
 ylab(0(20)120, grid) xlab(,grid) ///
 tlab(01jan1998 01jan2003 01jan2008 01jan2013 01jan2018) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("Date (month)") ytitle(" Microsoft stock price (USD)")


tsline p_SP500, lcolor(navy) lwidth(medium) /// 
 ylab(500(500)3000, grid) xlab(,grid) ///
 tlab(01jan1998 01jan2003 01jan2008 01jan2013 01jan2018) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 stock market index")




************************************************************
* EXPLORING MONTHLY TIME SERIES: PRICE LEVELS
************************************************************
use "$data_in/stock-prices-daily.dta",clear
keep if month!=month[_n+1] /* keep last day of month */
tab year
tsset ym


* prices
tsline p_MSFT, lcolor(navy) lwidth(medium) /// 
 ylab(10(10)120, grid) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Microsoft stock price (USD)")
graph export  "$output/ch12-stocks-msft-2.png",replace


tsline p_SP500, lcolor(navy) lwidth(medium) /// 
 ylab(500(500)3000, grid) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 stock market index")
graph export  "$output/ch12-stocks-sp500-2.png",replace


pperron p_MSFT
pperron p_SP500



************************************************************
* EXPLORING MONTHLY TIME SERIES: RETURNS
************************************************************
* PERCENTAGE RETURNS
gen r_MSFT  =  100*(p_MSFT - p_MSFT[_n-1]) /p_MSFT[_n-1]
 lab var r_MSFT "Microsoft returns"
gen r_SP500 =  100*(p_SP500 - p_SP500[_n-1]) /p_SP500[_n-1]
 lab var r_SP500 "SP500 returns"

* prices
tsline r_MSFT, lcolor(navy) lwidth(medium) /// 
 ylab(-40(10)40, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Microsoft monthly returns (percent)")
graph export  "$output/ch12-stocks-msft-3.png",replace


tsline r_SP500, lcolor(navy) lwidth(medium) /// 
 ylab(-40(10)40, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 monthly returns (percent)")
graph export  "$output/ch12-stocks-sp500-3.png",replace


pperron r_MSFT
pperron r_SP500


tabstat r_MSFT r_SP500, s(min max mean sd n) c(s) format(%3.2f)



/************************************************************
* CHANGES IN LEVELS
gen d_MSFT  =  p_MSFT - p_MSFT[_n-1]
gen d_SP500 =  p_SP500 - p_SP500[_n-1]

* prices
tsline d_MSFT, lcolor(navy) lwidth(medium) /// 
 ylab(, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Microsoft monthly price changes (USD)")
graph export  "$output/ch12-stocks-msft-4.png",replace


tsline d_SP500, lcolor(navy) lwidth(medium) /// 
 ylab(, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 monthly price changes")
graph export  "$output/ch12-stocks-sp500-4.png",replace
*/


************************************************************
* REGRESSION, PERCENTAGE RETURNS
************************************************************
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-stocks-reg-1.tex",  tex(frag) dec(2) label 2aster replace 

* visualizations of the regression
* scatterplot plus regression line
scatter r_MSFT r_SP500 ///
 || lfit r_MSFT r_SP500, lw(vthick ) lc(navy)  ///
 || line r_SP500 r_SP500, lc(mint) ///
 ylab(-40(10)40, grid) xlab(-40(10)40, grid) ///
 xtitle(" S&P 500 monthly returns (percent)") ///
 ytitle(" Microsoft monthly returns (percent)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 legend(order(2 3) label(2 "Regression line") label(3 "45 degree line"))
graph export  "$output/ch12-stocks-scatter.png",replace


* time series jointly
* entire time period
tsline r_MSFT r_SP500, lc(navy mint) ///
 ylab(-40(10)40, grid) yline(0) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Monthly returns (percent)")
graph export  "$output/ch12-stocks-together-1.png",replace

* last two years
tsline r_MSFT r_SP500 if year>=2017, lc(navy mint) ///
 ylab(-20(10)20, grid) yline(0) tlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Monthly returns (percent)")
graph export  "$output/ch12-stocks-together-2.png",replace


************************************************************
* ADDITIONAL REGRESSIONS: LOG CHAGE, DAILY FREQ
************************************************************
* MONTHLY
use "$data_in/stock-prices-daily.dta",clear
keep if month!=month[_n+1] /* keep last day of month */
tsset ym

gen r_MSFT  =  100*(p_MSFT - p_MSFT[_n-1]) /p_MSFT[_n-1]
 lab var r_MSFT "Microsoft returns"
gen r_SP500 =  100*(p_SP500 - p_SP500[_n-1]) /p_SP500[_n-1]
 lab var r_SP500 "SP500 returns"
gen dlnp_MSFT = ln(p_MSFT) - ln(p_MSFT[_n-1])
gen dlnp_SP500 = ln(p_SP500) - ln(p_SP500[_n-1])

*scatter dlnp_MSFT r_MSFT , xla(, grid) yla(, grid)
*scatter dlnp_SP500 r_SP500 , xla(, grid) yla(, grid)


reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-stocks-reg-2.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, monthly, pct change") replace 
replace r_MSFT = dlnp_MSFT /* to have the estimates in the same row in the table */
replace r_SP500 = dlnp_SP500 /* to have the estimates in the same row in the table */
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-stocks-reg-2.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, monthly, log change") append
 
* DAILY:Y
use "$data_in/stock-prices-daily.dta",clear
tsset date


gen r_MSFT  =  100*(p_MSFT - p_MSFT[_n-1]) /p_MSFT[_n-1]
 lab var r_MSFT "Microsoft returns"
gen r_SP500 =  100*(p_SP500 - p_SP500[_n-1]) /p_SP500[_n-1]
 lab var r_SP500 "SP500 returns"
gen dlnp_MSFT = ln(p_MSFT) - ln(p_MSFT[_n-1])
gen dlnp_SP500 = ln(p_SP500) - ln(p_SP500[_n-1])

*scatter dlnp_MSFT r_MSFT , xla(, grid) yla(, grid)
*scatter dlnp_SP500 r_SP500 , xla(, grid) yla(, grid)

reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-stocks-reg-2.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, daily, pct change") append
replace r_MSFT = dlnp_MSFT /* to have the estimates in the same row in the table */
replace r_SP500 = dlnp_SP500 /* to have the estimates in the same row in the table */
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-stocks-reg-2.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, daily, log change") append
 
