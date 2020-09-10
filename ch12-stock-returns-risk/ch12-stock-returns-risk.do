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
* CH12A Returns on a company stock and market returns
* using the stocks-sp500 dataset
* version 0.9 2020-09-10
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/GitHub/da_case_studies"


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


global data_in  "$data_dir/stocks-sp500/clean"
global work  	"ch12-stock-returns-risk"

cap mkdir 		"$work/output"
global output 	"$work/output"




************************************
* PART I: graphs
************************************

use "$data_in/stock-prices-daily.dta",clear
tsset date

* EXPLORING DAILY TIME SERIES 

* prices
tsline p_MSFT, lcolor(navy*0.8) lwidth(medium) /// 
 ylab(0(20)120, grid) xlab(,grid) ///
 tlab(01jan1998 01jan2003 01jan2008 01jan2013 01jan2018) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("Date (month)") ytitle(" Microsoft stock price (USD)")
graph export "$output/ch12-figure-2a-msft-daily-ts-Stata.png", replace

tsline p_SP500, lcolor(navy*0.8) lwidth(medium) /// 
 ylab(500(500)3000, grid) xlab(,grid) ///
 tlab(01jan1998 01jan2003 01jan2008 01jan2013 01jan2018) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 stock market index")
graph export "$output/ch12-figure-2b-sp500-daily-ts-Stata.png", replace

* EXPLORING MONTHLY TIME SERIES: PRICE LEVELS
keep if month!=month[_n+1] /* keep last day of month */
tab year
tsset ym


* prices
tsline p_MSFT, lcolor(navy*0.8) lwidth(thick) /// 
 ylab(10(10)120, grid) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Microsoft stock price (USD)")
graph export "$output/ch12-figure-3a-msft-monthly-ts-Stata.png", replace


tsline p_SP500, lcolor(navy*0.8) lwidth(thick) /// 
 ylab(500(500)3000, grid) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 stock market index")
graph export "$output/ch12-figure-3b-sp500-monthly-ts-Stata.png", replace


pperron p_MSFT
pperron p_SP500


* EXPLORING MONTHLY PERCENTAGE RETURNS
gen r_MSFT  =  100*(p_MSFT - p_MSFT[_n-1]) /p_MSFT[_n-1]
 lab var r_MSFT "Microsoft returns"
gen r_SP500 =  100*(p_SP500 - p_SP500[_n-1]) /p_SP500[_n-1]
 lab var r_SP500 "SP500 returns"

tsline r_MSFT, lcolor(navy*0.8) lwidth(thick) /// 
 ylab(-40(10)40, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Microsoft monthly returns (percent)")
graph export "$output/ch12-figure-4a-msft-monthly-returns-Stata.png", replace

tsline r_SP500, lcolor(navy*0.8) lwidth(thick) /// 
 ylab(-40(10)40, grid) yline(0) xlab(,grid) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" S&P 500 monthly returns (percent)")
graph export "$output/ch12-figure-4b-sp500-monthly-returns-Stata.png", replace


pperron r_MSFT
pperron r_SP500


tabstat r_MSFT r_SP500, s(min max mean sd n) c(s) format(%3.2f)



* REGRESSION, PERCENTAGE RETURNS
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-table-2-stocks-reg.tex",  tex(frag) dec(2) label 2aster replace 

* visualizations of the regression
* scatterplot plus regression line
scatter r_MSFT r_SP500, ms(O) mc(navy*0.6) ///
 || lfit r_MSFT r_SP500, lw(vthick ) lc(green*0.8)  ///
 || line r_SP500 r_SP500, lc(black) lp(dash) ///
 ylab(-40(10)40, grid) xlab(-40(10)40, grid) ///
 xtitle(" S&P 500 monthly returns (percent)") ///
 ytitle(" Microsoft monthly returns (percent)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 legend(order(2 3) label(2 "Regression line") label(3 "45 degree line"))
graph export "$output/ch12-figure-50-regression-Stata.png", replace

* nicer graph without the extreme values
* Figure 12.5
preserve
keep if r_MSFT>-20 & r_MSFT<30
scatter r_MSFT r_SP500, ms(O) mc(navy*0.6) ///
 || lfit r_MSFT r_SP500, lw(vthick ) lc(green*0.8)  ///
 || line r_SP500 r_SP500, lc(black) lp(dash) ///
 ylab(-20(10)30, grid) xlab(-20(10)30, grid) ///
 xtitle(" S&P 500 monthly returns (percent)") ///
 ytitle(" Microsoft monthly returns (percent)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 legend(order(2 3) label(2 "Regression line") label(3 "45 degree line"))
graph export "$output/ch12-figure-5-regression-Stata.png", replace
restore

* time series jointly
* entire time period
tsline r_MSFT r_SP500, lc(navy*0.8 green*0.8) lw(medthick medthick) ///
 ylab(-40(10)40, grid) yline(0) ///
 tlab(1998m1 2003m1 2008m1 2013m1 2018m1, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Monthly returns (percent)")
graph export "$output/ch12-figure-6a-returns-together-Stata.png", replace

* last two years
tsline r_MSFT r_SP500 if year>=2017, lc(navy*0.8 green*0.8) lw(medthick medthick) ///
 ylab(-12(4)12, grid) yline(0) tlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xtitle("") ytitle(" Monthly returns (percent)")
graph export "$output/ch12-figure-6b-returns-together-2017-9-Stata.png", replace


* ADDITIONAL REGRESSIONS: LOG CHAGE, DAILY FREQ
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
 outreg2 using "$output/ch12-table-3-stocks-reg.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, monthly, pct change") replace 
replace r_MSFT = dlnp_MSFT /* to have the estimates in the same row in the table */
replace r_SP500 = dlnp_SP500 /* to have the estimates in the same row in the table */
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-table-3-stocks-reg.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, monthly, log change") append
 
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
 outreg2 using "$output/ch12-table-3-stocks-reg.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, daily, pct change") append
replace r_MSFT = dlnp_MSFT /* to have the estimates in the same row in the table */
replace r_SP500 = dlnp_SP500 /* to have the estimates in the same row in the table */
reg r_MSFT r_SP500, robust
 outreg2 using "$output/ch12-table-3-stocks-reg.tex", tex(frag) dec(4) 2aster label ctitle("MSFT returns, daily, log change") append
 
