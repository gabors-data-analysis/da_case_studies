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
* Chapter 08
* CH0bB How is life expectancy related to the average income of a country?
* using the worldbank-lifeexpectancy dataset
* version 0.9 2020-09-06
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


global data_in  "$data_dir/worldbank-lifeexpectancy/clean"
global work  	"ch08-life-expectancy-income"

cap mkdir 		"$work/output"
global output 	"$work/output"




*********************************************************************
*** IMPORT DATA
use "$data_in/worldbank-lifeexpectancy.dta", clear


keep if year==2017
count

count if gdppc!=. & lifeexp!=.
sum population if gdppc!=. & lifeexp!=.
count if gdppc==. | lifeexp==.
sum population if gdppc==. | lifeexp==.
lis countryname population gdppc lifeexp  if (gdppc==. | lifeexp==. ) & population>1

keep if gdppc!=. & lifeexp!=.


compress

sort countryname

*** GDP total, log 
gen gdptotal = gdppc*population
 lab var gdptot "GDP total, billion USD (PPT constant 2011 prices)"
gen lngdptot=ln(gdptot)
 lab var lngdptot "ln GDP total"
*** log GDP per capita
gen lngdppc=ln(gdppc)
 lab var lngdppc "ln GDP per capita"

tabstat lifeexp gdppc gdptot lngdptot lngdppc, s(mean sd min median max n) col(s)

lis countryname population lifeexp if lifeexp<55
lis countryname population lifeexp if lifeexp>83


******************************************************************************
*DISTRIBUTION OF PER CAPITA GDP

* Figure 3a
hist gdppc, percent start(0) width(3) fcol(navy*0.8) lcol(white) ///
 xla(0(15)120, grid) yla(0(5)20, grid) ///
 xtitle("GDP per capita (thousand US dollars)")
graph export "$output/ch08-figure-3a-gdppercap-hist-Stata.png",replace

lis countryname population gdpp if gdppc>80
tabstat gdppc, s(mean median sd n)

* Figure 3b
hist lngdppc, percent width(0.2) fcol(navy*0.8) lc(white) ///
 xla(, grid) yla(0(2)10, grid) ///
 xtitle("ln(GDP per capita, thousand US dollars)")
graph export "$output/ch08-figure-3b-lngdppercap-hist-Stata.png",replace

lis countryname population gdpp lngdppc if gdppc<1
tabstat lngdppc, s(mean median sd n)
 
 
******************************************************************************
*LIFE EXPECTANCY VS LEVEL AND LOG PER CAPITA GDP

reg lifeexp gdppc
scatter lifeexp gdppc, ///
 ms(O) mcolor(navy*0.6) ///
 || lfit lifeexp gdppc, lw(thick) lc(green*0.8) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("GDP per capita (thousand US dollars)") ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
  ylabel(50(5)100, grid) xlab(0(10)100, grid) 
graph export "$output/ch08-figure-4-linreg-levlev-Stata.png",replace


reg lifeexp lngdppc
scatter lifeexp lngdppc, ///
 ms(O) mcolor(navy*0.6) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("ln(GDP per capita, thousand US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(0(1)5, grid) 
graph export "$output/ch08-figure-5a-linreg-levlog-Stata.png",replace


foreach x in 1 2 5 10 20 50 100 {
	dis `x' "   "  ln(`x')
}
 
scatter lifeexp lngdppc, ///
 ms(O) mcolor(navy*0.8) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("GDP per capita, thousand US dollars, ln scale") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) ///
 xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid)   
graph export "$output/ch08-figure-5b-linreg-levlog-logscale-Stata.png",replace

 
reg lifeexp lngdppc
cap drop resid
predict resid,resid
lis countryname population lifeexp resid if resid<-12
lis countryname population lifeexp resid if resid>7
lis countryname population lifeexp resid if countrycode=="USA"
lis countryname population lifeexp resid if countrycode=="JPN"
lis countryname population lifeexp resid if lngdppc>4.5
lis countryname population lifeexp gdppc resid if gdppc>65




******************************************************************************
*LIFE EXPECTANCY VS LEVEL AND LOG TOTAL GDP

reg lifeexp gdptot
scatter lifeexp gdptot, ///
 ms(O) mcolor(navy*0.6) ///
 || lfit lifeexp gdptot, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("Total GDP (billion US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(0(4000)24000, grid) 
graph export "$output/ch08-figure-6a-linreg-totalGDP-Stata.png",replace
 
reg lifeexp lngdptot
 scatter lifeexp lngdptot, ///
 ms(O) mcolor(navy*0.6) ///
 || lfit lifeexp lngdptot, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("ln(total GDP, billion US dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(-2(2)10, grid) 
graph export "$output/ch08-figure-6b-linreg-lntotalGDP-Stata.png",replace

lis countryname population gdppc gdptotal if gdptotal>5000
 
******************************************************************************
*PIECEWISE LINEAR REGRESSION

mkspline lngdppc_low 3.9 lngdppc_high = lngdppc /* knot at 50th USD; ln(50)=3.9 */
regress lifeexp lngdppc_*
cap drop pred_spline
predict pred_spline

scatter lifeexp lngdppc, ///
 ms(O) mcolor(navy*0.6) ///
 || line pred_spline lngdppc, sort lw(thick) lc(green) legend(off) ///
 xtitle("ln(GDP per capita, thousand US dollars)") ///
 ytitle("Life expectancy (years)") ylabel(50(5)85, grid) ///
  xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
 xline(3.9, lc(black) lp(dash)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-7a-spline-Stata.png",replace

regress lifeexp lngdppc_low lngdppc_high
predict resid_spline, resid
lis countryname population lifeexp resid_spline if resid_spline <-12
lis countryname population lifeexp resid_spline if resid_spline >6.5



******************************************************************************
*** QUADRATIC

*polynomials
cap gen lngdppc_sq=lngdppc^2

regress lifeexp lngdppc lngdppc_sq
cap drop pred_quad
predict pred_quad

scatter lifeexp lngdppc, ///
 ms(O) mcolor(navy*0.6) ///
 || line pred_quad lngdppc, sort lw(thick) lc(green) legend(off) ///
 xtitle("ln(GDP per capita, thousand dollars)") ///
 ytitle("Life expectancy (years)") ylabel(50(5)85, grid) ///
  xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-7b-quad-Stata.png",replace

regress lifeexp lngdppc_sq
predict resid_quad, resid
lis countryname population lifeexp resid_quad if resid_quad<-12
lis countryname population lifeexp resid_quad if resid_quad>7.5
 
******************************************************************************
*WEIGHTED AND UNWEIGHTED
scatter lifeexp lngdppc, ///
 ms(O) mcolor(navy*0.8) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("GDP per capita, thousand US dollars, ln scale") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) ///
 xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid)   
graph export "$output/ch08-figure-9a-linreg-unwgt-logscale-Stata.png",replace
 
 
reg lifeexp lngdppc [w=pop]
scatter lifeexp lngdppc [w=pop], ///
 ms(O) mcolor(navy*0.6) ///
 || lfit lifeexp lngdppc [w=pop], lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") ///
 xtitle("GDP per capita, thousand US dollars, ln scale") ///
 ylabel(50(5)85, grid) ///
 xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
 text(67 2.0 "India") text(77 2.6 "China") text(79 4.1 "USA") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch08-figure-9b-linreg-wgt-logscale-Stata.png",replace

 
