*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* FUNDAMENTALS OF REGRESSION ANALYSIS
* ILLUSTRATION STUDY FOR CHAPTER 8
* Life expectancy and GDP (per capita), xcountry regression
*
* data downloaded from the World Bank
*
*********************************************************************

* WHAT THIS CODES DOES:

* Imports data to Stata
* Manages data to get a clean dataset to work with
* Transforms GDP variable into per capita and then log
* Performs regression analysis of E[lifeexp|ln(GDP/capita)]


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook"

clear all
set more off

 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

global data_in   "da_data_repo/worldbank-lifeexpectancy/clean"
global data_out  "da_case_studies/ch08-life-expectancy-income"
global output 	 "da_case_studies/ch08-life-expectancy-income/output"


*********************************************************************
*** IMPORT DATA
use "$data_in/worldbank-lifeexpectancy.dta", clear

count if gdppc!=. & lifeexp!=.
sum population if gdppc!=. & lifeexp!=.
count if gdppc==. | lifeexp==.
sum population if gdppc==. | lifeexp==.
lis countryname population gdppc lifeexp  if (gdppc==. | lifeexp==. ) & population>1

keep if gdppc!=. & lifeexp!=.
keep if year==2017
count


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
hist gdppc, percent width(5) fcol(navy) lcol(white) ///
 xla(0(20)100, grid) yla(0(5)30, grid) ///
 xtitle("GDP per capita (thousand dollars)")
 graph export "$output/ch08_lifeexp_hist1.png",replace

lis countryname population gdpp if gdppc>80
tabstat gdppc, s(mean median sd n)
 
hist lngdppc, percent width(0.4) fcol(navy) lc(white) ///
 xla(, grid) yla(0(5)30, grid) ///
 xtitle("ln(GDP per capita, thousand dollars)")
 graph export "$output/ch08_lifeexp_hist2.png",replace

lis countryname population gdpp lngdppc if gdppc<1
tabstat lngdppc, s(mean median sd n)
 
 
******************************************************************************
*LIFE EXPECTANCY VS LEVEL AND LOG PER CAPITA GDP

reg lifeexp gdppc
scatter lifeexp gdppc, ///
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp gdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("GDP per capita, '000 USD PPP 2011") ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
  ylabel(50(5)100, grid) xlab(0(10)100, grid) 
 graph export "$output/ch08_lifeexp_linreg1.png",replace


reg lifeexp lngdppc
scatter lifeexp lngdppc, ///
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("ln(GDP per capita, thousand dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(0(1)5, grid) 
 graph export "$output/ch08_lifeexp_linreg2.png",replace


foreach x in 1 2 5 10 20 40 70 120 {
	dis `x' "   "  ln(`x')
}
 
scatter lifeexp lngdppc, ///
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("GDP per capita, thousand dollars, ln scale") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) ///
 xlab(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.7 "40" 4.2 "70" 4.8 "120", grid)   
 graph export "$output/ch08_lifeexp_linreg2a.png",replace

 
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
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp gdptot, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("Total GDP (billion dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(0(4000)24000, grid) 
 graph export "$output/ch08_lifeexp_linreg3.png",replace
 
reg lifeexp lngdptot
 scatter lifeexp lngdptot, ///
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp lngdptot, lw(thick) lc(green) legend(off) ///
 ytitle("Life expectancy (years)") xtitle("ln(total GDP, billion dollars)") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(50(5)85, grid) xlab(-2(2)10, grid) 
 graph export "$output/ch08_lifeexp_linreg4.png",replace

lis countryname population gdppc gdptotal if gdptotal>5000
 
******************************************************************************
*PIECEWISE LINEAR REGRESSION

mkspline lngdppc_low 4 lngdppc_high = lngdppc
regress lifeexp lngdppc_*
cap drop pred_spline
predict pred_spline

scatter lifeexp lngdppc, ///
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) xline(4) ///
 || line pred_spline lngdppc, sort lw(thick) lc(green) legend(off) ///
 xtitle("ln(GDP per capita, thousand dollars)") ///
 ytitle("Life expectancy (years)") ylabel(50(5)85, grid) xlab(0(1)5) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch08_lifeexp_spline.png",replace

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
 ms(o) msize(small) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || line pred_quad lngdppc, sort lw(thick) lc(green) legend(off) ///
 xtitle("ln(GDP per capita, thousand dollars)") ///
 ytitle("Life expectancy (years)") ylabel(50(5)85, grid) xlab(0(1)5) ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/ch08_lifeexp_quad.png",replace

regress lifeexp lngdppc_sq
predict resid_quad, resid
lis countryname population lifeexp resid_quad if resid_quad<-12
lis countryname population lifeexp resid_quad if resid_quad>7.5
 
******************************************************************************
*WEIGHTED AND UNWEIGHTED
reg lifeexp lngdppc
scatter lifeexp lngdppc, ///
 ms(Oh) msize(medium) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp lngdppc, lw(thick) lc(green) legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
  ytitle("Life expectancy (years)") ylabel(50(5)85, grid) xlab(0(1)5) ///
  xtitle("ln(GDP per capita, thousand dollars)")
 graph export "$output/ch08_lifeexp_unweighted.png",replace
 
 
reg lifeexp lngdppc [w=pop]
scatter lifeexp lngdppc [w=pop], ///
 ms(Oh) msize(medium) mcolor(%50)  mlw(thick) mcolor(navy) ///
 || lfit lifeexp lngdppc [w=pop], lw(thick) lc(green) legend(off) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ytitle("Life expectancy (years)") ylabel(50(5)85, grid) xlab(0(1)5) ///
 xtitle("ln(GDP per capita, thousand dollars)")
 graph export "$output/ch08_lifeexp_weighted.png",replace

 
