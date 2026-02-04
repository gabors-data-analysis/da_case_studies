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
* CH08B How is life expectancy related to the average income of a country?
* using the worldbank-lifeexpectancy dataset
* version 1.1 2025-12-09
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
global data_in  "${data_dir}/worldbank-lifeexpectancy/clean"
global work     "ch08-life-expectancy-income"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* LOAD DATA
********************************************************************

* Option 1: Load from local repository
use "${data_in}/worldbank-lifeexpectancy.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile wb_data
copy "https://osf.io/download/vxc67/" `wb_data'
use `wb_data', clear
*/


********************************************************************
* SAMPLE SELECTION
********************************************************************

* Keep only year 2017
keep if year == 2017
count
display as text "Observations in 2017: " as result r(N)

* Check data availability
count if gdppc != . & lifeexp != .
summarize population if gdppc != . & lifeexp != .

count if gdppc == . | lifeexp == .
summarize population if gdppc == . | lifeexp == .

* List countries with missing data and large populations
list countryname population gdppc lifeexp ///
    if (gdppc == . | lifeexp == .) & population > 1

* Keep only complete cases
keep if gdppc != . & lifeexp != .
compress
sort countryname


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* GDP total (billion USD, PPP constant 2011 prices)
generate gdptotal = gdppc * population
label variable gdptotal "GDP total, billion USD (PPP constant 2011 prices)"

* Log transformations
generate lngdptot = ln(gdptotal)
label variable lngdptot "ln GDP total"

generate lngdppc = ln(gdppc)
label variable lngdppc "ln GDP per capita"

* Summary statistics
tabstat lifeexp gdppc gdptotal lngdptot lngdppc, ///
    statistics(mean sd min median max n) columns(statistics)

* Identify extreme values
list countryname population lifeexp if lifeexp < 55
list countryname population lifeexp if lifeexp > 83


********************************************************************
* FIGURE 3: DISTRIBUTION OF PER CAPITA GDP
********************************************************************

* Figure 3a - GDP per capita histogram (level)
histogram gdppc, percent start(0) width(3) ///
    fcolor(navy*0.8) lcolor(white) ///
    xlabel(0(15)120, grid) ylabel(0(5)20, grid) ///
    xtitle("GDP per capita (thousand US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch08-figure-3a-gdppercap-hist-Stata.png", replace

* Check extreme values
list countryname population gdppc if gdppc > 80
tabstat gdppc, statistics(mean median sd n)


* Figure 3b - ln(GDP per capita) histogram
histogram lngdppc, percent width(0.2) ///
    fcolor(navy*0.8) lcolor(white) ///
    xlabel(, grid) ylabel(0(2)10, grid) ///
    xtitle("ln(GDP per capita, thousand US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch08-figure-3b-lngdppercap-hist-Stata.png", replace

* Check low-income countries
list countryname population gdppc lngdppc if gdppc < 1
tabstat lngdppc, statistics(mean median sd n)


********************************************************************
* FIGURE 4 & 5: LIFE EXPECTANCY VS GDP PER CAPITA
********************************************************************

* Figure 4 - Level-level regression
regress lifeexp gdppc
scatter lifeexp gdppc, ///
    msize(medium) mcolor(navy*0.6) ///
    || lfit lifeexp gdppc, lwidth(thick) lcolor(green*0.8) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("GDP per capita (thousand US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)100, grid) xlabel(0(10)100, grid)
graph export "${output}/ch08-figure-4-linreg-levlev-Stata.png", replace


* Figure 5a - Level-log regression (log scale on x-axis)
regress lifeexp lngdppc
scatter lifeexp lngdppc, ///
    msize(medium) mcolor(navy*0.6) ///
    || lfit lifeexp lngdppc, lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("ln(GDP per capita, thousand US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)85, grid) xlabel(0(1)5, grid)
graph export "${output}/ch08-figure-5a-linreg-levlog-Stata.png", replace


* Display ln transformations for reference
display as text _newline "Log transformation reference:"
foreach x in 1 2 5 10 20 50 100 {
	display as text %3.0f `x' "   " as result %5.2f ln(`x')
}


* Figure 5b - Level-log regression with custom x-axis labels (dollar scale)
scatter lifeexp lngdppc, ///
    msize(medium) mcolor(navy*0.8) ///
    || lfit lifeexp lngdppc, lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("GDP per capita, thousand US dollars, ln scale") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)85, grid) ///
    xlabel(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid)
graph export "${output}/ch08-figure-5b-linreg-levlog-logscale-Stata.png", replace


* Examine residuals from level-log model
regress lifeexp lngdppc
capture drop resid
predict resid, residual

* Identify outliers
list countryname population lifeexp resid if resid < -12
list countryname population lifeexp resid if resid > 7
list countryname population lifeexp resid if countrycode == "USA"
list countryname population lifeexp resid if countrycode == "JPN"
list countryname population lifeexp resid if lngdppc > 4.5
list countryname population lifeexp gdppc resid if gdppc > 65


********************************************************************
* FIGURE 6: LIFE EXPECTANCY VS TOTAL GDP
********************************************************************

* Figure 6a - Level-level regression with total GDP
regress lifeexp gdptotal
scatter lifeexp gdptotal, ///
    msize(medium) mcolor(navy*0.6) ///
    || lfit lifeexp gdptotal, lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("Total GDP (billion US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)85, grid) xlabel(0(4000)24000, grid)
graph export "${output}/ch08-figure-6a-linreg-totalGDP-Stata.png", replace


* Figure 6b - Level-log regression with total GDP
regress lifeexp lngdptot
scatter lifeexp lngdptot, ///
    msize(medium) mcolor(navy*0.6) ///
    || lfit lifeexp lngdptot, lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("ln(total GDP, billion US dollars)") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)85, grid) xlabel(-2(2)10, grid)
graph export "${output}/ch08-figure-6b-linreg-lntotalGDP-Stata.png", replace

* Identify large economies
list countryname population gdppc gdptotal if gdptotal > 5000


********************************************************************
* FIGURE 7: PIECEWISE LINEAR REGRESSION (SPLINE)
********************************************************************

* Create piecewise linear spline at $50k (ln(50) = 3.9)
mkspline lngdppc_low 3.9 lngdppc_high = lngdppc

* Regression with spline
regress lifeexp lngdppc_*
capture drop pred_spline
predict pred_spline

* Figure 7a - Spline regression
scatter lifeexp lngdppc, ///
    msize(medium) mcolor(navy*0.6) ///
    || line pred_spline lngdppc, sort lwidth(thick) lcolor(green) ///
    legend(off) ///
    xtitle("ln(GDP per capita, thousand US dollars)") ///
    ytitle("Life expectancy (years)") ///
    ylabel(50(5)85, grid) ///
    xlabel(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
    xline(3.9, lcolor(black) lpattern(dash)) ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch08-figure-7a-spline-Stata.png", replace

* Examine spline residuals
predict resid_spline, residual
list countryname population lifeexp resid_spline if resid_spline < -12
list countryname population lifeexp resid_spline if resid_spline > 6.5


********************************************************************
* FIGURE 7B: QUADRATIC REGRESSION
********************************************************************

* Create quadratic term
capture generate lngdppc_sq = lngdppc^2

* Quadratic regression
regress lifeexp lngdppc lngdppc_sq
capture drop pred_quad
predict pred_quad

* Figure 7b - Quadratic fit
scatter lifeexp lngdppc, ///
    msize(medium) mcolor(navy*0.6) ///
    || line pred_quad lngdppc, sort lwidth(thick) lcolor(green) ///
    legend(off) ///
    xtitle("ln(GDP per capita, thousand dollars)") ///
    ytitle("Life expectancy (years)") ///
    ylabel(50(5)85, grid) ///
    xlabel(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch08-figure-7b-quad-Stata.png", replace

* Examine quadratic residuals
regress lifeexp lngdppc_sq
predict resid_quad, residual
list countryname population lifeexp resid_quad if resid_quad < -12
list countryname population lifeexp resid_quad if resid_quad > 7.5


********************************************************************
* FIGURE 9: WEIGHTED VS UNWEIGHTED REGRESSION
********************************************************************

* Figure 9a - Unweighted regression
scatter lifeexp lngdppc, ///
    msize(medium) mcolor(navy*0.8) ///
    || lfit lifeexp lngdppc, lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("GDP per capita, thousand US dollars, ln scale") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white)) ///
    ylabel(50(5)85, grid) ///
    xlabel(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid)
graph export "${output}/ch08-figure-9a-linreg-unwgt-logscale-Stata.png", replace


* Figure 9b - Weighted regression (by population)
regress lifeexp lngdppc [weight=population]
scatter lifeexp lngdppc [weight=population], ///
    msize(medium) mcolor(navy*0.6) ///
    || lfit lifeexp lngdppc [weight=population], lwidth(thick) lcolor(green) ///
    legend(off) ///
    ytitle("Life expectancy (years)") ///
    xtitle("GDP per capita, thousand US dollars, ln scale") ///
    ylabel(50(5)85, grid) ///
    xlabel(0 "1" 0.7 "2" 1.6 "5" 2.3 "10" 3 "20" 3.9 "50" 4.6 "100", grid) ///
    text(67 2.0 "India") text(77 2.6 "China") text(79 4.1 "USA") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch08-figure-9b-linreg-wgt-logscale-Stata.png", replace


