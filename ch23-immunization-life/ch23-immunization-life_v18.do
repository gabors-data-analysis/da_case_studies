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
* Chapter 23
* CH23B Immunization against measles and saving children
* using the worldbank-immunization dataset
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
global data_in  "${data_dir}/worldbank-immunization/clean"
global work     "ch23-immunization-life"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* FIGURE 2: CONTINENT-LEVEL TIME SERIES
********************************************************************

* Load continent aggregates
use "${data_in}/worldbank-immunization-continents.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile continent_data
copy "https://osf.io/download/d9k6v/" `continent_data'
use `continent_data', clear
*/

* Figure 2a - Immunization rates over time
line imm* year, ///
    lwidth(. . . . . thick thick) ///
    lcolor(gs10 gs10 gs10 gs10 gs10 navy*0.8 green*0.6) ///
    ylabel(, grid) xlabel(1998(5)2018, grid) ///
    legend(off) ///
    ytitle("Immunization rate (percent)") ///
    xtitle("Date (year)") ///
    text(72 2005 "South Asia") ///
    text(67 2013 "Sub-Saharan Africa") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch23-figure-2a-tsimmun-Stata.png", replace

* Convert survival rates to percent (from per 1000)
forvalues i = 1/7 {
	replace surv`i' = surv`i' / 10
}

* Figure 2b - Child survival rates over time
line surv* year, ///
    lwidth(. . . . . thick thick) ///
    lcolor(gs10 gs10 gs10 gs10 gs10 navy*0.8 green*0.8) ///
    ylabel(, grid) xlabel(1998(5)2018, grid) ///
    legend(off) ///
    ytitle("Child survival rate (percent)") ///
    xtitle("Date (year)") ///
    text(93 2002 "South Asia") ///
    text(89 2013 "Sub-Saharan Africa") ///
    graphregion(fcolor(white) ifcolor(none)) ///
    plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch23-figure-2b-tssurvival-Stata.png", replace


********************************************************************
* COUNTRY-LEVEL PANEL ANALYSIS
********************************************************************

clear
use "${data_in}/worldbank-immunization-panel.dta"

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile panel_data
copy "https://osf.io/download/ku4fd/" `panel_data'
use `panel_data', clear
*/

* Check panel structure
xtdes

* Create balanced panel indicator
egen temp1 = count(imm), by(c)
egen temp2 = count(gdppc), by(c)

* Identify countries with incomplete data
table countryname if temp1 < 20, statistic(mean temp1)
* For Stata 15 and below, use: tab countryname if temp1<20, sum(temp1)

table countryname if temp2 < 20, statistic(mean temp2)
* For Stata 15 and below, use: tab countryname if temp2<20, sum(temp2)

* Keep only balanced panel
drop if temp1 < 20
drop if temp2 < 20

* Verify balanced panel
xtdes


********************************************************************
* FEATURE ENGINEERING
********************************************************************

* Log population
gen lnpop = ln(pop)

* First differences
gen d_surv = d.surv
gen d_imm = d.imm
gen d_lngdppc = d.lngdppc
gen d_lnpop = d.lnpop

* Variable labels
label var imm "Immunization rate"
label var lngdppc "ln GDP per capita"
label var lnpop "ln population"
label var surv "Survival rate"


********************************************************************
* DESCRIPTIVE STATISTICS
********************************************************************

* Summary of first differences
summarize d_surv, detail
summarize d_imm, detail

* Identify extreme changes
table countrycode if (d_surv < -1 | d_surv > 1) & year > 1998, ///
    statistic(frequency) nototals
* For Stata 15 and below, use: tab countrycode if d_surv<-1 | d_surv>1 & year>1998

table countrycode if (d_imm < -20 | d_imm > 20) & year > 1998, ///
    statistic(frequency) nototals
* For Stata 15 and below, use: tab countrycode if d_imm<-20 | d_imm>20 & year>1998

* Summary statistics for levels
summarize surv imm pop lngdppc


********************************************************************
* FIXED EFFECTS REGRESSIONS
********************************************************************

* Create average population for weights
egen avgpop = mean(pop), by(c)

* Table 2, Column 1: FE with immunization only
xtreg surv imm i.year [weight=avgpop], fe cluster(c)
outreg2 using "${output}/ch23-table-2-immun-fe-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label keep(imm) replace

* Table 2, Column 2: FE with controls
xtreg surv imm lngdppc lnpop i.year [weight=avgpop], fe cluster(c)
outreg2 using "${output}/ch23-table-2-immun-fe-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label keep(imm lngdppc lnpop) append


********************************************************************
* TABLE 3: CLUSTERED VS SIMPLE STANDARD ERRORS
********************************************************************

* Clustered SE (correct)
xtreg surv imm lngdppc lnpop i.year [weight=avgpop], fe cluster(c)
outreg2 using "${output}/ch23-table-3-immun-fese-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label drop(i.year _cons) ctitle("Clustered SE") replace

* Simple SE (biased - for comparison)
xtreg surv imm lngdppc lnpop i.year [weight=avgpop], fe
outreg2 using "${output}/ch23-table-3-immun-fese-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label drop(i.year _cons) ctitle("Simple SE") append


********************************************************************
* FIRST DIFFERENCE REGRESSIONS
********************************************************************

* Set lag length
local maxlag = 5
local maxlag_1 = `maxlag' - 1

* Labels for FD variables
label var d_surv "$\Delta surv$"
label var d_imm "$\Delta imm$"

* Table 4, Column 1: Basic FD
reg d_surv d_imm [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-4-immun-fd1-Stata.tex", ///
    se bdec(3) 2aster label tex(fragment) nonotes replace

* Table 4, Column 2: FD with 5 lags
reg d_surv L(0/`maxlag').d_imm [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-4-immun-fd1-Stata.tex", ///
    se bdec(3) 2aster label tex(fragment) nonotes append

* Table 4, Column 3: FD, 5 lags, cumulative
gen d2_imm = d.d_imm
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-4-immun-fd1-Stata.tex", ///
    se bdec(3) 2aster label tex(fragment) nonotes ///
    drop(L(0/`maxlag_1').d2_imm) append

* Table 4, Column 4: FD, 5 lags, cumulative, with leads
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm F(1/3).d_imm ///
    [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-4-immun-fd1-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes label ///
    drop(L(0/`maxlag_1').d2_imm) append


********************************************************************
* TABLE 5: FD WITH TRENDS AND CONFOUNDERS
********************************************************************

* Column 1: FD, 5 lags, cumulative, aggregate trend
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year ///
    [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-5-immun-fd2-Stata.tex", ///
    se bdec(3) 2aster ///
    label keep(L`maxlag'.d_imm) tex(fragment) nonotes replace

* Column 2: FD, 5 lags, cumulative, aggregate trend, confounders
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
    L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
    i.year [weight=pop], cluster(c)
outreg2 using "${output}/ch23-table-5-immun-fd2-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label keep(L`maxlag'.d_imm) append

* Test cumulative coefficients on confounders
test d_lngdppc + L.d_lngdppc + L2.d_lngdppc + ///
     L3.d_lngdppc + L4.d_lngdppc + L5.d_lngdppc = 0
     
test d_lnpop + L.d_lnpop + L2.d_lnpop + ///
     L3.d_lnpop + L4.d_lnpop + L5.d_lnpop = 0

* Check robustness: same sample as column 2
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year ///
    if d_lngdppc != . [weight=pop], cluster(c)

* Column 3: FD, 5 lags, cumulative, aggregate trend, country trends
areg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
    L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
    i.year [weight=pop], cluster(c) absorb(c)
outreg2 using "${output}/ch23-table-5-immun-fd2-Stata.tex", ///
    se bdec(3) 2aster tex(fragment) nonotes ///
    label keep(L`maxlag'.d_imm) append


********************************************************************
* END OF SCRIPT
********************************************************************

display as text _newline(2) "Analysis complete!"
display as text "Output saved to: ${output}"
display as text "Stata version: " as result c(stata_version)
display as text "Date: " as result c(current_date)
