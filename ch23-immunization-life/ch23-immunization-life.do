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
* version 1.1 2026-09-04
*
* STATA VERSION: This code is written for Stata 18
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"


* STEP 2: * Directory for data
* Option 1: run directory-setting do file
capture do set-data-directory.do
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"


global data_in  "${data_dir}/worldbank-immunization/clean"
global work  	"ch23-immunization-life"

capture mkdir 		"${work}/output"
global output 	"${work}/output"






**************************************************
* info graph on measles vaccination continent aggregates

use "${data_in}/worldbank-immunization-continents.dta", clear
* Or download directly from OSF:
/*
copy "https://osf.io/download/d9k6v/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

line imm* year, lw(. . . . . thick thick) ///
 lc(gs10 gs10 gs10 gs10 gs10 navy*0.8 green*0.6) ///
 ylab(, grid) xlab(1998(5)2018, grid) ///
 legend(off) ytitle("Immunization rate (percent)") xtitle("Date (year)") ///
 text(72 2005 "South Asia") text(67 2013 "Sub-Saharan Africa") 
graph export "${output}/ch23-figure-2a-tsimmun-Stata.png",replace

* survival rates to percent from 1/1000
forvalues i=1/7 {
	replace surv`i' = surv`i'/10
}

line surv* year, lw(. . . . . thick thick) ///
 lc(gs10 gs10 gs10 gs10 gs10 navy*0.8 green*0.8) ///
 ylab(, grid) xlab(1998(5)2018, grid) legend(off) ///
 ytitle("Child survival rate (percent)") xtitle("Date (year)") ///
 text(93 2002 "South Asia") text(89 2013 "Sub-Saharan Africa") 
graph export "${output}/ch23-figure-2b-tssurvival-Stata.png",replace
*/

**************************************************
* regressions on countries

clear
use "${data_in}/worldbank-immunization-panel.dta"
* Or download directly from OSF:
/*
copy "https://osf.io/download/ku4fd/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

xtdes

* keep balanced panel
egen temp1=count(imm), by(c)
egen temp2=count(gdppc), by(c)

tabulate countryname if temp1<20, sum(temp1)
tabulate countryname if temp2<20, sum(temp2)

drop if temp1<20
drop if temp2<20
xtdes

generate lnpop=ln(pop)
generate d_surv = d.surv
generate d_imm = d.imm
generate d_lngdppc=d.lngdppc
generate d_lnpop = d.lnpop

label variable imm "Immunization rate"
label variable lngdppc "ln GDP per capita"
label variable lnpop "ln population"
label variable surv "Survival rate"

*************************
* DESCRIBE
summarize d_surv,d
summarize d_imm, d
tabulate countrycode if d_surv<-1 | d_surv>1 & year>1998
tabulate countrycode if d_imm<-20 | d_imm>20 & year>1998

* graphs not in the textbook
graph box d_surv 
graph box d_surv if d_surv>-1 & d_surv<1
histogram d_surv, fcol(navy*0.8) lcol(white) percent ylab(, grid)
histogram d_surv if d_surv>-1 & d_surv<1, fcol(navy*0.8) lcol(white) percent ylab(, grid)

graph box d_imm 
graph box d_imm if d_imm>-10 & d_imm<10
histogram d_imm, fcol(navy*0.8) lcol(white) percent ylab(, grid)
histogram d_imm if d_imm>-10 & d_imm<10, disc fcol(navy*0.8) lcol(white) percent ylab(, grid)

summarize surv imm pop lngdppc 
*/

*****************************************************
* FE REGRESSSIONS
egen avgpop=mean(pop), by(c) /* for weights in xtreg fe */

*areg surv imm i.year [w=pop] , absorb(c) cluster(c)
xtreg surv imm i.year [w=avgpop] , fe cluster(c)
 outreg2 using "${output}/ch23-table-2-immun-fe-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(imm ) replace
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) cluster(c)
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe cluster(c)
 outreg2 using "${output}/ch23-table-2-immun-fe-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep( imm lngdppc lnpop ) append

 * not in book - unweighted 
xtreg surv imm lngdppc lnpop i.year , fe cluster(c)

*************************
** CLUSTER SE VS BIASED SE 
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) cluster(c)
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe cluster(c)
 outreg2 using "${output}/ch23-table-3-immun-fese-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label drop(i.year _cons) ctitle("Clustered SE") replace
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) 
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe
 outreg2 using "${output}/ch23-table-3-immun-fese-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label drop( i.year _cons ) ctitle("Simple SE") append



*************************
* FD REGRESSIONS

local maxlag = 5
local maxlag_1 = `maxlag'-1

label variable d_surv "$\Delta surv$"
label variable d_imm "$\Delta imm$"

* basic FD
regress d_surv d_imm [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-4-immun-fd1-Stata", se bdec(3) 2aster label tex(fragment) nonotes replace

* FD, 5 lags
regress d_surv L(0/`maxlag').d_imm [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-4-immun-fd1-Stata", se bdec(3) 2aster label tex(fragment) nonotes append
more

* FD, 5 lags, cumul
generate d2_imm=d.d_imm
regress d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-4-immun-fd1-Stata", se bdec(3) 2aster label tex(fragment) nonotes drop(L(0/`maxlag_1').d2_imm) append

* FD, 5 lags, cumul, lead
regress d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm F(1/3).d_imm [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-4-immun-fd1-Stata", se bdec(3) 2aster tex(fragment) nonotes label drop(L(0/`maxlag_1').d2_imm) append
more

 
*************************
* AGGREG TREND, CONFOUNDERS, CTRY TRENDS
* FD, 5 lags, cumul, aggreg trend
regress d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-5-immun-fd2-Stata", se bdec(3) 2aster ///
 label keep(L`maxlag'.d_imm )  tex(fragment) nonotes replace
more

* FD, 5 lags, cumul, aggreg trend, confounders 
regress d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
  L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
 i.year [w=pop], cluster(c)
 outreg2 using "${output}/ch23-table-5-immun-fd2-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(L`maxlag'.d_imm ) append
more
* check: cumulative coeffs on the confounders
test d_lngdppc + L.d_lngdppc + L2.d_lngdppc + L3.d_lngdppc + L4.d_lngdppc + L5.d_lngdppc =0
test d_lnpop + L.d_lnpop + L2.d_lnpop + L3.d_lnpop + L4.d_lnpop + L5.d_lnpop =0
more
* check: it's not the number of obsrevations
regress d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year if d_lngdppc!=. [w=pop], cluster(c)
more


* FD, 5 lags, cumul, aggreg trend, , country linear trend
areg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
  L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
  i.year [w=pop], cluster(c) absorb(c)
 outreg2 using "${output}/ch23-table-5-immun-fd2-Stata", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(L`maxlag'.d_imm ) append
more

