***************************************************************
* ch23 
*
* Case Study immunization against measels and child mortality age 0-5
* whole World

* Data: worldbank-immunizaton-panel


***************************************************************

* v 1.1. 2019-11-19
* v 1.2. 2019-12-10 corrected folders, deleted cleaning
* v.1.3. 2020-02-03 some edits
* v.1.4. 2020-04-16 just look at graphs



* WHAT THIS CODES DOES:
* looks at continents for aggregate trends
* country level models



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/

cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data



global data_in "da_data_repo\worldbank-immunization\clean"
global data_out "da_case_studies\ch23-immunization-life"
global output "da_case_studies\ch23-immunization-life\output"




**************************************************
* info graph on measles vaccination continent aggregates

use "$data_in/worldbank-immunization-continents.dta", clear


line imm* year, lw(. . . . . thick thick) ///
 lc(gs10 gs10 gs10 gs10 gs10 blue green) ///
 ylab(, grid) xlab(1998(5)2018, grid) legend(off) ytitle("Immunization rate (percent)")
* save in Stata graph format to edit: add legend next to lines 
graph save "$output\ch23-figure-2a-tsimmun-toedit",replace
* save in png format 
graph export "$output\ch23-figure-2a-tsimmun-toedit.png",replace

line surv* year, lw(. . . . . thick thick) ///
 lc(gs10 gs10 gs10 gs10 gs10 blue green) ///
 ylab(, grid) xlab(1998(5)2018, grid) legend(off) ytitle("Child survival rate (percent)")
* save in Stata graph format to edit: add legend next to lines 
graph save "$output\ch23-figure-2b-tssurvival-toedit",replace
* save in png format 
graph export "$output\ch23-figure-2b-tssurvival-toedit.png",replace
*/

**************************************************
* regressions on countries

clear
use "$data_in/worldbank-immunization-panel.dta"

xtdes

* keep balanced panel
egen temp1=count(imm), by(c)
egen temp2=count(gdppc), by(c)

tab countryname if temp1<20, sum(temp1)
tab countryname if temp2<20, sum(temp2)

drop if temp1<20
drop if temp2<20
xtdes

gen lnpop=ln(pop)
gen d_surv = d.surv
gen d_imm = d.imm
gen d_lngdppc=d.lngdppc
gen d_lnpop = d.lnpop

lab var imm "Immunization rate"
lab var lngdppc "ln GDP per capita"
lab var lnpop "ln population"
lab var surv "Survival rate"

/*************************
* DESCRIBE
sum d_surv,d
sum d_imm, d
tab countrycode if d_surv<-1 | d_surv>1 & year>1998
tab countrycode if d_imm<-20 | d_imm>20 & year>1998

graph box d_surv 
hist d_surv, col(blue) percent ylab(, grid)
hist d_surv if d_surv>-2 & d_surv<2, col(blue) percent ylab(, grid)

graph box d_imm 
hist d_imm, col(blue) percent ylab(, grid)

sum surv imm pop lngdppc 
*/

*****************************************************
* FE REGRESSSIONS
egen avgpop=mean(pop), by(c) /* for weights in xtreg fe */

*areg surv imm i.year [w=pop] , absorb(c) cluster(c)
xtreg surv imm i.year [w=avgpop] , fe cluster(c)
 outreg2 using "$output\ch23-table-2-immun-fe", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(imm ) replace
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) cluster(c)
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe cluster(c)
 outreg2 using "$output\ch23-table-2-immun-fe", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep( imm lngdppc lnpop ) append


*************************
** CLUSTER SE VS BIASED SE 
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) cluster(c)
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe cluster(c)
 outreg2 using "$output\ch23-table-3-immun-fese", se bdec(3) 2aster tex(fragment) nonotes ///
 label drop(i.year _cons) ctitle("Clustered SE") replace
*areg surv imm lngdppc lnpop i.year [w=pop], absorb(c) 
xtreg surv imm lngdppc lnpop i.year [w=avgpop], fe
 outreg2 using "$output\ch23-table-3-immun-fese", se bdec(3) 2aster tex(fragment) nonotes ///
 label drop( i.year _cons ) ctitle("Simple SE") append



*************************
* FD REGRESSIONS

local maxlag = 5
local maxlag_1 = `maxlag'-1

label var d_surv "$\Delta surv$"
label var d_imm "$\Delta imm$"

* basic FD
reg d_surv d_imm [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-4-immun-fd1", se bdec(3) 2aster label tex(fragment) nonotes replace

* FD, 5 lags
reg d_surv L(0/`maxlag').d_imm [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-4-immun-fd1", se bdec(3) 2aster label tex(fragment) nonotes append
more

* FD, 5 lags, cumul
gen d2_imm=d.d_imm
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-4-immun-fd1", se bdec(3) 2aster label tex(fragment) nonotes drop(L(0/`maxlag_1').d2_imm) append

* FD, 5 lags, cumul, lead
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm F(1/3).d_imm [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-4-immun-fd1", se bdec(3) 2aster tex(fragment) nonotes label drop(L(0/`maxlag_1').d2_imm) append
more

 
*************************
* AGGREG TREND, CONFOUNDERS, CTRY TRENDS
* FD, 5 lags, cumul, aggreg trend
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-5-immun-fd2", se bdec(3) 2aster ///
 label keep(L`maxlag'.d_imm )  tex(fragment) nonotes replace
more

* FD, 5 lags, cumul, aggreg trend, confounders 
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
  L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
 i.year [w=pop], cluster(c)
 outreg2 using "$output\ch23-table-5-immun-fd2", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(L`maxlag'.d_imm ) append
more
* check: cumulative coeffs on the confounders
test d_lngdppc + L.d_lngdppc + L2.d_lngdppc + L3.d_lngdppc + L4.d_lngdppc + L5.d_lngdppc =0
test d_lnpop + L.d_lnpop + L2.d_lnpop + L3.d_lnpop + L4.d_lnpop + L5.d_lnpop =0
more
* check: it's not the number of obsrevations
reg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm i.year if d_lngdp!=. [w=pop], cluster(c)
more


* FD, 5 lags, cumul, aggreg trend, , country linear trend
areg d_surv L`maxlag'.d_imm L(0/`maxlag_1').d2_imm ///
  L(0/`maxlag').d_lngdppc L(0/`maxlag').d_lnpop ///
  i.year [w=pop], cluster(c) absorb(c)
 outreg2 using "$output\ch23-table-5-immun-fd2", se bdec(3) 2aster tex(fragment) nonotes ///
 label keep(L`maxlag'.d_imm ) append
more

