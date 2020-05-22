
***************************************************************

* Ch 23
* Case Study: Import demand and industrial production
* Data: asia industry


***************************************************************


* v.1.0. 2019-08
* v.1.1. 2019-11-07 major changes, new country focus
* v.1.2. 2019-12-16 minor changes, graphs adjusted

* WHAT THIS CODES DOES:
* time series graphs
* panel regressions




********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cap cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cap cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cap cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"
 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
global data_in 		"da_data_repo\asia-industry\clean"
global data_out 	"da_case_studies\ch23-import-demand-and-production"
global output 		"da_case_studies\ch23-import-demand-and-production\output"


use "$data_in\asia-industry_tidy.dta", clear

keep if year>=1998
drop if year==1998 & month==1 /* missin data */
drop if year==2018 & month>4 /* missin data */

tab countrycode, sum(ind_prod_const)

* feature engineering

gen temp= ind_prod_const_sa if countrycode =="USA"
bys time: egen usa_ip_sa=max(temp)
drop temp
gen temp= ind_prod_const_sa if countrycode =="CHN"
bys time: egen chn_ip_sa=max(temp)
drop temp


gen ln_ip=ln( ind_prod_const_sa )
gen ln_usa_ip=ln( usa_ip_sa)
gen ln_chn_ip=ln( chn_ip_sa)
gen ln_usa_imports=ln(usa_imp_sa )
gen ln_er_usd=ln(exchnage_rate_vs_usd )
keep if ln_ip!=.

* keep countries of choice
keep if inlist(countrycode, "MYS", "PHL", "SGP", "THA")
tab countrycode

* panel setup
encode country , gen(cc)
sort cc time
tsset cc time

*panel
xtset cc time

gen dln_ip = d.ln_ip
gen dln_usa_ip = d.ln_usa_ip
gen dln_chn_ip = d.ln_chn_ip
gen dln_usa_imports = d.ln_usa_imports
gen dln_er_usd=d.ln_er_usd


save "$data_out\work.dta", replace


*********************************************************
* DESCRIBE
*********************************************************
use "$data_out\work.dta", replace
gen ym = ym(year,month)
 format ym %tm

tab year if countryc=="THA" 
 
 sum dln_ip if countrycode=="THA"
 sum dln_usa_imp if countrycode=="THA"

  
 * create TS graphs
 * TODO: add annual grid lines. 
line ln_ip ym if countrycode=="THA", lw(medium) lc(navy) ///
 ylab(, grid) ytitle("Ln industrial production, Thailand") ///
 xlab(, grid) xtitle("") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1 )  ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(22.7 2008m12 " 2008-09 crisis" 22.7 2011m12 "2011 Dec flood", orient(vert)) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\lnip_THA.png",replace
 
 line dln_ip ym if countrycode=="THA", lw(medium) lc(navy) ///
 ylab(, grid) ytitle("Chance in ln industrial production, Thailand") ///
 xlab(, grid) xtitle("") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1 ) ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(-0.2 2008m12 " 2008-09 crisis" -0.2 2011m5 "2011 Dec flood", orient(vert)) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\dlnip_THA.png",replace

 line ln_usa_imp ym  if countrycode=="THA", lw(medium)  lc(navy) ///
 ylab(, grid) ytitle("Ln industrial production, Thailand") ///
 xlab(, grid) xtitle("") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1 ) ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(11.5 2008m12 " 2008-09 crisis" , orient(vert)) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\lnusaimp.png",replace

 line dln_usa_imp ym  if countrycode=="THA", lw(medium) lc(navy) ///
 ylab(, grid) ytitle("Change in ln industrial production, Thailand") ///
 xlab(, grid) xtitle("") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1 ) ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(-0.1 2008m5 " 2008-09 crisis" , orient(vert)) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
 graph export "$output\dlnusaimp.png",replace
 


*********************************************************
* REGRESSIONS
*********************************************************
use "$data_out\work.dta", replace
gen ym = ym(year,month)
 format ym %tm


* Thailand
reg dln_ip l(0/4)dln_usa_imports  l(1/2)dln_ip if countrycode=="THA" 
 outreg2 using "$output/asia-ip-imports-reg1", dec(2) ctitle(Thailand) 2aster tex(fragment) nonotes replace
reg dln_ip l(0/4)dln_usa_imports  l(1/2)dln_ip if countrycode=="THA" 
 outreg2 using "$output/asia-ip-imports-reg1", dec(2) ctitle(Thailand) 2aster tex(fragment) nonotes append


* long-term coeff, Thailand
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="THA" 
 outreg2 using "$output/asia-ip-imports-reg1", dec(2) ctitle(Thailand) 2aster tex(fragment) nonotes append
* same with newey-west se
newey dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="THA" , lag(4)

* long-term coeff, lagged dy, countries separately
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="THA" 
 outreg2 using "$output/asia-ip-imports-reg2", dec(3) ctitle(Thailand) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) replace
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="MYS" 
 outreg2 using "$output/asia-ip-imports-reg2", dec(3) ctitle(Malaysia) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="PHL" 
 outreg2 using "$output/asia-ip-imports-reg2", dec(3) ctitle(Philippines) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="SGP" 
 outreg2 using "$output/asia-ip-imports-reg2", dec(3) ctitle(Singapore) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append


* long-term coeff, lagged dy, countries pooled
gen Malaysia = cc==1
gen Philippines = cc==2
gen Singapore = cc==3
global countries Malaysia Philippines Singapore

reg d.ln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  $countries 
 outreg2 using "$output/asia-ip-imports-reg2", dec(3) ctitle(pooled) ///
 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip $countries ) append

 
 
