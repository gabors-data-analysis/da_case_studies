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
* CH23A Import demand and industrial production
* using the asia-industry dataset
* version 0.9 2020-09-13
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


global data_in  "$data_dir/asia-industry/clean"
global work  	"ch23-import-demand-and-production"

cap mkdir 		"$work/output"
global output 	"$work/output"




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


save "$work\work.dta", replace


*********************************************************
* DESCRIBE
*********************************************************
use "$work\work.dta", replace
gen ym = ym(year,month)
 format ym %tm

tab year if countryc=="THA" 
 
 sum dln_ip if countrycode=="THA"
 sum dln_usa_imp if countrycode=="THA"

* ts graphs
* Figure 23.1a  
line ln_ip ym if countrycode=="THA", lw(medium) lc(navy) ///
 ylab(, grid) ytitle("Ln industrial production, Thailand") ///
 xtitle("Date (month)") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1, grid)  ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(22.7 2008m12 " 2008-09 crisis" 22.7 2011m11 "2011 Dec flood", orient(vert)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output\ch23-figure-1a-lnipTHA-Stata.png",replace
 
line ln_usa_imp ym  if countrycode=="THA", lw(medium)  lc(navy) ///
 ylab(, grid) ytitle("Ln industrial production, Thailand") ///
 xlab(, grid) xtitle("") ///
 tlab(1998m1 2002m1 2006m1 2010m1 2014m1 2018m1 ) ///
 ttick(1999m1 2000m1 2001m1 2003m1 2004m1 2005m1 2007m1 2008m1 2009m1 2011m1 2012m1 2013m1 2015m1 2016m1 2017m1, tpos(in)) ///
 ttext(11.5 2008m12 " 2008-09 crisis" , orient(vert)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output\ch23-figure-1b-lnUSAimp-Stata.png",replace
 


*********************************************************
* REGRESSIONS
*********************************************************
use "$work\work.dta", replace
gen ym = ym(year,month)
 format ym %tm


* Thailand
reg dln_ip l(0/4)dln_usa_imports  l(1/2)dln_ip if countrycode=="THA" 
reg dln_ip l(0/4)dln_usa_imports  l(1/2)dln_ip if countrycode=="THA" 


* long-term coeff, Thailand
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="THA" 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(2) ctitle(Thailand) 2aster tex(fragment) nonotes append
* same with newey-west se
newey dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="THA" , lag(4)

* long-term coeff, lagged dy, countries separately
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1	/1)dln_ip  if countrycode=="THA" 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(3) ctitle(Thailand) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) replace
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="MYS" 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(3) ctitle(Malaysia) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="PHL" 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(3) ctitle(Philippines) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append
reg dln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  if countrycode=="SGP" 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(3) ctitle(Singapore) 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip ) append


* long-term coeff, lagged dy, countries pooled
gen Malaysia = cc==1
gen Philippines = cc==2
gen Singapore = cc==3
global countries Malaysia Philippines Singapore

reg d.ln_ip l(4/4)dln_usa_imports  l(0/3)d.dln_usa_imports  l(1/1)dln_ip  $countries 
 outreg2 using "$output/ch23-table-1-asia-ip-imports-reg-Stata", dec(3) ctitle(pooled) ///
 2aster tex(fragment) nonotes keep(l(4/4).dln_usa_imports l(1/1)dln_ip $countries ) append

 
 
