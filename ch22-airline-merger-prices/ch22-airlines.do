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
* Chapter 22
* CH22A How does a merger between airlines affect prices?
* using the airline-tickets-usa dataset
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


global data_in  "${data_dir}/airline-tickets-usa/clean"
global work  	"ch22-airline-merger-prices"

capture mkdir 		"${work}/output"
global output 	"${work}/output"




* CREATE Workfile : only before and after period

use "${data_in}/originfinal-panel",replace

* Or download directly from OSF:
/*
copy "https://osf.io/download/zw2h9/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

***************************************************************************
*   market = origin X final destination 
*  (note final destination is:
*	airport at end of one-way routes if 4 or fewer
*	airport in middle of return routes if there is middle & 9 or fewer



* before = 2011 (all year)
* after  = 2016 (all year)

generate after = year==2016
generate before = year==2011

* workfile 1: drop all other years
keep if year==2011 | year==2016

* create total number of passengers from shares 
* so we can get aggreagate shares
generate ptotalAA = shareAA*passengers 
generate ptotalUS = shareUS*passengers 
generate ptotallargest = sharelargest*passengers 

collapse (first) after before airports return_sym stops (sum) ptotal* passengers itinfare , by(origin finaldest return year)

generate avgprice = itinfare/passengers
generate shareAA = ptotalAA/passengers
generate shareUS = ptotalUS/passengers
generate sharelargest = ptotallargest/passengers

generate AA = shareAA>0 /* share variables never missing */
generate US = shareUS>0 
generate AA_and_US = shareAA>0 & shareUS>0
generate AA_or_US = shareAA>0 | shareUS>0


* create numeric ID for market
sort origin finaldest
egen market = group(origin finaldest return)
order market
sort market year
label variable market "Market ID"

* tell Stata it's xt data with time difference of 5 yearas
local d = 2016-2011
xtset market year, delta(`d')
xtdes

* passengers before and after
generate pass_bef = passengers if before
 replace pass_bef = L.pass_bef if after
generate pass_aft = passengers if after
 replace pass_aft = F.pass_aft if before


* balanced vs unbalanced part of panel
sort market year
egen balanced = count(avgprice), by(market)
 recode balanced 1=0 2=1
 label variable balanced "Balanced panel: market observed both before & after"

tabstat passengers, by(balanced) s(sum n) format(%12.0fc)

* Define treated and untreated markets
*  treated: both AA and US present in the before period
*  untreated: neither AA nor US present in the before periodOR only AA or only US in before period

* NOTE There is a error in the book on p628:
* Original version with error:
* This definition of treated and untreated markets left some markets neither treated 
*  nor untreated: those with only American or only US Airways present in 2011. For the
*  main analysis we **dropped these from the data**.
*
* Corrected:
* This definition of treated and untreated markets left some markets neither treated 
*  nor untreated: those with only American or only US Airways present in 2011. For the 
*  main analysis we **kept these in the data as untreated**.

sort market year 
generate treated = L.AA_and_US
 replace treated  = F.treated if treated==.
generate untreated = 1 - L.AA_or_US
 replace untreated  = F.untreated if untreated==.

generate smallmkt = passengers<5000 if before
 replace smallmkt = L.smallmkt if smallmkt==.

label variable after ""
label variable before ""
label variable treated "Treated market; both AA and US peresent before"
label variable untreated "Untreated market; neither AA nor US peresent before"
label variable smallmkt "Small market (<5000 passengers in 2011)"
label variable airports "# airports in route"
label variable return "Return route"
label variable return_sym "Symmetric return route"
label variable stops "# stops"
label variable ptotalAA "Total # passengers by AA"
label variable ptotalUS "Total # passengers by US"
label variable ptotallargest "Total # passengers by largest carrier"
label variable passengers "Total # passeners"
label variable pass_bef "Total # passeners in before"
label variable pass_aft "Total # passeners in after"
label variable itinfare "Total sum of airfare"
label variable avgprice "Average airfare"
label variable shareAA "Market share of AA"
label variable shareUS "Market share of US"
label variable sharelargest "Market share of largest carrier"
label variable AA "AA is on market"
label variable US "US is on market"
label variable AA_and_US "AA and US both on market"
label variable AA_or_US "AA or US on market"

order market year balanced origin-return return_sym airports stops ///
 before after treated untreated smallmkt pass* itinfare avgprice

compress
label data "Airline diff-in-diffs workfile, T=2, before=2011 after=2016, market=origin + final destination"
save "${work}/ch22-airline-workfile.dta" ,replace



**************************************************************************
* DESCRIBE
* and create d_ln(y)

use "${work}/ch22-airline-workfile.dta" ,replace

* describe yearly data
tabstat passengers, by(year) s(p50 p75 p90 mean sum n) format(%12.0fc)
list market origin finaldest return year passengers if origin=="JFK" & finaldest=="LAX"

tabstat passengers if year==2011, by(smallmkt) s(min max median mean sum n) format(%12.0fc)

* describe balanced
tabulate year balanced
tabulate year balanced, sum(passengers) mean

**** SAMPLE DESIGN 
* keep if balanced
keep if balanced==1

* describe treatment
tabstat passengers if   treated==1 , by(year) s(mean sum n) format(%12.0fc)
tabstat passengers if untreated==1 , by(year) s(mean sum n) format(%12.0fc)
tabstat passengers if treated==0 & untreated==0 , by(year) s(mean sum n) format(%12.0fc)

* describe outcome
* graph not in textbook
histogram avgprice if before, percent col(navy*0.8) lcol(white) ylab(,grid) xlab(,grid)
tabstat avgprice if before, s(min p25 med p75 max mean sd) format(%4.0f)

* investigate if price is zero (can't take log)
tabstat passengers if avgprice==0, s(mean sum n) by(year)

capture gen lnavgp=ln(avgprice)
capture gen d_lnavgp = d.lnavgp

**** SAMPLE DESIGN 
* keep if nonzero price in both years
sort market year
 generate p0 = avgprice==0
 egen p0sum = sum(p0), by(market)
 tabulate p0sum,mis
keep if p0sum==0
tabulate year


**************************************************************************
* ANALYSIS
**************************************************************************

**************************************************************************
* Basic diff-in-diffs regtrssion
*  weighted by # passengers on market, in before period

* variable label to get nice LaTex output table
label variable treated "$ AAUS_{before} $"

* Table 22.2
regress d_lnavgp treated [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab2-airlines-Stata", dec(2) ctitle(All markets) 2aster tex(frag) lab nonotes replace
regress d_lnavgp treated if smallmkt==1 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab2-airlines-Stata", dec(2) ctitle(Small markets) 2aster tex(frag) lab nonotes append
regress d_lnavgp treated if smallmkt==0 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab2-airlines-Stata", dec(2) ctitle(Large markets) 2aster tex(frag) lab nonotes append

* Table 22.3
* Corresponding diff-in-diffs table
tabulate after treated [w=pass_bef], sum(lnavgp) mean noobs


*****************************************************
* Examining pre-treatment trends in ln average price (log of the passenger-weighted mean price, not the mean of log prices)

* use workfile to identify treated and untreated markets
use "${work}/ch22-airline-workfile.dta" ,replace
keep if balanced==1
sort market year
drop if market==L.market 
keep origin finaldest return treated smallmkt
save "${work}/ch22-airline-trends",replace



* use year-quarter panel data 
*  and merge to it treated-untreated 
*	(keep matched ones; no unmatched from "using")

use "${data_in}/originfinal-panel",replace
* Or download directly from OSF:
/*
copy "https://osf.io/download/zw2h9/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 
merge m:1 origin finaldest return using "${work}/ch22-airline-trends", keep(3) nogen

generate yq=yq(year,quarter)
format yq %tq

save "${work}/ch22-airline-trends",replace


* aggregate data to create average price by treated-untreated and year-quarter
* and draw time series graphs of log avg price
* all markets
use "${work}/ch22-airline-trends",replace

collapse (mean) avgprice year quarter [w=passengers], by(treated yq)

generate lnavgprice = ln(avgprice)
reshape wide avgprice lnavgprice, i(yq) j(treated)
tsset yq
label variable lnavgprice0 "Untreated markets"
label variable lnavgprice1 "Treated markets"

* Figure 22.2
tsline lnavgprice1 lnavgprice0 ///
 , lw(vthick vthick) lc(green*0.8 navy*0.8) lp(solid solid) ///
   ylab(5.0(0.1)5.6,grid) tlab(2010q1 (4) 2016q1) ///
   tline(2012q1 2015q3, lp(dash)) ///
   legend(off) ///
   ttitle("Date (quarters)") ytitle("ln(average price, US dollars)") ///
   ttext(5.15 2013q1 "Treated markets") ttext(5.44 2013q1 "Untreated markets") ///
   ttext(5.57 2011q1 "Merger annuounced") ttext(5.57 2014q3 "Merger completed") 
graph export "${output}/ch22-figure-2-pretrends-all-Stata.png",replace


* small markets
use "${work}/ch22-airline-trends",replace
keep if smallmkt==1

collapse (mean) avgprice [w=passengers], by(treated yq)

generate lnavgprice = ln(avgprice)
reshape wide avgprice lnavgprice, i(yq) j(treated)
tsset yq
label variable lnavgprice0 "Untreated markets"
label variable lnavgprice1 "Treated markets"

* Figure 22.3a
tsline lnavgprice1 lnavgprice0 ///
 , lw(vthick vthick) lc(green*0.8 navy*0.8) lp(solid solid) ///
   ylab(5.3(0.1)5.6,grid) tlab(2010q1 (4) 2016q1) ///
   tline(2012q1 2015q3, lp(dash)) ///
   legend(off) ///
   ttitle("Date (quarters)") ytitle("ln(average price, US dollars)") ///
   ttext(5.4 2013q1 "Treated markets") ttext(5.58  2013q1 "Untreated markets") ///
   ttext(5.3 2011q1 "Merger annuounced") ttext(5.3 2014q3 "Merger completed") 
graph export "${output}/ch22-figure-3a-pretrends-small-Stata.png",replace

 
* large markets
use "${work}/ch22-airline-trends",replace
keep if smallmkt==0

collapse (mean) avgprice [w=passengers], by(treated yq)

generate lnavgprice = ln(avgprice)
reshape wide avgprice lnavgprice, i(yq) j(treated)
tsset yq
label variable lnavgprice0 "Untreated markets"
label variable lnavgprice1 "Treated markets"

* Figure 22.3p
tsline lnavgprice1 lnavgprice0 ///
 , lw(vthick vthick) lc(green*0.8 navy*0.8) lp(solid solid) ///
   ylab(3.75(0.25)5.0,grid) tlab(2010q1 (4) 2016q1) ///
   tline(2012q1 2015q3, lp(dash)) ///
   legend(off) ///
   ttitle("Date (quarters)") ytitle("ln(average price, US dollars)") ///
   ttext(4.92 2013q1 "Treated markets") ttext(4.0  2013q1 "Untreated markets") ///
   ttext(4.5 2011q1 "Merger annuounced") ttext(4.5 2014q3 "Merger completed") 
graph export "${output}/ch22-figure-3b-pretrends-large-Stata.png",replace



**************************************************************************
* Diff-in-diffs regerssion with confounder variables
*  weighted by # passengers on market, in before period

use "${work}/ch22-airline-workfile.dta",replace
keep if balanced==1
sort market year
capture gen lnavgp=ln(avgprice)
capture gen d_lnavgp = d.lnavgp

* potential confouders: # passengers before, share of largest carrier before
generate lnpass_bef = ln(passengers) if before
generate sharelarge_bef = sharelargest if before
 * technical fix: the regression is run on after observations
 *  because delta is defined as t - (t-1).
 *  so we need the before variables for the after observations
 replace lnpass_bef = L.lnpass_bef if lnpass_bef ==. 
 replace sharelarge_bef = L.sharelarge_bef if sharelarge_bef ==. 

* check functional forms (not in the text)
*lpoly d_lnavgp lnpass_bef, nosca ci
 * some nonlinearity but it doesn't matter for regression
*lpoly d_lnavgp lnavgp_bef, nosca ci


*global RHS lnpass_bef lnavgp_bef 
global RHS lnpass_bef return stops sharelarge_bef 

* variable labels to get nice LaTex output table
label variable treated "$ AAUS_{before} $"
label variable lnpass_bef "$ \ln passengers_{before} $"
label variable return "$ return $"
label variable stops "$ stops $"
label variable sharelarge_bef "$ sharelargest_{before} $"

regress d_lnavgp treated $RHS [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab4-airlines-Stata", dec(2) ctitle(All markets) 2aster tex(frag) label nonotes replace
regress d_lnavgp treated $RHS if smallmkt==1 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab4-airlines-Stata", dec(2) ctitle(Small markets) 2aster tex(frag) label nonotes append
regress d_lnavgp treated $RHS if smallmkt==0 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab4-airlines-Stata", dec(2) ctitle(Large markets) 2aster tex(frag) label nonotes append


**************************************************************************
* Diff-in-diffs regerssion with quantitative treatment
*  weighted by # passengers on market, in before period

global RHS lnpass_bef return stops sharelarge_bef 

sort market year

* new treatment variable: combined share before treatment
generate share_bef = shareAA+shareUS if before
 replace share_bef = L.share_bef if share_bef==.
 label variable share_bef "Market share of AA & US combined, at baseline"

tabstat passengers if before & share_bef==0, s(sum mean n) format(%12.0fc)
tabstat passengers if before & share_bef>0 & share_bef<1, s(sum mean n) format(%12.0fc)
tabstat passengers if before & share_bef==1, s(sum mean n) format(%12.0fc)

* Figure 22.4
histogram share_bef if before [w=pass_bef], bin(20) percent col(navy*0.8) lcol(white) ///
 ylab(,grid) xlab(, grid) 
graph export "${output}/ch22-figure-4-sharehist-Stata.png",replace


* variable labels to get nice LaTex output table
label variable share_bef "$ AAUSshare_{before} $"
label variable lnpass_bef "$ \ln passengers_{before} $"
label variable return "$ return $"
label variable stops "$ stops $"
label variable sharelarge_bef "$ sharelargest_{before} $"

* Table 22.5
regress d_lnavgp share_bef $RHS [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab5-airlines-Stata", dec(2) ctitle(All markets) 2aster tex(frag) label nonotes replace
regress d_lnavgp share_bef $RHS if smallmkt==1 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab5-airlines-Stata", dec(2) ctitle(Small markets) 2aster tex(frag) label nonotes append
regress d_lnavgp share_bef $RHS if smallmkt==0 [w=pass_bef], robust
 outreg2 using "${output}/ch22-tab5-airlines-Stata", dec(2) ctitle(Large markets) 2aster tex(frag) label nonotes append
 

**************************************************************************
* Diff-in-diffs on pooled cross-sections regeression 
* use entire unbalanced panel 
*   - well not the entire unbalanced panel, because "after only" is dropped 
*     because we need pre-treatment covariates and weights (see text)
*  weighted by # passengers on market, in before period

use "${work}/ch22-airline-workfile.dta",replace

capture gen lnavgp=ln(avgprice)

generate lnpass_bef = ln(pass_bef)
 replace lnpass_bef = L.lnpass_bef if after
generate sharelarge_bef = sharelargest if before
 replace sharelarge_bef = L.sharelarge_bef if after

* confounders intereacted with after
global RHS lnpass_bef return stops sharelarge_bef
foreach x in $RHS {
	generate `x'Xafter = `x'*after
}
global RHSXafter lnpass_befXafter returnXafter stopsXafter sharelarge_befXafter

tabstat passengers if before, by(balanced) s(sum n) format(%12.0fc)
tabstat passengers if after, by(balanced) s(sum n) format(%12.0fc)


* treatment group defined if observed before only or both before and after
sort market year
generate treatment = AA_and_US if before
 replace treatment = treatment[_n-1] if after & market==market[_n-1]
generate treatmentXafter = treatment*after

tabulate balanced if treatment==. /* observed after only */
tabulate balanced if treatment!=. /* balanced or observed before only */

tabstat passengers if treatment==. , s(sum)
tabstat passengers if treatment!=. , by(balanced) s(sum)

* variable labels to get nice LaTex output table
label variable treatmentXafter "$ AAUS_{before} \times after $"
label variable treatment "$ AAUS_{before} $"
label variable after "$ after $ "

label variable lnpass_bef "$ \ln passengers_{before} $"
label variable return "$ return $"
label variable stops "$ stops $"
label variable sharelarge_bef "$ sharelargest_{before} $"

label variable lnpass_befXafter "$ \ln passengers_{before} \times after $"
label variable returnXafter "$ return \times after $"
label variable stopsXafter "$ stops \times after $"
label variable sharelarge_befXafter "$ sharelargest_{before} \times after $"

* Table 22.6
regress lnavgp treatmentXafter treatment after $RHS $RHSXafter [w=pass_bef], cluster(market)
 outreg2 using "${output}/ch22-tab6-airlines-pooledxsec-Stata", dec(2) ctitle(All markets) 2aster tex(frag) label nonotes replace
regress lnavgp treatmentXafter treatment after $RHS $RHSXafter [w=pass_bef] if smallmkt==1, cluster(market)
 outreg2 using "${output}/ch22-tab6-airlines-pooledxsec-Stata", dec(2) ctitle(Small markets) 2aster tex(frag) label nonotes append
regress lnavgp treatmentXafter treatment after $RHS $RHSXafter [w=pass_bef] if smallmkt==0, cluster(market)
 outreg2 using "${output}/ch22-tab6-airlines-pooledxsec-Stata", dec(2) ctitle(Large markets) 2aster tex(frag) label nonotes append

