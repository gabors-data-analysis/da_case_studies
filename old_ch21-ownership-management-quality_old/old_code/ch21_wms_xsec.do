********************************************************************************
* Chapter 21
*
* wms-management
* v0.8

* using
* world management survey

**********************************************************************


* set the path
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
*cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook\"
*cd "C:\Users\viktoriakonya\Dropbox\bekes_kezdi_textbook"
*cd "C:\Users\GB\Box Sync\GaborsGuide_box\_Textbook\21_causal_regression\wms"

*location folders
global data_in   "cases_studies_public/wms-management-survey/clean"
global data_out  "textbook_work/ch21/wms-management-survey"
global output   "$data_out/output"


ssc install psmatch2, replace

* prep for double post-estimation lasso
* https://statalasso.github.io/
* ssc install lassopack
* ssc install pdslasso



***************************************************************
*
* PART I
*
* Understanding nature and patterns of data
* Data prep
***************************************************************


clear
set matsize 4000
use "$data_in/wms_2004_2010_xsec.dta",replace

* Describe data: observations
count
tab wave
codebook country
codebook sic
tab country



* Ownership: defone founder/family owned 
*  and drop ownership that's missing or not relevant
codebook ownership
tab ownership,mis

gen foundfam_owned = ownership=="Family owned, external CEO" ///
 | ownership=="Family owned, family CEO" ///
 | ownership=="Founder owned, CEO unknown" ///
 | ownership=="Founder owned, external CEO" ///
 | ownership=="Founder owned, family CEO" ///
 | ownership=="Founder owned, founder CEO" 

tab ownership foundfam_own,mis


* proportion with college degree
replace degree_m = degree_m/100
 lab var degree_m "Proportion of managers with a college degree"
replace degree_nm = degree_nm/100
 lab var degree_nm "Proportion of non-managers with a college degree"
* bins from degree_nm
tabstat degree_nm, s(min p10 p25 p50 p75 p90 max)
tabstat degree_nm if degree_nm>0, s(min p10 p25 p50 p75 p90 max)

egen degree_nm_bins = cut(degree_nm), at(0,0.001,0.05,0.20,1.01)
tabstat degree_nm, by(degree_nm_bins) s(min max n)


* practice: 
* create a nice table/graph summarizing key info
* table should be exported in tex. / graph should be in jpg


* log employment
gen lnemp=ln(emp)
*lpoly management lnemp, nosca ci ylab(,grid) xlab(,grid)

* competition 
rename competition competition_string
encode competition_string, gen(competition)
 recode competition 1=2
 lab def competition 2 "0-4 competitors", modify
gen compet_weak  = competition==2
gen compet_moder  = competition==3
gen compet_strong = competition==4

tabstat compet_*, by(competition) 

* industry in 2 digits
gen industry=int(sic/10)

* country numeric variable
encode cty, gen(countrycode)

**********************************************************
***** SAMPLE SELECTION

drop if ownership==""
drop if ownership == "Employees/COOP" ///
 | ownership == "Foundation/Research Institute" ///
 | ownership == "Government" ///
 | ownership == "Other" 
 
* keep observations with non-missing variables
foreach v of varlist management foundfam  ///
 degree_nm compet_moder compet_strong industry countrycode lnemp {
	drop if `v'==.
}

* drop tiny and large firms
tabstat emp, s(min p1 p50 p99 max n)
count if emp<50
drop if emp<50 
count if emp>5000
drop if emp>5000

count


*************************************************************
* VARIATION IN X, DISTRUBUTION OF OTHER VARIABLES


tab foundfam,mis

tab competition, sum(foundfam)
tab degree_nm_bins
tab country, sum(foundfam)
tab industry, sum(foundfam)

* look az data
hist emp, percent ylab(, grid)

hist lnemp, width(0.3) percent ylab(, grid)

 * practice
 * make a nice graph with the histogram of management quality
 * table should be exported in tex. / graph should be in jpg

 
hist degree_nm, width(0.05) percent ylab(, grid)
lpoly management degree_nm, nosca ci xlab(, grid) ylab(, grid)

* practice: 
* create a graph that combines this with frequency
* table should be exported in tex. / graph should be in jpg

lpoly management lnemp, nosca ci xlab(, grid) ylab(, grid)

gen degree_nm_sq= degree_nm^2

sum management foundfam
sum degree_nm* compet_moder compet_strong industry countrycode



* matching may need random ordering
set seed 12345
tempvar sortorder
gen `sortorder' = runiform()
sort `sortorder'


* save workfile for matching and regression
save "$data_out/Ch21_wms_workfile_xsec.dta",replace




***************************************************************
*
* PART II
*
* Matching and regressions for ATE
***************************************************************



*************************************************************
* EXACT MATCHING
***************************************************************** 

use "$data_out/Ch21_wms_workfile_xsec.dta" ,clear
count
gen n=1


collapse (sum) n foundfam_sum=foundfam (mean) foundfam_mean=foundfam management, ///
 by(degree_nm_bins competition industry countrycode)
 
* # theoretical combinations
dis 4*3*20*21

* # combinations in the data
count

* theoretical combinations not in the data
tab country industry
* an example: Japan, furniture
tab degree_nm_bins competition if countrycode==14 & industry==25 



* few firms in a cell
dis 6137/2758 
tab n

* number of cells with no exact matches
gen other_sum = n-foundfam_su,
tab foundfam_sum other_sum

* number of trreated firms with no exact match
tabstat n foundfam_sum other_sum if other_sum==0, s(sum n)

* total number of firms
tabstat n foundfam_sum other_sum, s(sum n)


* practice
* produce a nice table with these info
* table should be exported in tex. / graph should be in jpg

* examples of trreated firms with no exact match
list industry countrycode degree_nm_bins competition foundfam_sum other_sum ///
 if other_sum==0 & _n<100
* U.S. food industry
list industry countrycode degree_nm_bins competition foundfam_sum other_sum ///
 if countrycode==21 & industry==20

**********************
* exact matching estimate of ATE (using successful matches only)
use "$data_out/Ch21_wms_workfile_xsec.dta" ,clear

gen other_owned = 1-foundfam_owned

* management score if treated vs if untreated;
* we'll take their average separately when aggregating
gen management1 = management if foundfam_owned
gen management0 = management if other_owned

* aggregate by cell
* count treated and untreated firms, take treated and untreated avg of outcome
collapse (sum) foundfam_owned other_owned (mean) management0 management1 ///
 , by(degree_nm_bins competition industry countrycode )

* exact matching: keep only those with both founder/family and other
keep if foundfam_owned>0 & other_owned>0

* average of treated and untreated outcomes
*  weighted by n: total # observations in each cell
gen n = foundfam_owned + other_owned
sum management* [w=n] 

* instead of computing the diff by hand we can also do it this way
sum management0 [w=n]
 local avgy0 = r(mean)
sum management1 [w=n]
 local avgy1 = r(mean)
local ATE = `avgy1' - `avgy0'
dis `ATE'
 
 
***************************************************************** 
* Matching on the propensity score 
***************************************************************** 


* SOLUTION With replacement
* a) Without employment
use "$data_out/Ch21_wms_workfile_xsec.dta" ,clear
global RHS1 degree_nm degree_nm_sq compet_moder compet_strong
* Step 1 - Matching
psmatch2 foundfam $RHS1 i.industry i.countrycode, neighbor(1) logit ate
* Step 2 & 3 - Keep only matched untreated & calculate ATE
reg management foundfam_owned if (_treated==0 & _weight!=.) | _treated==1 

* b) With employment
use "$data_out/Ch21_wms_workfile.dta" ,clear
global RHS1 degree_nm degree_nm_sq compet_moder compet_strong
* Step 1 - Matching
psmatch2 foundfam $RHS1 i.industry i.countrycode lnemp, n(1) logit
* Step 2 & 3 - Keep only matched untreated & calculate ATE
reg management foundfam_owned if (_treated==0 & _weight!=.) | _treated==1




* practice
* Produce a nice output from this
* table should be exported in tex. / graph should be in jpg

*************************************************************
* REGRESSIONS
*************************************************************

use "$data_out/Ch21_wms_workfile_xsec.dta" ,clear

global RHS1 degree_nm degree_nm_sq compet_moder compet_strong

* y on x
reg management foundfam_own , robust
 outreg2 using "$output/Ch21_foundfam_reg1", dec(2) tex replace

* y on x and all z except employment
reg management foundfam ///
 $RHS1 i.industry i.countrycode, robust
 outreg2 using "$output/Ch21_foundfam_reg1", dec(2) tex append

* y on x and all z incl. employment
reg management foundfam ///
 $RHS1 lnemp i.industry i.countrycode, robust
 outreg2 using "$output/Ch21_foundfam_reg1", dec(2) tex append

 
 * practice: 
 * create nice tables, ie coefficient plus two two rows control variables: NO, YES, YES; employment : NO, NO, YES
 * add a footnote similar to tables we used in class/handouts 
* table should be exported in tex.
 

***************************************************************** 
* CHECK common support
***************************************************************** 
use "$data_out/Ch21_wms_workfile_xsec.dta" ,clear


qui tab industry, gen(i)
qui tab cty, gen(c)

tabstat compet_mod compet_str i1-i20 c1-c21, by(foundfam)
tabstat degree_nm, by(foundfam) s(min p1 p5 p95 p99 max n) 
tabstat emp, by(foundfam) s(min p1 p5 p95 p99 max n)
* common support check passed


*practice
* produce a nice table
* table should be exported in tex.






