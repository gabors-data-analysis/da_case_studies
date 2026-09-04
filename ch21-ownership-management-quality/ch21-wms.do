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
* Chapter 21
* CH21A Founder/family ownership and quality of management
* using the wms-management-survey dataset
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


global data_in  "${data_dir}/wms-management-survey/clean"
global work  	"ch21-ownership-management-quality"

capture mkdir 		"${work}/output"
global output 	"${work}/output"




***************************************************************
*
* PART I
*
* Data prep
***************************************************************

* Tech note.
* In some Stata versions, this is needed, others will ignore it.  
set matsize 2000
* Some versions of Stata IC allows a maximum matsize of 800, but more is needed for the last psmatch2. There is nothing we can do. 

clear
use "${data_in}/wms_da_textbook-xsec.dta"

* Or download directly from OSF:
/*
copy "https://osf.io/download/qx4fn/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 


* statistics in text
* Section A1
* number of oervations in the entire dataset
* years and countries covered
count 
* 10,282
tabulate wave
codebook country
tabulate country


* Ownership: define founder/family owned 
*  and drop ownership that's missing or not relevant
codebook ownership
tabulate ownership,mis

generate foundfam_owned = ///
   ownership=="Family owned, external CEO" ///
 | ownership=="Family owned, family CEO" ///
 | ownership=="Family owned, CEO unknown" ///
 | ownership=="Founder owned, external CEO" ///
 | ownership=="Founder owned, CEO unknown" ///
 | ownership=="Founder owned, founder CEO" 

 


* proportion with college degree
* need correction: -44 means do not know, -99 means missing
replace degree_nm=. if degree_nm<0 
replace degree_m=. if degree_m<0 

replace degree_m = degree_m/100
 label variable degree_m "Proportion of managers with a college degree"
replace degree_nm = degree_nm/100
 label variable degree_nm "Proportion of non-managers with a college degree"
* bins from degree_nm
tabstat degree_nm, s(min p10 p25 p50 p75 p90 max)
tabstat degree_nm if degree_nm>0, s(min p10 p25 p50 p75 p90 max)

egen degree_nm_bins = cut(degree_nm), at(0,0.001,0.05,0.20,1.01)
tabstat degree_nm, by(degree_nm_bins) s(min max n)



* log employment
generate lnemp=ln(emp_firm)
*lpoly management lnemp, nosca ci ylab(,grid) xlab(,grid)

* competition 
tabulate competition,mis
rename competition competition_string
encode competition_string, gen(competition)
 recode competition 1=2
 label def competition 2 "0-4 competitors", modify
generate compet_weak  = competition==2
generate compet_moder  = competition==3
generate compet_strong = competition==4

tabstat compet_*, by(competition) 

* industry in 2 digits
generate industry=sic
label def sic2 20 food 21 tobacco 22 textile 23 apparel 24 lumber 25 furniture ///
 26 paper 27 printing 28 chemical 29 petrol 30 rubber 31 leather 32 glass ///
 33 primary_metal 34 fabricated_metal 35 ind_machinery 36 electronic ///
 37 transport 38 instrument 39 misc_manuf
label val industry sic2

* country numeric variable
encode cty, gen(countrycode)



**********************************************************
***** SAMPLE SELECTION

drop if ownership=="" ///
 | ownership == "Government" ///
 | ownership == "Other" 

* Statistics in text
* last paragraph of Section A1
count
tabulate foundfam_owned

drop if competition_string==""
 
* keep observations with non-missing variables
foreach v of varlist management foundfam_owned  ///
 degree_nm compet_moder compet_strong industry countrycode lnemp {
	drop if `v'==.
}

* drop tiny and large firms
tabstat emp_firm, s(min p1 p50 p99 max n)
count if emp_firm<50
drop if emp_firm<50 
count if emp_firm>5000
drop if emp_firm>5000

count
* 8,439


*************************************************************
* DESCRIPTIVE STATISTICS

* distribution of y
summarize management

* variation in x
tabulate foundfam_owned,mis

* average y by x
tabulate foundfam_owned, sum(management)


* DISTRUBUTION OF OTHER VARIABLES
tabulate competition_string
tabulate competition
tabulate degree_nm_bins
tabulate country
tabulate industry

* look at data for modelling
histogram emp_firm, percent col(navy*0.8) lcol(white) ylab(, grid)
histogram lnemp, percent width(0.3) col(navy*0.8) lcol(white) ylab(, grid)
histogram degree_nm, width(0.05) percent col(navy*0.8) lcol(white) ylab(, grid)

lpoly management degree_nm, nosca ci xlab(, grid) ylab(, grid)
lpoly management lnemp, nosca ci xlab(, grid) ylab(, grid)

generate degree_nm_sq= degree_nm^2

*age 
generate age_young=firmage<30
generate age_old=firmage>80 & firmage<.
generate age_unknown=firmage==.

summarize management foundfam_owned
summarize industry countrycode degree_nm* compet_moder compet_strong age_*

summarize management
summarize foundfam_owned
tabulate foundfam_owned, sum(management)



* save workfile for matching and regression
save "${work}/wms-workfile.dta",replace


***************************************************************
* PART II
* Analysis: regression and matching
***************************************************************

*************************************************************
* REGRESSIONS
*************************************************************

use "${work}/wms-workfile.dta" ,clear

summarize industry country degree_nm degree_nm_sq compet_moder compet_strong ///
 age_young age_old age_unknown lnemp 

* y on x
regress management foundfam_owned , robust
 outreg2 using "${output}/ch21-foundfam-reg-Stata", 2aster dec(2) symbol(**, *) ///
 ctitle("no confounders") tex(frag) nonotes replace


* y on x and all confounders
regress management foundfam_owned ///
 i.industry i.countrycode ///
 degree_nm degree_nm_sq compet_moder compet_strong ///
 age_young age_old age_unknown lnemp ///
 , robust
 outreg2 using "${output}/ch21-foundfam-reg-Stata", 2aster dec(2) symbol(**, *) ///
 keep(foundfam_owned) ctitle("with confounders") tex(frag) nonotes append

* y on x and all confounders interacted with industry and country
regress management foundfam_owned ///
 i.industry##i.countrycode ///
 i.industry##c.degree_nm i.industry##c.degree_nm_sq i.industry##i.compet_moder i.industry##i.compet_strong ///
 i.industry##i.age_young i.industry##i.age_old i.industry##i.age_unknown i.industry##c.lnemp ///
 i.countrycode##c.degree_nm i.countrycode##c.degree_nm_sq i.countrycode##i.compet_moder i.countrycode##i.compet_strong ///
 i.countrycode##i.age_young i.countrycode##i.age_old i.countrycode##i.age_unknown i.countrycode##c.lnemp ///
 , robust
 outreg2 using "${output}/ch21-foundfam-reg-Stata", 2aster dec(2) symbol(**, *) ///
 keep(foundfam_owned) ctitle("with confounders interacted") tex(frag) nonotes append


 
*************************************************************
* EXACT MATCHING
***************************************************************** 

use "${work}/wms-workfile.dta" ,clear

egen empbin5=cut(emp_firm), group(5)
 tabstat emp_firm, by(empbin5) s(min max n)
generate n=1
generate n1 = foundfam_owned
generate n0 =1-foundfam_owned
generate age_mid = age_young==0 & age_old==0 & age_unknown==0
generate agecat = age_young + 2*age_mid + 3*age_old + 4*age_unknown
tabstat firmage, by(agecat) s(min max n)

generate y0 = management if foundfam_owned==0
generate y1 = management if foundfam_owned==1

collapse (sum) n n0 n1 (mean) y0 y1 , ///
 by(degree_nm_bins agecat competition empbin5 industry countrycode)
 
* # theoretical combinations
display 4*4*3*5*20*24

* # combinations in the data
count

* # firms with exact match
tabstat n if n0>=1 & n1>=1, s(sum)

* # firms with no exact match
tabstat n if n0==0 | n1==0, s(sum)

* examples with founder/family only
* random order just for the examples
set seed 12345
tempvar sortorder
generate `sortorder' = runiform()
sort `sortorder'
list industry countrycode degree_nm_bins competition agecat empbin5 n1 n0 if n1==1 & n0==0 & _n<20

* examples with other only: 
list industry countrycode degree_nm_bins competition agecat empbin5 n1 n0 if n1==0 & n0==1 & _n<20

* examples of similar firms unmatched
sort countrycode industry degree_nm_bins competition agecat empbin5 n
list industry countrycode degree_nm_bins competition agecat empbin5 n1 n0 ///
 if countrycode==2 & industry==20 & n==1 & _n<340

generate d=y1-y0

* ATE
summarize d [w=n]
* ATET
summarize d [w=n1]


 
***************************************************************** 
* Matching on the propensity score 
***************************************************************** 

* SOLUTION With replacement

use "${work}/wms-workfile.dta" ,clear

psmatch2  foundfam_owned ///
 i.industry i.countrycode ///
 degree_nm degree_nm_sq compet_moder compet_strong ///
 age_young age_old age_unknown lnemp ///
 , out(management) ate logit
 

psmatch2  foundfam_owned ///
 i.industry##i.countrycode ///
 i.industry##c.degree_nm i.industry##c.degree_nm_sq i.industry##i.compet_moder i.industry##i.compet_strong ///
 i.industry##i.age_young i.industry##i.age_old i.industry##i.age_unknown i.industry##c.lnemp ///
 i.countrycode##c.degree_nm i.countrycode##c.degree_nm_sq i.countrycode##i.compet_moder i.countrycode##i.compet_strong ///
 i.countrycode##i.age_young i.countrycode##i.age_old i.countrycode##i.age_unknown i.countrycode##c.lnemp ///
 , out(management) ate logit
 
 

***************************************************************** 
* CHECK common support
***************************************************************** 
use "${work}/wms-workfile.dta" ,clear


quietly tab industry, gen(i)
quietly tab cty, gen(c)

tabstat compet_moder compet_strong i1-i20 c1-c21, by(foundfam_owned)
tabstat degree_nm, by(foundfam_owned) s(min p1 p5 p95 p99 max n) 
tabstat emp_firm, by(foundfam_owned) s(min p1 p5 p95 p99 max n)
* common support check passed








