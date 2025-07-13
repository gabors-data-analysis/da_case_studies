********************************************************************
* Chapter 21
* CH21A Founder/family ownership and quality of management
* using the wms-management-survey dataset
* Data prep 
* Pscore matching versions (simple model only)
********************************************************************


* SETTING UP DIRECTORIES

cd "C:/Users/kezdi/GitHub/da_case_studies"
do set-data-directory.do 

global data_in  "$data_dir/wms-management-survey/clean"
global work  	"ch21-ownership-management-quality"

cap mkdir 		"$work/output"
global output 	"$work/output"




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
use "$data_in\wms_da_textbook-xsec.dta"


* statistics in text
* Section A1
* number of oervations in the entire dataset
* years and countries covered
count 
* 10,282
tab wave
codebook country
tab country


* Ownership: define founder/family owned 
*  and drop ownership that's missing or not relevant
codebook ownership
tab ownership,mis

gen foundfam_owned = ///
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
 lab var degree_m "Proportion of managers with a college degree"
replace degree_nm = degree_nm/100
 lab var degree_nm "Proportion of non-managers with a college degree"
* bins from degree_nm
tabstat degree_nm, s(min p10 p25 p50 p75 p90 max)
tabstat degree_nm if degree_nm>0, s(min p10 p25 p50 p75 p90 max)

egen degree_nm_bins = cut(degree_nm), at(0,0.001,0.05,0.20,1.01)
tabstat degree_nm, by(degree_nm_bins) s(min max n)



* log employment
gen lnemp=ln(emp)
*lpoly management lnemp, nosca ci ylab(,grid) xlab(,grid)

* competition 
tab competition,mis
rename competition competition_string
encode competition_string, gen(competition)
 recode competition 1=2
 lab def competition 2 "0-4 competitors", modify
gen compet_weak  = competition==2
gen compet_moder  = competition==3
gen compet_strong = competition==4

tabstat compet_*, by(competition) 

* industry in 2 digits
gen industry=sic
lab def sic2 20 food 21 tobacco 22 textile 23 apparel 24 lumber 25 furniture ///
 26 paper 27 printing 28 chemical 29 petrol 30 rubber 31 leather 32 glass ///
 33 primary_metal 34 fabricated_metal 35 ind_machinery 36 electronic ///
 37 transport 38 instrument 39 misc_manuf
lab val industry sic2

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
tab foundfam

drop if competition_string==""
 
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
* 8,439



 
***************************************************************** 
* Matching on the propensity score 
***************************************************************** 

******************
* ATET BY HAND

use "$work/wms-workfile.dta" ,clear

qui logit foundfam ///
 i.industry i.countrycode ///
 degree_nm degree_nm_sq compet_moder compet_strong ///
 age_young age_old age_unknown lnemp 
predict p
sort p foundfam
order p foundfam management
gen matched_p=9 if foundfam==1
gen matched_y=9 if foundfam==1
count if matched_p==9
qui forvalue i=1/30 {
	replace matched_p = p[_n+`i'] if matched_p==9 & foundfam[_n+`i']==0 
	replace matched_p = p[_n-`i'] if matched_p==9 & foundfam[_n-`i']==0 
	replace matched_y = management[_n+`i'] if matched_y==9 & foundfam[_n+`i']==0 
	replace matched_y = management[_n-`i'] if matched_y==9 & foundfam[_n-`i']==0 
}
count if matched_p==9

order foundfam p management matched*

gen atet=management-matched_y
sum management matched_y ate if foundfam==1

***********************
* ATET BY PSMATCH2

psmatch2  foundfam ///
 i.industry i.countrycode ///
 degree_nm degree_nm_sq compet_moder compet_strong ///
 age_young age_old age_unknown lnemp ///
 , out(management) ate logit
 
* for some reason we don't get ATE here, and the SE are off, sill investigate that further
