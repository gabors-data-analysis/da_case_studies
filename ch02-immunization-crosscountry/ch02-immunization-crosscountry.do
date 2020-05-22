**********************************************
* Chapter 02
*
* hotels-vienna
*
*
* PART A: basic look, some descriptives 
* uses clean data

* PART B: repeat of the cleaning code
* uses raw data

* v1.1
**********************************************


* set the path
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

* ssc install listtab

*location folders
global data_in   "da_data_repo/world-bank-immunization/clean"
global data_out  "da_case_studies/ch02-immunization-crosscountry"
global output    "da_case_studies/ch02-immunization-crosscountry/output"

 
* load in clean and tidy data and create workfile
use "$data_in/world-bank_immunization-panel.dta", clear

keep countryname year imm gdppc 
keep if imm!=.
keep if year>=2015
keep if countryname=="Pakistan" | countryname=="India" 
sum

sort countryname year
lis

*****************************
* Table 2.5
*****************************

listtab countryname year imm gdppc ///
 using "$output/xt_immun_long.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrr}" ///
 `"Country & Year & imm & gdppc \\"') ///
 foot("\end{tabular}")

reshape wide imm gdppc, i(countryname) j(year)
order countryname imm* gdp*
lis


*****************************
* Table 2.4
*****************************


listtab countryname imm* gdppc* ///
 using "$output/xt_immun_wide.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Country & imm2015 & imm2016 & imm2017 & gdppc2015 & gdppc2016 & gdppc2017  \\"') ///
 foot("\end{tabular}")
