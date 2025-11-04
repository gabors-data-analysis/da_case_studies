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
* Chapter 02
* CH02C Displaying immunization rates across countries
* using the world-bank-immunization dataset
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Modernized table output using collect
*   - Enhanced comments
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"


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

global data_in  "$data_dir/worldbank-immunization/clean"
global work  	"ch02-immunization-crosscountry"

cap mkdir 		"$work/output"
global output 	"$work/output"

 
* Load in clean and tidy data and create workfile
use "$data_in/worldbank-immunization-panel.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/ku4fd/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

keep countryname year imm gdppc 
keep if imm!=.
keep if year>=2015
keep if countryname=="Pakistan" | countryname=="India" 
sum

sort countryname year
list

*****************************
* Table 2.5: Long format (country-year observations)
*****************************

* Export to LaTeX using Stata 18 collect
preserve
collect create table5
collect get countryname year imm gdppc, ///
        tag(var[countryname year imm gdppc])
collect layout (var) ()
collect export "$output/xt_immun_long.tex", replace as(tex)
restore

* Reshape from long to wide format
* Creates separate variables for each year: imm2015, imm2016, imm2017, etc.
reshape wide imm gdppc, i(countryname) j(year)
order countryname imm* gdp*
list


*****************************
* Table 2.4: Wide format (one row per country)
*****************************

* Export to LaTeX using Stata 18 collect
collect create table4
collect get countryname imm* gdppc*, ///
        tag(var[countryname imm2015 imm2016 imm2017 gdppc2015 gdppc2016 gdppc2017])
collect layout (var) ()
collect export "$output/xt_immun_wide.tex", replace as(tex)

