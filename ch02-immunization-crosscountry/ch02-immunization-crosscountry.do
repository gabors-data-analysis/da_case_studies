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
* CH02C Immunization against measles and GDP per capita
* using the worldbank-immunization dataset
* version 1.1 2025-12-09
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


********************************************************************
* SETTING UP DIRECTORIES
********************************************************************

* STEP 1: set working directory for da_case_studies
* Example: cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global data_in  "${data_dir}/worldbank-immunization/clean"
global work     "ch02-immunization-crosscountry"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


cap mkdir 		"${work}/output"
global output 	"${work}/output"

 
* Load in clean and tidy data and create workfile
use "${data_in}/worldbank-immunization-panel.dta", clear

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
summarize

sort countryname year
list

*****************************
* Table 2.5: Long format (country-year observations)
*****************************

* Export to LaTeX using Stata 18 collect
listtab countryname year imm gdppc ///
 using "${output}/xt_immun_long.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrr}" ///
 `"Country & Year & imm & gdppc \\"') ///
 foot("\end{tabular}")

* Reshape from long to wide format
* Creates separate variables for each year: imm2015, imm2016, imm2017, etc.
reshape wide imm gdppc, i(countryname) j(year)
order countryname imm* gdp*
list


*****************************
* Table 2.4: Wide format (one row per country)
*****************************

* Export to LaTeX using Stata 18 collect
listtab countryname imm* gdppc* ///
 using "${output}/xt_immun_wide.tex", replace ///
 rstyle(tabular) ///
 head("\begin{tabular}{lrrrrrr}" ///
 `"Country & imm2015 & imm2016 & imm2017 & gdppc2015 & gdppc2016 & gdppc2017  \\"') ///
 foot("\end{tabular}")

