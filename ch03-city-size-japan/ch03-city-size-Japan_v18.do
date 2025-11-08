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
* Chapter 03
* Also good to know section, power law distribution
* using the city-size-japan dataset
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Fixed path separator for cross-platform compatibility
*   - Modernized import command
*   - Applied viridis colors properly
*   - Updated graph export syntax
*   - Fixed typo in header
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

global data_in  "$data_dir/city-size-japan/clean"
global work  	"ch03-city-size-japan"

cap mkdir 		"$work/output"
global output 	"$work/output"



clear
import delimited using "$data_in/city-size-japan.csv"

* Or download directly from OSF: 
/*
copy "https://osf.io/download/9mgep/" "workfile.csv"
import delimited "workfile.csv", clear
erase "workfile.csv"
*/


sum

* Convert population to thousands
gen pop = pop_2015/1000
gen lnpop = ln(pop)
gsort -pop	
gen rank = _n

*******************************
* Figure 3.12: ln(rank) vs ln(x)
*******************************
gen lnrank = ln(rank)

* Set up viridis color scheme
colorpalette viridis, n(4) select(2) nograph


scatter lnrank lnpop, mcolor(`r(p)') || ///
        lfit lnrank lnpop, lcolor(`r(p)') ///
 legend(off) ///
 ytitle("ln(rank)") ///
 xtitle("ln(population in thousand)") ///
 ylab(, grid) ///
 xlab(, grid) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-12-logrank-Stata.png", replace as(png)

 

*******************************
* Scale invariance demonstration
*******************************

local x1 = 200
local x2 = 300
local bound = 0.2

dis `x1' "    " `x2'
count if pop >= `x1'*(1-`bound') & pop <= `x1'*(1+`bound')
count if pop >= `x2'*(1-`bound') & pop <= `x2'*(1+`bound')

local shift = 3
local x3 = `x1'*`shift'
local x4 = `x2'*`shift'

dis `x3' "    " `x4'
count if pop >= `x3'*(1-`bound') & pop <= `x3'*(1+`bound')
count if pop >= `x4'*(1-`bound') & pop <= `x4'*(1+`bound')

