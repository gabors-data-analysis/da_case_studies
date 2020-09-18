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
* Also good to know section, power loaw distribution
* using the city-size-japan dataset
* version 0.9 2020-09-06
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
insheet using "$data_in\city-size-japan.csv"

sum

gen pop=pop_2015/1000
gen lnpop=ln(pop)
gsort -pop	
gen rank = _n

*******************************
** Figure: ln(rank) vs ln(x)
gen lnrank = ln(rank)

* Figure 3.12
colorpalette viridis, n(4) select(2) nograph
scatter lnrank lnpop|| lfit lnrank lnpop, ///
 color(`r(p)') ///
 legend(off) ytitle("ln(rank)") xtitle("ln(population in thousand)") ///
 ylab(, grid) xlab(,grid)
 graph export "$output/ch03-figure-12-logrank-Stata.png", replace

 

*******************************
** SCALE INVARIANCE

local x1=200
local x2=300
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

