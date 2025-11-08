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
* CH03D Distributions of body height and income
* using the height-income-distribution dataset
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Applied viridis colors properly
*   - Updated graph export syntax
*   - Fixed typos in header and variable label
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

global data_in  "$data_dir/height-income-distributions/clean"
global work  	"ch03-distributions-height-income"

cap mkdir 		"$work/output"
global output 	"$work/output"





use "$data_in/hrs_height_income.dta", clear

* Or download directly from OSF:

/*
copy "https://osf.io/download/2bqsg/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* Normal distribution: height of women age 55-59 
sum height if age>=55 & age<60 & female==1 
tab height if height>1.80 & age>=55 & age<60 & female==1, mis

count if age>=55 & age<60 & female==1 & height>1.3 
sum height if age>=55 & age<60 & female==1 & height>1.3 

* Histogram with normal density overlayed
* Figure 3.10
colorpalette viridis, n(4) select(2) nograph

hist height if age>=55 & age<60 & female==1 & height>1.3, ///
 percent width(0.025) ///
 color(`r(p)') lcol(white) ///
 normal ///
 ylabel(, grid) ///
 xlabel(1.4(0.1)1.9, grid) ///
 xtitle("Body height") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-10-hist-height-Stata.png", replace as(png)
 

* Lognormal distribution: family income of women age 55-59 

* Histogram of income and ln income with normal density overlayed
* Figure 3.11a
count if age>=55 & age<60 & female==1
count if age>=55 & age<60 & female==1 & hhincome<1
count if age>=55 & age<60 & female==1 & hhincome>1000 & hhincome!=.
count if age>=55 & age<60 & female==1 & hhincome==.
count if age>=55 & age<60 & female==1 & hhincome>1 & hhincome<1000

colorpalette viridis, n(4) select(2) nograph

hist hhincome if age>=55 & age<60 & female==1 & hhincome>1 & hhincome<1000, ///
 percent width(20) ///
 color(`r(p)') lcol(white) lw(vvthin) ///
 ylabel(0(5)25, grid) ///
 xlabel(0(200)1000, grid) ///
 xtitle("Household income (thousand US dollars)") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-11a-hist-inc-Stata.png", replace as(png)

* Figure 3.11b
gen lnincome = ln(hhincome)
lab var lnincome "ln(household income, thousand US dollars)"

colorpalette viridis, n(4) select(2) nograph

hist lnincome if age>=55 & age<60 & female==1 & lnincome>0 & hhincome<1000, ///
 percent width(0.25) start(0) ///
 color(`r(p)') lcol(white) ///
 ylabel(0(2.5)10, grid) ///
 xlabel(0(1)8, grid) ///
 normal ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch03-figure-11b-hist-lninc-Stata.png", replace as(png)

