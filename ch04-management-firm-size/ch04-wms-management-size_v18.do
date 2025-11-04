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
* Chapter 04
* CH04A Management quality and firm size: describing patterns of association
* using the wms-management-survey dataset
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade
*   - Fixed path separator for cross-platform compatibility
*   - Applied viridis colors properly
*   - Updated graph export syntax
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

global data_in  "$data_dir/wms-management-survey/clean"
global work  	"ch04-management-firm-size"

cap mkdir 		"$work/output"
global output 	"$work/output"



clear
use "$data_in/wms_da_textbook.dta"

* Or download directly from OSF:
/*
copy "https://osf.io/download/gwfbk/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 


* Sample selection
cap drop lean1_1-talent6_5
cap drop aa*
keep if country=="Mexico"
keep if wave==2013
keep if emp_firm>=100 
keep if emp_firm<=5000
count
sum emp_firm, d

save "$work/ch04-wms-work.dta", replace


* Describe data
sum management emp_firm, d


* Distribution of y, distribution of x
tabstat management emp_firm, s(min max mean median sd n) col(s)

* Figure 4.1
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist management, ///
 percent width(0.25) ///
 color("`color1'") lcol(white) ///
 ylabel(, grid) ///
 xtitle("Quality of management, average score") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-1-wms-mex-management-hist-Stata.png", replace as(png)

* Figure 4.2a
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist emp_firm, ///
 percent start(0) width(200) ///
 xlabel(0(500)5000, grid) ///
 ylabel(0(5)30, grid) ///
 xtitle("Number of employees") ///
 color("`color1'") lcol(white) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-2a-wms-mex-emp-hist-Stata.png", replace as(png)

* Figure 4.2b
gen lnemp = ln(emp_firm)
colorpalette viridis, n(1) nograph
local color1 `r(p)'

hist lnemp, ///
 percent width(0.25) start(4) ///
 xlabel(4(1)9, grid) ///
 ylabel(, grid) ///
 xtitle("Ln number of employees") ///
 color("`color1'") lcol(white) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-2b-wms-mex-lnemp-hist-Stata.png", replace as(png)


* Emp: 3 bins
gen emp3bins = 1 if emp_firm<200
replace emp3bins = 2 if emp_firm>=200 & emp_firm<1000
replace emp3bins = 3 if emp_firm>=1000
lab def emp3bins 1 "small" 2 "medium" 3 "large"
lab val emp3bins emp3bins
tabstat emp_firm, by(emp3bins) s(min max n)

* Stacked bar charts some management items by emp bins
qui foreach v of varlist lean* perf* talent* {
	forvalue i=1/5 {
		gen `v'_`i' = `v'==`i'
		lab var `v'_`i' "score=`i'"
	}
}

* Set up viridis palette for stacked bars
colorpalette viridis, n(5) nograph
local colors `r(p)'

tab lean1 emp3bins, col nofre
graph bar lean1_*, stack over(emp3bins) percent ///
 bar(1, col("`=word("`colors'", 1)'")) ///
 bar(2, col("`=word("`colors'", 2)'")) ///
 bar(3, col("`=word("`colors'", 3)'")) ///
 bar(4, col("`=word("`colors'", 4)'")) ///
 bar(5, col("`=word("`colors'", 5)'")) ///
 legend(label(1 "1") label(2 "2") label(3 "3") label(4 "4") label(5 "5") row(1)) ///
 title("Lean management. Score by three bins of firm emp") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-3a-wms-mex-lean1-emp3bins-Stata.png", replace as(png)

tab perf2 emp3bins, col nofre
graph bar perf2_*, stack over(emp3bins) percent ///
 bar(1, col("`=word("`colors'", 1)'")) ///
 bar(2, col("`=word("`colors'", 2)'")) ///
 bar(3, col("`=word("`colors'", 3)'")) ///
 bar(4, col("`=word("`colors'", 4)'")) ///
 bar(5, col("`=word("`colors'", 5)'")) ///
 legend(label(1 "1") label(2 "2") label(3 "3") label(4 "4") label(5 "5") row(1)) ///
 title("Performance tracking. Score by three bins of firm emp") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-3b-wms-mex-perf2-emp3bins-Stata.png", replace as(png)


* Bin scatters avg score by employment bins
tabstat emp_firm, s(min max median n) by(emp3bins)
gen emp3bins_num = (emp3bins==1)*150 + (emp3bins==2)*600 + (emp3bins==3)*3000
tabstat emp_firm, s(min max median n) by(emp3bins_num)
egen emp10bins = cut(emp_firm), group(10)
tabstat emp_firm, s(min max median n) by(emp10bins)
recode emp10bins 0=120 1=135 2=165 3=255 4=315 5=375 6=585 7=860 8=1600 9=3500
tabstat emp_firm, s(min max median n) by(emp10bins)
egen management_emp3bins  = mean(management), by(emp3bins)
egen management_emp10bins = mean(management), by(emp10bins)

colorpalette viridis, n(1) nograph
local color1 `r(p)'

scatter management_emp3bins emp3bins_num, ///
 msize(vlarge) mcolor("`color1'") ///
 ylab(2.4(0.2)3.4, grid) ///
 xlab(0(500)3500, grid) ///
 ytitle("Average management quality score") ///
 xtitle("Firm size (# employees), 3 bins") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-4a-wms-mex-mgmt-emp3bins-Stata.png", replace as(png)

scatter management_emp10bins emp10bins, ///
 msize(vlarge) mcolor("`color1'") ///
 ylab(2.5(0.25)3.5, grid) ///
 xlab(0(500)3500, grid) ///
 ytitle("Average management quality score") ///
 xtitle("Firm size (# employees), 10 bins") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-4b-wms-mex-mgmt-emp10bins-Stata.png", replace as(png)


* Scatterplot avg score by employment
scatter management emp_firm, ///
 ylab(, grid) ///
 xlab(, grid) ///
 mcolor("`color1'") ///
 xtitle("Number of employees") ///
 ytitle("Quality of management: average score") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-5a-wms-mex-mgmt-emp-scatter-Stata.png", replace as(png)

cap gen lnemp = ln(emp_firm)

scatter management lnemp, ///
 ylab(, grid) ///
 xlab(, grid) ///
 mcolor("`color1'") ///
 xtitle("Log of number of employees") ///
 ytitle("Quality of management: average score") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-5b-wms-mex-mgmt-lnemp-scatter-Stata.png", replace as(png)


* Box plots by emp bins
graph box management, ///
 over(emp3bins) ///
 ytitle("Management score") ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-6a-wms-mex-mgmt-emp3bins-box-Stata.png", replace as(png)

* Violin plots by emp bins
* Need to install vioplot ado file
* (type search vioplot, click on link and follow the instructions)
vioplot management, ///
 over(emp3bins) ///
 ylab(, grid) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

graph export "$output/ch04-figure-6b-wms-mex-mgmt-emp3bins-violin-Stata.png", replace as(png)


******************************************
* Correlation
corr management emp_firm

* By industry
cap drop industry_broad
gen industry_broad = ""
replace industry_broad = "food_drinks_tobacco" if sic<=21
replace industry_broad = "textile_apparel_leather_etc" if sic==22 | sic==23 | sic==31
replace industry_broad = "wood_furniture_paper" if sic>=24 & sic<=27
replace industry_broad = "chemicals_etc" if sic>=28 & sic<=30
replace industry_broad = "materials_metals" if sic>=32 & sic<35 
replace industry_broad = "electronics, equipment, machinery" if sic>=35 & sic<37
replace industry_broad = "auto" if sic==37
replace industry_broad = "other" if sic>=38
tab industry_broad, mis
tab sic industry_broad, mis


sort industry_broad
by industry_broad: corr management emp_firm
tab industry, mis sum(management)

