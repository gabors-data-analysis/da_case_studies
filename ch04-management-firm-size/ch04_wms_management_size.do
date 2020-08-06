********************************************************************************
* Chapter 04
*
* wms-management-survey
* v1.2

* using WMS data 2004-2015
* 
*
********************************************************************************


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"

clear all
set more off

 
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

 *global data_in   "cases_studies_public/wms-management-survey/clean"
global data_in   "da_data_repo/wms-management-survey/clean"
global data_out  "da_case_studies/ch04-management-firm-size"
global output 	 "da_case_studies/ch04-management-firm-size/output"




* use \cases_studies_public\wms-management-survey\wms_gbekes_v2.dta, replace

clear
* cleaned version, stata13
use "$data_in\wms_da_textbook.dta"

* sample selection
cap drop lean1_1-talent6_5
cap drop aa*
keep if country=="Mexico"
keep if wave==2013
keep if emp_firm>=100 
keep if emp_firm<=5000
count
sum emp_firm,d

save "$data_out/ch04-wms-work.dta", replace


* describe data
sum management emp_firm,d


* distribution of y, distribution of x
tabstat management emp_firm, s(min max mean median sd n) col(s)
hist management, percent width(0.25) fcolor(emerald) lcolor(gs12) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ylabel(, grid) xtitle("Quality of management, average score")
graph export "$output/wms_Mex_management_hist.eps",replace

hist emp_firm, percent fcolor(emerald) lcolor(gs12) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xlabel(0(500)5000, grid) ylabel(0(10)50, grid) width(200) xtitle("Number of employees") 
graph export "$output/wms_bra_emp_hist.png",replace
gen lnemp=ln(emp_firm)
hist lnemp, percent width(0.29) fcolor(emerald) lcolor(gs12) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xlabel(4(1)9, grid) ylabel(, grid) xtitle("Ln number of employees") 
graph export "$output/wms_Mex_lnemp_hist.eps",replace


* emp : 3 bins
gen emp3bins = 1 if emp_firm<200
 replace emp3bins = 2 if emp_firm>=200 & emp_firm<1000
 replace emp3bins = 3 if emp_firm>=1000
 lab def emp3bins 1 "small" 2 "medium" 3 "large"
 lab val emp3bins emp3bins
tabstat emp_firm, by(emp3bins) s(min max n)

* stacked bar charts some management items by emp bins
qui foreach v of varlist lean* perf* talent* {
	forvalue i=1/5 {
		gen `v'_`i' = `v'==`i'
		lab var `v'_`i' "score=`i'"
	}
}

tab lean1 emp3bins, col nofre
graph bar lean1_*, stack over(emp3bins) percent ///
 bar(1, col(navy)) bar(2, col(green)) bar(3, col(blue)) ///
 bar(4, col(lime)) bar(5, col(ltblue)) ///
 legend(label(1 "1") label(2 "2")  label(3 "3") label(4 "4") label(5 "5") row(1)) ///
 title("Lean management. Score by three bins of firm emp")
graph export "$output/wms_Mex_lean1_emp3bins.eps",replace

tab perf2 emp3bins, col nofre
graph bar perf2_*, stack over(emp3bins) percent ///
 bar(1, col(navy)) bar(2, col(green)) bar(3, col(blue)) ///
 bar(4, col(lime)) bar(5, col(ltblue)) ///
 legend(label(1 "1") label(2 "2")  label(3 "3") label(4 "4") label(5 "5") row(1)) ///
 title("Performance tracking. Score by three bins of firm emp")
graph export "$output/wms_Mex_perf2_emp3bins.eps",replace


* Bin scatters avg score by employment bins
tabstat emp_firm, s(min max median n) by(emp3bins)
 recode emp3bins 1=150 2=600 3=3000
 tabstat emp_firm, s(min max median n) by(emp3bins)
egen emp10bins = cut(emp_firm), group(10)
 tabstat emp_firm, s(min max median n) by(emp10bins)
 recode emp10bins 0=120 1=135 2=165 3=255 4=315 5=375 6=585 7=860 8=1600 9=3500
 tabstat emp_firm, s(min max median n) by(emp10bins)
egen management_emp3bins  = mean(management), by(emp3bins)
egen management_emp10bins = mean(management), by(emp10bins)

scatter management_emp3bins emp3bins, msize(vlarge) ///
 ylab(2.4(0.2)3.4, grid) xlab(0(500)3500, grid) ///
 ytitle("Average management quality score") xtitle("Firm size (# employees), 3 bins") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/wms_Mex_management_emp3bins.eps", replace
graph export "$output/wms_Mex_management_emp3bins.png", as(png) replace

scatter management_emp10bins emp10bins, msize(vlarge) ///
 ylab(2.5(0.25)3.5, grid) xlab(0(500)3500, grid) ///
 ytitle("Average management quality score") xtitle("Firm size (# employees), 10 bins") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/wms_Mex_management_emp10bins.eps", replace
graph export "$output/wms_Mex_management_emp10bins.png", as(png) replace


* Scatterplot avg score by employment
scatter management emp_firm, ylab(, grid) xlab(, grid) ///
 xtitle("Number of employees") ytitle("Quality of management: average score")
graph export "$output/wms_Mex_management_emp_scatter.eps", replace

cap gen lnemp=ln(emp_firm)
scatter management lnemp, ylab(, grid) xlab(, grid) ///
 xtitle("Log of number of employees") ytitle("Quality of management: average score")
graph export "$output/wms_Mex_management_lnemp_scatter.eps", replace


* Box plots by emp bins
lab def emp3bin 150 small 600 medium 3500 large
lab val emp3bin emp3bin 

graph box management, over(emp3bin) ytitle("Management score")
graph export "$output/wms_Mex_boxplot_management_emp3bins.eps", replace

violin management, by(emp3bin) 
graph export "$output/wms_Mex_violin_management_emp3bins.eps", replace


******************************************
* Correlation
corr management emp_firm

* by industry
cap drop industry_broad
gen industry_broad=""
replace industry_broad ="food_drinks_tobacco" if sic<22
replace industry_broad ="textile_apparel_leather_etc" if sic>=22 & sic<24 | sic>=31 & sic<32 | sic>=39 & sic<40
replace industry_broad ="wood_furniture_paper" if sic>=24 & sic<=26
replace industry_broad ="chemicals_etc" if sic>=28 & sic<31
replace industry_broad ="materials_metals" if sic>=32 & sic<35 
replace industry_broad ="electronics" if sic>35 & sic<37
replace industry_broad ="auto" if sic>=37 & sic<38
tab industry_broad,mis

sort industry_broad
by industry_broad: corr management emp_firm
tab industry, mis sum(management)


/*
*****************************************

* NOT USED IN THE END

*****************************************

* Bin scatters avg score by items
foreach v of varlist lean* perf* talent* {
	cap egen as_`v' = mean(management), by(`v')
	lab var as_`v' "Avg. score"
	scatter as_`v' `v', msize(huge) ylabel(1.5(0.5)4 ,grid) xlabel(, grid) saving(`v', replace)
}

graph combine lean1.gph lean2.gph perf1.gph perf2.gph perf3.gph perf4.gph ///
	perf5.gph perf6.gph perf7.gph perf8.gph perf9.gph perf10.gph ///
	talent1.gph talent2.gph talent3.gph talent4.gph talent5.gph talent6.gph 
graph export "$output/management_management_byitems.eps", replace
graph export "$output/management_management_byitems.png, replace
