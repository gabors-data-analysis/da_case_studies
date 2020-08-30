*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 03D
* Distributions of body height and income
*
* using the height-income-distributions dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch03-distributions-height-income"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-europe/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/height-income-distributions/clean"




use "$data_in/hrs_height_income.dta",clear

* Normal distribution: height of women age 55-59 
sum rheight if age>=55 & age<60 & female==1 
count if rheight>203 & age>=55 & age<60 & female==1 

count if age>=55 & age<60 & female==1 & rheight>1.3 & rheight<2.1
sum rheight if age>=55 & age<60 & female==1 & rheight>1.3 & rheight<2.1

* Histogram with normal density overlayed
* Figure 3.10
colorpalette viridis, n(4) select(2) nograph
hist rheight if age>=55 & age<60 & female==1 & rheight>1.3 & rheight<2.1, ///
 percent width(0.025) ///
 color(`r(p)') lcol(white) ///
 normal ///
 ylabel(,grid) xlabel(1.4(0.1)1.9, grid) 
 graph export "$output/ch03-figure-10-hist-height-Stata.png", replace
 

* Lognormal distribution: family income of women age 55-59 

gen income = hitot/1000
 lab var income "Household income, USD '000"

* Histogram of income and ln income with normal density overlayed
* Figure 3.11a
colorpalette viridis, n(4) select(2) nograph
hist income if age>=55 & age<60 & female==1 & income<1000, ///
 percent width(20) ///
 color(`r(p)') lcol(white) lw(vvthin) ///
 ylabel(0(5)25, grid) xlabel(0(200)1000, grid) 
 graph export "$output/ch03-figure-11a-hist-inc-Stata.png", replace

* Figure 3.11b
gen lnincome=ln(income)
 lab var lnincome "ln(income, thousand US dollars)"
colorpalette viridis, n(4) select(2) nograph
hist lnincome if age>=55 & age<60 & female==1 & lnincome<8 & lnincome>0, ///
 percent width(0.25) ///
 color(`r(p)') lcol(white) lw(vvthin) ///
 ylabel(, grid) xlabel(0(1)8, grid) ///
 normal
 graph export "$output/ch03-figure-11b-hist-lninc-Stata.png", replace
