*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH03
* height-income-distributions
*
*
* WHAT THIS CODES DOES:
* creates desrciptive stats
* 
********************************************************************





********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook"

*location folders
*location folders
global data_in   "da_data_repo/height-income-distributions/clean"
global data_out  "da_case_studies/ch03-distributions-height-income"
global output    "da_case_studies/ch03-distributions-height-income/output"


use "$data_in/hrs_height_income.dta",clear

* NORMAL: height of women age 55-59 
sum rheight if age>=55 & age<60 & female==1 & rheight>1.3 & rheight<2.1

hist rheight if age>=55 & age<60 & female==1 & rheight>1.3 & rheight<2.1, ///
 width(0.025) normal percent col(blue) lcol(ltblue) ///
 ylabel(,grid) xlabel(, grid) 
 graph export "$output/normal_hist_height.eps",replace
 

* LOGNORMAL: family income of women age 55-59 

gen income = hitot/1000
 lab var income "Household income, USD '000"

hist income if age>=55 & age<60 & female==1 & income>1 & income<1000, ///
 width(20) percent col(blue) lcol(ltblue) ///
 ylabel(,grid) xlabel(, grid) 
 graph export "$output/lognormal_hist_hhincome.eps",replace

gen lnincome = ln(income)
 lab var lnincome "ln household income (USD '000)"

hist lnincome if age>=55 & age<60 & female==1 & income>1 & income<1000, ///
 width(0.25) normal percent col(blue) lcol(ltblue) ///
 ylabel(,grid) xlabel(, grid) 
 graph export "$output/lognormal_hist_lnhhincome.eps",replace

