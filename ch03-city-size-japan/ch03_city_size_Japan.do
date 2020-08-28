*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Also good to know section, power law distribution
* Distribution of city size in Japan
*
* using the city-size-japan dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch03-city-size-japan"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-europe/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/city-size-japan/clean"



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

