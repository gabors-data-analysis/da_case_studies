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
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

*location folders
global data_in "da_data_repo\city-size-japan\clean"
global data_out "da_case_studies\ch03-city-size-japan"
global output "da_case_studies\ch03-city-size-japan\output"


clear
insheet using "$data_in\city-size-japan.csv"

sum

gen pop=pop_2015/1000
gen lnpop=ln(pop_2015)
gsort -pop	
gen rank = _n

*******************************
** ln(rank) vs ln(x)

gen lnrank = ln(rank)
scatter lnrank lnpop, mc(blue) || lfit lnrank lnpop, lc(gs10) ///
 legend(off) ytitle("ln(rank)") xtitle("ln(population)") ///
 ylab(, grid) xlab(,grid)
 graph export "$output/citysize_Japan_logrank.png",replace
more




/*******************************
** ln P(X>x) vs ln(x) figure
** should be the same s with ln(rank) except for constant shift

gen P = rank / _N
gen lnP = ln(P)
scatter lnP lnpop || lfit lnP lnpop
more
*/


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

