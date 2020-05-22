*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* FUNDAMENTALS OF REGRESSION ANALYSIS
* Ch07
*
* OLS fits line to scatterplot
* produce graph with artificial (simulated) data
* 
*************************************



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
cd "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook"


 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

*location folders
global data_in   	"da_data_repo/hotels-vienna/clean"
global data_out  	"da_case_studies/ch07-ols-simulation"
global output 		"da_case_studies/ch07-ols-simulation/output"


* clear environment
clear

* set the seed
set seed 1458

* sample size
global N=100
set obs $N

* uniformly distributed x, [0,4]
gen x = runiform(0,4)

* y  = a + bx + u (u normally distributed)
local a = 2
local b = 0.5
local sigmau = 0.7

gen y = `a' + `b'*x + rnormal(0,`sigmau')

summarize y x

* scatterplot and OLS regression line
* average y and average x shown

scatter y x, mc(navy) ms(O) msize(tiny) mlw(thick) mcolor(%50)  || ///
 lfit y x, legend(off) lc(dkgreen) lw(thick) ///
 ylabel(0(0.5)6, grid) xlabel(0(0.5)4, grid) ytitle("y") xtitle("x") ///
 yline(3, lc(dkgrey))  xline(1.93, lc(dkgrey)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
  graph export "$output\F07_OLSfit.png", replace
