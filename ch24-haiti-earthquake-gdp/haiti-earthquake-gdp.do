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
* Chapter 24
* CH24A Estimating the effect of the 2010 Haiti earthquake on GDP
* using the haiti-earthquake dataset
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/GitHub/da_case_studies"


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


global data_in  "$data_dir/haiti-earthquake/clean"
global work  	"ch24-haiti-earthquake-gdp"

cap mkdir 		"$work/output"
global output	"$work/output"

cap mkdir 		"$work/temp"
global temp 	"$work/temp"


use "$data_in/haiti-earthquake-mod.dta", clear	

*donor pool based on threshold calculations below: it is those countries with incomethreshold=1, and a balanced panel for all variables	
	gen dp=0
	replace dp=1 if inlist(country, "Benin","Burkina Faso","Burundi" ,"Bangladesh"  ,"Cambodia","Cameroon" )
	replace dp=1 if inlist(country, "Kenya"  	,"Kyrgyz Republic"  ,"Liberia","Madagascar"  ,"Mali","Moldova","Mozambique"  )
	replace dp=1 if inlist(country, "Nicaragua" ,"Nepal"  ,"Rwanda","Senegal","Sierra Leone","Sudan","Tanzania","Togo","Uganda"  )
    replace dp=1 if country=="Haiti"  
	lab var dp "Country in donor pool"
keep if dp==1	

sort country year
 egen ccode = group(countrycode) if country!="Haiti"
replace ccode = ccode+1
replace ccode = 1 if country=="Haiti"
	
xtset ccode year
xtdes


compress
clear matrix
save "$work/haiti-earthquake-workfile.dta",replace


* time series in Haiti
line gdptb_us year if ccode==1, lw(thick) lc(navy*0.8) ///
 xla(2004(2)2015, grid) yla(6(0.5)9, grid) ///
 xline(2010, lp(dash) lc(gray)) /// 
 text(8 2009 "Earthquake") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) 
graph export "$output/ch24-figure-1-haiti-gdp-Stata.png", replace


* Haiti and synthetic control

synth gdptb_us cons exp imp gcf land pop inf gdppc_w ///
  gdptb_us(2005) gdptb_us(2007) gdptb_us(2009) , ///
  trunit(1) trperiod(2010) xperiod(2004(1)2009) nested ///
  unitnames(country) keep("$temp/gdp-1")replace

use "$temp/gdp-1",replace
lab var _time "Year"


* total GDP in Haiti and synthetid control
* figure 24.2a
line _Y_treated _Y_synth _time, lw(vthick vthick) lc(navy*0.8 green*0.6) ///
 xla(2004(2)2015, grid) yla(6(0.5)9, grid) ///
 xline(2010, lp(dash) lc(gray)) /// 
 text(8 2009 "Earthquake") ///
 text(9.1 2013 "Synthetic Haiti") text(7 2012 "Haiti") legend(off) ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ytitle("Total GDP, constant USD, billion")
graph export "$output/ch24-figure-2a-haiti-gdp-synth-Stata.png", replace


* differenence in log total GDP 
* figure 24.2b
gen lndiffY = ln(_Y_treated) - ln(_Y_synth)
line lndiffY _time, lw(vthick ) lc(navy*0.8) ///
 xla(2004(2)2015, grid) yla(-0.2(0.05)0.05, grid) yline(0) ///
 xline(2010, lp(dash) lc(gray)) /// 
 text(-0.17 2009 "Earthquake") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ytitle("Effect estimate, log of total GDP") 
graph export "$output/ch24-figure-2b-haiti-gdp-synth-Stata.png", replace


****************************************************
* temporary stuff for textbook development
* for R - temp
use "$temp/gdp-1",replace
rename _Y_treated Ytreated
rename _Y_synthetic Ysynthetic
rename _time year
drop _W_Weight  _Co_Number
keep if year<.
save "$temp\gdp-1-temp.dta",replace
