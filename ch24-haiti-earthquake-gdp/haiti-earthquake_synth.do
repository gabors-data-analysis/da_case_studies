*********************************************************************
*
* DATA ANALYSIS TEXTBOOK

* ch24
* synth for Haiti
*********************************************************************

* WHAT THIS CODES DOES:

* v2.1.  (GB version; GK jst updated directory and fixed problems with folders)
* v2.3 - 2020-02-21
* v2.4 - 2020-04-27 BG added temp for R


********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:\Users\bekes.gabor\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

global data_in	 "da_data_repo/haiti-earthquake/clean" 
global data_out  "da_case_studies/ch24-haiti-earthquake-gdp"
global output    "da_case_studies/ch24-haiti-earthquake-gdp/output"

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
save "$data_out/haiti-earthquake-workfile.dta",replace

*create temp folder
capture confirm file "$data_out/temp/"
if _rc mkdir "$data_out/temp/"

* time series in Haiti
line gdptb_us year if ccode==1, lw(vthick) lc(blue) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 xla(2004(2)2015, grid) yla(6(0.5)9, grid) xline(2010, lw(thick) lc(green)) 
graph export "$output/haiti-gdp.eps", replace
graph export "$output/haiti-gdp.png", replace


* Haiti and synthetic control

synth gdptb_us cons exp imp gcf land pop inf gdppc_w ///
  gdptb_us(2005) gdptb_us(2007) gdptb_us(2009) , ///
  trunit(1) trperiod(2010) xperiod(2004(1)2009) nested ///
  unitnames(country) keep("$data_out/temp/gdp-1")replace

use "$data_out/temp/gdp-1",replace
lab var _time "Year"


* figure with total GDP in Haiti and synthetid control
line _Y_treated _Y_synth _time, lw(vthick vthick) lc(blue mint) ///
 xla(2004(2)2015, grid) yla(6(0.5)9, grid) xline(2010, lw(thick) lc(green)) ///
  graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ytitle("Total GDP, constant USD, billion")
graph export "$output/haiti-gdp-synth.eps", replace
graph export "$output/haiti-gdp-synth.png", replace


* figure with differenence in log total GDP 
gen lndiffY = ln(_Y_treated) - ln(_Y_synth)
line lndiffY _time, lw(vthick ) lc(blue ) ///
 xla(2004(2)2015, grid) yla(-0.2(0.05)0.05, grid) yline(0) ///
 xline(2010, lw(thick) lc(green)) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 ytitle("Effect estimate, log of total GDP") 
graph export "$output/haiti-lndiffgdp-synth.eps", replace
graph export "$output/haiti-lndiffgdp-synth.png", replace


* for R - temp
use "$data_out/temp/gdp-1",replace
rename _Y_treated Ytreated
rename _Y_synthetic Ysynthetic
rename _time year
drop _W_Weight  _Co_Number
keep if year<.
save " $data_out\temp\gdp-1-temp.dta"
