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
* Chapter 20
* CH20B Fine tuning social media advertising
* using the ab-test-social-media dataset
* version 0.9 2020-09-13
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


global data_in  "$data_dir/ab-test-social-media/raw"
global work  	"ch20-ab-test-social-media"

cap mkdir 		"$work/output"
global output 	"$work/output"



***************************************************************
* PART I
* sample size calculations

* sample size calculation with planned rates

local clickthrough = 0.01
local conversion = 0.05

local proportionA = `clickthrough' * `conversion'
dis `proportionA'

local proportionB = `proportionA' * 1.2
dis `proportionB'

power twoproportions `proportionA' `proportionB'




* sample size calculation with rates closer to actual

local clickthrough = 0.0032
local conversion = 0.0082

local proportionA = `clickthrough' * `conversion'
dis `proportionA'

local proportionB = `proportionA' * 1.2
dis `proportionB'

power twoproportions `proportionA' `proportionB'


*******************************-
* PART II
* p-value of tests 

clear
import excel "$data_in/ab-test-summary.xlsx", sheet("Sheet1") firstrow

foreach ab in A B{
	foreach var in show clicks action {
	su `var' if action_type =="Action `ab'", meanonly
	local `var'_`ab' = r(mean)
	dis ``var'_`ab''
	} 
}

clear
local obs = `show_A' + `show_B'
set obs `obs'
gen type_b=_n>`show_A'
tab type_b
gen clicks = _n<=`clicks_A' 
local sc= `show_B'+`clicks_B'
 replace clicks = 1 if _n>`show_B'& _n<=`sc'
tab type_b clicks
gen action = _n<=`action_A'

 local sa=`show_B'+`action_B'
 replace action = 1 if _n>`show_B' & _n<=`sa'
tab type_b action

reg clicks type_b, nohead robust
reg action type_b, nohead robust
