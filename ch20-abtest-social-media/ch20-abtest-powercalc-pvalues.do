
***************************************************************
*
* DATA ANALYSIS TEXTBOOK
* Case Study: ch20 ab testing
***************************************************************
clear
*cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"

***************************************************************
* PART I
* sample size calculations
***************************************************************

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
*******************************-
clear
global data_in	  "da_data_repo/ab-test-social-media/raw" 

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
