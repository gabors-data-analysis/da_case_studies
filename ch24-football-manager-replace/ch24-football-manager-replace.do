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
* CH24B Estimating the impact of replacing football managers
* using the football dataset
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


global data_in  "$data_dir/football/clean"
global work  	"ch24-football-manager-replace"

cap mkdir 		"$work/output"
global output 	"$work/output"




use "$data_in/football_managers_workfile",clear

* describe data
tab season
codebook gameno
codebook team
*tab team if gameno==1
tab points
sum points
*dis 1*0.25 + 1.5*0.75



* create manager change variable
gen managchange=0
sort team date
replace managchange= 1 if team==team[_n-1] & season==season[_n-1]   & manager_id !=manager_id[_n-1] 
tab managchange, mis

* some teams have multiple management changes in the season
egen countmanagchange = sum(managchange), by(team season)
tab countmanagchange if gameno==1 /* gameno = 1 so each team-season counted once */
* result: 0, 1, 2, or 3 manager change 

* create variables for number of games since season start or last manager change
gen temp1 = gameno if managchange==1
egen managch_1_gameno = min(temp1), by(team season)
 gen temp2 = temp1
 replace temp2 = . if managch_1_gameno == gameno
egen managch_2_gameno = min(temp2), by(team season)
 gen temp3 = temp2
 replace temp3 = . if managch_2_gameno == gameno
egen managch_3_gameno = min(temp3), by(team season)
*lis gameno team manager_id managch* if countmanagchange==3 & season==2017, noob
*lis gameno team manager_id managch* if countmanagchange==2 & season==2017, noob

gen gamesbefore = gameno-1 if managchange==1
 replace gamesbefore = managch_2_gameno - managch_1_gameno if gameno == managch_2_gameno
 replace gamesbefore = managch_3_gameno - managch_2_gameno if gameno == managch_3_gameno
gen gamesafter = 38-gameno if managchange==1
 replace gamesafter = managch_2_gameno - managch_1_gameno if gameno == managch_1_gameno & managch_2_gameno!=.
 replace gamesafter = managch_3_gameno - managch_2_gameno if gameno == managch_2_gameno & managch_3_gameno!=.
*lis gameno team manager_id managch* games* if countmanagchange==3 & season==2017, noob


* define intervention as management change 
*  at least 12 games before (since season started or previous management changed)
*  at least 12 games after (till season ends or next management change)
gen intervention = managchange 
 replace intervention = 0 if gamesbefore<12 | gamesafter<12
tab intervention managchange  
egen countinterv = sum(intervention), by(team season)
tab countinterv  if gameno==1 /* gameno = 1 so each team-season counted once */

 
 
tabstat points_lastseason, by(season) s(min max n)

* figure: average number interventions by game number
* bar chart
preserve
	collapse (sum) intervention, by(season gameno)
	graph bar (sum) intervention , ///
	 over(gameno, label(alternate)) ///
	 bar(1, col(navy*0.8)) ///
	 ytitle("Number of manager changes") 
	graph export "$output\ch14-figure-3-football-managchanges-Stata.png",replace
restore

* EVENT TIME
cap drop temp*
gen temp1 = gameno if interv==1
egen temp2 = min(temp1), by(team season )
gen t_event = gameno-temp2
replace t_event = t_event+1 if t_event>=0 & t_event<=38
lab var t_event "Intervention event study time (to -1 before, from +1 after)"
cap drop temp*

* for Stata to define xt panel: team-season
cap drop teamseason
egen teamseason = group(season team )
codebook teamseason

xtset teamseason gameno
xtdes

order teamseason team season gameno date* ///
 t_event interv count* points* home*
qui compress
save "$work/ch24-football-manager.dta",replace


*********************************************************************-
* EVENT STUDY GRAPH, INTERVENTIONS ONLY, BALANCED PANEL ONLY
*********************************************************************-

use "$work/ch24-football-manager.dta",replace

* keep only interventions and +-12 games
keep if countinterv==1 & t_event>=-12 & t_event<=12
*tab t_event, sum(points)
tab points if t_event==-1

* average points for graph
collapse points, by(t_event)
egen t_event_6 = cut(t_event), at(-12,-6,1,7,13)

* check if we indeed created 4x6-long periods
tabstat t_event, by(t_event_6) s(min max n)

egen points6avg = mean(points), by(t_event_6)
gen p6a_1 = points6avg if t_event>=-12 & t_event<=-7
 gen p6a_2 = points6avg if t_event>=-6 & t_event<=-1
 gen p6a_3 = points6avg if t_event>=1 & t_event<=6
 gen p6a_4 = points6avg if t_event>=7 & t_event<=12
 sum p6*
scatter points t_event, mcol(navy*0.8) connect(.) lc(green) lw(thin) ///
 || line p6a_1 p6a_2 p6a_3 p6a_4 t_event, ///
 lc(navy*0.8 navy*0.8 navy*0.8 navy*0.8) lw(vthick vthick vthick vthick) legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 xlab(-12 -6 -1 1 6 12, grid) xline(0, lw(thick) lp(dash)) ///
 ylab(0.0(0.2)1.6, grid) ///
 ytitle("Average points") xtitle("Event time: games to/from manager change")
graph export "$output/ch14-figure-4-football-manager-interv-Stata.png",replace
*/


*********************************************************************-
* CREATE CONTORL GROUP WITH PSEUDO-INTERVENTIONS
*********************************************************************-

use "$work/ch24-football-manager.dta",replace

* keep balanced intervention panel and team-seasons w/o any manager change
keep if (countinterv==1 & t_event>=-12 & t_event<=12) ///
 | countmanagch==0


* for each game, define 
* avg diff of points 12-7 before 
* dip: avg diff of points 6-1 before minus 12-7 before 
* points last game
gen points_b_7_12 = L12.points + L11.points + L10.points + ///
	L9.points + L8.points + L7.points 
gen points_b_1_6 = L6.points + L5.points + L4.points + ///
	L3.points + L2.points + L1.points 
gen dip = points_b_1_6/6 - points_b_7_12/6
gen points_b_1= L.points

* summary stats of dip when intervention
tabstat points_b_7_12 dip  points_b_1 if intervention, s(min p25 p75 max mean n) col(s)


* set ranges to define control group
local points_b_7_12min = 5
local points_b_7_12max = 8
local dipmin -8/6
local dipmax -1/6
local points_b_1min = 0
local points_b_1max = 0


* define pseudo-intervention
gen pseudo = countmanagch==0 & dip>=`dipmin' & dip<=`dipmax' ///
	& points_b_7_12>=`points_b_7_12min' & points_b_7_12<=`points_b_7_12max' ///
	& points_b_1>=`points_b_1min' & points_b_1<=`points_b_1max' ///
	
* games with 12 left in the season
replace pseudo = 0 if gameno>=(38-12)
tab pseudo ,mis
tabstat points_b_7_12 dip points_b_1 if intervention, s(min p25 p75 max mean n) col(s)
tabstat points_b_7_12 dip points_b_1 if pseudo, s(min p25 p75 max mean n) col(s)



* if more such games in a teamXseason, choose one randomly
egen tempcountpseudo = sum(pseudo), by(team season)
tab tempcountpseudo if gameno==1
set seed 27845
gen temprand = runiform() if pseudo==1
egen temprandmin = min(temprand), by(team season)
replace pseudo = 0 if tempcountpseudo>1 & temprand!=temprandmin
tab pseudo ,mis
tabstat points_b_7_12 dip points_b_1 if intervention, s(min p25 p75 max mean n) col(s)
tabstat points_b_7_12 dip points_b_1 if pseudo, s(min p25 p75 max mean n) col(s)

* count pseudo-interventions in team-season
egen countpseudo = sum(pseudo), by(team season)


* pseudo event-time
cap drop temp*
gen temp1 = gameno if pseudo==1
egen temp2 = min(temp1), by(team season )
gen t_pseudo = gameno-temp2
replace t_pseudo = t_pseudo+1 if t_pseudo>=0 & t_pseudo<=38
lab var t_pseudo "Pseudo intervention event study time (to -1 before, from +1 after)"
cap drop temp*

* event-time: intervention or pseudo
replace t_event = t_pseudo if t_event==.


* keep if intervention event time or pseudo-intervention event time is in range
keep if (t_event>=-12 & t_event<=12)
lab var t_event "Event study time, intervention or pseudo (to -1 before, from +1 after)"

tab intervention pseudo
tab countpseudo countinterv if intervention==1 | pseudo==1

******************************************************************
* FIGURE with intervention and pseudo-intervention averages

preserve
	gen treated = countinterv==1
	collapse points, by(t_event treated)
	 egen t6 = cut(t_event), at(-12,-6,1,7,13)
	 egen p6a = mean(points), by(t6 treated)
	 gen p6a_1 = p6a if t_event>=-12 & t_event<=-7
	 gen p6a_2 = p6a if t_event>=-6 & t_event<=-1
	 gen p6a_3 = p6a if t_event>=1 & t_event<=6
	 gen p6a_4 = p6a if t_event>=7 & t_event<=12
	reshape wide points p6*, i(t_event) j(treated)

	scatter points1 t_event, mcol(navy*0.8) ///
	|| line p6a_11 p6a_21 p6a_31 p6a_41 t_event, ///
	lc(navy*0.8 navy*0.8 navy*0.8 navy*0.8) lw(vthick vthick vthick vthick) ///
	|| scatter points0 t_event, mcol(green*0.5) ///
	|| line p6a_10 p6a_20 p6a_30 p6a_40 t_event, ///
	lc(green*0.5 green*0.5 green*0.5 green*0.5) lw(vthick vthick vthick vthick) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	xlab(-12 -6 -1 1 6 12, grid) xline(0, lw(thick) lp(dash)) ///
	ylab(0.0(0.2)1.6, grid) ///
	ytitle("Average points") ///
	xtitle("Event time: games to/from intervention or pseudo intervention") ///
	legend(rows(1) order(1 6)  label(1 "Intervention") label(6 "Pseudo-intervention"))
	graph export "$output/ch14-figure-5-football-manager-interv-pseudo-Stata.png",replace
	
restore


******************************************************************
* REGRESSION with 6-game averages
egen t6_event = cut(t_event), at(-12 -6 1 7 13)
tabstat t_event, by(t6_event) s(min max n)

gen treated = countinterv==1
collapse (mean) treated (mean) points6avg = points, by(teamseason t6_event)
egen t=group(t6_event)
xtset teamseason t

gen Dp6avg = points6avg - L.points6avg 

gen before_7_12 = t6_event==-12
gen before_1_6 = t6_event==-6
gen after_1_6 = t6_event==1
gen after_7_12 = t6_event==7

gen treatedXbefore_7_12 = treated*before_7_12
gen treatedXbefore_1_6 = treated*before_1_6
gen treatedXafter_1_6 = treated*after_1_6
gen treatedXafter_7_12 = treated*after_7_12

sum Dp6avg before_7_12 after_1_6 after_7_12 treated treatedXbefore_7_12 treatedXafter_1_6 treatedXafter_7_12 

* FD REGRESSIONS
lab var after_1_6 "$ post_{1-6} $"
lab var after_7_12 "$ post_{7-12} $"
lab var treated "$ treated $"
lab var treatedXafter_1_6 "$ treated \times post_{1-6} $"
lab var treatedXafter_7_12 "$ treated \times post_{7-12} $"

reg Dp6avg after_1_6 after_7_12 if treated==1 , cluster(teamseason)
 outreg2 using "$output/ch24-table-1-football-manager-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("treatment") replace
reg Dp6avg after_1_6 after_7_12 if treated==0 , cluster(teamseason)
 outreg2 using "$output/ch24-table-1-football-manager-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("control") append
reg Dp6avg after_1_6 after_7_12 ///
  treated treatedXafter_1_6 treatedXafter_7_12 , cluster(teamseason)
 outreg2 using "$output/ch24-table-1-football-manager-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("treatment+control") append
 
 
* FE REGRESSIONS
 
xtreg points6avg before_7_12 after_1_6 after_7_12 if treated==1 ///
 , fe cluster(teamseason)
xtreg points6avg before_7_12 after_1_6 after_7_12 if treated==0 ///
 , fe cluster(teamseason)
xtreg points6avg before_7_12 after_1_6 after_7_12 treatedXbefore_7_12  treatedXafter_1_6 treatedXafter_7_12 , fe cluster(teamseason)

 
 
