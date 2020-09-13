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
* Data exercise based on CH24B Estimating the impact of replacing football managers
* Unbalanced panel: keep all manager changes that happened the 1st time in season
*
* using the workfile created by the case study code, based on the football dataset
* version 0.9 2020-09-13
********************************************************************


* set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/GitHub/da_case_studies"

global work  	"ch24-football-manager-replace"

cap mkdir 		"$work/output"
global output 	"$work/output"


use "$work/ch24-football-manager.dta",replace
sort team season gameno


* re-define intervention
cap drop temp*
cap drop intervention
gen temp = gameno if managchange==1
 egen gameno1stch = min(temp), by(team season ) /* 1st change in season */
gen intervention = managchange if temp==gameno1stch
cap drop countinterv
egen countinterv = sum(intervention), by(team season)
tab intervention countinterv

* describe interventions
tab countmanagch if gameno==1
tab countmanagch if gameno==1 & countmanagch>0


* re-define event time
replace temp = . if temp==gameno1stch 
egen gameno2ndch = min(temp), by(team season ) /* 2nd change in season */
cap drop t_event
gen t_event = gameno-gameno1stch 
 replace t_event = . if gameno>=gameno2ndch
 replace t_event = t_event+1 if t_event>=0 & t_event<=38

* keep describing interventions
*tab t_event countinterv,mis
 
* average points for graph
preserve
keep if t_event>=-12 & t_event<=12 
collapse points, by(t_event)
egen t_event_6 = cut(t_event), at(-12,-6,1,7,13)

* check if we indeed created 4x6-long periods
*tabstat t_event, by(t_event_6) s(min max n)

egen points6avg = mean(points), by(t_event_6)
gen p6a_1 = points6avg if t_event>=-12 & t_event<=-7
 gen p6a_2 = points6avg if t_event>=-6 & t_event<=-1
 gen p6a_3 = points6avg if t_event>=1 & t_event<=6
 gen p6a_4 = points6avg if t_event>=7 & t_event<=12
 sum p6*
scatter points t_event, mcol(navy*0.8) connect(.) lc(navy*0.8) lw(thin) ///
 || line p6a_1 p6a_2 p6a_3 p6a_4 t_event, ///
 lc(navy*0.8 navy*0.8 navy*0.8 navy*0.8) lw(vthick vthick vthick vthick) legend(off) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 xlab(-12 -6 -1 1 6 12, grid) xline(0, lw(thick) lp(dash)) ///
 ylab(0.0(0.2)1.6, grid) ///
 ytitle("Average points") xtitle("Event time: games to/from manager change")
graph export "$output/ch14-unbalanced-football-manager-interv-Stata.png",replace
restore


* CREATE CONTORL GROUP WITH PSEUDO-INTERVENTIONS

* for each game, define 
* avg diff of points 12-7 before 
* avg diff of points 6-1 before 
* points last game
sort teamseason gameno
xtset teamseason gameno
qui forvalue i=1/12 {
	gen pL`i'=L`i'.points 

}
egen points_b_7_12 = rowmean(pL12 pL11 pL10 pL9 pL8 pL7) 
egen points_b_1_6 = rowmean(pL6 pL5 pL4 pL3 pL2 pL1) 
gen points_b_1= pL1 

* summary stats of dip when intervention
tabstat points_b_7_12 points_b_1_6 points_b_1 if intervention==1, s(min p25 p75 max mean n) col(s)


* set ranges to define control group
local points_b_7_12min = 4/6
local points_b_7_12max = 9/6
local points_b_1_6min = 2/6
local points_b_1_6max = 5/6
local points_b_1min = 0
local points_b_1max = 1


* define pseudo-intervention
gen pseudo = countmanagch==0 ///
	& points_b_7_12>=`points_b_7_12min' & points_b_7_12<=`points_b_7_12max' ///
	& points_b_1_6>=`points_b_1_6min' & points_b_1_6<=`points_b_1_6max' ///
	& points_b_1>=`points_b_1min' & points_b_1<=`points_b_1max' 
	
tab pseudo ,mis
tabstat points_b_7_12 points_b_1_6 points_b_1 if intervention==1, s(min p25 p75 max mean n) col(s)
tabstat points_b_7_12 points_b_1_6 points_b_1 if pseudo==1, s(min p25 p75 max mean n) col(s)



* if more such games in a teamXseason, choose one randomly
egen tempcountpseudo = sum(pseudo), by(team season)
tab tempcountpseudo if gameno==1
set seed 27845
gen temprand = runiform() if pseudo==1
egen temprandmin = min(temprand), by(team season)
replace pseudo = 0 if tempcountpseudo>1 & temprand!=temprandmin
tab pseudo ,mis
tabstat points_b_7_12 points_b_1_6 points_b_1 if intervention==1, s(min p25 p75 max mean n) col(s)
tabstat points_b_7_12 points_b_1_6 points_b_1 if pseudo==1, s(min p25 p75 max mean n) col(s)

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
tab t_event if countinterv==1
tab t_event if countpseudo==1

******************************************************************
* FIGURE with intervention and pseudo-intervention averages

preserve
	keep if countinterv==1 | countpseudo==1
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
	graph export "$output/ch14-unbalanced-football-manager-interv-pseudo-Stata.png",replace
	
restore


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
 outreg2 using "$output/ch14-football-manager-unbalanced-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("treatment") replace
reg Dp6avg after_1_6 after_7_12 if treated==0 , cluster(teamseason)
 outreg2 using "$output/ch14-football-manager-unbalanced-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("control") append
reg Dp6avg after_1_6 after_7_12 ///
  treated treatedXafter_1_6 treatedXafter_7_12 , cluster(teamseason)
 outreg2 using "$output/ch14-football-manager-unbalanced-reg-Stata.tex", tex(fragment) nonotes se dec(2) 2aster label ctitle("treatment+control") append
 
