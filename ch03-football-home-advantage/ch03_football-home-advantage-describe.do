*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 03B
* Comparing Hotel Prices in Europe: Vienna vs. London
*
* using the hotels-europe dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch03-football-home-advantage"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/hotels-europe/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/football/clean"




* use full 11 season data table on games
use "$data_in\epl_games.dta", clear

* sample design: keep one season
keep if season == 2016

* generate home goal advantage
gen home_goaladv = goals_home- goals_away
* generate direction of home goal advantage (negative - zero - positive)
gen home_goaldir = -1 if home_goaladv<0
 replace home_goaldir = 0 if home_goaladv==0
 replace home_goaldir = 1 if home_goaladv>0



* histogram
* Figure 3.9
colorpalette viridis, n(4) select(2) nograph
histogram home_goaladv,  ///
	discrete percent ///
	ylabel(0(5)25, grid) xlabel(-6(1)6, grid) /// 
	color(`r(p)') lcol(white) lw(vthin) ///
	ytitle(Share of games (percent)) xtitle(Goal difference) ///
	addlabel addlabopts(yvarformat(%4.1f))  ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white)) 	
 graph export "$output/ch03-figure-9-hist-goaldiff-Stata.png", replace

* statistics
* Table 3.7
* same as Table 3.9
tabstat home_goaladv, s(mean sd n) format(%4.1f)
tab home_goaldir

