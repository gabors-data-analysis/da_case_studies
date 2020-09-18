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
* Chapter 03
* CH03C Measurig home team advantage in football
* using the football dataset
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

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
global work  	"ch03-football-home-advantage"

cap mkdir 		"$work/output"
global output 	"$work/output"



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

