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
* CH03D Describe the likelihood of loss / tie / win for home team
* using the football dataset
* version 1.0 2025-01-04
*
* STATA VERSION: This code is optimized for Stata 18
* Backward compatibility notes for Stata 15 and below are included
********************************************************************

* Stata version check and setup
version 18
clear all
set more off
set varabbrev off


********************************************************************
* SETTING UP DIRECTORIES
********************************************************************

* STEP 1: set working directory for da_case_studies
* Example: cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"

* STEP 2: Set data directory
* Option 1: Run directory-setting do file (RECOMMENDED)
capture do set-data-directory.do 
	/* This one-line do file should sit in your working directory
	   It contains: global data_dir "path/to/da_data_repo"
	   More details: gabors-data-analysis.com/howto-stata/ */

* Option 2: Set directory directly here
* Example: global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

* Set up paths
global data_in  "${data_dir}/football/clean"
global work     "ch03-football-home-advantage"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"



* Option 2: set directory directly here
* for example:
* global data_dir "C:/Users/xy/gabors_data_analysis/da_data_repo"

global data_in  "$data_dir/football/clean"
global work  	"ch03-football-home-advantage"

capture mkdir 		"${work}/output"
global output 	"${work}/output"



* Use full 11 season data table on games
use "${data_in}/epl_games.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/tyjp8/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* Sample design: keep one season
keep if season == 2016

* Generate home goal advantage
generate home_goaladv = goals_home - goals_away

* Generate direction of home goal advantage (negative - zero - positive)
generate home_goaldir = -1 if home_goaladv<0
replace home_goaldir = 0 if home_goaladv==0
replace home_goaldir = 1 if home_goaladv>0



* Histogram
* Figure 3.9
colorpalette viridis, n(4) select(2) nograph
local color1 `r(p)'

histogram home_goaladv, ///
	discrete percent ///
	ylabel(0(5)25, grid) ///
	xlabel(-6(1)6, grid) ///
	color("`color1'") lcol(white) lw(vthin) ///
	ytitle(Share of games (percent)) ///
	xtitle(Goal difference) ///
	addlabel addlabopts(yvarformat(%4.1f)) ///
	graphregion(fcolor(white) ifcolor(none)) ///
	plotregion(fcolor(white) ifcolor(white))

graph export "${output}/ch03-figure-9-hist-goaldiff-Stata.png", replace as(png)

* Statistics
* Table 3.7
* Same as Table 3.9
tabstat home_goaladv, s(mean sd n) format(%4.1f)
tabulate home_goaldir

