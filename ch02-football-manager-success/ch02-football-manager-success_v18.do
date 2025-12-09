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
* Chapter 02
* CH02B Identifying successful football managers
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
global work     "ch02-football-manager-success"
global output   "${work}/output"

* Create directories
capture mkdir "${work}"
capture mkdir "${output}"


********************************************************************
* PART 1: EXPLORE DATA STRUCTURE
********************************************************************

* 1.1 Game-level data: each observation is a home-away game
* Option 1: Load from local repository
use "${data_in}/epl_games.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile games_data
copy "https://osf.io/download/tyjp8/" `games_data'
use `games_data', clear
*/

* Focus on 2016 season
keep if season == 2016
sort season team_home


* 1.2 Team-game level data: each observation is a team in a game
* Option 1: Load from local repository
use "${data_in}/epl_teams_games.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile teamgames_data
copy "https://osf.io/download/qzvx7/" `teamgames_data'
use `teamgames_data', clear
*/

* Focus on 2016 season, sorted by date
keep if season == 2016
sort season team date


* 1.3 Manager information: manager characteristics
* Option 1: Load from local repository
use "${data_in}/football-managers.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile managers_data
copy "https://osf.io/download/w6uph/" `managers_data'
use `managers_data', clear
*/


********************************************************************
* PART 2: ANALYZE MANAGER SUCCESS
********************************************************************

* 2.1 Load merged workfile with manager-game level data
* Option 1: Load from local repository
use "${data_in}/football-managers-workfile.dta", clear

* Option 2: Download directly from OSF (uncomment to use)
/*
tempfile workfile
copy "https://osf.io/download/hycmg/" `workfile'
use `workfile', clear
*/

sort season team

* 2.2 Calculate manager performance metrics
* For each manager-team combination:
* - Total games managed
* - Total points earned
* - Win ratio (points per game)

bysort team manager_id: egen manager_games = count(_n)
bysort team manager_id: egen manager_points = sum(points)
generate manager_win_ratio = manager_points / manager_games

* 2.3 Collapse to manager-team level (one row per manager-team)
collapse (mean) manager_games manager_points manager_win_ratio, ///
         by(manager_name team)

* 2.4 Sort by performance
gsort -manager_win_ratio

* Display top performers (win ratio >= 2)
list if manager_win_ratio >= 2


********************************************************************
* PART 3: VISUALIZE TOP MANAGERS
********************************************************************

* 3.1 Identify caretaker managers (< 18 games)
* Create separate variables for regular vs caretaker managers
separate manager_win_ratio, by(manager_games < 18)
* manager_win_ratio0 = regular managers (>= 18 games)
* manager_win_ratio1 = caretaker managers (< 18 games)

* 3.2 Create horizontal bar chart for top managers
* Focus on managers with win ratio >= 2 points per game
colorpalette viridis, n(4) select(2) nograph

graph hbar (mean) manager_win_ratio0 manager_win_ratio1 if manager_win_ratio>=2, ///
 nofill over(manager_name, sort(manager_win_ratio) descending) ///
 scheme(viridis) ///
 legend(off) yscale(r(1.6(0.2)3)) exclude0  ylabel(1.6(0.2)3, grid)  ///
  yline(1.6(0.2)3) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))
graph export "${output}/ch02-figure1-top-managers-Stata.png", replace
