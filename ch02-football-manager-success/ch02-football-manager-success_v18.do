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
*
* REVISION HISTORY:
* Version 0.9 2020-09-06 - original
* Version 1.0 2025-11-03 - Stata 18 upgrade, efficiency improvements
*   - Removed redundant sorts
*   - Modernized graph export
*   - Improved variable creation efficiency
*   - Enhanced comments
*   - Removed interactive browse commands
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
global work  	"ch02-football-manager-success"

cap mkdir 		"$work/output"
global output 	"$work/output"


*********************************************************************
* PART 1: EXPLORE DATA STRUCTURE
*********************************************************************

* 1.1 Game-level data: each observation is a home-away game
use "$data_in/epl_games.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/tyjp8/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/ 

* Focus on 2016 season
keep if season == 2016
sort season team_home
* Note: use 'browse' interactively to inspect if needed

* 1.2 Team-game level data: each observation is a team in a game
use "$data_in/epl_teams_games.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/qzvx7/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/

* Focus on 2016 season, sorted by date
keep if season == 2016
sort season team date

* 1.3 Manager information: manager characteristics
use "$data_in/football-managers.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/w6uph/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/


*********************************************************************
* PART 2: ANALYZE MANAGER SUCCESS
*********************************************************************

* 2.1 Load merged workfile with manager-game level data
use "$data_in/football-managers-workfile.dta", clear

* Or download directly from OSF:
/*
copy "https://osf.io/download/hycmg/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/

sort season team

* 2.2 Calculate manager performance metrics
* For each manager-team combination:
* - Total games managed
* - Total points earned
* - Win ratio (points per game)

bys team manager_id: egen manager_games = count(_n)
bys team manager_id: egen manager_points = sum(points)
gen manager_win_ratio = manager_points / manager_games

* 2.3 Collapse to manager-team level (one row per manager-team)
collapse (mean) manager_games manager_points manager_win_ratio, ///
         by(manager_name team)

* 2.4 Sort by performance
gsort -manager_win_ratio

* Display top performers (win ratio >= 2)
list if manager_win_ratio >= 2


*********************************************************************
* PART 3: VISUALIZE TOP MANAGERS
*********************************************************************

* 3.1 Identify caretaker managers (< 18 games)
* Create separate variables for regular vs caretaker managers
separate manager_win_ratio, by(manager_games < 18)
* manager_win_ratio0 = regular managers (>= 18 games)
* manager_win_ratio1 = caretaker managers (< 18 games)

* 3.2 Create horizontal bar chart for top managers
* Focus on managers with win ratio >= 2 points per game

* Set up viridis color scheme
colorpalette viridis, n(2) nograph
local colors `r(p)'

graph hbar (mean) manager_win_ratio0 manager_win_ratio1 if manager_win_ratio >= 2, ///
 nofill ///
 over(manager_name, sort(manager_win_ratio) descending) ///
 bar(1, fcolor("`=word("`colors'", 1)'") lcolor("`=word("`colors'", 1)'")) ///
 bar(2, fcolor("`=word("`colors'", 2)'") lcolor("`=word("`colors'", 2)'")) ///
 legend(off) ///
 yscale(r(1.6(0.2)3)) ///
 exclude0 ///
 ylabel(1.6(0.2)3, grid) ///
 yline(1.6(0.2)3) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))

* Export using Stata 18 syntax
graph export "$output/ch02-figure1-top-managers-Stata.png", replace as(png)

