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
global work  	"ch02-football-manager-success"

cap mkdir 		"$work/output"
global output 	"$work/output"


*********************************************************************
* look at entire clean data table on games
use "$data_in/epl_games",clear
sort team_home
sort season  team_home
keep if season == 2016
browse

* look at data for team-game level
use "$data_in/epl_teams_games",clear
sort team
sort season  team
keep if season == 2016
sort date 
browse

* look at data table on managers
use "$data_in/football_managers.dta", clear
browse

* look at the clean merged file 
use "$data_in/football_managers_workfile.dta", clear
sort season team
browse

gen a=1
bys team manager_id: egen manager_games=sum(a)
bys team manager_id: egen manager_points=sum(points)
gen manager_win_ratio=manager_points /manager_games

collapse (mean) manager_games manager_points manager_win_ratio  , by (manager_name team)
sort manager_win_ratio

gsort -manager_win_ratio

list if manager_win_ratio>=2


* denote caretakers
separate manager_win_ratio, by(manager_games<18)

colorpalette viridis, n(4) select(2) nograph
graph hbar (mean) manager_win_ratio0 manager_win_ratio1 if manager_win_ratio>=2, ///
 nofill over(manager_name, sort(manager_win_ratio) descending) ///
 scheme(virdis) ///
 legend(off) yscale(r(1.6(0.2)3)) exclude0  ylabel(1.6(0.2)3, grid)  ///
  yline(1.6(0.2)3) ///
 graphregion(fcolor(white) ifcolor(none)) ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch02-figure1-top-managers-Stata.png", replace

