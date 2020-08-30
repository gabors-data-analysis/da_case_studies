*********************************************************************
*
* GABORS' DATA ANALYSIS TEXTBOOK (Bekes & Kezdi)
*
* Case study 03D
* Distributions of body height and income
*
* using the height-income-distributions dataset
* 
********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************

* Directory for work
cd "C:\Users\kezdi\GitHub\da_case_studies" 
global work  "ch02-football-manager-success"
cap mkdir "$work/output"
global output "$work/output"
cap mkdir "$work/temp"
global temp "$work/temp"

* Directory for data
* Option 1: run directory-setting do file
*do "set-data-directory.do" /*data_dir must be first defined */
*global data_in   	"$da_data_repo/football/clean"
* Option 2: set directory here
global data_in "C:/Users/kezdi/Dropbox/bekes_kezdi_textbook/da_data_repo/football/clean"

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
graph export "$output/ch02-figures1-top-managers-Stata.png", replace

