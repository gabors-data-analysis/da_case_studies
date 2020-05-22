*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH02
* Teams and Managers infootball (ENGLISH PREMIER LEAGUE SEASONS)
*
* 
********************************************************************

* WHAT THIS CODES DOES:
* opens data tables, and creates a single graph

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data

*location folders
global data_in   	"da_data_repo/football/clean"
global data_out  	"da_case_studies/ch02-football-manager-success"
global output 		"da_case_studies/ch02-football-manager-success/output"

* look at basic data
use "$data_in/epl-games",clear
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

* now look at the managers
use "$data_in/football_managers.dta", clear
browse

* finally the merged file 
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

* graph top managers

* TODO 
* it should be manager (team, # games), "Pep Guardiola (Man City, 114)" 
* R: colors should be color1 and color3 of our viridis, instead of vertical lines, usual grid
 
* denote caretakers
separate manager_win_ratio, by(manager_games<18)

graph hbar (mean) manager_win_ratio0 manager_win_ratio1 if manager_win_ratio>=2, ///
 nofill over(manager_name, sort(manager_win_ratio) descending) ///
 scheme(virdis) ///
 legend(off) yscale(r(1.6(0.2)3)) exclude0  ylabel(1.6(0.2)3, grid)  ///
  yline(1.6(0.2)3, lpattern(solid) lcolor(gray%70)) ///
graphregion(fcolor(white) ifcolor(none))   plotregion(fcolor(white) ifcolor(white))
graph export "$output/03_top_managers.png", replace

