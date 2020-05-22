*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* CH03
* Home field advantage
* football dataset
*
*
* WHAT THIS CODES DOES:
* creates desrciptive stats
* 
********************************************************************





********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

*location folders
global data_in "da_data_repo\football\clean"
global data_out "da_case_studies\ch03\football"
global output "da_case_studies\ch03\football\output"



* use full 11 season data table on games
use "$data_in\epl_games.dta", clear
keep if season == 2016
gen home_goaladv= goals_home- goals_away

* summary stat
su home_goaladv, det
tab home_goaladv


* look at histogram
histogram home_goaladv,  ///
	discrete percent addlabel ///
	ytitle(Share of games in %) ylabel(0(4)24, format(%9.0f)) xtitle(Goal difference) ///
	xlabel(-6(1)6) addlabopts(yvarformat(%4.1f))  ///
	fcolor(emerald) lcolor(emerald) gap(1)  ///
	graphregion(fcolor(white) ifcolor(none))  plotregion(fcolor(white) ifcolor(white)) 	
graph export "$output/homeadvantage_hist.png", as(png) replace



* look at goal advantage by team 
* table used in book, but interesting
tabout team_home  if inlist(team_home, "Chelsea", "Arsenal", "Leicester", "Stoke", "West Ham")  ///
using "$output/homeadv_byteams.tex", replace ///
style(tex) font(bold)    npos(lab) sum format( 2 0 2 0 0) ///
c(mean home_goaladv median home_goaladv sd home_goaladv min home_goaladv max home_goaladv) /// 
 clab(Mean Median Std.dev. Min max ) 

