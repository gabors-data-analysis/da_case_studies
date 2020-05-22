

use ch20-wfh-workfile, clear


*******************************************************
* Balance


*des perform10 age-grosswage

replace ageyoungest = . if children==0


* table with means and sd
* tabstat in Stata, copied to Excel, 
* to laTex used https://www.latex-tables.com/
tabstat perform10 age-grosswage ordertaker  if treatment==1, c(s)
tabstat perform10 age-grosswage ordertaker  if treatment==0, c(s)
tabstat perform10 age-grosswage ordertaker, s(sd) c(s)


* t-tests for equal means (we do them by regression for simplicity)
* need to enter p-values one by one to LaTex or Excel table
foreach z of varlist perform10 age-grosswage ordertaker {
	reg treatment `z', robust nohead
}



*******************************************************
* outcomes: 
* quit firm during 8 months of experiment
* # phone calls worked, for order takers

des quit phonecalls1


tabstat quit , by(treatment) s(mean sd n)
tabstat phonecalls1 if ordertaker==1, by(treatment) s(mean sd n)

***************************
* Bar chart for quit rates
gen quit_pct = quitjob*100
gen stayed_pct = (1-quitjob)*100
lab def treatment 0 "non-treatment group" 1 "treatment group"
lab val treatment treatment
graph bar (mean) stayed_pct quit_pct, over(treatment) stack ///
 bar(1, col(navy)) bar(2, col(ltblue)) ///
 ytitle("Percent of employees") ///
 legend(label(1 "stayed") label(2 "quit"))
 graph export wfh-quitrates-barchart.png, replace


 
***************************
* Regression 1: ATE estimates, no covariates
lab var treatment "Treatment group"
lab var quitjob "Quit job "
lab var phonecalls1 "Phone calls (thousand)"
la var married "Married"
lab var children "Children"
lab var internet "Internet at home"


reg quitjob treatment , robust
 outreg2 using ch20-wfh-reg1, bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

reg phonecalls1 treatment if ordertaker==1, robust
 outreg2 using ch20-wfh-reg1, bdec(1) sdec(2) 2aster tex(frag) nonotes label append
 

***************************
* Regression 2: ATE estimates, with covariates of some unbalance

reg quitjob treatment married children internet, robust
 outreg2 using ch20-wfh-reg2, bdec(2) sdec(3) 2aster tex(frag) nonotes label replace

reg phonecalls1 treatment married children if ordertaker==1, robust
 outreg2 using ch20-wfh-reg2, bdec(1) sdec(2) 2aster tex(frag) nonotes label append
 
 
/* Regression 3: ATE estimates, order takers, 
* condition in orevious phone calls, and then all covariates
* (Data exercise)

reg phonecalls1 treatment if ordertaker==1, robust
 outreg2 using ch20-wfh-reg3, bdec(1) sdec(2) 2aster tex replace
reg phonecalls1 treatment phonecalls0 if ordertaker==1, robust
 outreg2 using ch20-wfh-reg3, bdec(1) sdec(2) 2aster tex append
reg phonecalls1 treatment phonecalls0 married children if ordertaker==1, robust
 outreg2 using ch20-wfh-reg3, bdec(1) sdec(2) 2aster tex append
 
 
 
