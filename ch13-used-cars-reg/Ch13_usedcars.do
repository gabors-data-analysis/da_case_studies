*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* PREDICTION
* Used car data for LA and Chicago
* prep - cleans the data, makes it ready for work

* v1.3 2018-12-01
* v1.4 2020-01-01 edits to tables, cuts
* v1.5 2020-01-03 minor edits to models, prediction

********************************************************************
*
*********************************************************************

* WHAT THIS CODES DOES:

* Models
* Measure of fits
* Cross validation

* !! need to install matmap



********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cap cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"
cap cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
cap cd "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook"

 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in	 "da_data_repo/used-cars/clean" 
global output    "da_case_studies/ch13-used-cars-reg/output"
global data_out  "da_case_studies/ch13-used-cars-reg"



use "$data_in/used-cars_2cities_prep.dta", clear


******************************************
* Sample design
******************************************



drop if Hybrid==1
drop Hybrid

tab fuel
keep if fuel=="gas"

tab condition
drop if condition=="new"
drop if condition=="fair"

* drop very small prices, likely error
drop if price<500
drop if price>25000
drop if odometer>100
drop if price<1000 & (condition=="like new" | age<8)
drop if price==.

tab transmission
drop if transmission =="manual"
drop pricestr

tab type
drop if type=="truck"

tab area
gen chicago=area=="chicago"
keep if chicago==1



*********************************************
*********************************************
* DESCRIPTIVES

*********************************************
* condition
gen cond_excellent = condition=="excellent"
gen cond_good = condition=="good"
gen cond_likenew = condition=="like new" 

*********************************************
* cylinders
gen cylind6 = cylinders=="6 cylinders"



* price: quadratic
gen agesq=age^2
gen agecu=age^3
gen odometersq=odometer^2

saveold "$data_out/usedcars_work.dta", replace

* Explorative data analysis; results are not in the textbook

 hist price, percent color(emidblue) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\F13_h_price.png", replace

hist lnprice, percent color(emidblue) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output\F13_h_lnprice.png", replace
more
 
table condition, c(freq mean price)
table drive, c(freq mean price)
table dealer, c(freq mean price)
table area, c(freq mean price)

 ************************
 * regressions
 ************************
 
* models	
local X1 age agesq 
local X2 age agesq odometer 
local X3 age agesq odometer odometersq        LE              cond_excellent cond_good         dealer 
local X4 age agesq odometer odometersq LE XLE SE cond_likenew cond_excellent cond_good cylind6 dealer 
local X5 age agesq agecu odometer odometersq i.LE##c.age i.XLE##c.age i.SE##c.age i.cond_likenew##c.age i.cond_excellent##c.age cond_good c.age##i.cylind6 c.age##c.odometer dealer dealer##c.age 


* Model 1
 quietly reg price age agesq, robust
	local r2a = e(r2_a)
	local sigmahat = e(rmse) 
	estat ic
	matrix out=r(S)
	local BIC=out[1,6]
	outreg2 using "$output/Ch13_multireg1.tex", nose bdec(2) noaster ctitle(Model 1) ///
	  tex(frag) replace 
/*	outreg2 using "$output/Ch13_multireg_sum.tex", nose bdec(2) noaster ctitle(Model 1) ///
	  tex(frag) replace  nocons keep(age) ///
	  addtext(age, YES, odometer, NO, basic_state_vars, NO, more_state_vars, NO, interactions, NO)
*/

* Models 2 to 4
forval v=2/4 {
quietly reg price `X`v'', robust
	local r2a = e(r2_a)
	local sigmahat = e(rmse)
	estat ic
	matrix out=r(S)
	local BIC=out[1,6]
	outreg2 using "$output/Ch13_multireg1.tex", nose bdec(2) noaster ctitle(Model `v') ///
		tex(frag) append  
	*outreg2 using "$output/Ch13_multireg_sum.tex", nose bdec(2) noaster ctitle(Model `v') tex(frag) ///
	*	append addstat(Adj.R-squared, `r2a', RMSE, `sigmahat', BIC, `BIC') nocons  keep(age)  addtext(age, YES, odometer, YES, basic_state_vars, -, more_state_vars, -, interactions, -)
}

* Model 5 (not in regression table)
 quietly reg price `X5', robust
	local r2a = e(r2_a)
	local sigmahat = e(rmse) 
	estat ic
	matrix out=r(S)
	local BIC=out[1,6]
/*	outreg2 using "$output/Ch13_multireg_sum.tex", nose bdec(2) noaster ctitle(Model 5) ///
		tex(frag) replace addstat(Adj.R-squared, `r2a', RMSE, `sigmahat', BIC, `BIC') ///
		nocons keep(age)  addtext(age, YES, odometer, NO, basic_state_vars, NO, more_state_vars, NO, interactions, NO)
*/


* CV of k
set seed 13505
local k=4
forval v=1/5 {

crossfold reg price `X`v'', robust k(`k') loud
matrix list r(est)
matrix mm`v'=r(est)

	*
}
matrix crossval= [mm1 , mm2, mm3, mm4, mm5 ]
mat U = J(rowsof(crossval),1,1)
matmap crossval crossval_sq, map(@^2) 
mat sum = U'*crossval_sq
matmap sum meanvec, map(sqrt(@))

*mat meanvec = sum/rowsof(crossval)
mat crossval2=(crossval \ meanvec)
matrix rownames crossval2 = estimate1 estimate2 estimate3 estimate4 average
matrix colnames crossval2 = model1 model2 model3 model4 model5
mat lis crossval2

* need to save this to tex



*********************************************
* PREDICTION for a our type of car
*********************************************


* generate  new observation with features our car
local nplus1=_N+1
set obs `nplus1'
replace age=10 if _n==`nplus1'
replace agesq=10^2 if _n==`nplus1'
replace odometer=12 if _n==`nplus1'
replace odometersq=12^2 if _n==`nplus1'
replace LE=1 if _n==`nplus1'
replace XLE=0 if _n==`nplus1'
replace SE=0 if _n==`nplus1'
replace cond_likenew=0 if _n==`nplus1'
replace cond_excellent=1 if _n==`nplus1'
replace cond_good=0 if _n==`nplus1'
replace cylind6=0 if _n==`nplus1'
replace dealer=0 if _n==`nplus1'
lis if _n==`nplus1'



* age only
reg price age agesq
 predict p1
 predict p1_pse, stdf
 gen p1_PIlow  = p1 - 1.96*p1_pse
 gen p1_PIhigh = p1 + 1.96*p1_pse

 
* all predictors based on model 3
reg price age agesq odometer odometersq LE cond_excellent cond_good dealer

 predict p2
	 gen resid_p=p2-price
		sum resid_p /*to capture SD of residual*/
 
 predict p2_pse, stdf
 gen p2_PIlow  = p2 - 1.96*p2_pse
 gen p2_PIhigh = p2 + 1.96*p2_pse

 sum p1* if _n==`nplus1'
 sum p2* if _n==`nplus1'


 

