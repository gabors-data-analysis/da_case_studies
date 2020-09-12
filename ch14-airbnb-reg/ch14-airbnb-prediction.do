*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* MODEL SELECTION
* ILLUSTRATION STUDY
* BAirbnb London 2017 march 05 data
*

* v 2019.01.01
* edit: 2019-07-23 
*********************************************************************

* WHAT THIS CODES DOES:

* Models
* Measure of fits
* Cross validation

*********************************************************************

* need to load this package
* ssc install elasticregress, replace

cd "C:\Users\GB\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

global data_in	 "cases_studies_public/airbnb/clean" 
global output    "textbook_work\ch14\airbnb\output"
global data_out  "textbook_work\ch14\airbnb"



clear
import delimited "$data_in\airbnb_hackney_workfile_adj.csv", varnames(1) encoding(utf8) 
destring n_*, replace force

cap log close
log using "$output/hackney_pred1.txt", replace

 ******************
 *
 * Setting up models
 *
 *******************

global basic_lev n_accommodates n_beds i.f_property_type i.f_room_type n_days_since 
global basic_log ln_accommodates ln_beds i.f_property_type i.f_room_type ln_days_since 

global basic_add i.f_bathroom i.f_cancellation_policy i.f_bed_type 
global reviews i.f_number_of_reviews n_review_scores_rating 
global poly_lev n_accommodates2 n_days_since2 n_days_since3
global poly_log ln_accommodates2 ln_days_since2 ln_days_since3
 

global amenities d_*
global X1  i.f_room_type##i.f_property_type i.f_number_of_reviews##i.f_property_type 
global X2  i.d_airconditioning##i.f_property_type i.d_cats##i.f_property_type i.d_dogs##i.f_property_type 
global X3  c.(d_airconditioning - d_wirelessinternet)# c.(f_property_type - f_bed_type)
 

* models in levels
local modellev1 n_accommodates 
local modellev2 $basic_lev
local modellev3 $basic_lev $basic_add
local modellev4 $basic_lev $basic_add $reviews $poly_lev
local modellev5 $basic_lev $basic_add $reviews $poly_lev $X1
local modellev6 $basic_lev $basic_add $reviews $poly_lev $X1 $X2
local modellev7 $basic_lev $basic_add $reviews $poly_lev $X1 $X2 $amenities
local modellev8 $basic_lev $basic_add $reviews $poly_lev $X1 $X2 $amenities $X3


* models in logs
local modellog1 ln_accommodates 
local modellog2 $basic_log
local modellog3 $basic_log $basic_add
local modellog4 $basic_log $basic_add $reviews $poly_log
local modellog5 $basic_log $basic_add $reviews $poly_log $X1
local modellog6 $basic_log $basic_add $reviews $poly_log $X1 $X2
local modellog7 $basic_log $basic_add $reviews $poly_log $X1 $X2 $amenities
local modellog8 $basic_log $basic_add $reviews $poly_log $X1 $X2 $amenities $X3

gen Ylev=price
gen Ylog=ln_price

* our sample
keep if n_review_scores_rating<.
count

*** FIRST: ENTIRE_SAMPLE REGRESSIONS , BIC
forvalue i=1/8 {
	dis "`i'"
	qui regress Ylev `modellev`i''
	dis "R-squared = " e(r2)
	dis "RMSE = " e(rmse)
	estimat stats
}



*** CREATE TRAINING-TEST SAMPLES

  cap drop test* train* 
  cap drop temp r
  set seed 13505
gen temp=runiform()
sort temp
gen r=_n/_N

local k=10

gen test=r<(1/`k')
gen train=1-test

forval j = 1/ `k' {

di "(`j'-1)/`k'" "and " "`j'/`k'"
gen test`j'= r>=(`j'-1)/`k' & r<`j'/`k'
}


forvalue i=1/`k' {
	gen train`i'=1-test`i'
}
*

  ******************
  *
  * Predictions with various models
  *
  *******************

********************************************************** 
*** IN-SAMPLE EVALUATION
*** & OUT-OF SAMPLE EVALUATION ON ONE SAMPLE

foreach v in lev log {
	 reg Y`v' `model`v'1' if train==1
	  do "$data_out/BIC"
	  do "$data_out/RMSE_`v'"
	  global df=e(df_m)
	 outreg2 using "$output/reg_in_onetest_`v'", excel se addstat(nvars, $df, BIC, $BIC, RSMSE_insample, $RMSEin, RSMSE_onetest, $RMSEtest) noaster label replace 
	forvalue i=2/8 {
		  reg Y`v' `model`v'`i'' if train==1
		   do "$data_out/BIC"
		  do "$data_out/RMSE_`v'"
		  global df=e(df_m)
		 outreg2 using "$output/reg_in_onetest_`v'", excel se addstat(nvars, $df, BIC, $BIC, RSMSE_insample, $RMSEin, RSMSE_onetest, $RMSEtest) noaster label append
	}
} 
************************************************************ 
	foreach v in lev log {
	noisily dis "RMSE in test sets"
	noisily dis "`v'" " " "model1"
	forvalue t=1/`k' {
		reg Y`v' `model`v'1' if train`t'==1
		replace test=test`t'
		do "$data_out/BIC"
		local BIC`t'=$BIC
		do "$data_out/RMSE_`v'"
		gen y`v'_m1_t`t'=ytest
		gen e`v'_m1_t`t'=ytest-Ylev
		local MSEin`t'=$MSEin
		local MSEtest`t'=$MSEtest
		local RMSEin`t'=sqrt(`MSEin`t'')
		local RMSEtest`t'=sqrt(`MSEtest`t'')
	
		noisily dis "RMSE test of fold `t' "  $RMSEtest`t'
	
	
	}
	local BIC`v'=(`BIC1'+`BIC2'+`BIC3'+`BIC4'+`BIC5'+`BIC6'+`BIC7'+`BIC8'+`BIC9'+`BIC10')/10
	local MSEin`v'=(`MSEin1'+`MSEin2'+`MSEin3'+`MSEin4'+`MSEin5'+`MSEin6'+`MSEin7'+`MSEin8'+`MSEin9'+`MSEin10')/10
	local MSEtest`v'=(`MSEtest1'+`MSEtest2'+`MSEtest3'+`MSEtest4'+`MSEtest5'+`MSEtest6'+`MSEtest7'+`MSEtest8'+`MSEtest9'+`MSEtest10')/10
	local RMSEin`v'=sqrt(`MSEin`v'')
	local RMSEtest`v'=sqrt(`MSEtest`v'')
	local RMSEtestM`v'=sqrt(max(`MSEtest1',`MSEtest2',`MSEtest3',`MSEtest4',`MSEtest5',`MSEtest6',`MSEtest7',`MSEtest8',`MSEtest9',`MSEtest10'))


	matrix `v'1=(`BIC`v'' \ `RMSEin`v'' \ `RMSEtest`v'' \ `RMSEtest1' \ `RMSEtestM`v'' )
	mat lis `v'1
	forvalue i=2/8 {
		noisily dis "`v'" "  " " model`i'"
		forvalue t=1/`k' {
			reg Y`v' `model`v'`i'' if train`t'==1
			 	replace test=test`t'
			do "$data_out/BIC"
			local BIC`t'=$BIC
			do "$data_out/RMSE_`v'"
			gen y`v'_m`i'_t`t'=ytest
			gen e`v'_m`i'_t`t'=ytest-Ylev  /* NEW LINE */
			local MSEin`t'=$MSEin
			local MSEtest`t'=$MSEtest
			*ide kell sqrt
			local RMSEin`t'=sqrt(`MSEin`t'')
			local RMSEtest`t'=sqrt(`MSEtest`t'')
			noisily dis "RMSE test of fold `t' "  $RMSEtest`t'
		}

	local BIC`v'=(`BIC1'+`BIC2'+`BIC3'+`BIC4'+`BIC5'+`BIC6'+`BIC7'+`BIC8'+`BIC9'+`BIC10')/10
	local MSEin`v'=(`MSEin1'+`MSEin2'+`MSEin3'+`MSEin4'+`MSEin5'+`MSEin6'+`MSEin7'+`MSEin8'+`MSEin9'+`MSEin10')/10
	local MSEtest`v'=(`MSEtest1'+`MSEtest2'+`MSEtest3'+`MSEtest4'+`MSEtest5'+`MSEtest6'+`MSEtest7'+`MSEtest8'+`MSEtest9'+`MSEtest10')/10
	local RMSEin`v'=sqrt(`MSEin`v'')
	local RMSEtest`v'=sqrt(`MSEtest`v'')
	local RMSEtestM`v'=sqrt(max(`MSEtest1',`MSEtest2',`MSEtest3',`MSEtest4',`MSEtest5',`MSEtest6',`MSEtest7',`MSEtest8',`MSEtest9',`MSEtest10'))

		
		matrix `v'`i'=(`BIC`v'' \ `RMSEin`v'' \ `RMSEtest`v'' \ `RMSEtest1' \ `RMSEtestM`v'' )
		mat lis `v'`i'
	}
	matrix crossval_`v' = [`v'1 , `v'2 , `v'3 , `v'4 , `v'5 , `v'6 , `v'7 , `v'8 ]
	matrix rownames crossval_`v' = BIC RMSEin RMSEtestCV RMSEtest1 RMSEtest_max
	matrix colnames crossval_`v' = model1 model2 model3 model4 model5 model6 model7 model8
}

mat lis crossval_lev
mat lis crossval_log

* do lasso 

lassoregress ln_price $basic_log $basic_add $reviews $poly_log $X1 $X2 $amenities $X3
di "N: " `e(N)' " RMSE:"  (`e(cvmse_actual)')^0.5

lassoregress price $basic_lev $basic_add $reviews $poly_lev $X1 $X2 $amenities $X3
di "N: " `e(N)' " RMSE:"  (`e(cvmse_actual)')^0.5

*******************************
*** FIGURES FOR FITTED VS ACTUAL OOUTCOME VARIABLES 
***********************************
local m 7
sum Ylev if test5==1,d
local meanY=r(mean)
local sdY=r(sd)
local meanY_m2SE=`meanY'-2*`sdY'
local meanY_p2SE=`meanY'+2*`sdY'
local Y5p=r(p5)
local Y95p=r(p95)

sum Ylog if test5==1,d
local meanY=r(mean)


scatter ylog_m`m'_t5 ylev_m`m'_t5 Ylev if Ylev<400 , ms(Oh x)  ///
 || lfit ylev_m`m'_t5 Ylev if Ylev<400, lw(vthick) lp(dash) ///
 || lfit ylog_m`m'_t5 Ylev if Ylev<400, ///
 	 xtitle("Price") ///
	 ytitle("Predicted price") ///
 	 graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) 
	 graph export "$output\log_vs_lin_all.png", as(png) replace

	 scatter ylog_m`m'_t5 ylev_m`m'_t5 Ylev if Ylev>`Y5p' & Ylev<`Y95p', ms(Oh x)  ///
 || lfit ylev_m`m'_t5 Ylev if Ylev>`Y5p' & Ylev<`Y95p', lw(vthick) lp(dash) ///
 || lfit ylog_m`m'_t5 Ylev if Ylev>`Y5p' & Ylev<`Y95p', ///
 	 xtitle("Price") ///
	 ytitle("Predicted price") ///
 	 graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) 
	 graph export "$output\log_vs_lin_95.png", as(png) replace

	 
scatter ylev_m`m'_t5 Ylev if Ylev>`Y5p' & Ylev<`Y95p', ms(Oh x)  ///
 || lfit ylev_m`m'_t5 Ylev if Ylev>`Y5p' & Ylev<`Y95p', lw(vthick) lp(dash)  ///
 || lfit Ylev Ylev if Ylev>`Y5p' & Ylev<`Y95p',  ///
 	 xtitle("Price") ///
	 ytitle("Price, predicted price") ///
 	  graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white)) ///
legend(row(1) lab(1 "Predicted") lab(2 "45 degree line") lab( 3 "Fitted values"))
graph export "$output\lev_vs_45.png", as(png) replace


scatter elev_m7_t5 ylev_m7_t5, ylab(,grid) ///
	 ms(X) msize(small) mlw(thick) mcolor(orange) ///
	 xtitle("Price") ///
	 ytitle("Prediction error") ///
	 || lfit elev_m7_t5 ylev_m7_t5, lw(vthick) lc(navy) legend(off) ///
	  graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white))
	 graph export "$output/F14_preerr1.png", as(png) replace

scatter elev_m7_t5 ylev_m7_t5 if ylev_m7_t5>20 & ylev_m7_t5<200 & elev_m7_t5>-50 & elev_m7_t5<50   , ///
	 ms(X) msize(small) mlw(thick) mcolor(orange) ///
	xlab(20(20)200) ylab(-50(50)50, grid) ///
 	 xtitle("Price") ///
	 ytitle("Prediction error") ///
	 || lfit elev_m7_t5 ylev_m7_t5 if ylev_m7_t5>20 & ylev_m7_t5<200 & elev_m7_t5>-50 & elev_m7_t5<50   , lw(vthick) lc(navy) legend(off) ///
	  graphregion(fcolor(white) ifcolor(none))  ///
	 plotregion(fcolor(white) ifcolor(white))
	 graph export "$output/F14_preerr1_zoom.png", as(png) replace


log close
