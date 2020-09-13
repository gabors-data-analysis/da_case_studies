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
* Chapter 14
* CH014 Predicting Airbnb apartment prices: selecting a regression model
*
* Prepare data
*
* using the airbnb dataset
* version 0.9 2020-09-06
********************************************************************


* SETTING UP DIRECTORIES

* STEP 1: set working directory for da_case_studies.
* for example:
* cd "C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies"
cd "C:/Users/kezdi/Github/da_case_studies"

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


global data_in  "$data_dir/airbnb/clean"
global work  	"ch14-airbnb-reg"

cap mkdir 		"$work/output"
global output 	"$work/output"



clear



* need to download
* http://data.insideairbnb.com/united-kingdom/england/london/2017-03-04/data/listings.csv.gz
* unzip
* save to raw data folder 
/*
clear
import delimited "$data_in\listings.csv", bindquote(strict) 
drop last_scraped description experiences_offered neighborhood_overview notes transit access interaction house_rules host_about host_response_time name summary space host_location
drop *_url
export delimited using "$data_in\airbnb_london_listing", replace
save                   "$data_in\airbnb_london_listing.dta", replace
*/


* load in data
clear
import delimited "$data_in/airbnb_london_cleaned.csv", bindquote(strict) 

*****************
* sample design
* keep Hackney borough 
keep if neighbourhood_cleansed=="Hackney"

* quantitative variables

*search renvars /* nice program to rename variables; 
				click on appropriate link on screen to install */

rename price temp
destring temp, gen(price) ignore(",")

rename cleaning_fee usd_cleaning_fee 

destring host_response_rate, gen (p_host_response_rate) force

tab accommodates
destring accommodates, gen (n_accommodates) force

tab bathrooms
destring bathrooms, gen(n_bathrooms) force


destring review_scores_rating, force gen(n_review_scores_rating)
destring number_of_reviews   , force gen(n_number_of_reviews)
destring guests_included     , force gen(n_guests_included)

codebook first_review
destring reviews_per_month  , force gen (n_reviews_per_month)

renvars extra_people minimum_nights beds, prefix (n_)


* create days since first review
gen n_days_since= date(calendar_last_scraped,"YMD")-date(first_review,"YMD")


* factor (qualitative) vars
tab property_type
replace property_type="House" if property_type=="Townhouse"
*****************
* sample design
* keep only houses and apartments
keep if inlist(property_type, "Apartment", "House" ) 
count


encode property_type, gen( f_property_type)

tab room_type
encode room_type , gen(f_room_type)

tab cancellation_policy 
replace cancellation_policy="strict" if cancellation_policy=="super_strict_30" | cancellation_policy=="super_strict_60"
encode cancellation_policy , gen(f_cancellation_policy)


tab bed_type
replace bed_type="Couch" if bed_type=="Futon" | bed_type=="Pull-out Sofa" | bed_type=="Airbed"
encode bed_type, gen( f_bed_type)

encode neighbourhood_cleansed, gen (f_neighbourhood_cleansed)



* dummy vars
renvars hourcheckin - wirelessinternet, prefix(d_)

keep price d_* n_* f_* p_* usd_* neighbourhood_cleansed neighbourhood_cleansed cancellation_policy room_type property_type

order price n_* p_* f_* d_*

* DEAL WITH MISSING DATA, QUANTITATIVE VARS

global HASMISSING bathrooms beds review_scores reviews_per_month 

* where no review, replace days since first review as zero
replace n_days_since = 0 if n_days_since==.

* other vairiables: create flag, impute mean value
foreach x in $HASMISSING {
	gen d_missing_`x' = n_`x'==.
	sum n_`x'
	replace n_`x' = r(mean) if n_`x'==.
}
save "$work/airbnb_hackney_workfile.dta" , replace
count


* EXPLORATORY DATA ANALYSIS, FEATURE ENGINEERING
* some more sample design, dropping very large apartments

*********************
* price
su price, d
gen lnprice=log(price)

*****************
* sample design
count
drop if price==.
keep if price<1000 /* in R it's <=1000; it doesn't matter there but it matters here, due to differences in data storage */
count
drop if n_accommodates>7 | n_accommodates==.
count

* Fig 14.3a
hist price if price<=400, width(10) percent color(navy*0.8) lcolor(white) ///
 xlab(0(50)400, grid) ylab(0(5)15, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch14-figure-3a-hist-price-Stata.png", replace
 
* Fig 14.3b
hist lnprice, percent width(0.2) color(navy*0.8) lcolor(white) ///
 xlab(2(0.5)6.5, grid) ylab(0(5)15, grid) ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/ch14-figure-3b-hist-lnprice-Stata.png", replace
 
*******************************************************
* other feature engineering, functional forms

*****************
* sample design
* drop if very large (accommodates>7)
drop if n_accommodates>7 | n_accommodates==.

* size (number of people apartment can accommodate)
estpost tabstat price, by( n_accommodates ) stat(count mean min max) columns(statistics) listwise
esttab using "$output/t_n_accommodates.rtf", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of people accomodated")
esttab using "$output/t_n_accommodates.tex", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of people accomodated")

gen n_accommodates2= n_accommodates^2
reg lnprice n_accommodates n_accommodates2 

gen ln_accommodates=ln(n_accommodates)
gen ln_accommodates2= ln_accommodates^2
reg lnprice ln_accommodates 
reg lnprice n_accommodates 

* lowess with scatterplot
* Graph not in the textbook
lowess price n_accommodates if price<=800, bwidth(0.8) ///
 lineopts(lw(vthick) lc(dkgreen)) ///
 ms(O) mlw(thick) mcolor(navy*0.6) ///
 xlab(0(1)7) yscale(range(0 800)) ylab(0(100)800, grid) ///
 xtitle("Number of people accomodated") ///
 ytitle("Daily price (USD)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))

* beds
estpost tabstat price, by( n_beds) stat(count mean min max) columns(statistics) listwise
esttab using "$output/t_n_beds.rtf", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of beds")
esttab using "$output/t_n_beds.tex", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of beds")

gen ln_beds= ln(n_beds)

* bathrooms

gen     f_bathroom=1
replace f_bathroom=0 if n_bathroom<1
replace f_bathroom=2 if n_bathroom>=2
table f_bathroom, c(freq median price)

estpost tabstat price, by( f_bathroom) stat(count mean ) columns(statistics) listwise
esttab using "$output/t_n_bathrooms.rtf", cells("count mean(fmt(1)) ") replace title("T1: Price by number of bathrooms") addnotes("Categorical: 0: less than 1, 1: 1-2 , 2: More than 2") 
esttab using "$output/t_n_bathrooms.tex", cells("count mean(fmt(1)) ") replace title("T1: Price by number of bathrooms") addnotes("Categorical: 0: less than 1, 1: 1-2 , 2: More than 2")

* number of reviews
gen ln_number_of_reviews=ln( n_number_of_reviews+1)

gen f_number_of_reviews=1
replace f_number_of_reviews=0 if n_number_of_reviews==0
replace f_number_of_reviews=2 if n_number_of_reviews>50

table f_number_of_reviews, c(freq median price)
estpost tabstat price, by( f_number_of_reviews) stat(count mean ) columns(statistics) listwise
esttab using "$output/t_n_number_of_reviews.rtf", cells("count mean(fmt(1)) ") replace addnotes("Categorical: 0: zero reviews, 1: 1-50 review, 2: More than 50") title("T4: Price by number of reviews")
esttab using "$output/t_n_number_of_reviews.tex", cells("count mean(fmt(1)) ") replace addnotes("Categorical: 0: zero reviews, 1: 1-50 review, 2: More than 50") title("T4: Price by number of reviews")

reg lnprice i.f_number_of_reviews
outreg2 using "$output/t_n_number_of_reviews2", word tex replace
reg lnprice ln_number_of_reviews
outreg2 using "$output/t_n_number_of_reviews2", word tex append

* time since
gen ln_days_since= log(n_days_since)
gen ln_days_since2 = ln_days_since^2
reg lnprice ln_days_since*
gen ln_days_since3 = ln_days_since^3

gen n_days_since2=n_days_since^2
gen n_days_since3=n_days_since^3

* review score
* Graph not in the textbook
lowess price n_review_scores_rating , bwidth(0.8) ///
 lineopts(lw(vthick) lc(dkgreen)) ///
 ms(O)  mlw(thick) mcolor(navy*0.6) ///
 xlab(, grid) yscale(range(1 7)) ylab(, grid) ///
 xtitle("Review score") ///
 ytitle("Daily price (USD)") title("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))

gen ln_review_scores_rating =log(n_review_scores_rating )
reg lnprice n_review_scores_rating 
outreg2 using "$output/t6", word tex replace
reg lnprice ln_review_scores_rating 
outreg2 using "$output/t6", word tex append

* minimum nights
* Graph not in the textbook
lowess lnprice n_minimum_nights if n_minimum_nights <5, ms(O) mc(navy*0.6)

reg lnprice n_minimum_nights
gen f_minimum_nights=1
replace f_minimum_nights=2 if n_minimum_nights==2 |n_minimum_nights==3
replace f_minimum_nights=3 if n_minimum_nights>2
reg lnprice i.f_minimum_nights

* the most important qualitative variables
local varlist f_property_type f_room_type f_cancellation_policy f_bed_type 
foreach fval of local varlist {

estpost tabstat price, by( `fval') stat(count mean ) columns(statistics) listwise
esttab using "$output/t_`fval'.rtf", cells("count mean(fmt(1)) ") replace title("T1: Price by `fval'") 
esttab using "$output/t_`fval'.tex", cells("count mean(fmt(1)) ") replace title("T1: Price by `fval'") 
}


* price by room type: box plots
* Fig 14.4a
graph box price if price<300, over(f_room_type) nooutsides
graph export "$output/ch14-figure-4a-box-bytype-Stata.png",  replace
	
* price by size and property type: box plots
* Fig 14.4b
graph box price if price<400 , over(n_accommodates) by(f_property_type) ///
 nooutsides ylab(0(50)400)
graph export "$output/ch14-figure-4a-box-byaccom-Stata.png",  replace

 
* price by various feature variables: box plots
* Fig 14.5
lab def yesno 0 No 1 Yes
lab val d_family yesno
graph bar price, over(f_room) by(d_family) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Room type") ///
 title("By whether family friendly") 
graph export "$output/ch15-figure-5-bars-1-Stata.png",  replace

graph bar price, over(f_room) by(f_property) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Room type") ///
 title("By property type") 
graph export "$output/ch15-figure-5-bars-2-Stata.png",  replace
 
lab val d_family yesno
graph bar price, over(f_cancel) by(d_family) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Cancellation policy") ///
 title("By whether family friendly") 
graph export "$output/ch15-figure-5-bars-3-Stata.png",  replace

lab val d_tv yesno
graph bar price, over(f_cancel) by(d_tv) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Cancellation policy") ///
 title("By whether has TV") 
graph export "$output/ch15-figure-5-bars-4-Stata.png",  replace

lab val d_cats yesno
graph bar price, over(f_property) by(d_cats) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Property type") ///
 title("By whether cats are allowed") 
graph export "$output/ch15-figure-5-bars-5-Stata.png",  replace

lab val d_dogs yesno
graph bar price, over(f_property) by(d_dogs) ///
 bar(1, col(navy*0.8)) ylab(0(25)125) ///
 ytitle("Average price (US dollars)") b1title("Property type") ///
 title("By whether dogs are allowed") 
graph export "$output/ch15-figure-5-bars-6-Stata.png",  replace


save "$work/airbnb_hackney_workfile.dta" , replace
count
