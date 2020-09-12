*********************************************************************
*
* DATA ANALYSIS TEXTBOOK
* MODEL SELECTION
* ILLUSTRATION STUDY
* BAirbnb London 2017 march 05 data
*
*********************************************************************

* WHAT THIS CODES DOES:

* Transform variables and filter dataset
* Generate new features

*********************************************************************

********************************************************************
* SET YOUR DIRECTORY HERE
*********************************************************************
*cd "" /*set your dir*/
cap cd "D:\Dropbox (MTA KRTK)\bekes_kezdi_textbook"

cap cd "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
 * YOU WILL NEED TWO SUBDIRECTORIES
 * textbook_work --- all the codes
 * cases_studies_public --- for the data
 

global data_in	 "cases_studies_public/airbnb/raw" 
global output    "textbook_work\ch14\used-cars\output"
global data_out	 "textbook_work\ch14\used-cars" 

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



* ######
* MISSINF BIT, only in R
* from listing to cleaned
*
*
* ######
*/
* load in data
clear
import delimited "$data_in/airbnb_london_cleaned.csv", bindquote(strict) 


* factor vars
* look at categoricals
tab property_type
keep if inlist(property_type, "Apartment", "Townhouse", "House" ) 
replace property_type="House" if property_type=="Townhouse"
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


* numerical
rename price usd_price_day
destring usd_price_day, gen(price) ignore(",")
rename cleaning_fee usd_cleaning_fee 

destring host_response_rate, gen (p_host_response_rate) force

tab accommodates
destring accommodates, gen (n_accommodates) force

tab bathrooms
destring bathrooms, gen(n_bathrooms) force


destring review_scores_rating, force gen(n_review_scores_rating)
destring number_of_reviews   , force gen(n_number_of_reviews)
destring guests_included     , force gen(n_guests_included)

tab first_review
destring reviews_per_month  , force gen (n_reviews_per_month)

* create days since first review
gen n_days_since= date(calendar_last_scraped,"YMD")-date(first_review,"YMD")

* rename dummy vars. may need to install renvars
cap net install dm88_1.pkg
renvars extra_people minimum_nights beds, prefix (n_)

* dummy vars
renvars hourcheckin - wirelessinternet, prefix(d_)

keep price d_* n_* f_* p_* usd_* neighbourhood_cleansed neighbourhood_cleansed cancellation_policy room_type property_type

order price n_* p_* f_* d_*

save "$data_out/airbnb_london_workfile.dta", replace
export delimited using "$data_out/airbnb_london_workfile.csv", replace




* load in data
use "$data_out/airbnb_london_workfile.dta" , clear

* work with Hackney only
keep if neighbourhood_cleansed=="Hackney"
save "$data_out/airbnb_hackney_workfile.dta" , replace
export delimited using "$data_out/airbnb_hackney_workfile.csv", replace
count

*********************
* look at price
*********************
su price, d
gen ln_price=log(price)

* check why some place are so expensive
drop if price>1000
count
hist price

hist ln_price, width(0.25) color(emidblue) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/F14_h_lnprice.png", as(png) replace

 hist price, color(emidblue) lcolor(white) ///
graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
 graph export "$output/F14_h_price.png", as(png) replace

 
*****************************************
* look at some key vars, functional form
*****************************************
		        
estpost tabstat price, by( n_accommodates ) stat(count mean min max) columns(statistics) listwise
esttab using "$output/t_n_accommodates.rtf", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of people accomodated")
esttab using "$output/t_n_accommodates.tex", cells("count mean(fmt(1)) min max") replace title("T1: Price by number of people accomodated")

gen n_accommodates2= n_accommodates^2
reg ln_price n_accommodates n_accommodates2 

gen ln_accommodates=ln(n_accommodates)
gen ln_accommodates2= ln_accommodates^2
reg ln_price ln_accommodates 
reg ln_price n_accommodates 

 
*** lowess with scatterplot
lowess price n_accommodates if price<=800, bwidth(0.8) ///
 lineopts(lw(vthick) lc(dkgreen)) ///
 ms(X) msize(vlarge) mlw(thick) mcolor(orange) ///
 xlab(0(1)18) yscale(range(0 800)) ylab(0(100)800, grid) ///
 xtitle("Number of people accomodated") ///
 ytitle("Daily price (USD)") title("") note("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F14_l_n_accommodates.png", as(png) replace

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

reg ln_price i.f_number_of_reviews
outreg2 using "$output/t_n_number_of_reviews2", word tex replace
reg ln_price ln_number_of_reviews
outreg2 using "$output/t_n_number_of_reviews2", word tex append

* time since
gen ln_days_since= log(n_days_since)
gen ln_days_since2 = ln_days_since^2
reg ln_price ln_days_since*
gen ln_days_since3 = ln_days_since^3

gen n_days_since2=n_days_since^2
gen n_days_since3=n_days_since^3

* review score
lowess price n_review_scores_rating , bwidth(0.8) ///
 lineopts(lw(vthick) lc(dkgreen)) ///
 ms(X) msize(vlarge) mlw(thick) mcolor(orange) ///
 xlab(, grid) yscale(range(1 7)) ylab(, grid) ///
 xtitle("Review score") ///
 ytitle("Daily price (USD)") title("") ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "$output/F14_l_p_n_review_scores_rating.png", as(png) replace

gen ln_review_scores_rating =log(n_review_scores_rating )
reg ln_price n_review_scores_rating 
outreg2 using "$output/t6", word tex replace
reg ln_price ln_review_scores_rating 
outreg2 using "$output/t6", word tex append

* minimum nights
lowess ln_price n_minimum_nights if n_minimum_nights <5

reg ln_price n_minimum_nights
gen f_minimum_nights=1
replace f_minimum_nights=2 if n_minimum_nights==2 |n_minimum_nights==3
replace f_minimum_nights=3 if n_minimum_nights>2
reg ln_price i.f_minimum_nights

*********************
* look at categoricals
*********************

local varlist f_property_type f_room_type f_cancellation_policy f_bed_type 
foreach fval of local varlist {

estpost tabstat price, by( `fval') stat(count mean ) columns(statistics) listwise
esttab using "$output/t_`fval'.rtf", cells("count mean(fmt(1)) ") replace title("T1: Price by `fval'") 
esttab using "$output/t_`fval'.tex", cells("count mean(fmt(1)) ") replace title("T1: Price by `fval'") 
}

**************************************************************


save                   "$data_out/airbnb_hackney_workfile_adj.dta" , replace
export delimited using "$data_out/airbnb_hackney_workfile_adj.csv", replace
