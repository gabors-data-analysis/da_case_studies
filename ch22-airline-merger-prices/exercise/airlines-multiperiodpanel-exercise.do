clear

***************************************************************************
* market = origin X final destination 
*  (note final destination is:
*	airport at end of one-way routes if 4 or fewer
*	airport in middle of return routes if there is middle & 9 or fewer




**************************************************************************
* CREATE Workfile : only before and after period

global data "C:\Users\kezdi\Dropbox\bekes_kezdi_textbook\da_data_repo\airline-tickets-usa"

use "$data\originfinal-panel",replace

* before = 2010 or 2011 
* after  = 2016 

gen after = year>=2016
 replace after = 1 if year==2015 & quarter==4
gen before = year<=2011

gen AA_or_US = shareAA>0 | shareUS>0
gen AAUSinprogress = AA_or_US==1 & before==0 & after==0
gen AAUSmerged = AA_or_US==1 & after==1


* create numeric ID for market
sort origin finaldest
egen market = group(origin finaldest return)
order market
sort market year
lab var market "Market ID"

* create time variable
gen yq = yq(year,quarter)
format yq %tq

* creata indicator of balanced panel
egen countyq=count(yq), by(market)
sum countyq
gen balancedpanel = countyq==r(max)

* passengers before to use as weight
egen temp = mean(passengers) if before, by(market)
egen pass_before = max(temp), by(market)
sum pass_bef

* tell Stata it's xt data with quarterly frequency
xtset market yq

tab year AAUSinprogress
tab year AAUSmerged


* FE regression
xtreg avgprice AAUSinprogress AAUSmerged i.quarter i.year, fe cluster(market)
xtreg avgprice AAUSinprogress AAUSmerged i.quarter i.year [w=pass_before], fe cluster(market)

xtreg avgprice AAUSinprogress AAUSmerged i.quarter i.year [w=pass_before] if balancedpanel, fe cluster(market)
