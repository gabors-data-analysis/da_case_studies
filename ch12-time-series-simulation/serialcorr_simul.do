*******************************************
* Serially correlated vs serially uncorrelated series
* simulation exercies
global workdir "F:\"
global output  "$workdir\textbook_work\ch12\simulation\output"

clear
set seed 2016
set obs 100
global rho=0.8
global sde=0.5

gen t=_n
tsset t

** serially uncorrelated series
gen y1=rnormal(0,$sde)
 lab var y1 "simulated time series, no serial correlation"

tsline y1, lw(thick) lc(green) yline(0) xlabel(, grid) ylabel(, grid) xtitle("") ///
graphregion(fcolor(white) ifcolor(none))
graph export "$output\serialcorr_whitenoise.png", replace


** serially uncorrelated series
gen y2=0 if t==1
 lab var y2 "simulated time series, serial correlation = 0$rho"
 replace y2=$rho*y2[_n-1] + rnormal(0,$sde) if t>1

tsline y2, lw(thick) lc(red) yline(0) xlabel(, grid) ylabel(, grid) xtitle("") ///
graphregion(fcolor(white) ifcolor(none))
graph export "$output\serialcorr_corr08.png", replace

