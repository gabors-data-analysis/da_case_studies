*******************************************
* Random walk series
* simulation exercies
global workdir "F:\"
global output  "$workdir\textbook_work\ch12\simulation\output"

clear
set seed 201806
set obs 100
global sde=1
global ystart=0

gen t=_n
tsset t

forvalue i=1/5 {
	gen y`i' = $ystart if t==1
	replace y`i' = y`i'[_n-1] + rnormal(0,$sde) if t>1
}

tsline y1 y2 y3 y4 y5, xlab(, grid) ylab(,grid) legend(off) ///
 lw(thick thick thick thick thick) lp(solid dash shortdash longdash dash) ///
 lc(olive dkgreen blue red gs10) ///
graphregion(fcolor(white) ifcolor(none)) 
graph export "$output\randomwalks.png", replace

