***************************************************************
* Data exercise idea
* select country try double lasso
use "$data_out/Ch21_wms_workfile.dta" ,clear
gen degree_nm_sq = degree_nm^2

global RHS1 degree_nm degree_nm_sq compet_moder compet_strong



* New Zealand
keep if cty=="nz"

reg management foundfam_own , robust
 outreg2 using Ch21_foundfam_reg_nz, dec(2) tex replace

reg management foundfam $RHS1 i.industry , robust
 outreg2 using Ch21_foundfam_reg_nz, dec(2) tex append

 
** Double Lasso
cvlasso management $RHS1 i.industry, seed(24367) lopt 
cvlasso foundfam $RHS1 i.industry , seed(24367) lopt

qui tab industry, gen(i)

reg management foundfam degree_nm degree_nm_sq compet_moder ///
 i1 i3 i5 i6 i8 i10 i12 i13 i14 i16 i17 i19, robust
 outreg2 using Ch21_foundfam_reg_nz, dec(2) tex append


