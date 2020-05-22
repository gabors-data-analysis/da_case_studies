*************************************
* Ch03
*
* simulation
* v1.0
*************************************

cd "C:/Users/GB/Box Sync/GaborsGuide_box"


*location folders

global output "textbook_work/ch03/simulations/output"


*clear environment
clear

*set the seed
set seed 16460

*sample size
global N=100000
set obs $N




* Bernoulli
gen bernoulli=rbinomial(1,0.7) 
hist bernoulli, ///
 xtitle("") ytitle("Percent") xlab(0 1) color("midblue") percent
 graph export "$output/F02_Bernoulli.png", replace
 more
 
* Binomial
* with smaller sample 
global Nbinom = 20
gen rbinomial=rbinomial($Nbinom,.4)
hist rbinomial, disc width(0.5)  ///
 xtitle("") ytitle("Percent") xlab("") color("midblue") percent
 graph export "$output/F02_binomial.png", replace
 more

* uniform [0,1]
gen runif=runiform(0,1)
hist runif,   ///
 xtitle("") ytitle("Percent") color("midblue") percent
 graph export "$output/F02_uniform.png", replace
 more
 
* noromal
gen rnormal=rnormal(0,1)
hist rnormal,  ///
 xtitle("") ytitle("Percent") xlab("") color("midblue") percent
 graph export "$output/F02_normal.png", replace
 more
 
* lognoromal
* take the exponential of the randomly generated normal above 
generate lognormal = exp(rnormal)
hist lognormal  if lognormal <10 , ///
 xtitle("") ytitle("Percent") xlab("") color("midblue") percent
 graph export "$output/F02_lognormal.png", replace
 more

 
* power-law
global alpha = 6
global xmin = 1
cap gen x = _n
cap drop powerlaw
generate powerlaw = $xmin*x^(-$alpha)
 sum powerlaw, d
 replace powerlaw = powerlaw/r(sum)
 local histrange = r(p75)
hist powerlaw if powerlaw < `histrange', ///
 xtitle("") ytitle("Percent") xlab("") color("midblue") percent
 graph export "$output/F02_powerlaw.png", replace
 
 sum powerlaw,d


