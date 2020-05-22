*COMPUTE RMSE 
*in-sample and out-of-sample
* Y in logs

cap drop sig
local sig=e(rmse)
cap drop lny*
predict lnyin if e(sample)
predict lnytest if test==1
sum lny*



* IN-SAMPLE
* prediction in level
cap drop yin
gen yin=exp(lnyin)*exp(`sig'^2/2)
* here we show that the log correction makes things a lot better
sum Ylev yin
qui sum lnyin
dis exp(r(mean))
* here we create the residual in levels and the corresponding RMSE
cap drop e 
gen e=Ylev-yin
cap drop esq
gen esq=e^2
sum esq
global MSEin=r(mean)
global RMSEin=sqrt($MSEin)

* TEST SAMPLE
* prediction in level
cap drop ytest
gen ytest=exp(lnytest)*exp(`sig'^2/2)
* here we show that the log correction makes things a lot better
sum Ylev ytest if test==1
qui sum lnytest if test==1
dis exp(r(mean))
* here we create the residual in levels and the corresponding RMSE
cap drop e 
gen e=Ylev-ytest if test==1
cap drop esq
gen esq=e^2
sum esq if test==1
global MSEtest=r(mean)
global RMSEtest=sqrt($MSEtest)
