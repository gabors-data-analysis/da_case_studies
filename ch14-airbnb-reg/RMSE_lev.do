*COMPUTE RMSE 
*in-sample and out-of-sample
* Y in levels

** RMSE_in
global RMSEin=e(rmse)
global MSEin=($RMSEin)^2
** RMSE_test
cap drop ytest
predict ytest if test==1
cap drop e 
predict e if test==1, resid
cap drop esq
gen esq=e^2
sum esq
global MSEtest=r(mean)
global RMSEtest=sqrt($MSEtest)
