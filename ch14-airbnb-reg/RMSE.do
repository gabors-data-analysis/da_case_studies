 cap drop e 
 predict e if e(sample), resid
 cap drop esq
 gen esq=e^2
 sum esq
 local MSE=r(mean)
 global RMSEin=sqrt(`MSE')
 cap drop e 
 predict e if test==1, resid
 cap drop esq
 gen esq=e^2
 sum esq
 local MSE=r(mean)
 global RMSEout=sqrt(`MSE')
