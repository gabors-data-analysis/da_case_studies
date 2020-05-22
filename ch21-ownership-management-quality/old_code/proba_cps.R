rm(list=ls())


library(hdm)
data(cps2012)
X <- model.matrix(~-1 + female + female:(widowed + divorced + separated + nevermarried +hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3) + +(widowed +divorced + separated + nevermarried + hsd08 + hsd911 + hsg + cg + ad + mw + so +we + exp1 + exp2 + exp3)^2, data = cps2012)
dim(X)
## [1] 29217   136
X <- X[, which(apply(X, 2, var) != 0)]
# exclude all constant variables
dim(X)
## [1] 29217   116
index.gender <- grep("female", colnames(X))
y <- cps2012$lnw



ols1 <- lm(y~female+widowed + divorced + separated + nevermarried +hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3, data=cps2012)
summary(ols1)

effects.female <- rlassoEffects(x = X, y = y, index = index.gender)
summary(effects.female)