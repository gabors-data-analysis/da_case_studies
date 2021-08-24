###
# SIMPLE SCRIPT TO TRY FEOLS AND ETABLE
# I use multiple model types:
#   1) Simple xsec
#       - simple estimation w feols
#       - using heteroscedastic robust se (HCE-1)
#   2) Multivariate xsec
#   3) Hypothesis testing with 2)
#        - b2 := 0
#        - b1 := 0.5 (linearHypothesis from car package)
#        - b1 := b_2 (linearHypothesis)
#   4) Binary models 
#       - Different models: linear, logit, probit
#       - Model stats: SE, Squared correlation (~R2), Pseudo R2, BIC
#       - Marginal effects -> only mfx works, !not fixest methods!
#   5) Time series models
#       - simple estimation w feols
#       - Newey-West SE - !!!does not work!!!!
#       - Lags - works for 1 lag, but finds collinearity in case of multiple lags
#       - Differences ??
#       - Tests (unit root) ??
#       - Sesonality ??
#   
#
# areguly
# 17/08/2021
#
# Notes: linearHypothesis uses a Chi2 is there a good package to use a t-test?
#
# install_github("lrberge/fixest")

rm( list = ls() )

#install.packages(fixest)
library(fixest)
library(tidyverse)

####
# Create variables
N <- 1000
K <- 2
X <- matrix( rnorm( N * K ) , nrow = N , ncol = K )
# Simple model: intercept + 0.5 * X1 + noise
Y <- 3 + 0.5* X[ , 1 ] + rnorm( N )
# Create a tibble object and add the covariates
df <- tibble( Y = Y )
df <- df %>% mutate( as_tibble( X ) )
# Rename the covariates
colnames( df ) <- gsub("V","X",colnames( df ) )


####
# 1) Estimate simple XSEC with feols
lm1 <- feols(Y ~ X1 , df )
# Give summary
summary( lm1 )
# Estimate Heteroskedastic robust se
summary( lm1 , "hetero" )
# Report 1
etable(lm1, subtitles = c("OLS"))

###
# 2) Estimate multivariate XSEC with feols
lm2 <- feols( Y ~ X1 + X2 , df )
# Give a summary
summary( lm2 )
# Give a comparison with model lm1
etable(lm1, lm2 , subtitles = c("Simple","Multiple"))


###
# 3) Hypothesis testing
# Beta_2 := 0 - simply read out from summary
summary( lm2 )
# Beta_1 := 0.5
library( car )
linearHypothesis( lm2 , "X1 = 0.5")
# Beta_1 - Beta_2 := 0
linearHypothesis( lm2 , "X1 = X2")

###
# 4) Binary models
df <- df %>% mutate( Y2 = 1 * ( Y > 3 ) + 0 * (Y <= 3) )

# Linear probability model
lm_lp <- feols( Y2 ~ X1 , df )
etable( lm_lp )

# Logit model
lm_lg <- feglm( Y2 ~ X1 , df , family = binomial( link="logit" ) )
summary( lm_lg )

# Probit model
lm_pb <- feglm( Y2 ~ X1 , df , family = binomial( link="probit" ) )
summary( lm_pb )

# Compare the tree models
etable( lm_lp , lm_lg , lm_pb , 
        subtitles = c("Linear","Logit","Probit") )

# Create marginal effects:

# Within fixest it does not work...
url <- 'https://raw.githubusercontent.com/leeper/margins/master/R/find_terms_in_model.R'
source(url)
#library(margins)
margins.fixest(lm_lg)

# With mfx package + model summary it works, but not as neatly...
library(mfx)
me_lg <- mfx::logitmfx(Y2 ~ X1 , data = df)
me_pb <- mfx::probitmfx(Y2 ~ X1 , data = df)

library(modelsummary)
models <- list( "Linear" <- lm_lp , 
                "Logit"  <- me_lg , 
                "Probit" <- me_pb )
modelsummary( models )

###
# 5) Time series models

# Simple OLS with normal SE
ts_1 = feols(Y ~ X1, df )
summary( ts_1 )

# Estimate Newey-West SE:
# First add identifiers: period and ids
df <- df %>% mutate( period = 1 : N , id = 1 )
ts_2 <- feols(Y ~ X1, df, panel.id = ~id + period )
summary( ts_2 , "newey-west" )
etable(ts_2 , se = "NW")

# Lags: Y_1
ts_3 <- feols( Y ~ lag( Y , 1 ) , df )
summary( ts_3 )

# Lags: Y_1 to Y_3 ???
ts_4 <- feols( Y ~ lag( Y , 1 ) + lag( Y , 2 ) + lag( Y , 3 ) , df, 
               panel.id = ~id + period )
summary( ts_4 )

# Lags with X
ts_5 <- feols( Y ~ X1 + lag( X1 , 1 ) , df , panel.id = ~id + period )
summary( ts_5 )

# Lags with Y and X
ts_6 <- feols( Y ~ lag( Y , 1 ) + X1 + lag( X1 , 1 ) , df , panel.id = ~id + period )
summary( ts_6 )

# Summary
etable( ts_1 , ts_2 , ts_3 , ts_4 , ts_5 , ts_6 )
