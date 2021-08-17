###
# SIMPLE SCRIPT TO TRY FEOLS AND ETABLE
#
# areguly
# 17/08/2021
rm( list = ls() )

install.packages(fixest)
library(fixest)
library(tidyverse)

# vars
N <- 1000
X <- rnorm( N )
Y <- 3 + 0.5* X + rnorm( N )
df <- tibble(X = X, Y=Y, id = 1, period = 1:N)


# Estimate with feols
lm1 <- feols(Y~X,df)
# Give summary
summary( lm1 )
# Estimate Heteroskedastic robust se
summary( lm1 , "hetero" )

# Report 1
etable(lm1, subtitles = c("OLS"))

# Report 2
etable(lm1, lm1 , subtitles = c("OLS1", "OLS2"))

# Binary models
df <- df %>% mutate( Y2 = 1 * ( Y > 3 ) + 0 * (Y <= 3) )

# Linear probability model
lm2 <- feols( Y2 ~ X , df )
etable( lm2 )

# Estimate Newey-west se
lm2 = feols(Y ~ X, df, panel.id = ~id + period )
summary( lm2 , "newey_west" )
