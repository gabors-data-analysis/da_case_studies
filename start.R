# basix
install.packages("tidyverse")
install.packages("pacman")
library(tidyverse)
library(pacman)

#ch00
pacman::p_load(urca, sandwich, stargazer,stringr)
pacman::p_load(scales, data.table, knitr )
install.packages("devtools")

#Part I
pacman::p_load(lspline, cowplot, arm, pastecs, DataCombine, janitor)
pacman::p_load(haven, Hmisc, xtable, binsreg)

# Part II
pacman::p_load(viridis, grid, gridExtra, dyn, estimatr, huxtable, segmented)


#Part III
pacman::p_load(lmtest, caret, glmnet, skimr, directlabels, 
               prophet, timeDate, fpp3, aTSA, plotly)
pacman::p_load(glue, vctrs) # needed for FredR
devtools::install_github("sboysel/fredr")


# Part IV
p_load(MatchIt, Matching, gmodels)

