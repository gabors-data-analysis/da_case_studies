#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# To install libraries
# Using R 4.0.2
# version 0.8 2020-09-07


# NOT READY!!!

#########################################################################################



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
pacman::p_load(haven, Hmisc, xtable, binsreg, modelsummary)

# Part II
pacman::p_load(viridis, grid, gridExtra, dyn, estimatr, huxtable, segmented, rms)
pacman::p_load(mfx, margins, psych) 

#Part III
pacman::p_load(lmtest, caret, glmnet, skimr, directlabels, 
               prophet, timeDate, fpp3, aTSA, plotly)
pacman::p_load(glue, vctrs) # needed for FredR
devtools::install_github("sboysel/fredr")


# Part IV
p_load(MatchIt, Matching, gmodels)

